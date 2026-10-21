# physh on OCaml 5: runtime findings

Notes from stress-testing physh against OCaml 5.5.1. The line numbers refer
to `runtime/` in the 5.5.1 sources.

physh hashes objects by address, so it has to know when addresses may have
changed:

- **minor table** (young keys): stale after any minor collection
- **major table** (old keys): stale after any compaction

The OCaml 4 code detected both with GC counters. Neither counter works as-is
on OCaml 5.

## 1. `caml_stat_compactions` is always 0

`caml/gc_ctrl.h:33`:

```c
#define caml_stat_compactions 0
```

The macro is a compatibility shim from 5.0, which had no compaction. Compaction came back
in 5.2 (#12193), but the shim stayed. Code that uses it compiles fine and
never sees a compaction.

The real counter is `caml_compactions_count` (`caml/shared_heap.h:29`,
`CAML_INTERNALS` only). `caml_compact_heap` increments it at the end
(`shared_heap.c:1534`).

**Impact on physh:** after `Gc.compact ()`, the major table was never
rehashed. In a test with 6250 promoted keys, 6122 were moved and
**6115 could no longer be found**.

**Race check:** the increment is done by the STW leader only. A
`caml_global_barrier` follows it (`major_gc.c`, after
`caml_compact_heap`), so no domain resumes before the count is updated. The
counter is safe to use. A compaction started by *another* domain can still
happen in the middle of a C stub, wherever that stub allocates (see 3).

**Fix:** use `atomic_load(&caml_compactions_count)` when
`OCAML_VERSION >= 50200`, and `0` on 5.0/5.1.

## 2. `caml_minor_collections_count` is bumped before the other domains stop

The counter is incremented in `caml_empty_minor_heap_setup`
(`minor_gc.c:880`). That function is the `leader_setup` callback of
`caml_try_run_on_all_domains_with_spin_work`. It runs **before the other
domains are interrupted** (`domain.c:1885`, with the interrupt loop just
after it).

So another domain can still be running mutator/C code and see the new count
while its minor heap is still intact. For physh this meant:

- the minor table looks stale, so `flush_minor` meets young keys
  (`Assertion '!value_is_young(k)' failed`), or
- worse, the table gets marked valid with the *new* count and young keys
  are inserted. Then the collection actually runs, the count does not
  change again, and the table holds stale addresses with no way to notice.
  With `NDEBUG`, lookups silently fail and the length is wrong.

Reproduced with one domain doing physh operations while another domain
allocates in a loop: 6/6 runs aborted.

In short, the counter means "a minor GC has been *requested/started*", not
"has *completed*". It can't be used as an epoch by code running on other
domains.

**Fix (no runtime support needed):** each minor table keeps a *witness*,
one of its keys, stored in a field the GC scans. A minor collection
promotes every live young object at once, so:

```
minor table is valid  <=>  table empty || Is_young(witness)
```

Domains only stop at allocation/poll points, and the GC rewrites the
witness field along with the table, so this check is race-free. It also
works on OCaml 4, so the minor counter is gone.

## 3. Allocating from C can run a GC even when the minor heap has room

The old code assumed that `caml_alloc_small` cannot trigger a GC if
`young_ptr - size >= young_start`. On OCaml 5 that is false.

`caml_alloc_small` enters `caml_alloc_small_dispatch` whenever
`young_ptr < young_limit`. `young_limit` is raised for any pending
interrupt. From C, the dispatch calls `caml_handle_gc_interrupt`
(`minor_gc.c:1039`), which:

- handles incoming STW requests: **minor GC or compaction started by
  another domain**,
- calls `caml_poll_gc_work` (`domain.c:2094`). That function runs any requested minor
  GC. It also forces one when `young_ptr` is within `Max_young_wosize` words
  of `young_trigger`, not `young_start`.

Ways a minor GC gets requested without the minor heap being full:

- another domain (minor GCs are stop-the-world),
- ref table threshold crossed (`caml_realloc_ref_table` →
  `caml_request_minor_gc`, `minor_gc.c:1111`). `Store_field` of a young
  key into an old table can cause this,
- custom block / bigarray memory pressure.

Allocations from C do **not** run OCaml callbacks (signals, finalisers,
memprof callbacks are delayed), so there is no reentrancy risk.

**Fix:** every operation loops: `table_sync` (rehash/flush until both tables
are valid), then try the operation. Anything that allocates (`grow_minor`,
`rehash_major`) can return early. The caller then starts over. The last
check and the update happen with no allocation in between. `rehash_major`
reads the old table *after* allocating, so a compaction during that
allocation is fine.

The same loop fixes a duplicate-key bug: a young key already in the minor
table could get promoted by a GC inside `realloc_minor`. The old code would
then add it to the major table a second time.

## Other observations

- **Hash function.** `(n + c) * 2654435761 & mask` keeps the *low* bits of
  the product. Those only depend on the low bits of `n`, which are constant
  for aligned pointers. Measured average probe length at load ≤ 3/4: 3.6–4.4
  for pointer keys. Replaced with Fibonacci hashing (top bits of
  `n * 0x9E3779B97F4A7C15`): about 1.0–1.4 probes, and lookups/inserts on
  old keys are 20–25% faster.
- **Minor table is capped at `Max_young_wosize` words.** That is 128 slots / 96 entries
  for Map and 192 for Set. Inserting the 97th young key forces a minor GC: 1M
  inserts of fresh keys caused **10320 minor collections**. This was not changed. It
  could be fixed by letting the minor table grow in the major heap. That costs
  ref-table entries, but the store barrier pays that cost anyway once the
  table has been promoted.
- **Multi-domain:** each table must stay on one domain (no locking). Tables
  on different domains work: `Is_young` covers all minor heaps, and both
  minor GC and compaction are STW.
