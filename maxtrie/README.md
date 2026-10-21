# maxtrie

Immutable sets and maps over non-negative integers, represented as Patricia
tries whose nodes *hoist the maximum key* of their subtree.

A node `{k; v; l; r}` stores the largest key `k` of its subtree. With `m` the
most significant bit of `k`, the right child `r` holds the keys below `m`,
and the left child `l` holds the keys in `[m, k)` with bit `m` cleared. There
are no branching-only nodes: a structure with `n` keys has exactly `n` nodes,
and the maximum is at the root. The representation is canonical, and the depth
is bounded by the word size.

- `Maxtrie.PatSet`: sets of `int`. Each key carries a 63-bit bitmap, so dense
  sets use about one node per 63 elements (31 under js_of_ocaml). `PatSet.Sharing.{equal,compare}`
  also make structurally equal subtrees physically shared.
- `Maxtrie.PatMap`: maps from `int`, one binding per node, with a
  `Stdlib.Map.S`-like interface. Immutable, with a covariant `+'a t`.
  `PatMap.changes f m1 m2` reports the keys whose bindings are not
  physically equal, in increasing order, skipping shared subtrees: after a few
  updates on a map of 1M bindings it takes about a microsecond, even when an
  update shifts the root spine.
- `Maxtrie.PatMapSharing`: the same maps with mutable children, plus
  `Sharing.{equal,compare}`, which unify equal subtrees. Its type parameter is
  invariant. These comparisons must relate only interchangeable values. The
  module is generated from `PatMap.ml`/`.mli` by a dune rule, which makes the
  children mutable and appends `patMap_sharing.{ml,mli}.in`.

## Carried keys

When two trees whose roots share their most significant bit are merged, the
smaller root has to move into the upper half of the result. `union` and `diff`
carry it into the recursion instead of inserting it afterwards. When several
keys must be carried at once, they always form a segment of a left spine of an
input, so they are described without allocation as `(head, stop, tail?)` plus
a mask shared by both operands ("virtual trees", see `PatSet.ml`). As a
result, `union` allocates essentially only the nodes of the result that are
not shared with its inputs.

The boolean operations (`subset`, `disjoint`) keep a separate `lookup` for the
smaller root: carrying it was measured to be 20-55% slower, because these
operations do not allocate and the lookup is a tight loop.

## Representation of PatSet

`PatSet.t` is a sum type, `Empty | Node {kk; v; mutable l; mutable r}`.
The field `kk` packs the key in its low bits with the index of its most
significant bit (plus one) in the 6 high bits (5 on 32-bit integers), so the
branching bit is computed by shifts instead of a C call. The measured
alternatives, relative to a heap-allocated empty sentinel:

- the sum type makes the major GC 15-30% cheaper (no sentinel to scan through
  every leaf) and `cardinal` and large unions about 10% faster;
- caching the msb makes membership 3-11% faster, `remove` and `fold` on small
  sets 15-30% faster; computing the msb in OCaml instead is 20-35% slower;
- putting the cached index in the low bits instead is no faster, because
  untagging both fields costs more than masking;
- a statically allocated sentinel (`Obj.magic` of a constant) does not help:
  the OCaml 5 major GC still reads the header of static blocks.

Combined, no operation measured more than 4% slower than the sentinel version.

## Testing

    dune runtest maxtrie                  # the differential testers
    SEED=42 ROUNDS=2000 dune exec maxtrie/test/test_set.exe
    SEED=42 ROUNDS=2000 dune exec maxtrie/test/test_map.exe
    # under js_of_ocaml (32-bit integers); JS defaults to node
    JS=gjs dune build --build-dir _build_js @maxtrie/test/js/runtest-js

The testers run random operation sequences against `Stdlib.Set`/`Stdlib.Map`
over several key distributions (dense, sparse, clustered, near powers of two,
near `max_int`). After each operation they check the invariants and the
physical-equality guarantees documented in the interfaces.

## Benchmarks

    dune build --profile release maxtrie/bench
    ./_build/default/maxtrie/bench/bench.exe
    ./_build/default/maxtrie/bench/bench_map.exe
    ./_build/default/maxtrie/bench/bench_sharing.exe   # many equal maps/sets
    ./_build/default/maxtrie/bench/bench_changes.exe

`bench/orig.ml` is the original `bitsets/src/PatSet.ml`, kept unchanged for
comparison. Use the release profile: the dev profile passes `-opaque`, which
disables cross-module inlining.
