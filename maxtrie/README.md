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
  sets use about one node per 63 elements. `PatSet.Sharing.{equal,compare}`
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

## Testing

    dune runtest maxtrie                  # the differential testers
    SEED=42 ROUNDS=2000 dune exec maxtrie/test/test_set.exe
    SEED=42 ROUNDS=2000 dune exec maxtrie/test/test_map.exe

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
