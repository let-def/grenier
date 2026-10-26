(** {1 Type of balanced trees with a lazy measure}

    Like {!Mbt}, each node can hold a measure of its subtree, but the tree
    never computes it: it is a cache that users fill in, with {!set_measure},
    when they need it. Each node built by {!node} or {!join}, including those
    rebuilt by rebalancing, starts without a measure ([None]). Nodes that
    rebalancing leaves untouched keep theirs, which stays valid since their
    subtree does not change.
*)
type (+'a, 'b) t = private
  | Leaf
  | Node of { size: int; l: ('a, 'b) t; x: 'a; r: ('a, 'b) t;
              mutable m: 'b option }
  (** [Node {size; l; x; r; m}] where:
      - [size] is the number of elements in the tree,
      - [l] is the left sub-tree
      - [x] is a user-defined value
      - [r] is the right sub-tree
      - [m] is the measure of the tree, if it has been set.

      Trees are guaranteed balanced by construction, the depth of all branches
      is O(log [size]).
  *)

(** Leaf constructor, the empty tree *)
val leaf : ('a, 'b) t

(** Smart Node constructor, ensuring that the resulting tree is balanced and
    has the appropriate size.

    Cost of [node l x r] is expected to be
    O(log (max [size l] [size r] / min [size l] [size r])) amortized, i.e
    proportional to the logarithm of the disbalance.
    In particular, if [l] and [r] are similarly-sized, it operates in constant
    time on average.
    NOT PROVEN

    User-values can be moved in different subtrees of the result, but the
    ordering is preserved (so data stay correct if the operation applied on
    values is associative or the relation expected between them is transitive).
*)
val node : ('a, 'b) t -> 'a -> ('a, 'b) t -> ('a, 'b) t

(** {1 Convenience functions} *)

(** Accessor to the size *)
val size : ('a, 'b) t -> int

(** Concatenate two trees.
    Cost of [join l r] is O(log (max [size l] [size r])).
    NOT PROVEN
*)
val join : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(** Return the n'th node in tree order *)
val rank : int -> ('a, 'b) t -> 'a

(** {1 Measure} *)

(** The measure of a tree, if it has been set; [None] for [Leaf]. *)
val measure : ('a, 'b) t -> 'b option

(** [set_measure t m] stores [m] as the measure of the root of [t]. It does
    nothing on [Leaf]. *)
val set_measure : ('a, 'b) t -> 'b -> unit
