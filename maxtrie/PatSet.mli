(**Immutable sets of non-negative integers.

   Sets are represented as Patricia tries in which every node hoists the
   largest key of its subtree and carries a bitmap of [Sys.word_size - 1]
   consecutive elements. A set with [n] distinct bitmap keys has exactly [n]
   nodes, there are no internal branching nodes, and the depth of a tree is
   bounded by the number of bits of an integer.

   The representation is canonical: two sets are equal if and only if they
   are structurally equal. The maximum of a set is at the root of its tree.

   Elements must be non-negative: [add] and [singleton] raise
   [Invalid_argument] on a negative element; the other operations treat
   negative elements as absent. *)

type elt = int

(**The type of sets. *)
type t

(** {1 Construction} *)

(**[empty] is the empty set. *)
val empty: t

(**[singleton x] is a singleton set containing just the element [x]. *)
val singleton: elt -> t

(**[add x s] is the union of [singleton x] and [s]. If [x] is already in
   [s], then [s] itself is returned. *)
val add: elt -> t -> t

(**[remove x s] is the set [s] deprived of the element [x]. If [x] is not in
   [s], then [s] itself is returned. *)
val remove: elt -> t -> t

(**[union s1 s2] is the union of the sets [s1] and [s2]. If the union is
   equal to [s2], then [s2] itself is returned. *)
val union: t -> t -> t

(**[inter s1 s2] is the intersection of the sets [s1] and [s2]. If the
   result is equal to [s1], then [s1] itself is returned. *)
val inter: t -> t -> t

(**[diff s1 s2] is the set difference of [s1] and [s2]. If the result is
   equal to [s1], then [s1] itself is returned. *)
val diff: t -> t -> t

(**[above x s] is the set of the elements of [s] that are strictly greater
   than [x]. *)
val above: elt -> t -> t

(**[below x s] is the set of the elements of [s] that are strictly less
   than [x]. *)
val below: elt -> t -> t

(**[at_least x s] is the set of the elements of [s] that are greater than
   or equal to [x]. *)
val at_least: elt -> t -> t

(**[at_most x s] is the set of the elements of [s] that are less than or
   equal to [x]. *)
val at_most: elt -> t -> t

(**[split x s] is [(below x s, mem x s, above x s)]. *)
val split: elt -> t -> t * bool * t

(**[filter p s] is the set of the elements of [s] that satisfy [p]. The
   predicate is applied to the elements in increasing order. If every
   element satisfies [p], then [s] itself is returned. *)
val filter: (elt -> bool) -> t -> t

(**[partition p s] is [(filter p s, filter (fun x -> not (p x)) s)]. *)
val partition: (elt -> bool) -> t -> t * t

(**[of_list xs] is the set of the elements of the list [xs]. *)
val of_list: elt list -> t

(**[of_seq xs] is the set of the elements of the sequence [xs]. *)
val of_seq: elt Seq.t -> t

(**[add_seq xs s] adds the elements of the sequence [xs] to [s]. *)
val add_seq: elt Seq.t -> t -> t

(** {1 Cardinality} *)

(**[is_empty s] determines whether [s] is empty. Constant time. *)
val is_empty: t -> bool

(**[is_singleton s] tests whether [s] is a singleton set. Constant time. *)
val is_singleton: t -> bool

(**[cardinal s] is the cardinal of the set [s]. *)
val cardinal: t -> int

(** {1 Tests} *)

(**[mem x s] determines whether [x] is a member of the set [s]. *)
val mem: elt -> t -> bool

(**[equal s1 s2] determines whether the sets [s1] and [s2] are equal. *)
val equal: t -> t -> bool

(**[compare] is a total order on sets, compatible with [equal]. It is a
   structural order, not the lexicographic order on sorted elements. *)
val compare: t -> t -> int

(**Comparisons with sharing. [Sharing.equal] and [Sharing.compare] return
   the same results as [equal] and [compare], but as a side effect they make
   the structurally equal subtrees of their arguments physically equal. This
   makes later comparisons of these sets faster and reduces memory usage when
   many equal sets are built independently. It is invisible to every other
   operation, and safe in the presence of concurrent domains. *)
module Sharing : sig
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

(**[disjoint s1 s2] determines whether [s1] and [s2] are disjoint. *)
val disjoint: t -> t -> bool

(**[subset s1 s2] determines whether [s1] is a subset of [s2]. *)
val subset: t -> t -> bool

(**[quick_subset s1 s2] is a fast test for the property [s1 <> ∅ ∧ s1 ⊆ s2],
   under the precondition [s1 ⊆ s2 ∨ s1 ∩ s2 = ∅]. Under this precondition,
   it is equivalent to [not (disjoint s1 s2)]. Its cost is that of [mem]. *)
val quick_subset: t -> t -> bool

(** {1 Extraction} *)

(**[minimum s] is the least element of [s].
   @raise Not_found if [s] is empty. *)
val minimum: t -> elt

(**[maximum s] is the greatest element of [s]. Constant time.
   @raise Not_found if [s] is empty. *)
val maximum: t -> elt

(**[choose s] is an element of [s] (currently its maximum, which is the
   cheapest one to find).
   @raise Not_found if [s] is empty. *)
val choose: t -> elt

val min_elt_opt: t -> elt option
val max_elt_opt: t -> elt option

(** {1 Iteration} *)

(**[iter f s] applies [f] to the elements of [s] in increasing order. *)
val iter: (elt -> unit) -> t -> unit

(**[rev_iter f s] applies [f] to the elements of [s] in decreasing
   order. *)
val rev_iter: (elt -> unit) -> t -> unit

(**[fold f s b] folds [f] over the elements of [s] in increasing order. *)
val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b

(**[elements s] is the list of the elements of [s] in increasing order. *)
val elements: t -> elt list

(**[to_seq s] is the sequence of the elements of [s] in increasing
   order. *)
val to_seq: t -> elt Seq.t

(**[for_all p s] determines whether all elements of [s] satisfy [p]. *)
val for_all: (elt -> bool) -> t -> bool

(**[exists p s] determines whether some element of [s] satisfies [p]. *)
val exists: (elt -> bool) -> t -> bool

(**[find_first_opt p s] is the least element of [s] that satisfies [p], if
   any. The predicate need not be monotonic. *)
val find_first_opt: (elt -> bool) -> t -> elt option

(** {1 Decomposition} *)

(**[compare_minimum] is a total preorder on sets: the empty set is less than
   any non-empty set, and two non-empty sets are compared by their minimum
   elements. *)
val compare_minimum: t -> t -> int

(**[sorted_union ss] is the union of the sets in the list [ss]. (Any list of
   sets is accepted; this function is efficient when the sets are sorted and
   do not overlap.) *)
val sorted_union: t list -> t

(**[extract_unique_prefix s1 s2] requires [s2] to be non-empty. It splits
   [s1] into [(head1, tail1)] where [head1] is the subset of the elements of
   [s1] that are less than [minimum s2]. *)
val extract_unique_prefix: t -> t -> t * t

(**[extract_shared_prefix s1 s2] is [(head, (tail1, tail2))] where [head] is
   the longest common prefix of [s1] and [s2] (viewed as increasing
   sequences), [s1 = head ∪ tail1] and [s2 = head ∪ tail2]. *)
val extract_shared_prefix: t -> t -> t * (t * t)

(**[extract_unique_suffix s1 s2] requires [s2] to be non-empty. It splits
   [s1] into [(head1, tail1)] where [head1] is the subset of the elements of
   [s1] that are greater than [maximum s2]. *)
val extract_unique_suffix: t -> t -> t * t

(**[extract_shared_suffix s1 s2] is [(head, (tail1, tail2))] where [head] is
   the longest common suffix of [s1] and [s2] (viewed as increasing
   sequences), [s1 = head ∪ tail1] and [s2 = head ∪ tail2]. *)
val extract_shared_suffix: t -> t -> t * (t * t)

(** {1 Debugging} *)

(**[check s] verifies the internal invariants of [s] and raises [Failure]
   if one is violated. It is intended for testing. *)
val check: t -> unit
