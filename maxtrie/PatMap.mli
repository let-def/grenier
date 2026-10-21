(**Immutable maps whose keys are non-negative integers.

   Maps use the representation of {!PatSet}, with one binding per node
   instead of a bitmap: Patricia tries in which every node hoists the largest
   key of its subtree. A map with [n] bindings has exactly [n] nodes, there
   are no internal branching nodes, and the depth of a tree is bounded by the
   number of bits of an integer. The binding with the largest key is at the
   root.

   Keys must be non-negative: [add], [singleton], and [update] raise
   [Invalid_argument] on a negative key; the other operations treat negative
   keys as absent.

   See {!PatMapSharing} for a variant whose comparisons unify equal subtrees.

   The interface follows [Stdlib.Map.S]. Functions that take a key-dependent
   function apply it in increasing order of keys. *)

type key = int

(**The type of maps from [key] to ['a]. *)
type +'a t

(** {1 Construction} *)

val empty: 'a t
val singleton: key -> 'a -> 'a t

(**[add k v m] binds [k] to [v]. If [k] is already bound to a value that
   is physically equal to [v], then [m] itself is returned. *)
val add: key -> 'a -> 'a t -> 'a t

(**[update k f m] binds [k] according to [f], which receives the current
   binding of [k], if any. If [f] returns its argument (or [None] when there
   was no binding), then [m] itself is returned. *)
val update: key -> ('a option -> 'a option) -> 'a t -> 'a t

(**[remove k m] removes the binding of [k]. If [k] is not bound, then [m]
   itself is returned. *)
val remove: key -> 'a t -> 'a t

(**[union f m1 m2] has the bindings of [m1] and [m2]. When a key [k] is
   bound in both, to [v1] and [v2], [f k v1 v2] decides: [None] drops the
   key, [Some v] binds it to [v]. If the result has the same keys as [m2] and
   physically equal values, then [m2] itself is returned. *)
val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

(**[inter f m1 m2] has the keys bound in both [m1] and [m2]; for each such
   key [k], bound to [v1] and [v2], [f k v1 v2] decides the binding. *)
val inter: (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t

(**[diff m1 m2] is [m1] deprived of the keys bound in [m2]. If no key is
   removed, then [m1] itself is returned. *)
val diff: 'a t -> 'b t -> 'a t

(**[filter p m] has the bindings of [m] that satisfy [p]. If all of them
   do, then [m] itself is returned. *)
val filter: (key -> 'a -> bool) -> 'a t -> 'a t

val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

(**[split k m] is [(l, data, r)] where [l] has the bindings of [m] whose
   keys are less than [k], [r] those whose keys are greater than [k], and
   [data] is the binding of [k], if any. *)
val split: key -> 'a t -> 'a t * 'a option * 'a t

val of_list: (key * 'a) list -> 'a t
val of_seq: (key * 'a) Seq.t -> 'a t
val add_seq: (key * 'a) Seq.t -> 'a t -> 'a t

(** {1 Queries} *)

(**[is_empty m] determines whether [m] is empty. Constant time. *)
val is_empty: 'a t -> bool

(**[is_singleton m] determines whether [m] has exactly one binding. Constant
   time. *)
val is_singleton: 'a t -> bool

val cardinal: 'a t -> int
val mem: key -> 'a t -> bool

(**@raise Not_found if the key is not bound. *)
val find: key -> 'a t -> 'a

val find_opt: key -> 'a t -> 'a option

(**[min_binding m] is the binding with the least key.
   @raise Not_found if [m] is empty. *)
val min_binding: 'a t -> key * 'a

val min_binding_opt: 'a t -> (key * 'a) option

(**[max_binding m] is the binding with the greatest key. Constant time.
   @raise Not_found if [m] is empty. *)
val max_binding: 'a t -> key * 'a

val max_binding_opt: 'a t -> (key * 'a) option

(**[choose m] is a binding of [m] (currently [max_binding m], which takes
   constant time).
   @raise Not_found if [m] is empty. *)
val choose: 'a t -> key * 'a

val choose_opt: 'a t -> (key * 'a) option

(**[find_first p m] is the binding with the least key that satisfies [p].
   Unlike [Stdlib.Map.find_first], [p] need not be monotonic.
   @raise Not_found if there is none. *)
val find_first: (key -> bool) -> 'a t -> key * 'a

val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option

(**[disjoint m1 m2] determines whether [m1] and [m2] have no key in
   common. *)
val disjoint: 'a t -> 'b t -> bool

(**[equal eq m1 m2] determines whether [m1] and [m2] have the same keys,
   bound to values related by [eq]. *)
val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(**[compare cmp] is a total order on maps, compatible with [equal]. It is a
   structural order, not the lexicographic order on bindings. *)
val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

(**[changes f m1 m2] calls [f k x y], in increasing order of keys, for every
   key [k] such that [x], the binding of [k] in [m1], and [y], its binding in
   [m2], are not physically equal ([None] when there is no binding). Subtrees
   that the two maps share are skipped without being visited, so when [m2]
   was derived from [m1] by a few updates, the cost depends on the number of
   updates (times the depth), not on the size of the maps. Does not
   allocate (besides what [f] does and the options it receives). *)
val changes: (key -> 'a option -> 'a option -> unit) -> 'a t -> 'a t -> unit

(** {1 Iteration} *)

val iter: (key -> 'a -> unit) -> 'a t -> unit
val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val for_all: (key -> 'a -> bool) -> 'a t -> bool
val exists: (key -> 'a -> bool) -> 'a t -> bool

(**[bindings m] is the list of the bindings of [m] in increasing order of
   keys. *)
val bindings: 'a t -> (key * 'a) list

val to_seq: 'a t -> (key * 'a) Seq.t
val to_rev_seq: 'a t -> (key * 'a) Seq.t

(** {1 Debugging} *)

(**[check m] verifies the internal invariants of [m] and raises [Failure] if
   one is violated. It is intended for testing. *)
val check: 'a t -> unit
