(*
   A congruence closure algorithm, inspired from
    "Fast congruence closure and extensions"
   by Robert Nieuwenhuis and Albert Oliveras.
   https://www.sciencedirect.com/science/article/pii/S0890540106001581

   This implementation is backtrackable (see [snapshot] function), does not
   provide conflict explanation but implements two extensions:
   - support for constructors (injective functions)
   - interpretation of equivalence classes

   # Congruence closure?

   We are trying to partition a set of elements into equivalent classes.
   For instance, if [x = y] and [y = z], then [x = z].
   This problem can be implemented as:

     let gr = make ()
     let x = fresh gr ()
     let y = fresh gr ()
     let z = fresh gr ()

     let () =
       assume_equal x y; (* declare that x = y *)
       assume_equal y z; (* declare that y = z *)
       assert (same x z) (* we can deduce that x = z *)

   With only ground symbols,
   [union-find](https://en.wikipedia.org/wiki/Disjoint-set_data_structure),
   is enough to solve the problem.

   ## Uninterpreted functions

   Congruence closure also adds equations involving uninterpreted functions.
   Even if we make no assumptions about a (mathematical) function, we know
   that "applying" it to the same argument always leads to the same result:
   f(x) = f(x).

   So if [f = g] and [x = y], then [f(x) = g(y)]. This can be checked with:

     let f = fresh gr ()
     let g = fresh gr ()
     let x = fresh gr ()
     let y = fresh gr ()
     let f_x = fresh gr ()
     let g_y = fresh gr ()

     let () =
       assume_equal f g;
       assume_equal x y;
       assume_application f x ~equal:f_x;
       assume_application g y ~equal:g_y;
       assert (same f_x g_y)

   Only unary functions are provided. n-ary functions can be implemented by
   repeated application.

   ## Interpreting classes

   FIXME
*)

(** Creations of equality graph *)

(** The graph structure, used to keep global information about elements and
    implement interpretation.
    Only elements from the graph can be related. *)
type 'a graph

(** Handle to an equivalence class *)
type 'a node

(** Create a new graph. *)
type 'a merger = repr:'a node -> 'a node -> unit
val make : ?on_merge:'a merger -> unit -> 'a graph

val set_on_merge : 'a graph -> 'a merger -> unit

(** [fresh gr x] creates a new equivalence class in graph [gr] with associated
    value [x] *)
val fresh : 'a graph -> 'a -> 'a node

(** {1 Assume properties on classes} *)

(** [assume_equal x y] adds the equation [x = y] *)
val assume_equal : 'a node -> 'a node -> unit

(** [assume_application f x ~equal:y] adds the equation [y = f x] *)
val assume_application : 'a node -> 'a node -> equal:'a node -> unit

(** {1 Observe equivalence structure} *)

(** [propagate graph] updates the congruence closure to account for all added
    equations. *)
val propagate : 'a graph -> unit

(** [same x y] returns true iff [x] and [y] are in the same congruence.
    New equations are [propagated] if necessary. *)
val same : 'a node -> 'a node -> bool

(** [find_app f x] returns [Some z] if any node is already equal to [f x]
    (added via [assume_application]), or [None] otherwise. *)
val find_app : 'a node -> 'a node -> 'a node option

(** {1 Associating values with classes} *)

(** [get_tag n] returns the [tag] associated with the class of [n] (not the node).

    Warning: this function doesn't propagate equation.
    Call [propagate] before if necessary. *)
val get_tag : 'a node -> 'a

(** [set_tag n x] changes the [tag] associated with the class of [n] (not the node).

    Warning: this function doesn't propagate equation.
    Call [propagate] before if necessary. *)
val set_tag : 'a node -> 'a -> unit

(** [set_root_tag n x] changes the [tag] associated with the node [n].
    This function should be called before any equation is added about [n].
    The root tag is not backed up (it won't be restored by [restore]).

    The purpose of this function is to change the tag of a [fresh] node,
    immediately after its creation. This is useful if the tag needs to reference
    the node (to create a recursion between a node and its tag). *)
val set_root_tag : 'a node -> 'a -> unit

(** [get_id node] returns an integer identifying [node]
    (unique for the graph).
    This function is constant: the integer identify the node and not the
    equivalence class.
*)
val get_id : 'a node -> int

(** [compare a b] compares two nodes by their id. *)
val compare : 'a node -> 'a node -> int

(** [get_repr node] returns the node that is the representative of the
    equivalence class of [node] in the current state.
    The representative can change when new equations are propagated.

    Warning: this function doesn't propagate equations.
    Call [propagate] before if necessary. *)
val get_repr : 'a node -> 'a node

(** {1 Variables} *)


(** An ['a var] is like a ['a ref], but its contents is backed up and restored
    when using [snapshot] and [restore]. *)
type 'a var

(** [var graph x] creates a new variable with initial value [x], which contents
    is backed up and restored when snapshotting and restoring [graph]. *)
val var : _ graph -> 'a -> 'a var

(** [get_var v] retrieves the current value of the variable, like [!] *)
val get_var : 'a var -> 'a

(** [set_var v x] changes the current value of the variable, like [:=] *)
val set_var : 'a var -> 'a -> unit

(** {1 Snapshots} *)

(** A snapshot (efficiently) stores the state of the congruence closure (and its
    associated variables) at a specific point in time. *)
type snapshot

(** [snapshot graph] takes a snapshot of the current state. *)
val snapshot : 'a graph -> snapshot

(** [restore sn] restores the congruence closure to the exact same state it had
    when [sn = snapshot graph] was called.

    Precondition: snapshot must be valid, see [is_valid]. *)
val restore : snapshot -> unit

(** A [snapshot] becomes invalid when an earlier snapshot is restored.
    This only applies to the descendants: a snapshot that is restored remains
    valid, and can be restored multiple times.
    For instance, the following holds:
      let s1 = snapshot graph in
      let s2 = snapshot graph in
      restore s1;
      assert (is_valid s1);
      assert (not (is_valid s2)) *)
val is_valid : snapshot -> bool

(** [invalid_snapshot] for which [is_valid] is always [false].
    It cannot be restored.  Its purpose is to be used as a placeholder in places
    where a snapshot is expected but doesn't have to be valid. *)
val invalid_snapshot : snapshot
