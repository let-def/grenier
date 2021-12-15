open Strong

(* An intermediate datastructure used by Valmari automata minimization
   algorithm for efficiently representing incremental refinements of a set
   partition. *)

type set = int
(** Each set is identified by an integer *)

type 'n t
(** A partitioning structure for a set of cardinal 'n \
    (encoded as a Strong.Natural) *)

val create :
  ?partition:('n Finite.elt -> 'n Finite.elt -> int) ->
  'n Finite.set -> 'n t
(** [create ?partition n] create a fresh partitioning data structure for a set
    of cardinal [n].
    If [partition] is not provided, the datastructure is initialized with a
    single subset that contains all elements.
    Otherwise, [partition] must be a total ordering function and elements that
    can be distinguished are put in different subsets.
*)

val mark : 'n t -> 'n Finite.elt -> unit
(** [mark part elt] marks the element [elt] as active.
    The datastructure manages an active set by marking a certain number of
    elements, and then applying an operation to all of them at once.
*)

val split : 'n t -> unit
(** Put marked elements in different sets.
    That is, each input set is split in two subsets one with the marked and one
    with the unmarked elements.
    Active set is reset after (no elements are marked).
*)

val discard_unmarked : 'n t -> unit
(** Elements that are not marked are removed from the partition (they will be
    ignored by future operations).
    In practice, they are considered as belonging to set [-1] (which can be
    observed by [set_of] function), and this [-1] set is not counted by the
    [set_count] function.
    Active set is reset after (no elements are marked).
*)

val discard : 'n t -> ('n Finite.elt -> bool) -> unit
(** [discard part f] calls the function [f] for each element in the set
    and discard it if the function returns [true].
    Active set must be empty before and is reset after (no elements are marked).
*)

val set_count : 'n t -> int
(** Number of sets in the current partition *)

val set_of : 'n t -> 'n Finite.elt -> set
(** [set_of part elt] returns the index of the set that contains element [elt].
    Result is between [0] and [set_of part - 1] unless the element has been
    discarded, in which case it is [-1]. *)

val choose : 'n t -> set -> 'n Finite.elt
(** [choose part set] returns an arbitrary element that belongs to set [set].
    [set] must be between [0] and [set_of part - 1].
*)

val choose_opt : 'n t -> set -> 'n Finite.elt option
(** [choose part set] returns an arbitrary element that belongs to set [set].
    [set] must be between [0] and [set_of part - 1].
*)

val iter_elements : 'n t -> set -> ('n Finite.elt -> unit) -> unit
(** [iter_elements part set f] applies function [f] to each element that
    currently belongs to set [set].
*)

val iter_marked_elements : 'n t -> set -> ('n Finite.elt -> unit) -> unit
(** [iter_marked_elements part set f] applies function [f] to each element that
    currently belongs to set [set] and is marked.
*)

val clear_marks : 'n t -> unit
(** Remove all marks (reset the active set) *)

val marked_sets : 'n t -> set list
(** Returns all sets that have marked elements. *)

val is_first : 'n t -> 'n Finite.elt -> bool
