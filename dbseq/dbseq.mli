(** {1 Dbseq immutable sequence}
    [Dbseq] is a small data structure that offers operations halfway between
    a list and an immutable array.
    Most operations have a logarithmic cost. In practice, it is a log with
    base 4 and small constant factors.

    This data structure is particularly suitable to associate metadata to
    variables in De-Bruijn notation (hence the name).
*)


type +'a t
(** Sequences with element of type 'a **)

val empty : 'a t
(** The empty sequence *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] adds element [x] at the beginning of sequence [xs].
    [x] now has index 0 and [xs]'s elements are shifted by 1.
    Worst-case cost is [O(log n)], though the actual cost is quasi constant.
*)

val get : int -> 'a t -> 'a
(** [get i xs] access the i'th element of [xs] in cost O(log i).
    In particular, access to recent elements is quasi constant.

    The operation is only defined if [0 <= i < length xs], and will raise
    [Not_found] if [i] is out of bounds.
*)

val set : int -> 'a -> 'a t -> 'a t
(** [set i x xs] update the i'th element of [xs] to value [x] in cost O(log i).

    The operation is only defined if [0 <= i < length xs], and will raise
    [Not_found] if [i] is out of bounds.
*)

val update : 'a t -> int -> ('a -> 'a) -> 'a t
(** [update xs i f] behaves like [set i (f (get i xs)) xs] *)

val length : 'a t -> int
(** [length xs] returns the number of elements in [xs] *)
