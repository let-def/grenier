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
    Worst-case cost is O(log n), though the amortized cost is O(1).
*)

val get : int -> 'a t -> 'a
(** [get i xs] access the i'th element of [xs] in cost O(log i).
    In particular, access to recent elements is quasi constant.

    The operation is only defined if [0 <= i < length xs], and will raise
    [Not_found] if [i] is out of bounds.

    O(log i).
*)

val set : int -> 'a -> 'a t -> 'a t
(** [set i x xs] update the i'th element of [xs] to value [x] in cost O(log i).

    The operation is only defined if [0 <= i < length xs], and will raise
    [Not_found] if [i] is out of bounds.

    O(log i).
*)

val update : 'a t -> int -> ('a -> 'a) -> 'a t
(** [update xs i f] behaves like [set i (f (get i xs)) xs].

    O(log i).
*)

val length : 'a t -> int
(** [n = length xs] is the number of elements in [xs].

    O(log n).
*)

val is_empty : 'a t -> bool
(** [is_empty t] iff [t = empty] (equivalently [length t = 0]).

    O(1).
*)

val uncons : 'a t -> ('a * 'a t) option
(** Revert the effect of the last [cons] (with the same complexity). *)

val drop : int -> 'a t -> 'a t
(** [drop n x] removes [n] elements from [x].
    Faster than [uncons]'ing [n] times.
    (TODO: determine complexity) *)
