open Strong
type set = int

type 'a t
val create :
  ?partition:('a Finite.elt -> 'a Finite.elt -> int) ->
  'a Finite.set -> 'a t

val mark : 'a t -> 'a Finite.elt -> unit
val split : 'a t -> unit
val discard_unmarked : 'a t -> unit
val discard : 'a t -> ('a Finite.elt -> bool) -> unit

val set_count : 'a t -> int
val set_of : 'a t -> 'a Finite.elt -> set
val choose : 'a t -> set -> 'a Finite.elt
val iter_elements : 'a t -> set -> ('a Finite.elt -> unit) -> unit
val iter_marked_elements : 'a t -> set -> ('a Finite.elt -> unit) -> unit

val clear_marks : 'a t -> unit
val marked_sets : 'a t -> set list
