type set = int

type 'a t
val create :
  ?partition:('a Finite.element -> 'a Finite.element -> int) ->
  'a Finite.set -> 'a t

val mark : 'a t -> 'a Finite.element -> unit
val split : 'a t -> unit
val discard_unmarked : 'a t -> unit
val discard : 'a t -> ('a Finite.element -> bool) -> unit

val set_count : 'a t -> int
val set_of : 'a t -> 'a Finite.element -> set
val choose : 'a t -> set -> 'a Finite.element
val iter_elements : 'a t -> set -> ('a Finite.element -> unit) -> unit
val iter_marked_elements : 'a t -> set -> ('a Finite.element -> unit) -> unit

val clear_marks : 'a t -> unit
val marked_sets : 'a t -> set list
