(* Encoding of finite sets *)

type 'a element = private int

module type Set = sig
  type t
  type nonrec element = t element
  val n : int
end

module Set (X : sig val n : int end) : Set

type 't set = (module Set with type t = 't)

(* Return the number of elements of a given type *)

val cardinal : 'a set -> int

(* Type of one element from a finite set *)

module Element : sig
  type 'a t = 'a element
  val of_int : 'a set -> int -> 'a t
  val to_int : 'a t -> int
  val iter : 'a set -> ('a t -> unit) -> unit
  val rev_iter : 'a set -> ('a t -> unit) -> unit
  val all_elements : 'a set -> 'a t array
end

(* Type of finite maps *)

module type Map = sig
  module Domain : Set
  type codomain
  val element : Domain.t element -> codomain
end

type 'a map = (module Map with type codomain = 'a)

module Map_of_array (A : sig type codomain val table : codomain array end) :
  Map with type codomain = A.codomain

val iter_map : 'a map -> ('a -> unit) -> unit
val map_of_array : 'a array -> 'a map
