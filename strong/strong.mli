(* Type-level equality *)
type (_, _) eq = Refl : ('a, 'a) eq
val follow_eq : ('a, 'b) eq -> 'a -> 'b

(* Strongly typed ordering *)
module Order : sig type (_, _) t = Lt | Eq : ('a, 'a) t | Gt end
type ('a, 'b) order = ('a, 'b) Order.t
val order_from_comparison : int -> ('a, 'a) Order.t

(* Uninhabitated type *)
type void
val void : void -> 'a

(* Strongly typed natural:
   - a family of types indexed by postive integers
   - recover type equality of two ints are equal
*)
module Natural : sig
  type 'a t
  val order : 'a t -> 'b t -> ('a, 'b) order
  val lift_eq : ('a, 'b) eq -> ('a t, 'b t) eq

  val to_int : 'a t -> int

  type zero
  val zero : zero t

  type one
  val one : one t

  module type T = sig type n val n : n t end
  module Nth (N : sig val n : int end) : T
  val nth : int -> (module T)

  type ('a, 'b) sum
  val add : 'a t -> 'b t -> ('a, 'b) sum t
  val sum_comm : (('a, 'b) sum, ('b, 'a) sum) eq
  val sum_assoc : ((('a, 'b) sum, 'c) sum, ('a, ('b, 'c) sum) sum) eq

  type ('a, 'b) prod
  val mul : 'a t -> 'b t -> ('a, 'b) prod t
  val prod_comm : (('a, 'b) prod, ('b, 'a) prod) eq
  val prod_assoc : ((('a, 'b) prod, 'c) prod, ('a, ('b, 'c) prod) prod) eq
end

(* Finite sets: interpret naturals as the cardinality of a set *)
module Finite : sig
  type 'a set = 'a Natural.t
  module type Set = Natural.T
  val cardinal : 'a set -> int

  type 'a elt = private int
  val elt_of_int : 'a set -> int -> 'a elt
  val elt_to_int : 'a elt -> int
  val iter_set : 'a set -> ('a elt -> unit) -> unit
  val rev_iter_set : 'a set -> ('a elt -> unit) -> unit
  val all_elements : 'a set -> 'a elt array

  (* Temporary API, should be replaced by something array-like in the future *)
  module type Map = sig
    type domain
    val domain : domain set
    type codomain
    val get : domain elt -> codomain
  end
  type 'a map = (module Map with type codomain = 'a)

  module Map_of_array (A : sig type codomain val table : codomain array end) :
    Map with type codomain = A.codomain

  val iter_map : 'a map -> ('a -> unit) -> unit

  type ('n, 'a) map' = (module Map with type codomain = 'a and type domain = 'n)
  val init_map : 'n set -> ('n elt -> 'a) -> ('n, 'a) map'
  val map_map : ('n, 'a) map' -> ('a -> 'b) -> ('n, 'b) map'
end
