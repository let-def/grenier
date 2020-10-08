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
  type 'n set = 'n Natural.t
  type 'n elt = private int

  module Set : sig
    val cardinal : 'n set -> int
    val iter : 'n set -> ('n elt -> unit) -> unit
    val rev_iter : 'n set -> ('n elt -> unit) -> unit
    val fold_left : 'n set -> ('b -> 'n elt -> 'b) -> 'b -> 'b
    val fold_right : 'n set -> ('n elt -> 'b -> 'b) -> 'b -> 'b
  end

  module Elt : sig
    val of_int_opt : 'n set -> int -> 'n elt option
    val of_int : 'n set -> int -> 'n elt
    val to_int : 'n elt -> int
  end

  module Array : sig
    type ('n, 'a) t = private 'a array
    type 'a _array = A : ('n, 'a) t -> 'a _array [@@ocaml.unboxed]
    val empty : (Natural.zero, _) t
    val length : ('n, 'a) t -> 'n set
    external get : ('n, 'a) t -> 'n elt -> 'a = "%array_unsafe_get"
    external set : ('n, 'a) t -> 'n elt -> 'a -> unit = "%array_unsafe_set"
    val make : 'n set -> 'a -> ('n, 'a) t
    val init : 'n set -> ('n elt -> 'a) -> ('n, 'a) t
    val make_matrix : 'i set -> 'j set -> 'a -> ('i, ('j, 'a) t) t
    val append : ('n, 'a) t -> ('m, 'a) t -> (('n, 'm) Natural.sum, 'a) t
    val of_array : 'a array -> 'a _array
    module type T = sig include Natural.T type a val table : (n, a) t end
    val module_of_array : 'a array -> (module T with type a = 'a)
    val to_array : ('n, 'a) t -> 'a array
    val all_elements : 'n set -> ('n, 'n elt) t

    val iter : ('a -> unit) -> (_, 'a) t -> unit
    val iteri : ('n elt -> 'a -> unit) -> ('n, 'a) t -> unit
    val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
    val mapi : ('n elt -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
    val fold_left : ('a -> 'b -> 'a) -> 'a -> ('n, 'b) t -> 'a
    val fold_right : ('b -> 'a -> 'a) -> ('n, 'b) t -> 'a -> 'a
    val iter2 : ('a -> 'b -> unit) -> ('n, 'a) t -> ('n, 'b) t -> unit
    val map2 : ('a -> 'b -> 'c) -> ('n, 'a) t -> ('n, 'b) t -> ('n, 'c)  t
  end
end
