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
end

(* Biarray: arrays with two bounds to be used in subtyping contexts *)
module Biarray : sig
  type (+'r, -'w) t
  type 'r read_only = ('r, void) t

  external of_array : 'a array -> ('a, 'a) t = "%identity"
  external to_array : ('a, 'a) t -> 'a array = "%identity"

  external length : _ t -> int = "%array_length"
  external get : (_, 'a) t -> int -> 'a = "%array_safe_get"
  external set : ('a, _) t -> int -> 'a -> unit = "%array_safe_set"
  external make : int -> 'a -> ('a, 'a) t = "caml_make_vect"
  external create_float: int -> (float, float) t = "caml_make_float_vect"

  val init : int -> (int -> 'a) -> ('a, 'a) t
  val append : ('r, _) t -> ('r, _) t -> ('r, 'r) t
  val concat : ('r, _) t list -> ('r, 'r) t
  val sub : ('r, _) t -> int -> int -> ('r, 'r) t

  val copy : ('r, _) t -> ('r, 'r) t
  val fill : (_, 'w) t -> int -> int -> 'w -> unit
  val blit : ('a, _) t -> int -> (_, 'a) t -> int -> int -> unit
  val to_list : ('r, _) t -> 'r list
  val of_list : 'a list -> ('a, 'a) t

  val iter : ('r -> unit) -> ('r, _) t -> unit
  val iteri : (int -> 'r -> unit) -> ('r, _) t -> unit
  val map : ('a -> 'b) -> ('a, _) t -> ('b, 'b) t
  val mapi : (int -> 'a -> 'b) -> ('a, _) t -> ('b, 'b) t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, _) t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> ('b, _) t -> 'a -> 'a

  val iter2 : ('a -> 'b -> unit) -> ('a, _) t -> ('b, _) t -> unit
  val map2 : ('a -> 'b -> 'c) -> ('a, _) t -> ('b, _) t -> ('c, 'c) t

  val for_all : ('a -> bool) -> ('a, _) t -> bool
  val exists : ('a -> bool) -> ('a, _) t -> bool
  val mem : 'a -> ('a, _) t -> bool
  val memq : 'a -> ('a, _) t -> bool
  val sort : ('a -> 'a -> int) -> ('a, _) t -> unit
  val stable_sort : ('a -> 'a -> int) -> ('a, _) t -> unit
  val fast_sort : ('a -> 'a -> int) -> ('a, _) t -> unit
  (*This can be compiled with OCaml > 4.07 *)
  (*val to_seq : ('a, _) t -> 'a Seq.t*)
  (*val to_seqi : ('a, _) t -> (int * 'a) Seq.t*)
  (*val of_seq : 'a Seq.t -> ('a, 'a) t*)
  external unsafe_get : ('a, _) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : (_, 'a) t -> int -> 'a -> unit = "%array_unsafe_set"
end
