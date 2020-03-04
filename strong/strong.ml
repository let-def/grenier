(* Type-level equality *)
type (_, _) eq = Refl : ('a, 'a) eq
let follow_eq (type a b) (Refl : (a, b) eq) (x : a) : b = x

(* Strongly typed ordering *)
module Order = struct type (_, _) t = Lt | Eq : ('a, 'a) t | Gt end
type ('a, 'b) order = ('a, 'b) Order.t

let order_from_comparison n =
  if n < 0 then Order.Lt
  else if n > 0 then Order.Gt
  else Order.Eq

(* Uninhabitated type *)
type void = { void : 'a. 'a }
let void v = v.void

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
end = struct
  type 'a t = T : int -> unit t

  let order (type a b) (T a : a t) (T b : b t) : (a, b) order =
    Order.(if a < b then Lt else if a > b then Gt else Eq)

  let lift_eq (type a b) (Refl : (a, b) eq) : (a t, b t) eq =
    Refl

  let to_int (type n) (T n : n t) = n

  type zero = unit
  let zero : zero t = T 0

  type one = unit
  let one : one t = T 1

  module type T = sig type n val n : n t end

  module Nth (N : sig val n : int end) : T = struct
    type n = unit let n : n t = T N.n
  end

  let nth n =
    let module N = struct
      type n = unit
      let n = T n
    end
    in
    (module N : T)

  type ('a, 'b) sum = unit
  let add (type a b) (T a : a t) (T b : b t) : (a, b) sum t =
    T (a + b)
  let sum_comm (type a b)
    : ((a, b) sum, (b, a) sum) eq = Refl
  let sum_assoc (type a b c)
    : (((a, b) sum, c) sum, (a, (b, c) sum) sum) eq = Refl

  type ('a, 'b) prod = unit
  let mul (type a b) (T a : a t) (T b : b t) : (a, b) prod t =
    T (a * b)
  let prod_comm (type a b)
    : ((a, b) prod, (b, a) prod) eq = Refl
  let prod_assoc (type a b c)
    : (((a, b) prod, c) prod, (a, (b, c) prod) prod) eq = Refl
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
end = struct
  type 'a set = 'a Natural.t
  module type Set = Natural.T
  let cardinal = Natural.to_int

  type 'a elt = int

  let elt_of_int (type a) (set : a set) n : a elt =
    let c = cardinal set in
    if n >= 0 && n < c then n else
      Printf.ksprintf invalid_arg
        "Strong.Finite.of_int #%d %d: %d is not in [0; %d[" c n n c

  let elt_to_int x = x

  let iter_set (type a) (set : a set) f =
    for i = 0 to cardinal set - 1 do f i done

  let rev_iter_set (type a) (set : a set) f =
    for i = cardinal set - 1 downto 0 do f i done

  let all_elements (type a) (set : a set) =
    Array.init (cardinal set) (fun x -> x)

  (* Temporary API, should be replaced by something array-like in the future *)
  module type Map = sig
    type domain
    val domain : domain set
    type codomain
    val get : domain elt -> codomain
  end
  type 'a map = (module Map with type codomain = 'a)

  module Map_of_array (A : sig type codomain val table : codomain array end) :
    Map with type codomain = A.codomain =
  struct
    module Card = Natural.Nth(struct let n = Array.length A.table end)
    type domain = Card.n
    let domain = Card.n
    type codomain = A.codomain
    let get i = A.table.(i)
  end

  let iter_map (type a) ((module Map) : a map) (f : a -> unit) : unit =
    iter_set Map.domain (fun elt -> f (Map.get elt))
end
