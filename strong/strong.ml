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

type 'a natural = T : int -> unit natural

module Natural : sig
  type 'a t = 'a natural
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
  type 'a t = 'a natural

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
  type 'n set = 'n Natural.t
  type 'n elt = private int

  module Set : sig
    module type T = Natural.T
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
    val to_array : (_, 'a) t -> 'a array
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
end = struct
  type 'a set = 'a Natural.t
  type 'a elt = int

  module Set = struct
    module type T = Natural.T
    let cardinal = Natural.to_int
    let iter (type n) (set : n set) f =
      for i = 0 to cardinal set - 1 do f i done
    let rev_iter (type n) (set : n set) f =
      for i = cardinal set - 1 downto 0 do f i done
    let fold_left (type n) (set : n set) f acc =
      let acc = ref acc in
      for i = 0 to cardinal set - 1 do acc := f !acc i done;
      !acc
    let fold_right (type n) (set : n set) f acc =
      let acc = ref acc in
      for i = cardinal set - 1 downto 0 do acc := f i !acc done;
      !acc
  end

  module Elt = struct
    let of_int_opt (type n) (set : n set) n : n elt option =
      let c = Set.cardinal set in
      if n >= 0 && n < c then Some n else None

    let of_int (type n) (set : n set) n : n elt =
      let c = Set.cardinal set in
      if n >= 0 && n < c then n else
        Printf.ksprintf invalid_arg
          "Strong.Finite.Elt.of_int #%d %d: %d is not in [0; %d[" c n n c

    let to_int x = x
  end

  module Array = struct
    type ('n, 'a) t = 'a array
    type 'a _array = A : ('n, 'a) t -> 'a _array [@@ocaml.unboxed]
    let empty : (Natural.zero, _) t = [||]
    external get : ('n, 'a) t -> 'n elt -> 'a = "%array_unsafe_get"
    external set : ('n, 'a) t -> 'n elt -> 'a -> unit = "%array_unsafe_set"
    let length (a : ('n, 'a) t) : 'n set =
      (Obj.magic (T (Array.length a) : _ natural) : _ natural)
    let make n x = Array.make (Set.cardinal n) x
    let init n f = Array.init (Set.cardinal n) f
    let make_matrix is js v =
      Array.make_matrix (Set.cardinal is) (Set.cardinal js) v
    let append = Array.append
    let of_array arr = A arr
    module type T = sig include Natural.T type a val table : (n, a) t end
    let module_of_array (type a) (arr : a array) : (module T with type a = a) =
      let (module Nth) = Natural.nth (Array.length arr) in
      (module struct include Nth type nonrec a = a let table = arr end)

    let to_array x = x
    let all_elements (type a) (set : a set) =
      Array.init (Set.cardinal set) (fun x -> x)

    let iter = Array.iter
    let iteri = Array.iteri
    let map = Array.map
    let mapi = Array.mapi
    let fold_left = Array.fold_left
    let fold_right = Array.fold_right
    let iter2 = Array.iter2
    let map2 = Array.map2
  end
end
