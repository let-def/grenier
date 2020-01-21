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
  val to_seq : ('a, _) t -> 'a Seq.t
  val to_seqi : ('a, _) t -> (int * 'a) Seq.t
  val of_seq : 'a Seq.t -> ('a, 'a) t
  external unsafe_get : ('a, _) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : (_, 'a) t -> int -> 'a -> unit = "%array_unsafe_set"
end = struct
  type (+'r, -'w) t
  type 'r read_only = ('r, void) t

  external of_array : 'a array -> ('a, 'a) t = "%identity"
  external unsafe_to_array_r : ('r, _) t -> 'r array = "%identity"
  external unsafe_to_array_w : (_, 'w) t -> 'w array = "%identity"
  external to_array : ('a, 'a) t -> 'a array = "%identity"

  external length : _ t -> int = "%array_length"
  external get : (_, 'a) t -> int -> 'a = "%array_safe_get"
  external set : ('a, _) t -> int -> 'a -> unit = "%array_safe_set"
  external make : int -> 'a -> ('a, 'a) t = "caml_make_vect"
  external create_float: int -> (float, float) t = "caml_make_float_vect"

  let init n f = of_array (Array.init n f)
  let append r1 r2 =
    of_array (Array.append (unsafe_to_array_r r1) (unsafe_to_array_r r2))

  let concat lst =
    of_array (Array.concat (Obj.magic (lst : _ t list) : _ array list))

  let sub t i j =
    of_array (Array.sub (unsafe_to_array_r t) i j)

  let copy t = of_array (Array.copy (unsafe_to_array_r t))

  let fill t i j v =
    Array.fill (unsafe_to_array_w t) i j v

  let blit src i dst j k =
    Array.blit (unsafe_to_array_r src) i (unsafe_to_array_w dst) j k

  let to_list t = Array.to_list (unsafe_to_array_r t)
  let of_list l = of_array (Array.of_list l)

  let iter f t = Array.iter f (unsafe_to_array_r t)
  let iteri f t = Array.iteri f (unsafe_to_array_r t)
  let map f t = of_array (Array.map f (unsafe_to_array_r t))
  let mapi f t = of_array (Array.mapi f (unsafe_to_array_r t))
  let fold_left f acc t = Array.fold_left f acc (unsafe_to_array_r t)
  let fold_right f t acc = Array.fold_right f (unsafe_to_array_r t) acc

  let iter2 f t1 t2 =
    Array.iter2 f (unsafe_to_array_r t1) (unsafe_to_array_r t2)
  let map2 f t1 t2 =
    of_array (Array.map2 f (unsafe_to_array_r t1) (unsafe_to_array_r t2))

  let for_all f t = Array.for_all f (unsafe_to_array_r t)
  let exists f t = Array.exists f (unsafe_to_array_r t)
  let mem x t = Array.mem x (unsafe_to_array_r t)
  let memq x t = Array.memq x (unsafe_to_array_r t)
  let sort f t = Array.sort f (unsafe_to_array_r t)
  let stable_sort f t = Array.stable_sort f (unsafe_to_array_r t)
  let fast_sort f t = Array.fast_sort f (unsafe_to_array_r t)
  let to_seq t = Array.to_seq (unsafe_to_array_r t)
  let to_seqi t = Array.to_seqi (unsafe_to_array_r t)
  let of_seq seq = of_array (Array.of_seq seq)
  external unsafe_get : ('a, _) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : (_, 'a) t -> int -> 'a -> unit = "%array_unsafe_set"
end
