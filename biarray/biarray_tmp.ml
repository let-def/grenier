
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
  (*let to_seq t = Array.to_seq (unsafe_to_array_r t)*)
  (*let to_seqi t = Array.to_seqi (unsafe_to_array_r t)*)
  (*let of_seq seq = of_array (Array.of_seq seq)*)
  external unsafe_get : ('a, _) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : (_, 'a) t -> int -> 'a -> unit = "%array_unsafe_set"
end
