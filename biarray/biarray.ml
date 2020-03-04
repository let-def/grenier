type (+'r, -'w) t

external read_only : ('r, 'w) t -> ('r, Strong.void) t = "%identity"
external array_as_biarray : 'a array -> ('a, 'a) t = "%identity"
external biarray_as_array : ('a, 'a) t -> 'a array = "%identity"

external length : _ t -> int = "%array_length"
external get : (_, 'a) t -> int -> 'a = "%array_safe_get"
external set : ('a, _) t -> int -> 'a -> unit = "%array_safe_set"
external make : int -> 'a -> ('a, 'a) t = "caml_make_vect"
external create_float: int -> (float, float) t = "caml_make_float_vect"

external unsafe_as_array_r : ('r, _) t -> 'r array = "%identity"
external unsafe_as_array_w : (_, 'w) t -> 'w array = "%identity"

let init n f = array_as_biarray (Array.init n f)
let append r1 r2 =
  array_as_biarray
    (Array.append (unsafe_as_array_r r1) (unsafe_as_array_r r2))

let concat lst =
  array_as_biarray (Array.concat (Obj.magic (lst : _ t list) : _ array list))

let sub t i j =
  array_as_biarray (Array.sub (unsafe_as_array_r t) i j)

let copy t = array_as_biarray (Array.copy (unsafe_as_array_r t))

let fill t i j v =
  Array.fill (unsafe_as_array_w t) i j v

let blit src i dst j k =
  Array.blit (unsafe_as_array_r src) i (unsafe_as_array_w dst) j k

let to_list t = Array.to_list (unsafe_as_array_r t)
let of_list l = array_as_biarray (Array.of_list l)

let iter f t = Array.iter f (unsafe_as_array_r t)
let iteri f t = Array.iteri f (unsafe_as_array_r t)
let map f t = array_as_biarray (Array.map f (unsafe_as_array_r t))
let mapi f t = array_as_biarray (Array.mapi f (unsafe_as_array_r t))
let fold_left f acc t = Array.fold_left f acc (unsafe_as_array_r t)
let fold_right f t acc = Array.fold_right f (unsafe_as_array_r t) acc

let iter2 f t1 t2 =
  Array.iter2 f (unsafe_as_array_r t1) (unsafe_as_array_r t2)
let map2 f t1 t2 =
  array_as_biarray (Array.map2 f (unsafe_as_array_r t1) (unsafe_as_array_r t2))

let for_all f t = Array.for_all f (unsafe_as_array_r t)
let exists f t = Array.exists f (unsafe_as_array_r t)
let mem x t = Array.mem x (unsafe_as_array_r t)
let memq x t = Array.memq x (unsafe_as_array_r t)
let sort f t = Array.sort f (unsafe_as_array_r t)
let stable_sort f t = Array.stable_sort f (unsafe_as_array_r t)
let fast_sort f t = Array.fast_sort f (unsafe_as_array_r t)
(*let to_seq t = Array.to_seq (unsafe_as_array_r t)*)
(*let to_seqi t = Array.to_seqi (unsafe_as_array_r t)*)
(*let of_seq seq = array_as_biarray (Array.of_seq seq)*)
external unsafe_get : ('a, _) t -> int -> 'a = "%array_unsafe_get"
external unsafe_set : (_, 'a) t -> int -> 'a -> unit = "%array_unsafe_set"
