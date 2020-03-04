type (+'r, -'w) t
external read_only : ('r, 'w) t -> ('r, Strong.void) t = "%identity"

external array_as_biarray : 'a array -> ('a, 'a) t = "%identity"
external biarray_as_array : ('a, 'a) t -> 'a array = "%identity"

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
