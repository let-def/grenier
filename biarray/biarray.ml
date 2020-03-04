type void (* Uninhabitated type *)

type (+'r, -'w) bi_array
let read_only = Obj.magic

external of_array : 'a array -> ('a, 'a) bi_array = "%identity"
external to_array : ('a, 'a) bi_array -> 'a array = "%identity"

external length : _ bi_array -> int = "%bi_array_length"
external get : (_, 'a) bi_array -> int -> 'a = "%bi_array_safe_get"
external set : ('a, _) bi_array -> int -> 'a -> unit = "%bi_array_safe_set"
external make : int -> 'a -> ('a, 'a) bi_array = "caml_make_vect"
external create_float: int -> (float, float) bi_array = "caml_make_float_vect"

val init : int -> (int -> 'a) -> ('a, 'a) bi_array
val append : ('r, _) bi_array -> ('r, _) bi_array -> ('r, 'r) bi_array
val concat : ('r, _) bi_array list -> ('r, 'r) bi_array
val sub : ('r, _) bi_array -> int -> int -> ('r, 'r) bi_array

val copy : ('r, _) bi_array -> ('r, 'r) bi_array
val fill : (_, 'w) bi_array -> int -> int -> 'w -> unit
val blit : ('a, _) bi_array -> int -> (_, 'a) bi_array -> int -> int -> unit
val to_list : ('r, _) bi_array -> 'r list
val of_list : 'a list -> ('a, 'a) bi_array

val iter : ('r -> unit) -> ('r, _) bi_array -> unit
val iteri : (int -> 'r -> unit) -> ('r, _) bi_array -> unit
val map : ('a -> 'b) -> ('a, _) bi_array -> ('b, 'b) bi_array
val mapi : (int -> 'a -> 'b) -> ('a, _) bi_array -> ('b, 'b) bi_array
val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, _) bi_array -> 'a
val fold_right : ('b -> 'a -> 'a) -> ('b, _) bi_array -> 'a -> 'a

val iter2 : ('a -> 'b -> unit) -> ('a, _) bi_array -> ('b, _) bi_array -> unit
val map2 : ('a -> 'b -> 'c) -> ('a, _) bi_array -> ('b, _) bi_array -> ('c, 'c) bi_array

val for_all : ('a -> bool) -> ('a, _) bi_array -> bool
val exists : ('a -> bool) -> ('a, _) bi_array -> bool
val mem : 'a -> ('a, _) bi_array -> bool
val memq : 'a -> ('a, _) bi_array -> bool
val sort : ('a -> 'a -> int) -> ('a, _) bi_array -> unit
val stable_sort : ('a -> 'a -> int) -> ('a, _) bi_array -> unit
val fast_sort : ('a -> 'a -> int) -> ('a, _) bi_array -> unit
val to_seq : ('a, _) bi_array -> 'a Seq.t
val to_seqi : ('a, _) bi_array -> (int * 'a) Seq.t
val of_seq : 'a Seq.t -> ('a, 'a) bi_array
external unsafe_get : ('a, _) bi_array -> int -> 'a = "%bi_array_unsafe_get"
external unsafe_set : (_, 'a) bi_array -> int -> 'a -> unit = "%bi_array_unsafe_set"
