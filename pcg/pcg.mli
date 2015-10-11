type t

val create : state:int64 -> seq:int64 -> t
val reseed : t -> state:int64 -> seq:int64 -> unit

val get_int32 : t -> int32
val get_int : t -> int (* int, but at most 32 bits of randomness *)
