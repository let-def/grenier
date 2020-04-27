type +'a t
val empty : 'a t
val add : 'a -> 'a t -> 'a t
val get : int -> 'a t -> 'a
val set : int -> 'a -> 'a t -> 'a t
val update : 'a t -> int -> ('a -> 'a) -> 'a t
val length : 'a t -> int
