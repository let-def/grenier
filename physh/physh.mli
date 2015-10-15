(*
 * Hashtables and sets using objects addresses and physical equality, and
 * behave well with OCaml GC.
 *
 * This is useful to observe sharing, traverse cyclic structures, ...
 * Highly experimental, this relies on a lot of GC internals!
 *
 *)

module Set : sig
  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val mem : 'a t -> 'a -> bool
  val add : 'a t -> 'a -> unit
end

module Map : sig
  type ('a,'b) t
  val create : unit -> ('a,'b) t
  val length : ('a,'b) t -> int
  val find : ('a,'b) t -> 'a -> 'b
  val add : ('a,'b) t -> 'a -> 'b -> unit
end
