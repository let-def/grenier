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

  (** [add_if_new t x] adds [x] to [t] and returns [true] if [x] was
      already present (in which case [t] is unchanged). *)
  val add_if_new : 'a t -> 'a -> bool
end

module Map : sig
  type ('a,'b) t
  val create : unit -> ('a,'b) t
  val length : ('a,'b) t -> int
  val find : ('a,'b) t -> 'a -> 'b
  val add : ('a,'b) t -> 'a -> 'b -> unit

  (** [add_if_new t k v] binds [k] to [v] if [k] is not bound.
      Returns the previous binding: [None] if [k] was absent (it is now bound
      to [v]), [Some v'] if [k] was bound to [v'] (left unchanged). *)
  val add_if_new : ('a,'b) t -> 'a -> 'b -> 'b option

  (** [add_overwrite t k v] binds [k] to [v], replacing any existing binding.
      Returns the previous binding: [None] if [k] was absent, [Some v'] if
      [k] was bound to [v']. *)
  val add_overwrite : ('a,'b) t -> 'a -> 'b -> 'b option
end
