(*
 * Hashtables and sets using objects addresses and physical equality, and
 * behave well with OCaml GC.
 *
 * This is useful to observe sharing, traverse cyclic structures, ...
 * Highly experimental, this relies on a lot of GC internals!
 *
 * Keys are compared with [==]: two immediate values (integers, constant
 * constructors) are equal if they have the same representation, two blocks
 * are equal if they are the same object. Keys and values are kept alive by
 * the table.
 *
 * Tables are not thread-safe: a table must only be used by one domain at a
 * time.
 *)

(** Sets of objects, compared by physical equality. *)
module Set : sig
  (** A mutable set of elements of type ['a]. *)
  type 'a t

  (** [create ()] returns a new, empty set. It does not allocate any storage
      until the first element is added: empty sets are cheap. *)
  val create : unit -> 'a t

  (** [length t] is the number of elements of [t]. Constant time. *)
  val length : 'a t -> int

  (** [is_empty t] is [length t = 0]. *)
  val is_empty : 'a t -> bool

  (** [mem t x] is [true] if [x] is an element of [t]. *)
  val mem : 'a t -> 'a -> bool

  (** [add t x] adds [x] to [t]. Does nothing if [x] is already present. *)
  val add : 'a t -> 'a -> unit

  (** [add_if_new t x] adds [x] to [t] and returns [true] if [x] was
      already present (in which case [t] is unchanged). This is the
      "visited" test of a traversal, in a single lookup. *)
  val add_if_new : 'a t -> 'a -> bool

  (** [remove t x] removes [x] from [t]. Does nothing if [x] is absent. *)
  val remove : 'a t -> 'a -> unit

  (** [clear t] removes all elements of [t], keeping its storage for reuse. *)
  val clear : 'a t -> unit

  (** [reset t] removes all elements of [t] and releases its storage: [t]
      is then as cheap as a new set. *)
  val reset : 'a t -> unit

  (** [iter f t] applies [f] to each element of [t], in an unspecified
      order. [f] can look up [t]; if it modifies [t], it is unspecified
      whether the added (or removed) elements are visited. *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** [fold f t acc] is [f xN (... (f x1 acc))], where [x1 ... xN] are the
      elements of [t], in an unspecified order. Same restrictions as
      {!iter}. *)
  val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
end

(** Maps from objects, compared by physical equality, to arbitrary values.
    A key has at most one binding. *)
module Map : sig
  (** A mutable map from keys of type ['a] to values of type ['b]. *)
  type ('a,'b) t

  (** [create ()] returns a new, empty map. It does not allocate any storage
      until the first binding is added: empty maps are cheap. *)
  val create : unit -> ('a,'b) t

  (** [length t] is the number of bindings of [t]. Constant time. *)
  val length : ('a,'b) t -> int

  (** [is_empty t] is [length t = 0]. *)
  val is_empty : ('a,'b) t -> bool

  (** [find t k] returns the value bound to [k].
      @raise Not_found if [k] is not bound. *)
  val find : ('a,'b) t -> 'a -> 'b

  (** [find_opt t k] returns [Some v] if [k] is bound to [v], [None]
      otherwise. *)
  val find_opt : ('a,'b) t -> 'a -> 'b option

  (** [mem t k] is [true] if [k] is bound in [t]. *)
  val mem : ('a,'b) t -> 'a -> bool

  (** [add t k v] binds [k] to [v], replacing any existing binding. *)
  val add : ('a,'b) t -> 'a -> 'b -> unit

  (** [add_if_new t k v] binds [k] to [v] if [k] is not bound.
      Returns the previous binding: [None] if [k] was absent (it is now bound
      to [v]), [Some v'] if [k] was bound to [v'] (left unchanged). *)
  val add_if_new : ('a,'b) t -> 'a -> 'b -> 'b option

  (** [add_overwrite t k v] binds [k] to [v], replacing any existing binding.
      Returns the previous binding: [None] if [k] was absent, [Some v'] if
      [k] was bound to [v']. *)
  val add_overwrite : ('a,'b) t -> 'a -> 'b -> 'b option

  (** [remove t k] removes the binding of [k]. Does nothing if [k] is not
      bound. *)
  val remove : ('a,'b) t -> 'a -> unit

  (** [clear t] removes all bindings of [t], keeping its storage for
      reuse. *)
  val clear : ('a,'b) t -> unit

  (** [reset t] removes all bindings of [t] and releases its storage: [t] is
      then as cheap as a new map. *)
  val reset : ('a,'b) t -> unit

  (** [iter f t] applies [f k v] to each binding of [t], in an unspecified
      order. [f] can look up [t]; if it modifies [t], it is unspecified
      whether the added (or removed) bindings are visited, and which value
      is passed for an overwritten binding. *)
  val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit

  (** [fold f t acc] is [f kN vN (... (f k1 v1 acc))], where [(k1, v1) ...
      (kN, vN)] are the bindings of [t], in an unspecified order. Same
      restrictions as {!iter}. *)
  val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a,'b) t -> 'acc -> 'acc
end
