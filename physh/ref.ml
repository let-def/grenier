(* Placeholder: replace with your trusted Ref implementation *)
(* Must conform to the same interface as physh.mli *)

module H = Hashtbl.Make(struct
               type t = Obj.t
               let hash = Hashtbl.hash
               let equal = (==)
             end)

module Set : sig
  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val mem : 'a t -> 'a -> bool
  val add : 'a t -> 'a -> unit
end = struct
  type 'a t = unit H.t
  let create () = H.create 7
  let length t = H.length t
  let mem t x = H.mem t (Obj.repr x)
  let add t x = H.replace t (Obj.repr x) ()
end

module Map : sig
  type ('a,'b) t
  val create : unit -> ('a,'b) t
  val length : ('a,'b) t -> int
  val find : ('a,'b) t -> 'a -> 'b
  val add : ('a,'b) t -> 'a -> 'b -> unit
end = struct
  type ('a,'b) t = 'b H.t
  let create () = H.create 7
  let length t = H.length t
  let find t k = H.find t (Obj.repr k)
  let add t k v = H.replace t (Obj.repr k) v
end
