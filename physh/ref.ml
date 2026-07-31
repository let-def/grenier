(* Placeholder: replace with your trusted Ref implementation *)
(* Must conform to the same interface as physh.mli *)

module Set : sig
  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val mem : 'a t -> 'a -> bool
  val add : 'a t -> 'a -> unit
end = struct
  type 'a t = 'a list ref
  let create () = ref []
  let length t = List.length !t
  let mem t x = List.exists (fun y -> x == y) !t
  let add t x =
    if not (mem t x) then t := x :: !t
end

module Map : sig
  type ('a,'b) t
  val create : unit -> ('a,'b) t
  val length : ('a,'b) t -> int
  val find : ('a,'b) t -> 'a -> 'b
  val add : ('a,'b) t -> 'a -> 'b -> unit
end = struct
  type ('a,'b) t = ('a * 'b) list ref
  let create () = ref []
  let length t = List.length !t
  let find t k =
    match List.find_opt (fun (x, _) -> x == k) !t with
    | Some (_, v) -> v
    | None -> raise Not_found
  let add t k v =
    let entries = List.map (fun (x, w) -> if x == k then (k, v) else (x, w)) !t in
    if List.exists (fun (x, _) -> x == k) entries then
      t := entries
    else
      t := (k, v) :: entries
end
