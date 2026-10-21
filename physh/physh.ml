(* Tables are manipulated by the C stubs (ml_physh.c), the layout of the
   record must match. Creating a table does not call C: an empty table has no
   storage, tables are allocated on first use. *)
type table = {
  mutable witness : Obj.t;   (* a key of the minor table *)
  mutable minor_fill : int;
  mutable minor : Obj.t;     (* young keys *)
  mutable compactions : int; (* compaction count when major was hashed *)
  mutable major_fill : int;
  mutable major : Obj.t;     (* other keys *)
}

let create_table () = {
  witness = Obj.repr ();
  minor_fill = 0;
  minor = Obj.repr [||];
  compactions = 0;
  major_fill = 0;
  major = Obj.repr [||];
}

let length t = t.minor_fill + t.major_fill

(* Sentinels: static values, outside of the OCaml heap *)
external sentinel_empty : unit -> Obj.t = "ml_physh_empty" [@@noalloc]
external sentinel_stale : unit -> Obj.t = "ml_physh_stale" [@@noalloc]

let empty = sentinel_empty ()
let stale = sentinel_stale ()

module Set = struct
  type 'a t = table

  (* Fast paths do not allocate. They fail when the table needs a sync
     (after a minor collection or a compaction) or has to grow. *)
  external mem_fast : 'a t -> 'a -> int = "ml_physh_set_mem_fast" [@@noalloc]
  external add_fast : 'a t -> 'a -> bool = "ml_physh_set_add_fast" [@@noalloc]
  external add_slow : 'a t -> 'a -> unit = "ml_physh_set_add_slow"
  external sync : 'a t -> unit = "ml_physh_set_sync"

  let create = create_table
  let length = length

  let add t x =
    if not (add_fast t x) then add_slow t x

  let rec mem t x =
    match mem_fast t x with
    | 0 -> false
    | 1 -> true
    | _ -> sync t; mem t x
end

module Map = struct
  type ('a, 'b) t = table

  (* Returns [empty] if the key is not bound, [stale] if a sync is needed *)
  external find_fast : ('a, 'b) t -> 'a -> 'b = "ml_physh_map_find_fast" [@@noalloc]
  external add_fast : ('a, 'b) t -> 'a -> 'b -> bool = "ml_physh_map_add_fast" [@@noalloc]
  external add_slow : ('a, 'b) t -> 'a -> 'b -> unit = "ml_physh_map_add_slow"
  external sync : ('a, 'b) t -> unit = "ml_physh_map_sync"

  let create = create_table
  let length = length

  let add t k v =
    if not (add_fast t k v) then add_slow t k v

  let rec find t k =
    let v = find_fast t k in
    if Obj.repr v == empty then raise Not_found;
    if Obj.repr v == stale then (sync t; find t k) else v
end
