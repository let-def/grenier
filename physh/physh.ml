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

(* Fast paths do not allocate. They fail when the table needs a sync
   (after a minor collection or a compaction) or has to grow, the OCaml side
   then calls a slow path. *)

(* State of a key, as reported by the fast paths of sets.
   Must match the enum in ml_physh.c. *)
type state = Absent | Present | Need_sync
[@@warning "-37"] (* built by C stubs *)

module Set = struct
  type 'a t = table

  external mem_fast : 'a t -> 'a -> state = "ml_physh_set_mem_fast" [@@noalloc]
  external sync : 'a t -> unit = "ml_physh_set_sync"

  (* Add and return the prior state ([Need_sync]: nothing done) *)
  external add_fast : 'a t -> 'a -> state = "ml_physh_set_add_fast" [@@noalloc]
  external add_slow : 'a t -> 'a -> bool = "ml_physh_set_add_slow"

  let create = create_table
  let length = length

  let add_if_new t x =
    match add_fast t x with
    | Absent -> false
    | Present -> true
    | Need_sync -> add_slow t x

  let add t x = ignore (add_if_new t x : bool)

  let rec mem t x =
    match mem_fast t x with
    | Absent -> false
    | Present -> true
    | Need_sync -> sync t; mem t x
end

module Map = struct
  type ('a, 'b) t = table

  (* Returns [empty] if the key is not bound, [stale] if a sync is needed *)
  external find_fast : ('a, 'b) t -> 'a -> 'b = "ml_physh_map_find_fast" [@@noalloc]
  external sync : ('a, 'b) t -> unit = "ml_physh_map_sync"

  (* [add_fast t k v overwrite] binds [k] to [v] if [k] is absent or
     [overwrite] is true. Returns the previous binding, [empty] if there was
     none, [stale] if nothing was done. [add_slow] never returns [stale]. *)
  external add_fast : ('a, 'b) t -> 'a -> 'b -> bool -> 'b = "ml_physh_map_add_fast" [@@noalloc]
  external add_slow : ('a, 'b) t -> 'a -> 'b -> bool -> 'b = "ml_physh_map_add_slow"

  let create = create_table
  let length = length

  let add_gen t k v overwrite =
    let r = add_fast t k v overwrite in
    if Obj.repr r == stale then add_slow t k v overwrite else r

  let prior r = if Obj.repr r == empty then None else Some r

  let add_if_new t k v = prior (add_gen t k v false)
  let add_overwrite t k v = prior (add_gen t k v true)
  let add t k v = ignore (add_gen t k v true)

  let rec find t k =
    let v = find_fast t k in
    if Obj.repr v == empty then raise Not_found;
    if Obj.repr v == stale then (sync t; find t k) else v
end
