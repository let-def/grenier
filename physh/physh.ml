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
let is_empty t = length t = 0

(* Sentinels: static values, outside of the OCaml heap *)
external sentinel_empty : unit -> Obj.t = "ml_physh_empty" [@@noalloc]
external sentinel_stale : unit -> Obj.t = "ml_physh_stale" [@@noalloc]

let empty = sentinel_empty ()
let stale = sentinel_stale ()

(* Drop the storage *)
let reset t =
  t.witness <- Obj.repr ();
  t.minor_fill <- 0;
  t.minor <- Obj.repr [||];
  t.major_fill <- 0;
  t.major <- Obj.repr [||]

(* Keep the storage: empty slots hold [empty].
   A table without storage is the static [[||]], which must not be passed to
   Array.fill (OCaml 4 asserts that the array is in the heap). *)
let clear t =
  let fill (a : Obj.t) =
    let n = Obj.size a in
    if n > 0 then Array.fill (Obj.obj a) 0 n empty
  in
  fill t.minor;
  fill t.major;
  t.witness <- Obj.repr ();
  t.minor_fill <- 0;
  t.major_fill <- 0

(* Fast paths do not allocate. They fail when the table needs a sync
   (after a minor collection or a compaction) or has to grow, the OCaml side
   then calls a slow path. *)

(* State of a key, as reported by the fast paths of sets.
   Must match the enum in ml_physh.c. *)
type state = Absent | Present | Need_sync
[@@warning "-37"] (* built by C stubs *)

module Set_core = struct
  type 'a t = table

  external mem_fast : 'a t -> 'a -> state = "ml_physh_set_mem_fast" [@@noalloc]
  external sync : 'a t -> unit = "ml_physh_set_sync"

  (* Add and return the prior state ([Need_sync]: nothing done) *)
  external add_fast : 'a t -> 'a -> state = "ml_physh_set_add_fast" [@@noalloc]
  external add_slow : 'a t -> 'a -> bool = "ml_physh_set_add_slow"

  (* Remove and return the prior state ([Need_sync]: nothing done) *)
  external remove_fast : 'a t -> 'a -> state = "ml_physh_set_remove_fast" [@@noalloc]

  let create = create_table
  let length = length
  let is_empty = is_empty
  let clear = clear
  let reset = reset

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

  let rec remove t x =
    match remove_fast t x with
    | Absent | Present -> ()
    | Need_sync -> sync t; remove t x
end

(* [fold_slots width f t acc] calls [f table i] for each used slot.

   Iterating does not require a sync: the GC keeps the keys in the arrays
   up-to-date, only their positions can be stale. Visit the major table,
   then the minor table, as they were at the start.

   A lookup in [f] can sync the table: flushing the minor table inserts its
   entries in the major table, in place, while we are visiting it. Detect it
   (the minor table is replaced) and from then on, skip the keys of the
   minor table we started with: they are visited with the minor table. This
   costs O(minor) only if a flush happens, the major table is never copied.

   Modifying the table during the iteration is unspecified, but safe. *)
let fold_slots width f t acc =
  let major : Obj.t array = Obj.obj t.major in
  let minor : Obj.t array = Obj.obj t.minor in
  let flushed = ref None in
  let skip k =
    match !flushed with
    | Some s -> Set_core.mem s k
    | None when t.minor == Obj.repr minor -> false
    | None ->
      let s = Set_core.create () in
      for i = 0 to Array.length minor / width - 1 do
        let k = Array.unsafe_get minor (i * width) in
        if k != empty then Set_core.add s k
      done;
      flushed := Some s;
      Set_core.mem s k
  in
  let acc = ref acc in
  for i = 0 to Array.length major / width - 1 do
    let k = Array.unsafe_get major (i * width) in
    if k != empty && not (skip k) then acc := f major (i * width) !acc
  done;
  for i = 0 to Array.length minor / width - 1 do
    if Array.unsafe_get minor (i * width) != empty then
      acc := f minor (i * width) !acc
  done;
  !acc

module Set = struct
  include Set_core

  let fold f t acc =
    fold_slots 1 (fun table i acc -> f (Obj.obj (Array.unsafe_get table i)) acc) t acc

  let iter f t = fold (fun x () -> f x) t ()
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

  (* Returns the previous binding, [empty] if there was none, [stale] if a
     sync is needed (nothing done) *)
  external remove_fast : ('a, 'b) t -> 'a -> 'b = "ml_physh_map_remove_fast" [@@noalloc]

  let create = create_table
  let length = length
  let is_empty = is_empty
  let clear = clear
  let reset = reset

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

  let rec find_opt t k =
    let v = find_fast t k in
    if Obj.repr v == empty then None
    else if Obj.repr v == stale then (sync t; find_opt t k)
    else Some v

  let rec mem t k =
    let v = find_fast t k in
    if Obj.repr v == stale then (sync t; mem t k) else Obj.repr v != empty

  let rec remove t k =
    let r = remove_fast t k in
    if Obj.repr r == stale then (sync t; remove t k)

  let fold f t acc =
    fold_slots 2 (fun table i acc ->
        f (Obj.obj (Array.unsafe_get table i))
          (Obj.obj (Array.unsafe_get table (i + 1))) acc) t acc

  let iter f t = fold (fun k v () -> f k v) t ()
end
