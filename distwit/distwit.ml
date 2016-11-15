(* OCaml magic *)

let is_extension_constructor obj =
  Obj.tag obj = Obj.object_tag &&
  Obj.size obj = 2 &&
  Obj.is_block (Obj.field obj 0) &&
  Obj.tag (Obj.field obj 0) = Obj.string_tag &&
  Obj.is_int (Obj.field obj 1)

let extension_constructor obj = (
  assert (Obj.is_block obj && Obj.size obj >= 2);
  let tag = Obj.tag obj in
  let ext =
    if tag = Obj.object_tag then
      (* Constant constructor *)
      obj
    else if tag = 0 then
      (* Parametrized constructor *)
      Obj.field obj 0
    else
      assert false
  in
  assert (is_extension_constructor ext);
  (Obj.obj ext : extension_constructor)
)

let replace_extension_constructor (ext : extension_constructor) obj =
  assert (Obj.is_block obj && Obj.size obj >= 2);
  let tag = Obj.tag obj in
  if tag = Obj.object_tag then (
    (* Constant constructor *)
    assert (is_extension_constructor obj);
    (Obj.repr ext)
  ) else if tag = 0 then (
    (* Parametrized constructor *)
    assert (is_extension_constructor (Obj.field obj 0));
    Obj.set_field obj 0 (Obj.repr ext);
    obj
  ) else
    assert false

let extension_name (ext : extension_constructor) : string =
  Obj.obj (Obj.field (Obj.repr ext) 0)

let extension_id (ext : extension_constructor) : int =
  Obj.obj (Obj.field (Obj.repr ext) 1)

module Exttbl = Hashtbl.Make(struct
    type t = extension_constructor
    let equal = (==)
    let hash = Hashtbl.hash
  end)

(* W *)

module type GENSYM = sig
  include Hashtbl.HashedType

  val fresh : unit -> t
end

let process_mark = ref ()

module Make_unsafe (W : GENSYM) : sig
  type +'a t

  val register : W.t -> extension_constructor -> unit

  val wrap : 'a -> 'a t
  val unwrap : 'a t -> 'a

  val witness : 'a t -> W.t
end =
struct
  type 'a t = {
    mutable process_mark: unit ref;
    witness: W.t;
    mutable value: Obj.t;
  }

  module Wittbl = Hashtbl.Make(W)

  let exttbl : W.t Exttbl.t =
    Exttbl.create 7

  let wittbl : extension_constructor Wittbl.t =
    Wittbl.create 7

  let register wit ext =
    Exttbl.replace exttbl ext wit;
    Wittbl.replace wittbl wit ext

  let witness t = t.witness

  let wrap (value : 'a) : 'a t =
    let value = Obj.repr value in
    let ext = extension_constructor value in
    let witness =
      try Exttbl.find exttbl ext
      with Not_found ->
        let wit = W.fresh () in
        register wit ext;
        wit
    in
    { process_mark; witness; value }

  let unwrap t = Obj.obj (
      if t.process_mark == process_mark then
        (* t is from same address space *)
        t.value
      else match Wittbl.find wittbl t.witness with
        | exception Not_found ->
          t.value
        | ext ->
          t.process_mark <- process_mark;
          t.value <- replace_extension_constructor ext t.value;
          t.value
    )
end
