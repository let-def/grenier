(* Checks that Sharing keeps the copy that lives in the major heap: after
   unifying an old set with an equal young one, both point to the old
   children, whichever argument order is used. *)

module type S = sig
  type t
  val empty: t
  val add: int -> t -> t
  module Sharing : sig val equal: t -> t -> bool end
end

(* The children of the root: its fields from the index 2 on (fields 2 and 3
   of a PatSet node; in PatSetCompact, they depend on the constructor). *)
let children s =
  let s = Obj.repr s in
  List.init (Obj.size s - 2) (fun i -> Obj.field s (i + 2))

let check (type t) (module S : S with type t = t) name order =
  let build () = List.fold_left (fun s x -> S.add x s) S.empty (List.init 5000 (fun i -> i * 97)) in
  let old = build () in
  Gc.full_major ();                       (* [old] is now in the major heap *)
  let oc = children old in
  let young = build () in                 (* [young] is in the minor heap *)
  let eq = if order then S.Sharing.equal old young else S.Sharing.equal young old in
  assert eq;
  if not (List.for_all2 (==) (children young) oc && List.for_all2 (==) (children old) oc)
  then begin
    Printf.printf "%s: the major-heap children were not kept\n" name; exit 1
  end

let () =
  check (module Maxtrie.PatSet) "PatSet, old first" true;
  check (module Maxtrie.PatSet) "PatSet, young first" false;
  check (module Maxtrie.PatSetCompact) "PatSetCompact, old first" true;
  check (module Maxtrie.PatSetCompact) "PatSetCompact, young first" false;
  print_endline "sharecheck: OK"

(* [PatMap.t] is covariant: this coercion only type-checks if it is. *)
let _widen (m : [ `A ] Maxtrie.PatMap.t) : [ `A | `B ] Maxtrie.PatMap.t =
  (m :> [ `A | `B ] Maxtrie.PatMap.t)
