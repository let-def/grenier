(* Checks that Sharing keeps the copy that lives in the major heap: after
   unifying an old set with an equal young one, both point to the old
   children, whichever argument order is used. *)

module S = Maxtrie.PatSet

let build () = List.fold_left (fun s x -> S.add x s) S.empty (List.init 5000 (fun i -> i * 97))

(* The children of a set node: fields 2 and 3 of the record. *)
let children (s : S.t) = Obj.field (Obj.repr s) 2, Obj.field (Obj.repr s) 3

let check name order =
  let old = build () in
  Gc.full_major ();                       (* [old] is now in the major heap *)
  let ol, or_ = children old in
  let young = build () in                 (* [young] is in the minor heap *)
  let eq = if order then S.Sharing.equal old young else S.Sharing.equal young old in
  assert eq;
  let yl, yr = children young in
  let ol', or' = children old in
  if not (yl == ol && yr == or_ && ol' == ol && or' == or_) then begin
    Printf.printf "%s: the major-heap children were not kept\n" name; exit 1
  end

let () =
  check "old first" true;
  check "young first" false;
  print_endline "sharecheck: OK"

(* [PatMap.t] is covariant: this coercion only type-checks if it is. *)
let _widen (m : [ `A ] Maxtrie.PatMap.t) : [ `A | `B ] Maxtrie.PatMap.t =
  (m :> [ `A | `B ] Maxtrie.PatMap.t)
