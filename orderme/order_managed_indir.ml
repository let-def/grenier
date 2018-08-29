open Order_indir
type order = t

type t = {
  t : order;
  (* This strange dance with protect is to prevent the GC from collecting
     values in the middle of an operation. *)
  lock : gc_lock;
}

and gc_lock = {
  mutable locks: int;
  mutable forgotten: order list;
}

let lock lock =
  lock.locks <- lock.locks + 1

let unlock lock =
  lock.locks <- lock.locks - 1;
  if lock.locks = 0 then
    match lock.forgotten with
    | [] -> ()
    | forgotten ->
      lock.forgotten <- [];
      List.iter forget forgotten

let forget {lock; t} =
  if lock.locks > 0 then
    lock.forgotten <- t :: lock.forgotten
  else
    forget t

let is_valid t =
  lock t.lock;
  let result = is_valid t.t in
  unlock t.lock;
  result

let root () =
  let t = {t = root (); lock = { locks = 0; forgotten = [] }} in
  Gc.finalise forget t;
  t

let after t =
  lock t.lock;
  let t' = {t = after t.t; lock = t.lock} in
  Gc.finalise forget t';
  unlock t.lock;
  t'

let before t =
  lock t.lock;
  let t' = {t = before t.t; lock = t.lock} in
  Gc.finalise forget t';
  unlock t.lock;
  t'

let same_order t1 t2 =
  same_order t1.t t2.t

let compare t1 t2 =
  compare t1.t t2.t

let cardinal t =
  cardinal t.t

let unsafe_check t msg =
  lock t.lock;
  unsafe_check t.t ("(Order_managed) " ^ msg);
  unlock t.lock
