module O = Order_indir

type t = {
  a : O.t;
  b : O.t;
  (* This strange dance with protect is to prevent the GC from collecting
     values in the middle of an operation. *)
  lock : gc_lock;
}

and gc_lock = {
  mutable locks: int;
  mutable forgotten: O.t list;
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
      List.iter O.forget forgotten

let forget {lock; a; b} =
  if lock.locks > 0 then
    lock.forgotten <- a :: b :: lock.forgotten
  else
    (O.forget a; O.forget b)

let is_valid t =
  lock t.lock;
  let result = O.is_valid t.a in
  unlock t.lock;
  result

let root () =
  let a = O.root () in
  let b = O.after a in
  let t = {a; b; lock = { locks = 0; forgotten = [] }} in
  Gc.finalise forget t;
  t

let after t =
  lock t.lock;
  let b = O.after t.b in
  let a = O.before b in
  let t' = {a; b; lock = t.lock} in
  Gc.finalise forget t';
  unlock t.lock;
  t'

let before t =
  lock t.lock;
  let a = O.before t.a in
  let b = O.after a in
  let t' = {a; b; lock = t.lock} in
  Gc.finalise forget t';
  unlock t.lock;
  t'

let inside t =
  lock t.lock;
  let a = O.after t.a in
  let b = O.before t.b in
  let t' = {a; b; lock = t.lock} in
  Gc.finalise forget t';
  unlock t.lock;
  t'

let outside t =
  lock t.lock;
  let a = O.before t.a in
  let b = O.after t.b in
  let t' = {a; b; lock = t.lock} in
  Gc.finalise forget t';
  unlock t.lock;
  t'

let same_order t1 t2 =
  O.same_order t1.a t2.a

type rel =
  | Before
  | Inside
  | Equal
  | Outside
  | After

let compare t1 t2 =
  if t1 == t2 then Equal else
    let ca = O.compare t1.a t2.a <= 0 in
    let cb = O.compare t1.b t2.b <= 0 in
    match ca, cb with
    | true, true   -> Before
    | true, false  -> Outside
    | false, true  -> Inside
    | false, false -> After

let cardinal t =
  O.cardinal t.a / 2

let unsafe_check t msg =
  lock t.lock;
  O.unsafe_check t.a ("(Order_managed_interval a) " ^ msg);
  O.unsafe_check t.b ("(Order_managed_interval b) " ^ msg);
  unlock t.lock
