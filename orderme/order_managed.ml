type t = {
  t : Order_list.t;
  (* This strange dance with protect is to prevent the GC from collecting
     values in the middle of an operation. *)
  mutable protect: int;
}

open Order_list

let lock1 t =
  t.protect <- t.protect + 1

let unlock1 t =
  if t.protect = 0 then forget t.t;
  t.protect <- t.protect - 1

let lock2 t1 t2 =
  lock1 t1; lock1 t2

let unlock2 t1 t2 =
  unlock1 t1; unlock1 t2

let forget t =
  if t.protect = 0 then
    forget t.t
  else
    t.protect <- t.protect - 1

let is_valid t =
  lock1 t;
  let result = is_valid t.t in
  unlock1 t;
  result

let root () =
  let t = {t = root (); protect = 0} in
  Gc.finalise forget t;
  t

let after t =
  lock1 t;
  let t' = {t = after t.t; protect = 0} in
  Gc.finalise forget t';
  unlock1 t;
  t'

let before t =
  lock1 t;
  let t' = {t = before t.t; protect = 0} in
  Gc.finalise forget t';
  unlock1 t;
  t'

let same_order t1 t2 =
  lock2 t1 t2;
  let result = same_order t1.t t2.t in
  unlock2 t1 t2;
  result

let compare t1 t2 =
  lock2 t1 t2;
  let result = compare t1.t t2.t in
  unlock2 t1 t2;
  result

let cardinal t =
  lock1 t;
  let result = cardinal t.t in
  unlock1 t;
  result

let unsafe_check t msg =
  lock1 t;
  unsafe_check t.t ("(Order_managed) " ^ msg);
  unlock1 t
