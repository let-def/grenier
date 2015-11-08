type t = {t : Order_list.t}

open Order_list

let is_valid t = is_valid t.t

let forget t = forget t.t
let root () =
  let t = {t = root ()} in
  Gc.finalise forget t;
  t

let after t =
  let t' = {t = after t.t} in
  Gc.finalise forget t';
  ignore t; (* t must be alive so that garbage collection doesn't
               cause t to be forgotten in the middle of before *)
  t'

let before t =
  let t' = {t = before t.t} in
  ignore t; (* t must be alive so that garbage collection doesn't
               cause t to be forgotten in the middle of before *)
  Gc.finalise forget t';
  t'

let same_order t1 t2 = same_order t1.t t2.t
let compare t1 t2 = compare t1.t t2.t
let cardinal t = cardinal t.t
