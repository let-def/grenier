type t = {t : Order_list.t}

(* Some values must be alive so that garbage collection doesn't
   cause them to be forgotten in the middle of a computation. *)
external i'am'alive : t -> unit =
  "ml_orderme_i_am_live" "ml_orderme_i_am_live" "noalloc"

open Order_list

let is_valid t =
  let result = is_valid t.t in
  i'am'alive t;
  result

let forget t = forget t.t
let root () =
  let t = {t = root ()} in
  Gc.finalise forget t;
  t

let after t =
  let t' = {t = after t.t} in
  Gc.finalise forget t';
  i'am'alive t;
  t'

let before t =
  let t' = {t = before t.t} in
  Gc.finalise forget t';
  i'am'alive t;
  t'

let same_order t1 t2 =
  let result = same_order t1.t t2.t in
  i'am'alive t1;
  i'am'alive t2;
  result

let compare t1 t2 =
  let result = compare t1.t t2.t in
  i'am'alive t1;
  i'am'alive t2;
  result

let cardinal t =
  let result = cardinal t.t in
  i'am'alive t;
  result

let unsafe_check t msg =
  unsafe_check t.t ("(Order_managed) " ^ msg);
  i'am'alive t
