type order = {
  mutable n: int;
  mutable n0: int;
}

type t = {
  mutable repr: Order_list.t;
  mutable tag: int;
  order: order;
  mutable prev: t;
  mutable next: t;
}

let rec sentinel = { repr = Order_list.root (); tag = 0;
                     order = { n = 0; n0 = 0 };
                     prev = sentinel; next = sentinel }

let is_global_last t = t.next == t
let is_global_first t = t.prev == t

let is_local_last t = is_global_last t || t.next.repr != t.repr
let is_local_first t = is_global_first t || t.prev.repr != t.repr

let average x y = (x land y) + (x lxor y) / 2

(** Check if two elements belong to the same order. O(1) *)
let same_order t1 t2 =
  t1.order == t2.order

(** Compare two elements. O(1) *)
let compare t1 t2 =
  if t1.repr == t2.repr then
    compare t1.tag t2.tag
  else
    Order_list.compare t1.repr t2.repr

(** How many elements are ordered. O(1) *)
let cardinal t = t.order.n

let is_valid t = t.prev != sentinel

let root () =
  let rec t = { repr = Order_list.root ();
                tag = 0; order = { n = 1; n0 = 1};
                prev = t; next = t}
  in
  t

let global_relabel t =
  (* prerr_endline "global_relabel"; *)
  let t =
    let rec first t =
      if is_global_first t then t
      else first t.prev
    in
    first t
  in
  let n = t.order.n in
  t.order.n0 <- n;
  let count = int_of_float (float n /. log (float n)) in
  let step = max_int / (count + 1) * 2 in
  let tag  = ref min_int in
  let repr = ref t.repr in
  let k    = ref count in
  let r    = ref t in
  while !r != sentinel do
    let t = !r in

    if !k = 0 then begin
      let repr' = Order_list.unsafe_next !repr in
      repr := (if repr' == !repr then
                 Order_list.after repr'
               else repr');
      tag := min_int;
      k := count;
    end;

    tag := !tag + step;
    (* Printf.eprintf "tag = %d\n" !tag; *)

    t.tag <- !tag;
    t.repr <- !repr;
    decr k;

    if is_global_last t then
      r := sentinel
    else
      r := (!r).next
  done;
  if !repr != Order_list.unsafe_next !repr then begin
    let rec release repr =
      let repr' = Order_list.unsafe_next repr in
      Order_list.forget repr;
      if repr != repr' then release repr'
    in
    release (Order_list.unsafe_next !repr)
  end

let local_relabel t =
  (* prerr_endline "local_relabel"; *)
  let count = ref 1 in
  let first =
    let t = ref t in
    while not (is_local_first !t) do
      incr count;
      t := (!t).prev
    done;
    !t
  in
  let count =
    let t = ref t in
    while not (is_local_last !t) do
      incr count;
      t := (!t).next
    done;
    !count
  in
  let step = max_int / (count + 1) * 2 in
  let rec aux0 t tag = function
    | 0 -> t
    | n ->
      t.tag <- tag;
      (* Printf.eprintf "tag0 = %d\n" tag; *)
      aux0 t.next (tag + step) (n - 1)
  in
  let mid = aux0 first (min_int + step) (count / 2) in
  let repr = Order_list.after t.repr in
  let rec aux1 t tag =
    let is_local_last = is_local_last t in
    t.repr <- repr;
    t.tag <- tag;
    (* Printf.eprintf "tag1 = %d\n" tag; *)
    if not is_local_last then
      aux1 t.next (tag + step)
  in
  aux1 mid (min_int + step)

let relabel t =
  (* prerr_endline "relabel"; *)
  let {n; n0} = t.order in
  if n > Sys.word_size && (n * 3 < n0 * 2 || n0 * 3 < n * 2) then
    global_relabel t
  else
    local_relabel t

let after t =
  (* prerr_endline "after"; *)
  assert (is_valid t);
  let tag1 = t.tag in
  let tag2 = if is_local_last t then max_int else t.next.tag in
  let tag = average tag1 tag2 in
  let {next; repr; order} = t in
  let t' = {repr; tag; order; prev = t; next} in
  if is_global_last t then
    t'.next <- t'
  else
    next.prev <- t';
  t.next <- t';
  order.n <- order.n + 1;
  if tag = tag1 || tag = tag2 then relabel t;
  t'

let before t =
  (* prerr_endline "before"; *)
  assert (is_valid t);
  let tag1 = if is_local_first t then min_int else t.prev.tag in
  let tag2 = t.tag in
  let tag = average tag1 tag2 in
  let {prev; repr; order} = t in
  let t' = {repr; tag; order; prev; next = t} in
  if is_global_first t then
    t'.prev <- t'
  else
    prev.next <- t';
  t.prev <- t';
  order.n <- order.n + 1;
  if tag = tag1 || tag = tag2 then relabel t';
  t'

let forget t =
  (* prerr_endline "forget"; *)
  if is_valid t then
  begin
    (* Update inner order *)
    if is_local_first t && is_local_last t then
      Order_list.forget t.repr;
    (* Update linked list *)
    let {next; prev} = t in
    if is_global_first t then
      next.prev <- next
    else
      next.prev <- prev;
    if is_global_last t then
      prev.next <- prev
    else
      prev.next <- next;
    (* Update global order *)
    t.order.n <- t.order.n - 1;
    t.prev <- sentinel;
    t.next <- sentinel;
    t.repr <- sentinel.repr;
  end

let check t =
  assert (Order_list.is_valid t.repr);
  assert (t.order == t.next.order);
  assert (t.order == t.prev.order);
  assert (Order_list.compare t.prev.repr t.repr <= 0);
  assert (Order_list.compare t.repr t.next.repr <= 0);
  if is_local_first t then begin
    assert (Order_list.same_order t.prev.repr t.repr);
    if not (is_global_first t) then
      assert (Order_list.compare t.prev.repr t.repr < 0);
  end
  else begin
    assert (t.repr == t.prev.repr);
    assert (t.prev.tag < t.tag);
  end;
  if is_local_last t then begin
    assert (Order_list.same_order t.repr t.next.repr);
    if not (is_global_last t) then
      assert (Order_list.compare t.repr t.next.repr < 0);
  end
  else begin
    assert (t.repr == t.next.repr);
    assert (t.tag < t.next.tag);
  end

let unsafe_check t msg =
  Order_list.unsafe_check t.repr msg;
  try
    if is_valid t then check t
    else begin
      assert (t.prev == sentinel);
      assert (t.next == sentinel);
    end
  with Assert_failure (file, line, col) ->
    raise (Assert_failure (msg ^ ": " ^ file, line, col))
