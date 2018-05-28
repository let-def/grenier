type t = {
  mutable tag: int;
  mutable prev: t;
  mutable next: t;
  counter: int ref;
}

let average x y = (x land y) + (x lxor y) / 2

let curr_index t = t.tag

let rec sentinel = { tag = 0; prev = sentinel; next = sentinel; counter = ref 0 }

let is_first t = t.prev == t
let is_last  t = t == t.next

let is_valid t = t.next != sentinel

let prev_index t =
  if is_first t then
    min_int
  else
    t.prev.tag

let next_index t =
  if is_last t then
    max_int
  else
    t.next.tag

let check t =
  assert (is_valid t);
  assert (is_valid t.prev);
  assert (is_valid t.next);
  assert (t == t.prev || t.prev.next == t);
  assert (t == t.next || t.next.prev == t);
  if t.next != t then
    assert (t.next.tag > t.tag);
  if t.prev != t then
    assert (t.prev.tag < t.tag)

let consistent _ = ()
let consistents _ _ = ()

(*let rec consistents t = function
  | 0 -> ()
  | 1 -> consistent t
  | n ->
    consistent t;
    assert (t.next != t);
    consistents t (n - 1)*)

let root () =
  let rec t = { prev = t; next = t; tag = 0; counter = ref 1 } in
  consistent t;
  t

let forget t =
  if is_valid t then begin
    let {prev; next; counter; _} = t in
    if is_first t then
      next.prev <- next
    else if is_last t then
      prev.next <- prev
    else (
      prev.next <- next;
      next.prev <- prev;
    );
    decr counter;
    t.next <- sentinel;
    t.prev <- sentinel;
    consistent prev;
    consistent next;
    assert (not (is_valid t));
  end

let same_order t1 t2 =
  is_valid t1 &&
  is_valid t2 &&
  t1.counter == t2.counter

let compare t1 t2 =
  assert (same_order t1 t2);
  compare t1.tag t2.tag

let cardinal t = if is_valid t then !(t.counter) else 0

let uint_size = Sys.word_size - 2
let pow = 2.0 ** float uint_size
let inv = 1.0 /. float uint_size

let optimal_t count = (pow /. float count) ** inv

let find_span n =
  let t = optimal_t !(n.counter) in
  let count = ref 1
  and left  = ref n
  and right = ref n
  and tag   = n.tag
  and low   = ref n.tag
  and high  = ref n.tag
  and bit = ref 1
  and thresh = ref 1.0
  in
  while !bit > 0 && (float !count >= float !bit *. !thresh) do
    let to_left = (tag land !bit) <> 0 in
    if to_left then begin
      low := !low lxor !bit;
      while !left.tag > !low && not (is_first !left) do
        left := !left.prev;
        incr count;
      done
    end else begin
      high := !high lxor !bit;
      while !right.tag < !high && not (is_last !right) do
        right := !right.next;
        incr count;
      done
    end;
    bit := !bit lsl 1;
    thresh := !thresh /. t;
  done;
  !left, !low, (!bit lsr 1), !count

let rec relabel_span_big root step tag = function
  | 1 ->
    root.tag <- tag;
    assert (tag < next_index root || is_last root)
  | n ->
    root.tag <- tag;
    assert (tag > prev_index root);
    relabel_span_big root.next step (tag + step) (n - 1)

let rec relabel_span_small node root slack tag = function
  | 1 ->
    root.tag <- tag;
    assert (tag < next_index root || is_last root)
  | n ->
    root.tag <- tag;
    (*Printf.eprintf "assert (%d > %d); slack = %d\n"
      tag (prev_index root) slack;*)
    assert (tag > prev_index root);
    relabel_span_small node root.next slack
      (tag + if node == root then slack + 1 else 1) (n - 1)

let relabel node =
  let root, tag, range, count = find_span node in
  let step = range / count in
  (*Printf.eprintf "range = %d, count = %d\n" range count;*)
  if step <= 1 then
    (assert (range >= count);
     relabel_span_small node root (range - count) (tag + 1) count)
  else
    relabel_span_big root step (tag + step) count;
  consistents root count

let after t =
  assert (is_valid t);
  let tag = average (curr_index t) (next_index t) in
  (* IMPORTANT
     Allocation must be done before reading t.prev/t.next.
     It might trigger a garbage collection which can invalidate the
     linked list (e.g if used through Order_managed).
  *)
  let t' = {prev = t; next = t; tag; counter = t.counter} in
  let {next; counter; _} = t in
  if t == next then
    t'.next <- t'
  else (
    t'.next <- next;
    next.prev <- t'
  );
  t.next <- t';
  incr counter;
  if t'.tag = prev_index t' then
    relabel t';
  consistent t;
  consistent t';
  t'

let before t =
  assert (is_valid t);
  let tag = average (prev_index t) (curr_index t) in
  (* IMPORTANT
     Allocation must be done before reading t.prev/t.next.
     It might trigger a garbage collection which can invalidate the
     linked list (e.g if used through Order_managed).
  *)
  let t' = {prev = t; next = t; tag; counter = t.counter} in
  let {prev; counter; _} = t in
  if t == prev then
    t'.prev <- t'
  else (
    t'.prev <- prev;
    prev.next <- t'
  );
  t.prev <- t';
  incr counter;
  if t'.tag = prev_index t' then
    relabel t';
  consistent t;
  consistent t';
  t'

let unsafe_next t = t.next
let unsafe_prev t = t.prev

let unsafe_check t msg =
  try
    if is_valid t then check t
    else begin
      assert (t.prev == sentinel);
      assert (t.next == sentinel);
    end
  with Assert_failure (file, line, col) ->
    raise (Assert_failure (msg ^ ": " ^ file, line, col))
