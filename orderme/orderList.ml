type t = {
  mutable tag: int;
  mutable prev: t;
  mutable next: t;
  counter: int ref;
}

let average x y = (x land y) + (x lxor y) / 2

let curr_index t = t.tag

let is_first t = t.prev == t || t.prev.tag > t.tag
let is_last  t = t == t.next || t.next.tag < t.tag
let is_valid t = t != t.next || t.tag = 0 ||
(Printf.eprintf "tag is %d, counter is %d\n" t.tag !(t.counter); false)

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

let root () =
  let rec t = { prev = t; next = t; tag = 0; counter = ref 1 } in
  t

let forget t =
  if t.next != t then
    begin
      let {prev; next; counter} = t in
      prev.next <- next;
      next.prev <- prev;
      t.prev <- t;
      t.next <- t;
      decr counter;
      if !counter = 1 then
        next.tag <- 0;
      t.tag <- min_int
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
  and low   = ref n.tag
  and high  = ref n.tag
  and level = ref 0
  and thresh = ref 1.0
  and range = ref 1
  in
  while (float !count >= float !range *. !thresh) && !level < uint_size do
    let toggle_bit = 1 lsl !level in
    incr level;
    thresh := !thresh /. t;
    range  := !range * 2;
    let to_left = (n.tag land toggle_bit) <> 0 in
    if to_left then begin
      low := !low lxor toggle_bit;
      while !left.tag > !low && not (is_first !left) do
        left := !left.prev;
        incr count;
      done
    end else begin
      high := !high lxor toggle_bit;
      while !right.tag < !high && not (is_last !right) do
        right := !right.next;
        incr count;
      done
    end;
  done;
  !left, !low, !range, !count

let rec relabel_span_big root step tag = function
  | 1 ->
    root.tag <- tag;
    assert (tag < next_index root)
  | n ->
    root.tag <- tag;
    relabel_span_big root.next step (tag + step) (n - 1)

let rec relabel_span_small node root slack tag = function
  | 1 ->
    root.tag <- tag;
    assert (tag < next_index root)
  | n ->
    root.tag <- tag;
    relabel_span_small node root.next slack
      (tag + if node == root then slack + 1 else 1) (n - 1)

let relabel node =
  let root, tag, range, count = find_span node in
  let step = range / count in
  if step = 1 then
    relabel_span_small node root (range - count) tag count
  else
    relabel_span_big root step tag count

let after t =
  assert (is_valid t);
  let tag = average (curr_index t) (next_index t) in
  let {next; counter} = t in
  let t' = {prev = t; next; tag; counter} in
  let next = if t.next != next then (t'.next <- t.next; t.next) else next in
  t.next <- t';
  next.prev <- t';
  incr counter;
  if t'.tag = prev_index t' then
    relabel t';
  t'

let before t =
  assert (is_valid t);
  let tag = average (prev_index t) (curr_index t) in
  let {prev; counter} = t in
  let t' = {prev; next = t; tag; counter} in
  let prev = if t.prev != prev then (t'.prev <- t.prev; t.prev) else prev in
  t.prev <- t';
  prev.next <- t';
  incr counter;
  if t'.tag = prev_index t' then
    relabel t';
  t'
