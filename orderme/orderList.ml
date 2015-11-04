module L : sig
  type 'a t
  val root : 'a -> 'a t
  val after  : 'a t -> 'a t
  val before : 'a t -> 'a t
  val get_data : 'a t -> 'a
  val set_data : 'a t -> 'a -> unit
  val get_order : 'a t -> int ref
  val set_order : 'a t -> int ref -> unit
  val cardinal : 'a t -> int
  val compare : 'a t -> 'a t -> int
  val forget : 'a t -> unit
  val is_valid : 'a t -> bool
  val split : 'a t t -> unit
  val relabel : int -> 'a t t -> unit
end = struct
  type 'a t = {
    mutable tag: int;
    mutable prev: 'a t;
    mutable next: 'a t;
    mutable data: 'a;
    mutable order: int ref;
  }

  let average x y = (x land y) + (x lxor y) / 2

  let curr_index t = t.tag

  let is_first t = t.order != t.prev.order
  let is_last  t = t.order != t.next.order

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

  let invalid_order = ref 0

  let root data =
    let rec t = { prev; next; tag = 0; order = ref 1; data }
    and prev = { prev; next = t; tag = 0; order = invalid_order; data }
    and next = { prev = t; next; tag = 0; order = invalid_order; data }
    in
    t

  let is_valid t = t.order != invalid_order

  let forget t =
    if t.next != t then
      begin
        let {prev; next; order} = t in
        prev.next <- next;
        next.prev <- prev;
        t.prev <- t;
        t.next <- t;
        decr order;
        t.order <- invalid_order
      end

  let get_data t = t.data
  let set_data t x = t.data <- x

  let get_order t = t.order
  let set_order t order = t.order <- order
  let cardinal t = !(t.order)

  let compare t1 t2 =
    assert (t1.order == t2.order);
    compare t1.tag t2.tag

  let cardinal t = !(t.order)

  let uint_size = Sys.word_size - 2
  let pow = 2.0 ** float uint_size
  let inv = 1.0 /. float uint_size

  let optimal_t count = (pow /. float count) ** inv

  let find_span n =
    let t = optimal_t !(n.order) in
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
    let {next; order; data} = t in
    let t' = {prev = t; next; tag; order; data} in
    let next = if t.next != next then (t'.next <- t.next; t.next) else next in
    t.next <- t';
    next.prev <- t';
    incr order;
    if t'.tag = prev_index t' then
      relabel t';
    t'

  let before t =
    assert (is_valid t);
    let tag = average (prev_index t) (curr_index t) in
    let {prev; order; data} = t in
    let t' = {prev; next = t; tag; order; data} in
    let prev = if t.prev != prev then (t'.prev <- t.prev; t.prev) else prev in
    t.prev <- t';
    prev.next <- t';
    incr order;
    if t'.tag = prev_index t' then
      relabel t';
    t'

  let rec split_reorder step order data t tag = function
    | 0 -> t
    | n ->
      assert (t.next != t);
      t.tag <- tag;
      if t.order != order then t.order <- order;
      if t.data  != data  then t.data  <- data;
      split_reorder step order data t.next (tag + step) (n - 1)

  let split tt =
    let rec first tt =
      if not (is_first tt) then
        first tt.prev
      else
        tt
    in
    let tt = first tt in
    let n0 = cardinal tt in
    let n  = n0 / 2 in
    let n' = n0 - n in
    tt.order := n;
    let order' = ref n0 in
    let data' = after (get_data tt) in
    let step = max_int / n in
    let tt = split_reorder step tt.order tt.data tt (min_int + step) n in
    let tt = split_reorder step order' data' tt (min_int + step) n' in
    ignore tt


  let next_outer outer =
    if is_last outer then
      after outer
    else
      outer.next

  let forget_tail t =
    if t.next == t.next.next then ()
    else
      let rec sentinel t =
        if t.next == t then t
        else (decr t.order; sentinel t.next)
      in
      let sentinel = sentinel t.next in
      let next = t.next in
      next.prev <- next;
      sentinel.prev <- t;
      t.next <- sentinel

  let rec relabel chunk total outer t =
    if total <= chunk then
      let chunk = total in
      let step = max_int / chunk in
      let t = split_reorder step (ref chunk) outer t (min_int + step) chunk in
      assert (t == t.next);
      forget_tail outer
    else
      let step = max_int / chunk in
      let t = split_reorder step (ref chunk) outer t (min_int + step) chunk in
      relabel chunk (total - chunk) (next_outer outer) t

  let relabel cardinal t =
    let rec sentinel t =
      if t.prev == t then t
      else sentinel t.prev
    in
    let t = (sentinel t).next in
    let chunk = int_of_float (float cardinal /. log (float cardinal)) in
    relabel chunk cardinal t.data t
end

type order = {
  mutable cardinal: int;
  mutable n0: int;
}

type t = order L.t L.t

let root () =
  let order = { cardinal = 1; n0 = 1 } in
  let outer = L.root order in
  let inner = L.root outer in
  inner

let global_rebalance order t =
  let cardinal = order.cardinal in
  order.n0 <- cardinal;
  L.relabel cardinal t

let rebalance order t =
  if order.cardinal >= 60 then begin
    if 3 * order.cardinal <= order.n0 || order.cardinal >= 2 * order.n0 then
      global_rebalance order t
    else if (L.cardinal t) > 4 * (L.cardinal (L.get_data t)) then
      L.split t
  end

let order t = L.get_data (L.get_data t)

let after t =
  let t = L.after t in
  let order = order t in
  order.cardinal <- order.cardinal + 1;
  rebalance order t;
  t

let before t =
  let t = L.before t in
  let order = order t in
  order.cardinal <- order.cardinal + 1;
  rebalance order t;
  t

let same_order t1 t2 =
  L.is_valid t1 && L.is_valid t2 && order t1 == order t2

let compare t1 t2 =
  let t1' = L.get_data t1 and t2' = L.get_data t2 in
  if t1' == t2' then L.compare t1 t2 else L.compare t1' t2'

let cardinal t = (order t).cardinal

let forget t =
  if L.is_valid t then
    let order = order t in
    order.cardinal <- order.cardinal - 1;
    if L.cardinal t = 1 then
      begin
        L.forget (L.get_data t)
      end;
    L.forget t

let is_valid t = L.is_valid t
