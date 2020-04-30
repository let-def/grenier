type +'a t =
  | Empty
  | Leaf of { mutable mark: int; v: 'a; }
  | Join of { mutable mark: int; l: 'a t; r: 'a t; }

type (+'a, 'b) xform =
  | XEmpty
  | XLeaf of { a: 'a t; mutable b: 'b option; }
  | XJoin of { a: 'a t; mutable b: 'b option;
               l: ('a, 'b) xform; r: ('a, 'b) xform; }

let empty = Empty

let element v = Leaf { mark = 0; v }

let concat a b = match a, b with
  | Empty, x | x, Empty -> x
  | l, r -> Join { mark = 0; l; r }

type stats = {
  mutable marked: int;
  mutable shared: int;
}
let mk_stats () = { marked = 0; shared = 0 }

let mask_bits = 2
let old_mask = 1
let new_mask = 2
let both_mask = 3

let rec discard stats mask = function
  | Empty -> ()
  | Leaf t ->
    let mark = t.mark in
    if mark <> 0 && mark land mask = 0 then (
      stats.marked <- stats.marked + 1;
      stats.shared <- stats.shared + 1;
      t.mark <- mark lor mask;
    )
  | Join t ->
    let mark = t.mark in
    if mark <> 0 && mark land mask = 0 then (
      stats.marked <- stats.marked + 1;
      stats.shared <- stats.shared + 1;
      t.mark <- mark lor mask;
      discard stats mask t.l;
      discard stats mask t.r;
    )

let enqueue stats q mask = function
  | Empty -> ()
  | Leaf t -> t.mark <- t.mark lor mask
  | Join t as node ->
    let mark = t.mark in
    if mark land mask = 0 then (
      stats.marked <- stats.marked + 1;
      if mark = 0 then (
        t.mark <- mask;
        Queue.push node q
      ) else (
        stats.shared <- stats.shared + 1;
        t.mark <- mark lor mask;
        discard stats mask t.l;
        discard stats mask t.r;
      )
    )

let dequeue stats q mask =
  match Queue.pop q with
  | Join t ->
    if t.mark = mask then (
      enqueue stats q mask t.l;
      enqueue stats q mask t.r;
    )
  | _ -> assert false

let traverse1 stats q mask =
  while not (Queue.is_empty q) do
    dequeue stats q mask
  done

let rec traverse sold snew qold qnew =
  if Queue.is_empty qold then
    traverse1 snew qnew new_mask
  else if Queue.is_empty qnew then
    traverse1 sold qold old_mask
  else (
    dequeue sold qold old_mask;
    dequeue snew qnew new_mask;
    traverse sold snew qold qnew
  )

type ('a, 'b) unmark_state = {
  dropped : 'b option array;
  mutable dropped_index : int;
  shared : ('a, 'b) xform array;
  mutable shared_index: int;
}

let rec unmark_old st = function
  | XEmpty -> ()
  | XLeaf {a = Empty | Join _; _} -> assert false
  | XJoin {a = Empty | Leaf _; _} -> assert false
  | XLeaf {a = Leaf t'; b}  as t ->
    let mark = t'.mark land both_mask in
    if mark = old_mask then (
      let dropped_index = st.dropped_index in
      st.dropped.(dropped_index) <- b;
      st.dropped_index <- dropped_index + 1;
      t'.mark <- 0
    ) else if mark = both_mask then (
      let shared_index = st.shared_index in
      st.shared.(shared_index) <- t;
      st.shared_index <- shared_index + 1;
      t'.mark <- shared_index lsl mask_bits;
    )
  | XJoin {a = Join t'; l; r; b} as t ->
    let mark = t'.mark land both_mask in
    if mark <> 0 then (
      if mark = old_mask then (
        let dropped_index = st.dropped_index in
        st.dropped.(dropped_index) <- b;
        st.dropped_index <- dropped_index + 1;
        t'.mark <- 0
      )
      else if mark = both_mask then (
        let shared_index = st.shared_index in
        st.shared.(shared_index) <- t;
        st.shared_index <- shared_index + 1;
        t'.mark <- shared_index lsl mask_bits;
      );
      unmark_old st l;
      unmark_old st r;
    )

let rec unmark_new st = function
  | Empty -> XEmpty
  | Leaf t' as t ->
    let mark = t'.mark land both_mask in
    if mark = new_mask then (
      let shared_index = st.shared_index in
      let x = XLeaf {a = t; b = None} in
      st.shared.(shared_index) <- x;
      st.shared_index <- shared_index + 1;
      t'.mark <- shared_index lsl 2;
      x
    ) else (
      assert (mark = 0);
      st.shared.(t'.mark lsr mask_bits)
    )
  | Join t' as t ->
    let mark = t'.mark land both_mask in
    if mark = new_mask then (
      let shared_index = st.shared_index in
      st.shared_index <- shared_index + 1;
      let l = unmark_new st t'.l in
      let r = unmark_new st t'.r in
      let x = XJoin {a = t; b = None; l; r} in
      st.shared.(shared_index) <- x;
      t'.mark <- shared_index lsl 2;
      x
    ) else (
      assert (mark = 0);
      st.shared.(t'.mark lsr mask_bits)
    )

let diff xold tnew = match xold, tnew with
  | XEmpty, Empty -> [||], XEmpty
  | (XLeaf {a; _} | XJoin {a; _}), _ when a == tnew -> [||], xold
  | _ ->
    let qold = Queue.create () in
    let sold = mk_stats () in
    let qnew = Queue.create () in
    let snew = mk_stats () in
    begin match xold with
      | XEmpty -> ()
      | (XLeaf {a; _} | XJoin {a; _}) ->
        enqueue sold qold old_mask a
    end;
    enqueue snew qnew new_mask tnew;
    traverse sold snew qold qnew;
    let nb_shared = sold.shared + snew.shared in
    let nb_dropped = sold.marked - nb_shared in
    let st = {
      dropped = Array.make nb_dropped None;
      dropped_index = 0;
      shared = Array.make snew.marked XEmpty;
      shared_index = 0;
    } in
    unmark_old st xold;
    assert (st.dropped_index = nb_dropped);
    let result = unmark_new st tnew in
    Array.iter (function
        | XEmpty
        | XLeaf {a = Empty | Join _; _}
        | XJoin {a = Empty | Leaf _; _} -> assert false
        | XLeaf {a = Leaf t; _} -> t.mark <- 0
        | XJoin {a = Join t; _} -> t.mark <- 0
      ) st.shared;
    st.dropped, result

type ('a, 'b) map_reduce = ('a -> 'b) * ('b -> 'b -> 'b)
let map (f, _) x = f x
let reduce (_, f) x y = f x y

let eval map_reduce = function
  | XEmpty -> None
  | other ->
    let rec aux = function
      | XEmpty | XLeaf {a = Empty | Join _; _} -> assert false
      | XLeaf {b = Some b; _} | XJoin {b = Some b; _} -> b
      | XLeaf ({a = Leaf t';_ } as t) ->
        let result = map map_reduce t'.v in
        t.b <- Some result;
        result
      | XJoin t ->
        let l = aux t.l and r = aux t.r in
        let result = reduce map_reduce l r in
        t.b <- Some result;
        result
    in
    Some (aux other)

type ('a, 'b) reducer = ('a, 'b) map_reduce * ('a, 'b) xform

let reducer ~map ~reduce = ((map, reduce), XEmpty)

let update (map_reduce, old_tree : _ reducer) new_tree : _ array * _ reducer =
  let dropped, tree = diff old_tree new_tree in
  dropped, (map_reduce, tree)

let reduce (map_reduce, tree : _ reducer) =
  eval map_reduce tree
