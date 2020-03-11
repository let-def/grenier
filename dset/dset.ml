type +'a t =
  | Empty
  | Leaf of {
      mutable mark: int;
      v: 'a;
    }
  | Join of {
      mutable mark: int;
      l: 'a t;
      r: 'a t;
    }

let empty = Empty

let element v = Leaf { mark = 0; v }

let union a b = match a, b with
  | Empty, x | x, Empty -> x
  | l, r -> Join { mark = 0; l; r }

let rec mark_all mask = function
  | Empty -> ()
  | Leaf t -> t.mark <- t.mark lor mask
  | Join t ->
    let mark = t.mark in
    if mark <> 0 && mark land mask = 0 then (
      t.mark <- mark lor mask;
      mark_all mask t.l;
      mark_all mask t.r;
    )

let enqueue q mask = function
  | Empty -> ()
  | Leaf t -> t.mark <- t.mark lor mask
  | Join t as node ->
    let mark = t.mark in
    if mark land mask = 0 then (
      if mark = 0 then (
        t.mark <- mask;
        Queue.push node q
      ) else (
        t.mark <- mark lor mask;
        mark_all mask t.l;
        mark_all mask t.r;
      )
    )

let dequeue q mask =
  match Queue.pop q with
  | Join t ->
    if t.mark = mask then (
      enqueue q mask t.l;
      enqueue q mask t.r;
    )
  | _ -> assert false

let traverse1 q mask =
  while not (Queue.is_empty q) do
    dequeue q mask
  done

let old_mask = 1
let new_mask = 2

let rec traverse qold qnew =
  if Queue.is_empty qold then
    traverse1 qnew new_mask
  else if Queue.is_empty qnew then
    traverse1 qold old_mask
  else (
    dequeue qold old_mask;
    dequeue qnew new_mask;
    traverse qold qnew
  )

type +'a diff = { left_only : 'a list; right_only : 'a list }

type +'a marking = {
  mutable valid : bool;
  left : 'a t;
  right : 'a t;
}

let mark ~left ~right =
  if left != right then (
    let qold = Queue.create () in
    let qnew = Queue.create () in
    enqueue qold old_mask left;
    enqueue qnew new_mask right;
    traverse qold qnew
  );
  { valid = true; left; right }

let unmark_and_diff marking =
  assert (marking.valid);
  marking.valid <- false;
  let right_only = ref [] in
  let left_only = ref [] in
  let rec unmark = function
    | Empty -> ()
    | Leaf ({mark; v} as t) ->
      t.mark <- 0;
      if mark = old_mask then (
        left_only := v :: !left_only;
      ) else if mark = new_mask then (
        right_only := v :: !right_only;
      )
    | Join t ->
      if t.mark <> 0 then (
        t.mark <- 0;
        unmark t.l;
        unmark t.r;
      )
  in
  unmark marking.left;
  unmark marking.right;
  { left_only = !left_only; right_only = !right_only }

let unmark marking =
  assert (marking.valid);
  marking.valid <- false;
  let rec unmark = function
    | Empty -> ()
    | Leaf ({mark; v} as t) ->
      t.mark <- 0
    | Join t ->
      if t.mark <> 0 then (
        t.mark <- 0;
        unmark t.l;
        unmark t.r;
      )
  in
  unmark marking.left;
  unmark marking.right

let diff ~left ~right =
  if left == right then
    { left_only = []; right_only = [] }
  else
    unmark_and_diff (mark left right)

type mark =
  | Left
  | Right
  | Both

let get_mark marking = function
  | Leaf {mark; _} ->
    assert (marking.valid);
    if mark = old_mask then Left
    else if mark = new_mask then Right
    else Both
  | _ -> Both

type 'a view =
  | Empty
  | Union of 'a t * 'a t
  | Element of 'a

let view : 'a t -> 'a view = function
  | Empty -> Empty
  | Leaf {v; _} -> Element v
  | Join {l; r; _} -> Union (l, r)
