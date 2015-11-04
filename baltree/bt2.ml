type (+'a, +'b) t =
  | Leaf
  | Node of int * ('a, 'b) t * 'a * 'b * ('a, 'b) t

let size = function
  | Node (s, _, _, _, _) -> s
  | Leaf -> 0

(** {1 Balance criteria}
  Functions are not symmetric.
  The first argument should always be of the same power of two or smaller
  (guaranteed by construction). *)

(** [smaller_ell smin smax] iff
    - [smin] is less than [smax]
    - [smin] and [smax] differs by less than two magnitude orders, i.e
      msbs(smin) >= msbs(smax) - 1
    where msbs is the index of the most significant bit set *)
let smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)

(** [disbalanced smin smax] check if two sub-trees of size [smin] and [smax],
    are disbalanced. That is, msbs(smin) < msbs(smax) - 1 *)
let disbalanced smin smax = smaller_ell smin (smax lsr 1)

(** {1 Smart but not too much constructors} *)

(** Construct node and check balance *)
let node_ l x0 x1 r =
  let sl = size l and sr = size r in
  if sl < sr then
    assert (not (disbalanced sl sr))
  else
    assert (not (disbalanced sr sl));
  Node (sl + 1 + sr, l, x0, x1, r)

(** Construct Node *)
let node_ l x0 x1 r = Node (size l + 1 + size r, l, x0, x1, r)

(** Rotations *)
let rot_left l x0 x1 r k = match r with
  | Node (_, rl, y0, y1, rr) ->
    k (node_ l x0 x1 rl) y0 y1 rr
  | _ -> assert false

let rot_right l y0 y1 r k = match l with
  | Node (_, ll, x0, x1, lr) ->
    k ll x0 x1 (node_ lr y0 y1 r)
  | _ -> assert false

(** Balancing *)
let smaller_ell a b = (a < b) && ((a land b) lsl 1 < b)

let inc_left l x0 x1 r k =
  let r = match r with
    | Node (_, rl, y0, y1, rr) when smaller_ell (size rr) (size rl) ->
      rot_right rl y0 y1 rr node_
    | _ -> r
  in
  rot_left l x0 x1 r k

let inc_right l y0 y1 r k =
  let l = match l with
    | Node (_, ll, x0, x1, lr) when smaller_ell (size ll) (size lr) ->
      rot_left ll x0 x1 lr node_
    | _ -> l
  in
  rot_right l y0 y1 r k

(** Balance trees leaning to the right *)
let rec node_left l x0 x1 r =
  if disbalanced (size l) (size r) then
    inc_left l x0 x1 r node_left
  else
    node_ l x0 x1 r

(** Balance trees leaning to the left *)
let rec node_right l y0 y1 r =
  if disbalanced (size r) (size l) then
    inc_right l y0 y1 r node_right
  else
    node_ l y0 y1 r

(** Public interface *)

let leaf = Leaf

let node l x0 x1 r = match l, r with
  | Leaf, Leaf -> node_ leaf x0 x1 leaf
  | l, r when size l < size r ->
    node_left l x0 x1 r
  | l, r ->
    node_right l x0 x1 r

let rec join l r = match l, r with
  | Leaf, t | t, Leaf -> t
  | Node (sl, ll, x0, x1, lr), Node (sr, rl, y0, y1, rr) ->
    if sl <= sr then
      node (join l rl) y0 y1 rr
    else
      node ll x0 x1 (join lr r)

let rec rank n = function
  | Leaf -> raise Not_found
  | Node (_, l, x0, x1, r) ->
    let sl = size l in
    if n = sl then
      x0, x1
    else if n < sl then
      rank n l
    else
      rank (n - 1 - sl) r
