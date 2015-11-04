type 'a t =
  | Leaf
  | Node of int * 'a t * 'a * 'a t

let size = function
  | Node (s, _, _, _) -> s
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
let node_ l x r =
  let sl = size l and sr = size r in
  if sl < sr then
    assert (not (disbalanced sl sr))
  else
    assert (not (disbalanced sr sl));
  Node (sl + 1 + sr, l, x, r)

(** Construct Node *)
let node_ l x r = Node (size l + 1 + size r, l, x, r)

(** Rotations *)
let rot_left l x r k = match r with
  | Node (_, rl, y, rr) ->
    k (node_ l x rl) y rr
  | _ -> assert false

let rot_right l y r k = match l with
  | Node (_, ll, x, lr) ->
    k ll x (node_ lr y r)
  | _ -> assert false

(** Balancing *)
let smaller_ell a b = (a < b) && ((a land b) lsl 1 < b)

let inc_left l x r k =
  let r = match r with
    | Node (_, rl, y, rr) when smaller_ell (size rr) (size rl) ->
      rot_right rl y rr node_
    | _ -> r
  in
  rot_left l x r k

let inc_right l y r k =
  let l = match l with
    | Node (_, ll, x, lr) when smaller_ell (size ll) (size lr) ->
      rot_left ll x lr node_
    | _ -> l
  in
  rot_right l y r k

(** Balance trees leaning to the right *)
let rec node_left l x r =
  if disbalanced (size l) (size r) then
    inc_left l x r node_left
  else
    node_ l x r

(** Balance trees leaning to the left *)
let rec node_right l y r =
  if disbalanced (size r) (size l) then
    inc_right l y r node_right
  else
    node_ l y r

(** Public interface *)

let leaf = Leaf

let node l x r = match l, r with
  | Leaf, Leaf -> node_ leaf x leaf
  | l, r when size l < size r ->
    node_left l x r
  | l, r ->
    node_right l x r

let rec join l r = match l, r with
  | Leaf, t | t, Leaf -> t
  | Node (sl, ll, x, lr), Node (sr, rl, y, rr) ->
    if sl <= sr then
      node (join l rl) y rr
    else
      node ll x (join lr r)

let rec rank n = function
  | Leaf -> raise Not_found
  | Node (_, l, x, r) ->
    let sl = size l in
    if n = sl then
      x
    else if n < sl then
      rank n l
    else
      rank (n - 1 - sl) r
