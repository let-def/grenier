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

(** Construct node and check balance
    let node_ l x r =
      let sl = size l and sr = size r in
      if sl < sr then
        assert (not (disbalanced sl sr))
      else
        assert (not (disbalanced sr sl));
      Node (sl + 1 + sr, l, x, r)
*)

(** Construct Node *)
let node_ l x r = Node (size l + 1 + size r, l, x, r)

(** Rotations *)
let rot_left l x r k = match r with
  | Node (_, rl, y, rr) ->
    k (k l x rl) y rr
  | _ -> assert false

let rot_right l y r k = match l with
  | Node (_, ll, x, lr) ->
    k ll x (k lr y r)
  | _ -> assert false

(** Balancing *)
let inc_left l x r k =
  let r = match r with
    | Node (_, rl, y, rr) when smaller_ell (size rr) (size rl) ->
      rot_right rl y rr k
    | _ -> r
  in
  rot_left l x r k

let inc_right l y r k =
  let l = match l with
    | Node (_, ll, x, lr) when smaller_ell (size ll) (size lr) ->
      rot_left ll x lr k
    | _ -> l
  in
  rot_right l y r k

(** Balance a node in both directions: a rotation that fixes one side can
    leave the other side too heavy, so the nodes it builds are balanced the
    same way. *)
let rec node_balance l x r =
  let sl = size l and sr = size r in
  if disbalanced sl sr then
    inc_left l x r node_balance
  else if disbalanced sr sl then
    inc_right l x r node_balance
  else
    node_ l x r

(** Public interface *)

let leaf = Leaf

let node l x r = match l, r with
  | Leaf, Leaf -> node_ leaf x leaf
  | l, r -> node_balance l x r

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
