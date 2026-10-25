type (+'a, +'b) t =
  | Leaf
  | Node of int * ('a, 'b) t * 'a * 'b * ('a, 'b) t

let[@inline] size = function
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
let[@inline] smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)

(** [disbalanced smin smax] check if two sub-trees of size [smin] and [smax],
    are disbalanced. That is, msbs(smin) < msbs(smax) - 1 *)
let[@inline] disbalanced smin smax = smaller_ell smin (smax lsr 1)

(** {1 Smart but not too much constructors} *)

(** Construct node and check balance
    let node_ l x0 x1 r =
      let sl = size l and sr = size r in
      if sl < sr then
        assert (not (disbalanced sl sr))
      else
        assert (not (disbalanced sr sl));
      Node (sl + 1 + sr, l, x0, x1, r)
*)

(** Construct Node *)
let node_ sl l x0 x1 sr r = Node (sl + 1 + sr, l, x0, x1, r)

(** Rotations *)
let rot_left l x0 x1 r k = match r with
  | Node (_, rl, y0, y1, rr) ->
    k (k l x0 x1 rl) y0 y1 rr
  | _ -> assert false

let rot_right l y0 y1 r k = match l with
  | Node (_, ll, x0, x1, lr) ->
    k ll x0 x1 (k lr y0 y1 r)
  | _ -> assert false

(** Balancing *)

let inc_left l x0 x1 r k =
  let r = match r with
    | Node (_, rl, y0, y1, rr) when smaller_ell (size rr) (size rl) ->
      rot_right rl y0 y1 rr k
    | _ -> r
  in
  rot_left l x0 x1 r k

let inc_right l y0 y1 r k =
  let l = match l with
    | Node (_, ll, x0, x1, lr) when smaller_ell (size ll) (size lr) ->
      rot_left ll x0 x1 lr k
    | _ -> l
  in
  rot_right l y0 y1 r k

(** Balance a node in both directions: a rotation that fixes one side can
    leave the other side too heavy, so the nodes it builds are balanced the
    same way. *)
let rec node_balance l x0 x1 r =
  let sl = size l and sr = size r in
  (* Fast path: [disbalanced a b] requires [a < b / 2]. *)
  if sr <= 2 * sl + 1 && sl <= 2 * sr + 1 then
    node_ sl l x0 x1 sr r
  else if disbalanced sl sr then
    inc_left l x0 x1 r node_balance
  else if disbalanced sr sl then
    inc_right l x0 x1 r node_balance
  else
    node_ sl l x0 x1 sr r

let[@inline] balanced sa sb = not (disbalanced sa sb) && not (disbalanced sb sa)

(** [node_left sl l x0 x1 sr r] builds a node whose left child may be too
    heavy, by no more than [link] makes it: a single or double rotation
    restores the balance, without checking the nodes it builds. *)
let node_left sl l x0 x1 sr r =
  if not (disbalanced sr sl) then node_ sl l x0 x1 sr r else
    match l with
    | Node (_, ll, y0, y1, lr) ->
      let sll = size ll and slr = size lr in
      if balanced slr sr && balanced sll (slr + 1 + sr) then
        node_ sll ll y0 y1 (slr + 1 + sr) (node_ slr lr x0 x1 sr r)
      else begin match lr with
        | Node (_, lrl, z0, z1, lrr) ->
          let slrl = size lrl and slrr = size lrr in
          node_ (sll + 1 + slrl) (node_ sll ll y0 y1 slrl lrl) z0 z1
            (slrr + 1 + sr) (node_ slrr lrr x0 x1 sr r)
        | Leaf -> assert false
      end
    | Leaf -> assert false

let node_right sl l x0 x1 sr r =
  if not (disbalanced sl sr) then node_ sl l x0 x1 sr r else
    match r with
    | Node (_, rl, y0, y1, rr) ->
      let srl = size rl and srr = size rr in
      if balanced sl srl && balanced (sl + 1 + srl) srr then
        node_ (sl + 1 + srl) (node_ sl l x0 x1 srl rl) y0 y1 srr rr
      else begin match rl with
        | Node (_, rll, z0, z1, rlr) ->
          let srll = size rll and srlr = size rlr in
          node_ (sl + 1 + srll) (node_ sl l x0 x1 srll rll) z0 z1
            (srlr + 1 + srr) (node_ srlr rlr y0 y1 srr rr)
        | Leaf -> assert false
      end
    | Leaf -> assert false

(** [link l x r] joins balanced trees [l] and [r] of any sizes around [x],
    descending the spine of the larger one until the sizes are compatible,
    then fixing each node on the way back with one rotation, as in Baby
    (Blelloch, Ferizovic & Sun, "Joinable parallel balanced binary trees").
    Cost is O(log (max sl sr / min sl sr)). *)
let rec link_aux sl l x0 x1 sr r =
  (* Fast path: [disbalanced a b] requires [a < b / 2]. *)
  if sr <= 2 * sl + 1 && sl <= 2 * sr + 1 then
    node_ sl l x0 x1 sr r
  else if disbalanced sl sr then
    match r with
    | Node (_, rl, y0, y1, rr) ->
      let srl = size rl in
      node_left (sl + 1 + srl) (link_aux sl l x0 x1 srl rl) y0 y1 (size rr) rr
    | Leaf -> assert false
  else if disbalanced sr sl then
    match l with
    | Node (_, ll, y0, y1, lr) ->
      let slr = size lr in
      node_right (size ll) ll y0 y1 (slr + 1 + sr) (link_aux slr lr x0 x1 sr r)
    | Leaf -> assert false
  else
    node_ sl l x0 x1 sr r

let link l x0 x1 r =
  let sl = size l and sr = size r in
  if sr <= 2 * sl + 1 && sl <= 2 * sr + 1 then
    node_ sl l x0 x1 sr r
  else
    link_aux sl l x0 x1 sr r

(** Remove the last element, returned with the remaining tree. *)
let rec pop_max = function
  | Node (_, l, x0, x1, Leaf) -> l, (x0, x1)
  | Node (_, l, x0, x1, r) -> let r, m = pop_max r in node_balance l x0 x1 r, m
  | Leaf -> assert false

(** Remove the first element, returned with the remaining tree. *)
let rec pop_min = function
  | Node (_, Leaf, x0, x1, r) -> (x0, x1), r
  | Node (_, l, x0, x1, r) -> let m, l = pop_min l in m, node_balance l x0 x1 r
  | Leaf -> assert false

(** Public interface *)

let leaf = Leaf

let node l x0 x1 r = link l x0 x1 r

(* The root of the result is the last element of [l] or the first of [r],
   taken from the smaller tree. *)
let join l r = match l, r with
  | Leaf, t | t, Leaf -> t
  | Node (sl, _, _, _, _), Node (sr, _, _, _, _) ->
    if sl <= sr then
      let l, (x0, x1) = pop_max l in link l x0 x1 r
    else
      let (x0, x1), r = pop_min r in link l x0 x1 r

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
