module type MEASURE = sig
  type +'a measurable
  type measure
  val empty : measure
  val cat : measure -> 'a measurable -> measure -> measure
end

module Make(M : MEASURE) = struct

  type 'a t =
    | Leaf
    | Node of int * 'a t * 'a M.measurable * 'a t * M.measure

  let size = function
    | Node (s, _, _, _, _) -> s
    | Leaf -> 0

  let measure = function
    | Node (_, _, _, _, m) -> m
    | Leaf -> M.empty

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
        let ml = measure l and mr = measure r in
        Node (sl + 1 + sr, l, x, r, M.cat ml x mr)
  *)

  (** Construct Node *)
  let node_ sl l x sr r =
    Node (sl + 1 + sr, l, x, r, M.cat (measure l) x (measure r))

  (** Rotations *)
  let rot_left l x r k = match r with
    | Node (_, rl, y, rr, _) ->
      k (k l x rl) y rr
    | _ -> assert false

  let rot_right l y r k = match l with
    | Node (_, ll, x, lr, _) ->
      k ll x (k lr y r)
    | _ -> assert false

  (** Balancing *)

  let inc_left l x r k =
    let r = match r with
      | Node (_, rl, y, rr, _) when smaller_ell (size rr) (size rl) ->
        rot_right rl y rr k
      | _ -> r
    in
    rot_left l x r k

  let inc_right l y r k =
    let l = match l with
      | Node (_, ll, x, lr, _) when smaller_ell (size ll) (size lr) ->
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
      node_ sl l x sr r

  (** [link l x r] joins balanced trees [l] and [r] of any sizes around [x],
      descending the spine of the larger one until the sizes are compatible.
      Cost is O(log (max sl sr / min sl sr)). *)
  let rec link l x r =
    let sl = size l and sr = size r in
    if disbalanced sl sr then
      match r with
      | Node (_, rl, y, rr, _) -> node_balance (link l x rl) y rr
      | Leaf -> assert false
    else if disbalanced sr sl then
      match l with
      | Node (_, ll, y, lr, _) -> node_balance ll y (link lr x r)
      | Leaf -> assert false
    else
      node_ sl l x sr r

  (** Remove the last element, returned with the remaining tree. *)
  let rec pop_max = function
    | Node (_, l, x, Leaf, _) -> l, (x)
    | Node (_, l, x, r, _) -> let r, m = pop_max r in node_balance l x r, m
    | Leaf -> assert false

  (** Remove the first element, returned with the remaining tree. *)
  let rec pop_min = function
    | Node (_, Leaf, x, r, _) -> (x), r
    | Node (_, l, x, r, _) -> let m, l = pop_min l in m, node_balance l x r
    | Leaf -> assert false

  (** Public interface *)

  let leaf = Leaf

  let node l x r = link l x r

  (* The root of the result is the last element of [l] or the first of [r],
     taken from the smaller tree. *)
  let join l r = match l, r with
    | Leaf, t | t, Leaf -> t
    | Node (sl, _, _, _, _), Node (sr, _, _, _, _) ->
      if sl <= sr then
        let l, (x) = pop_max l in link l x r
      else
        let (x), r = pop_min r in link l x r

  let rec rank n = function
    | Leaf -> raise Not_found
    | Node (_, l, x, r, _) ->
      let sl = size l in
      if n = sl then
        x
      else if n < sl then
        rank n l
      else
        rank (n - 1 - sl) r

end
