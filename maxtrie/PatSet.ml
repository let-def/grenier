(* Sets of non-negative integers, represented as Patricia tries whose nodes
   hoist the maximum key of their subtree, with a word-sized bitmap per key.

   {2 Representation}

   An element [x] is encoded as a key [x / w] and a bit [1 lsl (x mod w)],
   where [w = Sys.word_size - 1] is the number of bits of an OCaml integer.
   A set is a map from keys to non-zero bitmaps.

   A tree is a record [{k; v; l; r}] where [k] is the largest key of the tree
   and [v] its bitmap. Let [m] be the most significant bit of [k] ([m = 0]
   when [k = 0]). The other keys of the tree are split as follows:
   - [r] holds the keys that are less than [m];
   - [l] holds the keys that are in the interval [\[m, k)], with their bit [m]
     cleared: this is the "frame" of the left child.
   Hence, every key of [r] is less than [m], and every key of [l] is less
   than [k - m]. In particular, if [k] is a power of two, [l] is empty, and if
   [k = 0], both children are empty.

   The empty tree is a unique, cyclic sentinel whose bitmap is [0], and no
   other node has an empty bitmap. Because its key is [0], a lookup always
   terminates when it reaches the sentinel.

   This representation is canonical: a set has exactly one representation,
   so structural equality coincides with set equality. Each node stores data,
   so a set with [n] distinct keys has exactly [n] nodes. The most significant
   bit of the key strictly decreases along every path, so the depth of a tree
   is bounded by the number of bits of a key.

   The elements of a tree are enumerated in increasing order by visiting [r],
   then [l], then the root. The key of a node is relative to its frame: the
   absolute key is obtained by or-ing the most significant bits of the
   ancestors at which a left branch was taken (the "prefix"). *)

(* The children are mutable only so that [Sharing.equal] and
   [Sharing.compare] can replace a child with a structurally equal one. No
   other operation mutates a tree. *)
type t = {
  k: int;
  v: int;
  mutable l: t;
  mutable r: t;
}

type elt = int

let rec empty = {k = 0; v = 0; l = empty; r = empty}

let msb = Bit_lib.extract_msb
let lt_msb = Bit_lib.lt_msb

(* -------------------------------------------------------------------------- *)
(* Encoding of elements. *)

let word = Sys.word_size - 1

let[@inline] key_of x = x / word
let[@inline] bit_of x = 1 lsl (x mod word)
let[@inline] base k = k * word

(* The least and greatest elements of the bitmap [v] at absolute key [k]. *)
let[@inline] lowest k v = base k + Bit_lib.lsb_index v
let[@inline] highest k v = base k + Bit_lib.msb_index v

let check_elt fn x =
  if x < 0 then invalid_arg ("PatSet." ^ fn ^ ": negative element")

(* -------------------------------------------------------------------------- *)
(* Low-level operations on keys and bitmaps. *)

let[@inline] leaf k v = {k; v; l = empty; r = empty}

(* [lookup k t] is the bitmap associated with key [k] in [t] ([0] if none). *)
let rec lookup k t =
  if k >= t.k then
    if k = t.k then t.v else 0
  else
    let m = msb t.k in
    if k land m <> 0
    then lookup (k lxor m) t.l
    else lookup k t.r

(* [push k v t] adds the key [k] with bitmap [v <> 0] to [t], where [k] is
   greater than every key of [t]. The new key becomes the root and the old
   root sinks into the left child if it shares the most significant bit. *)
let rec push k v t =
  if t.v = 0 then leaf k v
  else
    let m = msb k in
    if t.k land m <> 0
    then {k; v; l = push (t.k lxor m) t.v t.l; r = t.r}
    else {k; v; l = empty; r = t}

(* [join k l r] is the tree obtained by removing the root of a node with key
   [k] and children [l] and [r]: the maximum of [l], if any, is promoted. *)
let rec join k l r =
  if l.v = 0 then r
  else {k = msb k lor l.k; v = l.v; l = join l.k l.l l.r; r}

(* [rebuild t v l r] is the node [t] with new bitmap [v] and children [l] and
   [r], reusing [t] when nothing changed, and removing the root if [v = 0]. *)
let[@inline] rebuild t v l r =
  if v = 0 then join t.k l r
  else if v = t.v && l == t.l && r == t.r then t
  else {k = t.k; v; l; r}

let[@inline] with_l t l = if l == t.l then t else {t with l}
let[@inline] with_r t r = if r == t.r then t else {t with r}

(* [add_bits k v t] adds the bits [v <> 0] at key [k]. *)
let rec add_bits k v t =
  if k >= t.k then
    if k = t.k then
      let v' = t.v lor v in
      if v' = t.v then t else {t with v = v'}
    else
      push k v t
  else
    let m = msb t.k in
    if k land m <> 0
    then with_l t (add_bits (k lxor m) v t.l)
    else with_r t (add_bits k v t.r)

(* [remove_bits k v t] removes the bits [v] at key [k]. *)
let rec remove_bits k v t =
  if k >= t.k then
    if k = t.k then
      let v' = t.v land lnot v in
      if v' = t.v then t
      else if v' = 0 then join t.k t.l t.r
      else {t with v = v'}
    else
      t
  else
    let m = msb t.k in
    if k land m <> 0
    then with_l t (remove_bits (k lxor m) v t.l)
    else with_r t (remove_bits k v t.r)

(* -------------------------------------------------------------------------- *)
(* Elementary operations. *)

let is_empty t = t.v = 0

let is_singleton t =
  t.v <> 0 && (t.v land (t.v - 1)) lor t.l.v lor t.r.v = 0

let singleton x =
  check_elt "singleton" x;
  leaf (key_of x) (bit_of x)

let add x t =
  check_elt "add" x;
  add_bits (key_of x) (bit_of x) t

let remove x t =
  if x < 0 then t else remove_bits (key_of x) (bit_of x) t

let mem x t =
  x >= 0 && lookup (key_of x) t land bit_of x <> 0

let cardinal t =
  let rec loop t acc =
    if t.v = 0 then acc
    else loop t.r (loop t.l (acc + Bit_lib.pop_count t.v))
  in
  loop t 0

(* -------------------------------------------------------------------------- *)
(* Minimum and maximum. *)

(* [min_in pre t] is the least element of the non-empty tree [t], whose keys
   are relative to the prefix [pre]. *)
let rec min_in pre t =
  if t.r.v <> 0 then min_in pre t.r
  else if t.l.v <> 0 then min_in (pre lor msb t.k) t.l
  else lowest (pre lor t.k) t.v

let minimum t =
  if t.v = 0 then raise Not_found;
  min_in 0 t

let maximum t =
  if t.v = 0 then raise Not_found;
  highest t.k t.v

(* The maximum is at the root, hence cheaper than the minimum. *)
let choose = maximum

let min_elt_opt t = if t.v = 0 then None else Some (min_in 0 t)
let max_elt_opt t = if t.v = 0 then None else Some (highest t.k t.v)

let compare_minimum a b =
  match a.v, b.v with
  | 0, 0 -> 0
  | 0, _ -> -1
  | _, 0 -> 1
  | _ -> Int.compare (min_in 0 a) (min_in 0 b)

(* -------------------------------------------------------------------------- *)
(* Carried keys.

   When two trees whose roots share their most significant bit [m] are
   merged, the smaller root moves into the upper half of the result, where it
   must be merged with the left child of the other tree. Rather than building
   this "pennant" (the root minus [m], followed by the left child), we carry
   it into the recursion: [union_push] and [diff_push] handle a single
   carried key. When a second key must be carried, we switch to "virtual
   trees", which describe any number of carried keys without allocating.

   Carried keys always form a segment of a left spine of an input tree. In
   the current frame, whose keys are below some power of two [bound], the key
   of a carried node [c] is [c.k land (bound - 1)]: the bits of [c.k] above
   [bound] are exactly those stripped since [c] was carried. We write [mk]
   for [bound - 1]; it is shared by both operands.

   A virtual tree is described by three values [h s p]:
   - the chain: the nodes [h], [h.l], [h.l.l], ..., up to [s] excluded, which
     stand for the keys [c.k land mk] with the bitmaps [c.v], in decreasing
     order;
   - the tail: [s] if [p], [empty] otherwise; its keys are below those of the
     chain. A plain tree [t] is the virtual tree [t t true].

   Splitting a virtual tree at the bit [m] (when every key is below [2m])
   yields an upper and a lower virtual tree, again without allocation: the
   chain keys that have the bit [m] are a prefix of the chain; the others,
   followed by the tail, form the lower part. If every chain key has the bit
   [m], the root of the tail may join the chain, in which case its left child
   becomes the new tail and its right child goes to the lower part. Both
   parts use the mask [m - 1]. *)

(* [vmax h s p mk] is the largest key of a virtual tree, or [-1] if it is
   empty. The corresponding bitmap is [h.v]. *)
let[@inline] vmax h s p mk =
  if h != s then h.k land mk
  else if p && s.v <> 0 then s.k
  else -1

let[@inline] vtail s p = if p then s else empty

(* [materialize h s p mk] builds a virtual tree. *)
let rec materialize h s p mk =
  if h == s then vtail s p
  else push (h.k land mk) h.v (materialize h.l s p mk)

(* [first_low m c s] is the first node of the chain [c .. s] whose key does
   not have the bit [m], or [s] if there is none. *)
let rec first_low m c s =
  if c == s || c.k land m = 0 then c
  else first_low m c.l s

(* [joins m s p] tells whether the root of the tail joins the chain when
   splitting at [m], assuming every chain key has the bit [m]. *)
let[@inline] joins m s p =
  p && s.v <> 0 && s.k land m <> 0


(* -------------------------------------------------------------------------- *)
(* Comparisons. *)

let rec equal a b =
  a == b ||
  (a.k = b.k && a.v = b.v && equal a.l b.l && equal a.r b.r)

(* A structural total order (it is not the lexicographic order on sorted
   sequences of elements). *)
let rec compare a b =
  if a == b then 0 else
  let c = Int.compare a.k b.k in
  if c <> 0 then c else
  let c = Int.compare a.v b.v in
  if c <> 0 then c else
  let c = compare a.l b.l in
  if c <> 0 then c else
  compare a.r b.r

(* Comparisons that, as a side effect, make structurally equal subtrees of
   their arguments physically equal. This speeds up later comparisons and
   lets the garbage collector reclaim duplicates. Since the representation is
   canonical, a subtree can be replaced with any structurally equal one.

   The copy to keep is chosen by [Bit_lib.share_choose], a C stub that
   prefers a value outside the minor heap (so that unification creates no
   major-to-minor pointers and the young copy can be collected early), and
   otherwise the one at the lower address. Concurrent use from several
   domains is safe: a racing reader sees either child, and both are equal. *)
module Sharing = struct
  let[@inline] unify_l a b =
    if Bit_lib.share_choose a.l b.l == a.l then b.l <- a.l else a.l <- b.l

  let[@inline] unify_r a b =
    if Bit_lib.share_choose a.r b.r == a.r then b.r <- a.r else a.r <- b.r

  let rec equal a b =
    a == b || (
      a.k = b.k && a.v = b.v &&
      (a.l == b.l || (equal a.l b.l && (unify_l a b; true))) &&
      (a.r == b.r || (equal a.r b.r && (unify_r a b; true)))
    )

  let rec compare a b =
    if a == b then 0 else
    let c = Int.compare a.k b.k in
    if c <> 0 then c else
    let c = Int.compare a.v b.v in
    if c <> 0 then c else
    let c = if a.l == b.l then 0 else compare a.l b.l in
    if c <> 0 then c else begin
      if a.l != b.l then unify_l a b;
      let c = if a.r == b.r then 0 else compare a.r b.r in
      if c = 0 && a.r != b.r then unify_r a b;
      c
    end
end

(* The root of the larger tree is irrelevant to [subset] and [disjoint]; the
   root of the smaller one is looked up in the other tree. Carrying it into the
   recursion instead, as [union] does, avoids this second descent but makes
   every step along its path more expensive: measured, it is slower. *)
let rec subset a b =
  a == b || a.v = 0 || (
    b.v <> 0 &&
    let ka = a.k and kb = b.k in
    if ka = kb then
      a.v land lnot b.v = 0 && subset a.l b.l && subset a.r b.r
    else if ka > kb then
      false (* the maximum of [a] is not in [b] *)
    else if lt_msb ka kb then
      subset a b.r
    else
      (* Same most significant bit [m], [ka < kb]: the root of [a] is looked
         up in [b.l]; the root of [b] is irrelevant. *)
      let m = msb kb in
      a.v land lnot (lookup (ka lxor m) b.l) = 0 &&
      subset a.l b.l && subset a.r b.r
  )

let rec disjoint a b =
  a.v = 0 || b.v = 0 || (
    a != b &&
    let ka = a.k and kb = b.k in
    if ka = kb then
      a.v land b.v = 0 && disjoint a.l b.l && disjoint a.r b.r
    else if lt_msb ka kb then
      disjoint a b.r
    else if lt_msb kb ka then
      disjoint a.r b
    else if ka < kb then
      a.v land lookup (ka lxor msb kb) b.l = 0 &&
      disjoint a.l b.l && disjoint a.r b.r
    else
      b.v land lookup (kb lxor msb ka) a.l = 0 &&
      disjoint a.l b.l && disjoint a.r b.r
  )

(* Under the precondition [a ⊆ b ∨ a ∩ b = ∅], it suffices to test one
   element of [a]: its maximum is at the root. *)
let quick_subset a b =
  lookup a.k b land a.v <> 0

(* -------------------------------------------------------------------------- *)
(* Union. *)

(* [reuse2 a b k v l r] builds the node [{k; v; l; r}], returning [b] or [a]
   if it is structurally identical. *)
let[@inline] reuse2 a b k v l r =
  if v = b.v && l == b.l && r == b.r then b
  else if v = a.v && l == a.l && r == a.r then a
  else {k; v; l; r}

(* Physical equality: if the result of [union a b] is equal to [b], then [b]
   is returned. (The same holds for [a] as long as [a] and [b] do not have
   structurally equal but physically distinct subtrees.) *)
let rec union a b =
  if a == b || a.v = 0 then b else
  if b.v = 0 then a else
  let ka = a.k and kb = b.k in
  if ka = kb then
    let l = union a.l b.l in
    let r = union a.r b.r in
    reuse2 a b ka (a.v lor b.v) l r
  else if lt_msb ka kb then
    (* [a] lies below the most significant bit of [kb]. *)
    with_r b (union a b.r)
  else if lt_msb kb ka then
    with_r a (union a.r b)
  else
    (* Same most significant bit [m]: the smaller root is carried into the
       upper half. *)
    let m = msb ka in
    let r = union a.r b.r in
    if ka > kb then
      let l = union_push false a.l b (kb lxor m) in
      if l == a.l && r == a.r then a else {k = ka; v = a.v; l; r}
    else
      let l = union_push true b.l a (ka lxor m) in
      if l == b.l && r == b.r then b else {k = kb; v = b.v; l; r}

(* [union_o xb x y] is the union of [x] and [y], where [xb] tells whether [x]
   comes from the side whose physical identity is preserved. *)
and union_o xb x y =
  if xb then union y x else union x y

(* [union_push xb x h k] is the union of the tree [x] and the "pennant" made
   of the carried node [h], standing for the key [k] (greater than every key
   of [h.l]) with the bitmap [h.v], followed by the tree [h.l]. If the result
   is equal to [x], then [x] is returned. *)
and union_push xb x h k =
  let v = h.v and y = h.l in
  if x.v = 0 then push k v y else
  let kx = x.k in
  if kx = k then
    let m = msb k in
    let l, r =
      if y.k land m <> 0 (* implies that [y] is not empty *)
      then union_push xb x.l y (y.k lxor m), union_o xb x.r y.r
      else x.l, union_o xb x.r y
    in
    let v' = x.v lor v in
    if v' = x.v && l == x.l && r == x.r then x else {k; v = v'; l; r}
  else if lt_msb kx k then
    (* [x] lies in the lower half of the result. *)
    let m = msb k in
    if y.k land m <> 0
    then {k; v; l = push (y.k lxor m) y.v y.l; r = union_o xb x y.r}
    else {k; v; l = empty; r = union_o xb x y}
  else if lt_msb k kx then
    with_r x (union_push xb x.r h k)
  else
    (* Same most significant bit [m], [k <> kx]. *)
    let m = msb k in
    let up = y.k land m <> 0 in
    let r = union_o xb x.r (if up then y.r else y) in
    if kx > k then
      (* Both [k] and the root of [y] are carried into the upper half. *)
      let l =
        if not up then add_bits (k lxor m) v x.l
        else if xb then vunion h y.l true x.l x.l true (m - 1)
        else vunion x.l x.l true h y.l true (m - 1)
      in
      if l == x.l && r == x.r then x else {k = kx; v = x.v; l; r}
    else
      (* [kx < k]: the upper halves are the pennants of [x] and [y]. *)
      let l =
        if not up then push (kx lxor m) x.v x.l
        else if xb then vunion y y.l true x x.l true (m - 1)
        else vunion x x.l true y y.l true (m - 1)
      in
      {k; v; l; r}

(* [vunion ha sa pa hb sb pb mk] is the union of two virtual trees. If the
   result is equal to an argument that is a plain tree, this argument is
   returned (preferably the second one). *)
and vunion ha sa pa hb sb pb mk =
  if ha == sa then
    if hb == sb then union (vtail sa pa) (vtail sb pb)
    else if hb.l == sb then
      (* A single carried key, possibly followed by its left child. *)
      if pb then union_push false (vtail sa pa) hb (hb.k land mk)
      else add_bits (hb.k land mk) hb.v (vtail sa pa)
    else vunion_gen ha sa pa hb sb pb mk
  else if hb == sb && ha.l == sa then
    if pa then union_push true (vtail sb pb) ha (ha.k land mk)
    else add_bits (ha.k land mk) ha.v (vtail sb pb)
  else vunion_gen ha sa pa hb sb pb mk

and vunion_gen ha sa pa hb sb pb mk =
  let ka = vmax ha sa pa mk and kb = vmax hb sb pb mk in
  if ka < 0 then materialize hb sb pb mk
  else if kb < 0 then materialize ha sa pa mk
  else if lt_msb ka kb then vunion_below ha sa pa hb sb pb mk kb false
  else if lt_msb kb ka then vunion_below hb sb pb ha sa pa mk ka true
  else vunion_same ha sa pa hb sb pb ka kb

(* [vunion_below] handles the case where [x] lies below the most significant
   bit [m] of the maximum [ky] of [y]: the result has the root of [y], the
   upper half of [y], and the union of [x] with the lower half of [y]. The
   flag [sw] tells whether [x] is the second argument of [vunion]. *)
and vunion_below hx sx px hy sy py mk ky sw =
  if hy == sy then
    (* [y] is a plain tree. *)
    let r =
      if sw then vunion sy.r sy.r true hx sx px mk
      else vunion hx sx px sy.r sy.r true mk
    in
    with_r sy r
  else
    let m = msb ky in
    let mk = m - 1 in
    let h = hy.l in
    let c = first_low m h sy in
    let j = c == sy && joins m sy py in
    let l = materialize h (if j then sy.l else c) j mk in
    let r =
      if c != sy then
        if sw then vunion c sy py hx sx px mk
        else vunion hx sx px c sy py mk
      else
        let t = if j then sy.r else vtail sy py in
        if sw then vunion t t true hx sx px mk
        else vunion hx sx px t t true mk
    in
    {k = ky; v = hy.v; l; r}

(* [vunion_same] handles the case where the maxima [ka] and [kb] of [a] and
   [b] have the same most significant bit [m]. The largest maximum (or both,
   if they are equal) becomes the root. Both virtual trees are split at [m]:
   the upper halves are merged into the left child, and the lower halves
   into the right child. *)
and vunion_same ha sa pa hb sb pb ka kb =
  let k = if ka > kb then ka else kb in
  let m = msb k in
  (* Split [a] into [uha usa upa] and [lha lsa lpa]. If [ka = k], its
     maximum is popped first. *)
  let uha, usa, upa, lha, lsa, lpa =
    if ka = k && ha == sa then
      (* Popping the root of a plain tree yields its two children. *)
      sa.l, sa.l, true, sa.r, sa.r, true
    else
      let h = if ka = k then ha.l else ha in
      let c = first_low m h sa in
      if c != sa then h, c, false, c, sa, pa
      else if joins m sa pa then h, sa.l, true, sa.r, sa.r, true
      else h, sa, false, sa, sa, pa
  in
  let uhb, usb, upb, lhb, lsb, lpb =
    if kb = k && hb == sb then
      sb.l, sb.l, true, sb.r, sb.r, true
    else
      let h = if kb = k then hb.l else hb in
      let c = first_low m h sb in
      if c != sb then h, c, false, c, sb, pb
      else if joins m sb pb then h, sb.l, true, sb.r, sb.r, true
      else h, sb, false, sb, sb, pb
  in
  let v =
    if ka = kb then ha.v lor hb.v else if ka = k then ha.v else hb.v in
  let l = vunion uha usa upa uhb usb upb (m - 1) in
  let r = vunion lha lsa lpa lhb lsb lpb (m - 1) in
  if kb = k && hb == sb && v = sb.v && l == sb.l && r == sb.r then sb
  else if ka = k && ha == sa && v = sa.v && l == sa.l && r == sa.r then sa
  else {k; v; l; r}

(* -------------------------------------------------------------------------- *)
(* Intersection. *)

(* If the result of [inter a b] is equal to [a], then [a] is returned. *)
let rec inter a b =
  if a == b then a else
  if a.v = 0 || b.v = 0 then empty else
  let ka = a.k and kb = b.k in
  if ka = kb then
    let l = inter a.l b.l in
    let r = inter a.r b.r in
    let v = a.v land b.v in
    if v = 0 then join ka l r else reuse2 b a ka v l r
  else if lt_msb ka kb then inter a b.r
  else if lt_msb kb ka then inter a.r b
  else if ka < kb then inter_lt a b
  else inter_lt b a

(* [ka < kb] with the same most significant bit. The root of [b] is not in
   [a]; the maximum of the result is at most [ka]. *)
and inter_lt a b =
  let l = inter a.l b.l in
  let r = inter a.r b.r in
  rebuild a (a.v land lookup (a.k lxor msb b.k) b.l) l r

(* -------------------------------------------------------------------------- *)
(* Difference. *)

(* If the result of [diff a b] is equal to [a], then [a] is returned. *)
let rec diff a b =
  if a == b || a.v = 0 then empty else
  if b.v = 0 then a else
  let ka = a.k and kb = b.k in
  if ka = kb then
    let l = diff a.l b.l in
    let r = diff a.r b.r in
    rebuild a (a.v land lnot b.v) l r
  else if lt_msb ka kb then diff a b.r
  else if lt_msb kb ka then with_r a (diff a.r b)
  else
    let m = msb ka in
    let r = diff a.r b.r in
    if ka > kb then
      (* The root of [a] survives; the root of [b] is carried. *)
      let l = diff_push a.l b (kb lxor m) in
      if l == a.l && r == a.r then a else {k = ka; v = a.v; l; r}
    else
      let l = diff a.l b.l in
      rebuild a (a.v land lnot (lookup (ka lxor m) b.l)) l r

(* [diff_push x h k] is the difference between [x] and the pennant made of
   the carried node [h], standing for the key [k] with the bitmap [h.v],
   followed by the tree [h.l]. *)
and diff_push x h k =
  let y = h.l in
  if x.v = 0 then x else
  let kx = x.k in
  if kx = k then
    let m = msb k in
    let l, r =
      if y.k land m <> 0
      then diff_push x.l y (y.k lxor m), diff x.r y.r
      else x.l, diff x.r y
    in
    rebuild x (x.v land lnot h.v) l r
  else if lt_msb kx k then
    (* [x] lies in the lower half of the pennant. *)
    diff x (if y.k land msb k <> 0 then y.r else y)
  else if lt_msb k kx then
    with_r x (diff_push x.r h k)
  else if kx < k then
    (* The carried key is above every key of [x]: drop it. *)
    diff x y
  else
    let m = msb k in
    let up = y.k land m <> 0 in
    let r = diff x.r (if up then y.r else y) in
    let l =
      if up then vdiff x.l h y.l true (m - 1)
      else remove_bits (k lxor m) h.v x.l
    in
    if l == x.l && r == x.r then x else {k = kx; v = x.v; l; r}

(* [vdiff x hb sb pb mk] is the difference between the tree [x] and a
   virtual tree. If nothing is removed, [x] is returned. *)
and vdiff x hb sb pb mk =
  if hb == sb then diff x (vtail sb pb)
  else if hb.l == sb then
    if pb then diff_push x hb (hb.k land mk)
    else remove_bits (hb.k land mk) hb.v x
  else if x.v = 0 then x
  else
    let kb = hb.k land mk and kx = x.k in
    if kx = kb then
      let m = msb kx in
      let h = hb.l in
      let c = first_low m h sb in
      let l, r =
        if c != sb then vdiff x.l h c false (m - 1), vdiff x.r c sb pb (m - 1)
        else if joins m sb pb then
          vdiff x.l h sb.l true (m - 1), diff x.r sb.r
        else
          vdiff x.l h sb false (m - 1), diff x.r (vtail sb pb)
      in
      rebuild x (x.v land lnot hb.v) l r
    else if lt_msb kx kb then
      (* [x] lies below the most significant bit [m] of [kb]: only the lower
         half of [b] matters. *)
      let m = msb kb in
      let c = first_low m hb sb in
      if c != sb then vdiff x c sb pb (m - 1)
      else if joins m sb pb then diff x sb.r
      else diff x (vtail sb pb)
    else if lt_msb kb kx then
      with_r x (vdiff x.r hb sb pb mk)
    else if kx < kb then
      (* The maximum of [b] is above every key of [x]: drop it. *)
      vdiff x hb.l sb pb mk
    else
      (* [kx > kb], same most significant bit [m]: the root of [x]
         survives. *)
      let m = msb kx in
      let c = first_low m hb sb in
      let l, r =
        if c != sb then vdiff x.l hb c false (m - 1), vdiff x.r c sb pb (m - 1)
        else if joins m sb pb then
          vdiff x.l hb sb.l true (m - 1), diff x.r sb.r
        else
          vdiff x.l hb sb false (m - 1), diff x.r (vtail sb pb)
      in
      if l == x.l && r == x.r then x else {k = kx; v = x.v; l; r}

(* -------------------------------------------------------------------------- *)
(* Iteration. *)

let[@inline] iter_bits f k v =
  let b = base k in
  let v = ref v in
  while !v <> 0 do
    let i = Bit_lib.lsb_index !v in
    v := !v land (!v - 1);
    f (b + i)
  done

let[@inline] rev_iter_bits f k v =
  let b = base k in
  let v = ref v in
  while !v <> 0 do
    let i = Bit_lib.msb_index !v in
    v := !v lxor (1 lsl i);
    f (b + i)
  done

let rec iter f pre t =
  if t.v <> 0 then begin
    iter f pre t.r;
    iter f (pre lor msb t.k) t.l;
    iter_bits f (pre lor t.k) t.v
  end

let iter f t = iter f 0 t

let rec rev_iter f pre t =
  if t.v <> 0 then begin
    rev_iter_bits f (pre lor t.k) t.v;
    rev_iter f (pre lor msb t.k) t.l;
    rev_iter f pre t.r
  end

let rev_iter f t = rev_iter f 0 t

let rec fold f pre t acc =
  if t.v = 0 then acc else
  let acc = fold f pre t.r acc in
  let acc = fold f (pre lor msb t.k) t.l acc in
  let b = base (pre lor t.k) in
  let v = ref t.v and acc = ref acc in
  while !v <> 0 do
    let i = Bit_lib.lsb_index !v in
    v := !v land (!v - 1);
    acc := f (b + i) !acc
  done;
  !acc

let fold f t acc = fold f 0 t acc

let rec fold_right f pre t acc =
  if t.v = 0 then acc else
  let b = base (pre lor t.k) in
  let v = ref t.v and acc = ref acc in
  while !v <> 0 do
    let i = Bit_lib.msb_index !v in
    v := !v lxor (1 lsl i);
    acc := f (b + i) !acc
  done;
  let acc = fold_right f (pre lor msb t.k) t.l !acc in
  fold_right f pre t.r acc

(* The elements in increasing order. *)
let elements t = fold_right (fun x xs -> x :: xs) 0 t []

(* [find_bits p k v] is the least element of the bitmap [v] at key [k] that
   satisfies [p], or [-1]. *)
let find_bits p k v =
  let b = base k in
  let rec loop v =
    if v = 0 then -1 else
    let x = b + Bit_lib.lsb_index v in
    if p x then x else loop (v land (v - 1))
  in
  loop v

let rec find_first p pre t =
  if t.v = 0 then -1 else
  let x = find_first p pre t.r in
  if x >= 0 then x else
  let x = find_first p (pre lor msb t.k) t.l in
  if x >= 0 then x else
  find_bits p (pre lor t.k) t.v

let find_first_opt p t =
  let x = find_first p 0 t in
  if x < 0 then None else Some x

let rec for_all p pre t =
  t.v = 0 || (
    for_all p pre t.r &&
    for_all p (pre lor msb t.k) t.l &&
    find_bits (fun x -> not (p x)) (pre lor t.k) t.v < 0
  )

let for_all p t = for_all p 0 t

let exists p t = find_first p 0 t >= 0

(* [filter_bits p k v] is the sub-bitmap of [v] whose elements satisfy [p].
   The predicate is applied in increasing order. *)
let filter_bits p k v =
  let b = base k in
  let rec loop v acc =
    if v = 0 then acc else
    let i = Bit_lib.lsb_index v in
    let v = v land (v - 1) in
    loop v (if p (b + i) then acc lor (1 lsl i) else acc)
  in
  loop v 0

let rec filter p pre t =
  if t.v = 0 then t else
  let r = filter p pre t.r in
  let l = filter p (pre lor msb t.k) t.l in
  rebuild t (filter_bits p (pre lor t.k) t.v) l r

let filter p t = filter p 0 t

let partition p t =
  let yes = filter p t in
  yes, diff t yes

(* -------------------------------------------------------------------------- *)
(* Sequences and lists. *)

type enum =
  | End
  | Tree of int * t * enum      (* a prefix and a tree *)
  | Bits of int * int * enum    (* an absolute key and a non-empty bitmap *)

let rec seq_of_enum e () =
  match e with
  | End -> Seq.Nil
  | Tree (pre, t, e) ->
    if t.v = 0 then seq_of_enum e ()
    else
      seq_of_enum
        (Tree (pre, t.r,
               Tree (pre lor msb t.k, t.l,
                     Bits (pre lor t.k, t.v, e)))) ()
  | Bits (k, v, e) ->
    let i = Bit_lib.lsb_index v in
    let v = v land (v - 1) in
    Seq.Cons (base k + i, seq_of_enum (if v = 0 then e else Bits (k, v, e)))

let to_seq t = seq_of_enum (Tree (0, t, End))

let add_seq s t = Seq.fold_left (fun t x -> add x t) t s
let of_seq s = add_seq s empty
let of_list xs = List.fold_left (fun t x -> add x t) empty xs

(* -------------------------------------------------------------------------- *)
(* Splitting. *)

(* [above_bits k mask t] keeps the keys greater than [k], plus the bits
   [mask] of key [k]. *)
let rec above_bits k mask t =
  if t.v = 0 || t.k < k then empty
  else if t.k = k then
    let v = t.v land mask in
    if v = 0 then empty
    else if v = t.v && t.l.v lor t.r.v = 0 then t
    else leaf k v
  else
    let m = msb t.k in
    if k land m <> 0 then
      let l = above_bits (k lxor m) mask t.l in
      if l == t.l && t.r.v = 0 then t else {t with l; r = empty}
    else
      with_r t (above_bits k mask t.r)

(* [below_bits k mask t] keeps the keys smaller than [k], plus the bits
   [mask] of key [k]. *)
let rec below_bits k mask t =
  if t.v = 0 || t.k < k then t
  else if t.k = k then
    rebuild t (t.v land mask) t.l t.r
  else
    let m = msb t.k in
    if k land m <> 0
    then join t.k (below_bits (k lxor m) mask t.l) t.r
    else below_bits k mask t.r

(* Masks selecting the bits strictly above / below / at most / at least the
   bit [b] (a power of two). *)
let[@inline] bits_gt b = - b lsl 1
let[@inline] bits_ge b = - b
let[@inline] bits_lt b = b - 1
let[@inline] bits_le b = b lor (b - 1)

let above x t =
  if x < 0 then t else above_bits (key_of x) (bits_gt (bit_of x)) t

let below x t =
  if x <= 0 then empty else below_bits (key_of x) (bits_lt (bit_of x)) t

let at_least x t =
  if x <= 0 then t else above_bits (key_of x) (bits_ge (bit_of x)) t

let at_most x t =
  if x < 0 then empty else below_bits (key_of x) (bits_le (bit_of x)) t

let split x t =
  below x t, mem x t, above x t

(* -------------------------------------------------------------------------- *)
(* Decomposition. *)

let sorted_union ss =
  List.fold_left (fun acc s -> union acc s) empty ss

let extract_unique_prefix s1 s2 =
  let x = minimum s2 in
  below x s1, at_least x s1

let extract_unique_suffix s1 s2 =
  let x = maximum s2 in
  above x s1, at_most x s1

(* [max_sym_diff a b] is the greatest element of the symmetric difference of
   [a] and [b], or [-1] if they are equal. Since the maximum of a tree is at
   its root, the first mismatching root decides. *)
let rec max_sym_diff pre a b =
  if a == b then -1
  else if a.k <> b.k || a.v = 0 || b.v = 0 then
    if a.k > b.k || b.v = 0 then highest (pre lor a.k) a.v
    else highest (pre lor b.k) b.v
  else if a.v <> b.v then
    highest (pre lor a.k) (a.v lxor b.v)
  else
    let d = max_sym_diff (pre lor msb a.k) a.l b.l in
    if d >= 0 then d else max_sym_diff pre a.r b.r

let extract_shared_suffix s1 s2 =
  let x = max_sym_diff 0 s1 s2 in
  if x < 0 then s1, (empty, empty)
  else above x s1, (at_most x s1, at_most x s2)

(* [min_gt pre k t] is the least element of [t] whose key is greater than
   [k], or [-1]. *)
let rec min_gt pre k t =
  if t.v = 0 || t.k <= k then -1 else
  let m = msb t.k in
  let x =
    if k land m <> 0
    then min_gt (pre lor m) (k lxor m) t.l
    else
      let x = min_gt pre k t.r in
      if x >= 0 || t.l.v = 0 then x else min_in (pre lor m) t.l
  in
  if x >= 0 then x else lowest (pre lor t.k) t.v

(* [min_sym_diff pre a b] is the least element of the symmetric difference
   of [a] and [b], or [-1] if they are equal. *)
let rec min_sym_diff pre a b =
  if a == b then -1
  else if a.v = 0 then min_in pre b
  else if b.v = 0 then min_in pre a
  else
    let ka = a.k and kb = b.k in
    if ka = kb then
      let x = min_sym_diff pre a.r b.r in
      if x >= 0 then x else
      let x = min_sym_diff (pre lor msb ka) a.l b.l in
      if x >= 0 then x else
      let d = a.v lxor b.v in
      if d = 0 then -1 else lowest (pre lor ka) d
    else if lt_msb ka kb then min_sym_diff_low pre a b
    else if lt_msb kb ka then min_sym_diff_low pre b a
    else if ka < kb then min_sym_diff_lt pre a b
    else min_sym_diff_lt pre b a

(* [a] lies in the lower half of [b]: all the upper half of [b] differs. *)
and min_sym_diff_low pre a b =
  let x = min_sym_diff pre a b.r in
  if x >= 0 then x
  else if b.l.v <> 0 then min_in (pre lor msb b.k) b.l
  else lowest (pre lor b.k) b.v

(* Same most significant bit [m], [ka < kb]. In the upper halves, [a] holds
   [a.l] and the key [ka - m], [b] holds [b.l] and the key [kb - m], which is
   above everything in [a]. *)
and min_sym_diff_lt pre a b =
  let x = min_sym_diff pre a.r b.r in
  if x >= 0 then x else
  let m = msb a.k in
  let pre' = pre lor m in
  let ka = pre lor a.k in       (* absolute key of the root of [a] *)
  let x = min_sym_diff pre' a.l b.l in
  if x >= 0 && key_of x < ka then x
  else if x < 0 || key_of x > ka then
    (* [b.l] does not have the key [ka]. *)
    lowest ka a.v
  else
    (* [b.l] has the key [ka] and [a.l] does not. *)
    let d = a.v lxor lookup (a.k lxor m) b.l in
    if d <> 0 then lowest ka d
    else
      (* The next element of [b] above [ka] differs. *)
      let x = min_gt pre' (a.k lxor m) b.l in
      if x >= 0 then x else lowest (pre lor b.k) b.v

let extract_shared_prefix s1 s2 =
  let x = min_sym_diff 0 s1 s2 in
  if x < 0 then s1, (empty, empty)
  else below x s1, (at_least x s1, at_least x s2)

(* -------------------------------------------------------------------------- *)
(* Invariant checking. *)

let check t =
  let fail fmt = Printf.ksprintf failwith ("PatSet.check: " ^^ fmt) in
  (* [bound] is an exclusive upper bound on the keys, [-1] if unbounded. *)
  let rec check t bound =
    if t.v = 0 then begin
      if t != empty then fail "a node with an empty bitmap is not [empty]"
    end else begin
      if t.k < 0 then fail "negative key %d" t.k;
      if bound >= 0 && t.k >= bound then
        fail "key %d is out of its bound %d" t.k bound;
      let m = msb t.k in
      check t.l (t.k - m);
      check t.r m
    end
  in
  if empty.k <> 0 || empty.v <> 0 || empty.l != empty || empty.r != empty then
    fail "corrupted sentinel";
  check t (-1)
