(* Sets of non-negative integers, represented as Patricia tries whose nodes
   hoist the maximum key of their subtree, with a word-sized bitmap per key.

   {2 Representation}

   An element [x] is encoded as a key [x / w] and a bit [1 lsl (x mod w)],
   where [w = Sys.word_size - 1] is the number of bits of an OCaml integer.
   A set is a map from keys to non-zero bitmaps.

   A tree is either [Empty] or a node with a key [k], the largest key of the
   tree, its bitmap [v], and two children [l] and [r]. Let [m] be the most
   significant bit of [k] ([m = 0] when [k = 0]). The other keys of the tree
   are split as follows:
   - [r] holds the keys that are less than [m];
   - [l] holds the keys that are in the interval [\[m, k)], with their bit [m]
     cleared: this is the "frame" of the left child.
   Hence, every key of [r] is less than [m], and every key of [l] is less
   than [k - m]. In particular, if [k] is a power of two, [l] is empty, and if
   [k = 0], both children are empty. No node has an empty bitmap.

   This representation is canonical: a set has exactly one representation,
   so structural equality coincides with set equality. Each node stores data,
   so a set with [n] distinct keys has exactly [n] nodes. The most significant
   bit of the key strictly decreases along every path, so the depth of a tree
   is bounded by the number of bits of a key.

   The elements of a tree are enumerated in increasing order by visiting [r],
   then [l], then the root. The key of a node is relative to its frame: the
   absolute key is obtained by or-ing the most significant bits of the
   ancestors at which a left branch was taken (the "prefix").

   {2 Encoding of keys}

   Every descent needs the most significant bit of the key of each node it
   visits. Computing it costs a call to a C stub, so it is cached in the
   node: the field [kk] holds the key in its low [kbits] bits and
   [msb_index key + 1] (or [0] for the key [0]) in the bits above. Keys are
   below [max_int / w], hence they fit in [kbits] bits, and the remaining
   bits are enough for the index (see [kbits] below).

   Empty trees are an immediate constructor rather than a sentinel node: the
   garbage collector skips the empty children of the leaves instead of
   following a pointer to the sentinel, which makes major collections
   cheaper. *)

(* The children are mutable only so that [Sharing.equal] and
   [Sharing.compare] can replace a child with a structurally equal one. No
   other operation mutates a tree. *)
type t =
  | Empty
  | Node of { kk: int; v: int; mutable l: t; mutable r: t }

type elt = int

let empty = Empty

let msb = Bit_lib.extract_msb
let lt_msb = Bit_lib.lt_msb

(* -------------------------------------------------------------------------- *)
(* Encoding of keys. *)

(* Keys are below [max_int / w < 2^kbits] and the index of their most
   significant bit, plus one, is at most [kbits]: it fits in the remaining
   bits. With 63-bit integers, keys take 57 bits and the index 6 bits; with
   32-bit (js_of_ocaml) or 31-bit integers, keys take 27 or 26 bits and the
   index 5 bits. The index may use the sign bit: [kk] is never compared
   directly, only through [key] and [mbit]. *)
let kbits = Sys.int_size - (if Sys.int_size > 32 then 6 else 5)
let kmask = 1 lsl kbits - 1

(* The key of an encoded key [kk]. *)
let[@inline] key kk = kk land kmask

(* The most significant bit of the key of [kk], without a C call. *)
let[@inline] mbit kk = (1 lsl (kk lsr kbits)) lsr 1

let[@inline] pack i k = k lor (i lsl kbits)
let[@inline] index kk = kk lsr kbits

(* [enc_index m] is the index stored for the keys whose most significant bit
   is [m]. *)
let[@inline] enc_index m =
  if m = 0 then 0 else Bit_lib.msb_index m + 1

let[@inline] enc k =
  if k = 0 then 0 else pack (Bit_lib.msb_index k + 1) k

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

(* The bitmap and the left child of a tree, for the places where a tree is
   known to be non-empty but is not destructured. *)
let[@inline] bits = function Empty -> 0 | Node n -> n.v
let[@inline] left = function Empty -> Empty | Node n -> n.l

(* -------------------------------------------------------------------------- *)
(* Low-level operations on keys and bitmaps. *)

let[@inline] leaf k v = Node {kk = enc k; v; l = Empty; r = Empty}

(* [lookup k t] is the bitmap associated with key [k] in [t] ([0] if none). *)
let rec lookup k = function
  | Empty -> 0
  | Node t ->
    if k >= key t.kk then
      if k = key t.kk then t.v else 0
    else
      let m = mbit t.kk in
      if k land m <> 0
      then lookup (k lxor m) t.l
      else lookup k t.r

(* [push k v t] adds the key [k] with bitmap [v <> 0] to [t], where [k] is
   greater than every key of [t]. The new key becomes the root and the old
   root sinks into the left child if it shares the most significant bit. *)
let rec push k v = function
  | Empty -> leaf k v
  | Node n as t ->
    let kk = enc k in
    let m = mbit kk in
    if key n.kk land m <> 0
    then Node {kk; v; l = push (key n.kk lxor m) n.v n.l; r = n.r}
    else Node {kk; v; l = Empty; r = t}

(* [join kk l r] is the tree obtained by removing the root of a node with
   encoded key [kk] and children [l] and [r]: the maximum of [l], if any, is
   promoted. It has the same most significant bit as the removed root. *)
let rec join kk l r =
  match l with
  | Empty -> r
  | Node n ->
    Node {kk = pack (index kk) (mbit kk lor key n.kk); v = n.v;
          l = join n.kk n.l n.r; r}

(* [rebuild t tkk tv tl tr v l r], where [t] is the node
   [{kk = tkk; v = tv; l = tl; r = tr}], is the node [t] with new bitmap [v]
   and children [l] and [r], reusing [t] when nothing changed, and removing
   the root if [v = 0]. *)
let[@inline] rebuild t tkk tv tl tr v l r =
  if v = 0 then join tkk l r
  else if v = tv && l == tl && r == tr then t
  else Node {kk = tkk; v; l; r}

(* [add_bits k v t] adds the bits [v <> 0] at key [k]. *)
let rec add_bits k v = function
  | Empty -> leaf k v
  | Node n as t ->
    if k >= key n.kk then
      if k = key n.kk then
        let v' = n.v lor v in
        if v' = n.v then t else Node {n with v = v'}
      else
        push k v t
    else
      let m = mbit n.kk in
      if k land m <> 0 then
        let l = add_bits (k lxor m) v n.l in
        if l == n.l then t else Node {n with l}
      else
        let r = add_bits k v n.r in
        if r == n.r then t else Node {n with r}

(* [remove_bits k v t] removes the bits [v] at key [k]. *)
let rec remove_bits k v = function
  | Empty -> Empty
  | Node n as t ->
    if k >= key n.kk then
      if k = key n.kk then
        let v' = n.v land lnot v in
        if v' = n.v then t
        else if v' = 0 then join n.kk n.l n.r
        else Node {n with v = v'}
      else
        t
    else
      let m = mbit n.kk in
      if k land m <> 0 then
        let l = remove_bits (k lxor m) v n.l in
        if l == n.l then t else Node {n with l}
      else
        let r = remove_bits k v n.r in
        if r == n.r then t else Node {n with r}

(* -------------------------------------------------------------------------- *)
(* Elementary operations. *)

let is_empty t = t == Empty

let is_singleton = function
  | Node {v; l = Empty; r = Empty; _} -> v land (v - 1) = 0
  | _ -> false

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
    match t with
    | Empty -> acc
    | Node n -> loop n.r (loop n.l (acc + Bit_lib.pop_count n.v))
  in
  loop t 0

(* -------------------------------------------------------------------------- *)
(* Minimum and maximum. *)

(* [min_in pre t] is the least element of the non-empty tree [t], whose keys
   are relative to the prefix [pre]. *)
let rec min_in pre = function
  | Empty -> raise Not_found
  | Node n ->
    match n.r with
    | Node _ as r -> min_in pre r
    | Empty ->
      match n.l with
      | Node _ as l -> min_in (pre lor mbit n.kk) l
      | Empty -> lowest (pre lor key n.kk) n.v

let minimum t = min_in 0 t

let maximum = function
  | Empty -> raise Not_found
  | Node n -> highest (key n.kk) n.v

(* The maximum is at the root, hence cheaper than the minimum. *)
let choose = maximum

let min_elt_opt t = if t == Empty then None else Some (min_in 0 t)

let max_elt_opt = function
  | Empty -> None
  | Node n -> Some (highest (key n.kk) n.v)

let compare_minimum a b =
  match a, b with
  | Empty, Empty -> 0
  | Empty, _ -> -1
  | _, Empty -> 1
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
   of a carried node [c] is [key c.kk land (bound - 1)]: the bits of its key
   above [bound] are exactly those stripped since [c] was carried. We write
   [mk] for [bound - 1]; it is shared by both operands.

   A virtual tree is described by three values [h s p]:
   - the chain: the nodes [h], [h.l], [h.l.l], ..., up to [s] excluded, which
     stand for the keys [key c.kk land mk] with the bitmaps [c.v], in
     decreasing order;
   - the tail: [s] if [p], [Empty] otherwise; its keys are below those of the
     chain. A plain tree [t] is the virtual tree [t t true].

   Splitting a virtual tree at the bit [m] (when every key is below [2m])
   yields an upper and a lower virtual tree, again without allocation: the
   chain keys that have the bit [m] are a prefix of the chain; the others,
   followed by the tail, form the lower part. If every chain key has the bit
   [m], the root of the tail may join the chain, in which case its left child
   becomes the new tail and its right child goes to the lower part. Both
   parts use the mask [m - 1]. *)

(* [vmax h s p mk] is the largest key of a virtual tree, or [-1] if it is
   empty. The corresponding bitmap is [bits h]. *)
let[@inline] vmax h s p mk =
  match h with
  | Node n when h != s -> key n.kk land mk
  | _ ->
    if p then match s with Node n -> key n.kk | Empty -> -1
    else -1

let[@inline] vtail s p = if p then s else Empty

(* [materialize h s p mk] builds a virtual tree. *)
let rec materialize h s p mk =
  match h with
  | Node n when h != s -> push (key n.kk land mk) n.v (materialize n.l s p mk)
  | _ -> vtail s p

(* [first_low m c s] is the first node of the chain [c .. s] whose key does
   not have the bit [m], or [s] if there is none. *)
let rec first_low m c s =
  match c with
  | Node n when c != s && key n.kk land m <> 0 -> first_low m n.l s
  | _ -> c

(* -------------------------------------------------------------------------- *)
(* Comparisons. *)

let rec equal a b =
  a == b ||
  match a, b with
  | Node a, Node b ->
    a.kk = b.kk && a.v = b.v && equal a.l b.l && equal a.r b.r
  | _ -> false

(* A structural total order (it is not the lexicographic order on sorted
   sequences of elements). *)
let rec compare a b =
  if a == b then 0 else
  match a, b with
  | Empty, _ -> -1
  | _, Empty -> 1
  | Node a, Node b ->
    let c = Int.compare (key a.kk) (key b.kk) in
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
   otherwise the one at the higher address. Concurrent use from several
   domains is safe: a racing reader sees either child, and both are equal. *)
module Sharing = struct
  let rec equal a b =
    a == b ||
    match a, b with
    | Node a, Node b ->
      a.kk = b.kk && a.v = b.v &&
      (a.l == b.l || (equal a.l b.l && (
           if Bit_lib.share_choose a.l b.l == a.l
           then b.l <- a.l else a.l <- b.l;
           true))) &&
      (a.r == b.r || (equal a.r b.r && (
           if Bit_lib.share_choose a.r b.r == a.r
           then b.r <- a.r else a.r <- b.r;
           true)))
    | _ -> false

  let rec compare a b =
    if a == b then 0 else
    match a, b with
    | Empty, _ -> -1
    | _, Empty -> 1
    | Node a, Node b ->
      let c = Int.compare (key a.kk) (key b.kk) in
      if c <> 0 then c else
      let c = Int.compare a.v b.v in
      if c <> 0 then c else
      let c = if a.l == b.l then 0 else compare a.l b.l in
      if c <> 0 then c else begin
        if a.l != b.l then
          (if Bit_lib.share_choose a.l b.l == a.l
           then b.l <- a.l else a.l <- b.l);
        let c = if a.r == b.r then 0 else compare a.r b.r in
        if c = 0 && a.r != b.r then
          (if Bit_lib.share_choose a.r b.r == a.r
           then b.r <- a.r else a.r <- b.r);
        c
      end
end

(* The root of the larger tree is irrelevant to [subset] and [disjoint]; the
   root of the smaller one is looked up in the other tree. Carrying it into the
   recursion instead, as [union] does, avoids this second descent but makes
   every step along its path more expensive: measured, it is slower. *)
let rec subset a b =
  a == b ||
  match a, b with
  | Empty, _ -> true
  | _, Empty -> false
  | Node na, Node nb ->
    let ka = key na.kk and kb = key nb.kk in
    if ka = kb then
      na.v land lnot nb.v = 0 && subset na.l nb.l && subset na.r nb.r
    else if ka > kb then
      false (* the maximum of [a] is not in [b] *)
    else if lt_msb ka kb then
      subset a nb.r
    else
      (* Same most significant bit [m], [ka < kb]: the root of [a] is looked
         up in [b.l]; the root of [b] is irrelevant. *)
      let m = mbit nb.kk in
      na.v land lnot (lookup (ka lxor m) nb.l) = 0 &&
      subset na.l nb.l && subset na.r nb.r

let rec disjoint a b =
  match a, b with
  | Empty, _ | _, Empty -> true
  | Node na, Node nb ->
    a != b &&
    let ka = key na.kk and kb = key nb.kk in
    if ka = kb then
      na.v land nb.v = 0 && disjoint na.l nb.l && disjoint na.r nb.r
    else if lt_msb ka kb then
      disjoint a nb.r
    else if lt_msb kb ka then
      disjoint na.r b
    else if ka < kb then
      na.v land lookup (ka lxor mbit nb.kk) nb.l = 0 &&
      disjoint na.l nb.l && disjoint na.r nb.r
    else
      nb.v land lookup (kb lxor mbit na.kk) na.l = 0 &&
      disjoint na.l nb.l && disjoint na.r nb.r

(* Under the precondition [a ⊆ b ∨ a ∩ b = ∅], it suffices to test one
   element of [a]: its maximum is at the root. *)
let quick_subset a b =
  match a with
  | Empty -> false
  | Node n -> lookup (key n.kk) b land n.v <> 0

(* -------------------------------------------------------------------------- *)
(* Union. *)

(* Physical equality: if the result of [union a b] is equal to [b], then [b]
   is returned. (The same holds for [a] as long as [a] and [b] do not have
   structurally equal but physically distinct subtrees.) *)
let rec union a b =
  if a == b then b else
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | Node na, Node nb ->
    let ka = key na.kk and kb = key nb.kk in
    if ka = kb then
      let l = union na.l nb.l in
      let r = union na.r nb.r in
      let v = na.v lor nb.v in
      if v = nb.v && l == nb.l && r == nb.r then b
      else if v = na.v && l == na.l && r == na.r then a
      else Node {kk = na.kk; v; l; r}
    else if lt_msb ka kb then
      (* [a] lies below the most significant bit of [kb]. *)
      let r = union a nb.r in
      if r == nb.r then b else Node {nb with r}
    else if lt_msb kb ka then
      let r = union na.r b in
      if r == na.r then a else Node {na with r}
    else
      (* Same most significant bit [m]: the smaller root is carried into the
         upper half. *)
      let m = mbit na.kk in
      let r = union na.r nb.r in
      if ka > kb then
        let l = union_push false na.l b nb.v nb.l (kb lxor m) in
        if l == na.l && r == na.r then a else Node {na with l; r}
      else
        let l = union_push true nb.l a na.v na.l (ka lxor m) in
        if l == nb.l && r == nb.r then b else Node {nb with l; r}

(* [union_o xb x y] is the union of [x] and [y], where [xb] tells whether [x]
   comes from the side whose physical identity is preserved. *)
and union_o xb x y =
  if xb then union y x else union x y

(* [union_push xb x h v y k] is the union of the tree [x] and the "pennant"
   made of the carried node [h], standing for the key [k] (greater than every
   key of [y]) with the bitmap [v], followed by its left child [y]. If the
   result is equal to [x], then [x] is returned. *)
and union_push xb x h v y k =
  match x with
  | Empty -> push k v y
  | Node xn ->
    let kx = key xn.kk in
    if kx = k then
      let m = mbit xn.kk in
      let l, r =
        match y with
        | Node yn when key yn.kk land m <> 0 ->
          union_push xb xn.l y yn.v yn.l (key yn.kk lxor m),
          union_o xb xn.r yn.r
        | _ -> xn.l, union_o xb xn.r y
      in
      let v' = xn.v lor v in
      if v' = xn.v && l == xn.l && r == xn.r then x
      else Node {xn with v = v'; l; r}
    else if lt_msb kx k then
      (* [x] lies in the lower half of the result. *)
      let kk = enc k in
      let m = mbit kk in
      match y with
      | Node yn when key yn.kk land m <> 0 ->
        Node {kk; v; l = push (key yn.kk lxor m) yn.v yn.l;
              r = union_o xb x yn.r}
      | _ -> Node {kk; v; l = Empty; r = union_o xb x y}
    else if lt_msb k kx then
      let r = union_push xb xn.r h v y k in
      if r == xn.r then x else Node {xn with r}
    else
      (* Same most significant bit [m], [k <> kx]. *)
      let m = mbit xn.kk in
      match y with
      | Node yn when key yn.kk land m <> 0 ->
        let r = union_o xb xn.r yn.r in
        if kx > k then
          (* Both [k] and the root of [y] are carried into the upper half. *)
          let l =
            if xb then vunion h yn.l true xn.l xn.l true (m - 1)
            else vunion xn.l xn.l true h yn.l true (m - 1)
          in
          if l == xn.l && r == xn.r then x else Node {xn with l; r}
        else
          (* [kx < k]: the upper halves are the pennants of [x] and [y]. *)
          let l =
            if xb then vunion y yn.l true x xn.l true (m - 1)
            else vunion x xn.l true y yn.l true (m - 1)
          in
          Node {kk = pack (index xn.kk) k; v; l; r}
      | _ ->
        let r = union_o xb xn.r y in
        if kx > k then
          let l = add_bits (k lxor m) v xn.l in
          if l == xn.l && r == xn.r then x else Node {xn with l; r}
        else
          Node {kk = pack (index xn.kk) k; v;
                l = push (kx lxor m) xn.v xn.l; r}

(* [vunion ha sa pa hb sb pb mk] is the union of two virtual trees. If the
   result is equal to an argument that is a plain tree, this argument is
   returned (preferably the second one). *)
and vunion ha sa pa hb sb pb mk =
  if ha == sa then
    if hb == sb then union (vtail sa pa) (vtail sb pb)
    else match hb with
      | Node n when n.l == sb ->
        (* A single carried key, possibly followed by its left child. *)
        if pb then union_push false (vtail sa pa) hb n.v n.l (key n.kk land mk)
        else add_bits (key n.kk land mk) n.v (vtail sa pa)
      | _ -> vunion_gen ha sa pa hb sb pb mk
  else match ha with
    | Node n when hb == sb && n.l == sa ->
      if pa then union_push true (vtail sb pb) ha n.v n.l (key n.kk land mk)
      else add_bits (key n.kk land mk) n.v (vtail sb pb)
    | _ -> vunion_gen ha sa pa hb sb pb mk

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
  match hy with
  | Empty -> assert false
  | Node hyn ->
    if hy == sy then
      (* [y] is a plain tree. *)
      let r =
        if sw then vunion hyn.r hyn.r true hx sx px mk
        else vunion hx sx px hyn.r hyn.r true mk
      in
      if r == hyn.r then hy else Node {hyn with r}
    else
      let m = msb ky in
      let mk = m - 1 in
      let h = hyn.l in
      let c = first_low m h sy in
      let l, r =
        if c != sy then
          materialize h c false mk,
          (if sw then vunion c sy py hx sx px mk
           else vunion hx sx px c sy py mk)
        else match sy with
          | Node syn when py && key syn.kk land m <> 0 ->
            (* The root of the tail joins the chain. *)
            materialize h syn.l true mk,
            (let t = syn.r in
             if sw then vunion t t true hx sx px mk
             else vunion hx sx px t t true mk)
          | _ ->
            materialize h c false mk,
            (let t = vtail sy py in
             if sw then vunion t t true hx sx px mk
             else vunion hx sx px t t true mk)
      in
      Node {kk = pack (enc_index m) ky; v = hyn.v; l; r}

(* [vunion_same] handles the case where the maxima [ka] and [kb] of [a] and
   [b] have the same most significant bit [m]. The largest maximum (or both,
   if they are equal) becomes the root. Both virtual trees are split at [m]:
   the upper halves are merged into the left child, and the lower halves
   into the right child. *)
and vunion_same ha sa pa hb sb pb ka kb =
  let k = if ka > kb then ka else kb in
  let m = msb k in
  let kk = pack (enc_index m) k in
  (* Split [a] into [uha usa upa] and [lha lsa lpa]. If [ka = k], its
     maximum is popped first. *)
  let uha, usa, upa, lha, lsa, lpa =
    match sa with
    | Node n when ka = k && ha == sa ->
      (* Popping the root of a plain tree yields its two children. *)
      n.l, n.l, true, n.r, n.r, true
    | _ ->
      let h = if ka = k then left ha else ha in
      let c = first_low m h sa in
      if c != sa then h, c, false, c, sa, pa
      else match sa with
        | Node n when pa && key n.kk land m <> 0 ->
          h, n.l, true, n.r, n.r, true
        | _ -> h, sa, false, sa, sa, pa
  in
  let uhb, usb, upb, lhb, lsb, lpb =
    match sb with
    | Node n when kb = k && hb == sb -> n.l, n.l, true, n.r, n.r, true
    | _ ->
      let h = if kb = k then left hb else hb in
      let c = first_low m h sb in
      if c != sb then h, c, false, c, sb, pb
      else match sb with
        | Node n when pb && key n.kk land m <> 0 ->
          h, n.l, true, n.r, n.r, true
        | _ -> h, sb, false, sb, sb, pb
  in
  let v =
    if ka = kb then bits ha lor bits hb
    else if ka = k then bits ha
    else bits hb
  in
  let l = vunion uha usa upa uhb usb upb (m - 1) in
  let r = vunion lha lsa lpa lhb lsb lpb (m - 1) in
  match sb, sa with
  | Node n, _ when kb = k && hb == sb && v = n.v && l == n.l && r == n.r -> sb
  | _, Node n when ka = k && ha == sa && v = n.v && l == n.l && r == n.r -> sa
  | _ -> Node {kk; v; l; r}

(* -------------------------------------------------------------------------- *)
(* Intersection. *)

(* If the result of [inter a b] is equal to [a], then [a] is returned. *)
let rec inter a b =
  if a == b then a else
  match a, b with
  | Empty, _ | _, Empty -> Empty
  | Node na, Node nb ->
    let ka = key na.kk and kb = key nb.kk in
    if ka = kb then
      let l = inter na.l nb.l in
      let r = inter na.r nb.r in
      let v = na.v land nb.v in
      if v = 0 then join na.kk l r
      else if v = na.v && l == na.l && r == na.r then a
      else if v = nb.v && l == nb.l && r == nb.r then b
      else Node {kk = na.kk; v; l; r}
    else if lt_msb ka kb then inter a nb.r
    else if lt_msb kb ka then inter na.r b
    (* Same most significant bit: the larger root is not in the other tree,
       and the maximum of the result is at most the smaller root. *)
    else if ka < kb then
      let l = inter na.l nb.l in
      let r = inter na.r nb.r in
      rebuild a na.kk na.v na.l na.r
        (na.v land lookup (ka lxor mbit nb.kk) nb.l) l r
    else
      let l = inter nb.l na.l in
      let r = inter nb.r na.r in
      rebuild b nb.kk nb.v nb.l nb.r
        (nb.v land lookup (kb lxor mbit na.kk) na.l) l r

(* -------------------------------------------------------------------------- *)
(* Difference. *)

(* If the result of [diff a b] is equal to [a], then [a] is returned. *)
let rec diff a b =
  if a == b then Empty else
  match a, b with
  | Empty, _ -> Empty
  | _, Empty -> a
  | Node na, Node nb ->
    let ka = key na.kk and kb = key nb.kk in
    if ka = kb then
      let l = diff na.l nb.l in
      let r = diff na.r nb.r in
      rebuild a na.kk na.v na.l na.r (na.v land lnot nb.v) l r
    else if lt_msb ka kb then diff a nb.r
    else if lt_msb kb ka then
      let r = diff na.r b in
      if r == na.r then a else Node {na with r}
    else
      let m = mbit na.kk in
      let r = diff na.r nb.r in
      if ka > kb then
        (* The root of [a] survives; the root of [b] is carried. *)
        let l = diff_push na.l b nb.v nb.l (kb lxor m) in
        if l == na.l && r == na.r then a else Node {na with l; r}
      else
        let l = diff na.l nb.l in
        rebuild a na.kk na.v na.l na.r
          (na.v land lnot (lookup (ka lxor m) nb.l)) l r

(* [diff_push x h hv y k] is the difference between [x] and the pennant made
   of the carried node [h], standing for the key [k] with the bitmap [hv],
   followed by its left child [y]. *)
and diff_push x h hv y k =
  match x with
  | Empty -> x
  | Node xn ->
    let kx = key xn.kk in
    if kx = k then
      let m = mbit xn.kk in
      let l, r =
        match y with
        | Node yn when key yn.kk land m <> 0 ->
          diff_push xn.l y yn.v yn.l (key yn.kk lxor m), diff xn.r yn.r
        | _ -> xn.l, diff xn.r y
      in
      rebuild x xn.kk xn.v xn.l xn.r (xn.v land lnot hv) l r
    else if lt_msb kx k then
      (* [x] lies in the lower half of the pennant. *)
      let m = msb k in
      match y with
      | Node yn when key yn.kk land m <> 0 -> diff x yn.r
      | _ -> diff x y
    else if lt_msb k kx then
      let r = diff_push xn.r h hv y k in
      if r == xn.r then x else Node {xn with r}
    else if kx < k then
      (* The carried key is above every key of [x]: drop it. *)
      diff x y
    else
      let m = mbit xn.kk in
      let l, r =
        match y with
        | Node yn when key yn.kk land m <> 0 ->
          vdiff xn.l h yn.l true (m - 1), diff xn.r yn.r
        | _ -> remove_bits (k lxor m) hv xn.l, diff xn.r y
      in
      if l == xn.l && r == xn.r then x else Node {xn with l; r}

(* [vdiff x hb sb pb mk] is the difference between the tree [x] and a
   virtual tree. If nothing is removed, [x] is returned. *)
and vdiff x hb sb pb mk =
  if hb == sb then diff x (vtail sb pb) else
  match hb with
  | Empty -> assert false
  | Node hbn ->
    if hbn.l == sb then
      if pb then diff_push x hb hbn.v hbn.l (key hbn.kk land mk)
      else remove_bits (key hbn.kk land mk) hbn.v x
    else match x with
      | Empty -> x
      | Node xn ->
        let kb = key hbn.kk land mk and kx = key xn.kk in
        if kx = kb then
          let m = mbit xn.kk in
          let h = hbn.l in
          let c = first_low m h sb in
          let l, r =
            if c != sb then
              vdiff xn.l h c false (m - 1), vdiff xn.r c sb pb (m - 1)
            else match sb with
              | Node sbn when pb && key sbn.kk land m <> 0 ->
                vdiff xn.l h sbn.l true (m - 1), diff xn.r sbn.r
              | _ ->
                vdiff xn.l h sb false (m - 1), diff xn.r (vtail sb pb)
          in
          rebuild x xn.kk xn.v xn.l xn.r (xn.v land lnot hbn.v) l r
        else if lt_msb kx kb then
          (* [x] lies below the most significant bit [m] of [kb]: only the
             lower half of [b] matters. *)
          let m = msb kb in
          let c = first_low m hb sb in
          if c != sb then vdiff x c sb pb (m - 1)
          else match sb with
            | Node sbn when pb && key sbn.kk land m <> 0 -> diff x sbn.r
            | _ -> diff x (vtail sb pb)
        else if lt_msb kb kx then
          let r = vdiff xn.r hb sb pb mk in
          if r == xn.r then x else Node {xn with r}
        else if kx < kb then
          (* The maximum of [b] is above every key of [x]: drop it. *)
          vdiff x hbn.l sb pb mk
        else
          (* [kx > kb], same most significant bit [m]: the root of [x]
             survives. *)
          let m = mbit xn.kk in
          let c = first_low m hb sb in
          let l, r =
            if c != sb then
              vdiff xn.l hb c false (m - 1), vdiff xn.r c sb pb (m - 1)
            else match sb with
              | Node sbn when pb && key sbn.kk land m <> 0 ->
                vdiff xn.l hb sbn.l true (m - 1), diff xn.r sbn.r
              | _ ->
                vdiff xn.l hb sb false (m - 1), diff xn.r (vtail sb pb)
          in
          if l == xn.l && r == xn.r then x else Node {xn with l; r}

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

let rec iter f pre = function
  | Empty -> ()
  | Node t ->
    iter f pre t.r;
    iter f (pre lor mbit t.kk) t.l;
    iter_bits f (pre lor key t.kk) t.v

let iter f t = iter f 0 t

let rec rev_iter f pre = function
  | Empty -> ()
  | Node t ->
    rev_iter_bits f (pre lor key t.kk) t.v;
    rev_iter f (pre lor mbit t.kk) t.l;
    rev_iter f pre t.r

let rev_iter f t = rev_iter f 0 t

let rec fold f pre t acc =
  match t with
  | Empty -> acc
  | Node t ->
    let acc = fold f pre t.r acc in
    let acc = fold f (pre lor mbit t.kk) t.l acc in
    let b = base (pre lor key t.kk) in
    let v = ref t.v and acc = ref acc in
    while !v <> 0 do
      let i = Bit_lib.lsb_index !v in
      v := !v land (!v - 1);
      acc := f (b + i) !acc
    done;
    !acc

let fold f t acc = fold f 0 t acc

let rec fold_right f pre t acc =
  match t with
  | Empty -> acc
  | Node t ->
    let b = base (pre lor key t.kk) in
    let v = ref t.v and acc = ref acc in
    while !v <> 0 do
      let i = Bit_lib.msb_index !v in
      v := !v lxor (1 lsl i);
      acc := f (b + i) !acc
    done;
    let acc = fold_right f (pre lor mbit t.kk) t.l !acc in
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

let rec find_first p pre = function
  | Empty -> -1
  | Node t ->
    let x = find_first p pre t.r in
    if x >= 0 then x else
    let x = find_first p (pre lor mbit t.kk) t.l in
    if x >= 0 then x else
    find_bits p (pre lor key t.kk) t.v

let find_first_opt p t =
  let x = find_first p 0 t in
  if x < 0 then None else Some x

let rec for_all p pre = function
  | Empty -> true
  | Node t ->
    for_all p pre t.r &&
    for_all p (pre lor mbit t.kk) t.l &&
    find_bits (fun x -> not (p x)) (pre lor key t.kk) t.v < 0

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
  match t with
  | Empty -> t
  | Node n ->
    let r = filter p pre n.r in
    let l = filter p (pre lor mbit n.kk) n.l in
    rebuild t n.kk n.v n.l n.r (filter_bits p (pre lor key n.kk) n.v) l r

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
  | Tree (_, Empty, e) -> seq_of_enum e ()
  | Tree (pre, Node t, e) ->
    seq_of_enum
      (Tree (pre, t.r,
             Tree (pre lor mbit t.kk, t.l,
                   Bits (pre lor key t.kk, t.v, e)))) ()
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
  match t with
  | Empty -> Empty
  | Node n ->
    if key n.kk < k then Empty
    else if key n.kk = k then
      let v = n.v land mask in
      if v = 0 then Empty
      else if v = n.v && n.l == Empty && n.r == Empty then t
      else leaf k v
    else
      let m = mbit n.kk in
      if k land m <> 0 then
        let l = above_bits (k lxor m) mask n.l in
        if l == n.l && n.r == Empty then t else Node {n with l; r = Empty}
      else
        let r = above_bits k mask n.r in
        if r == n.r then t else Node {n with r}

(* [below_bits k mask t] keeps the keys smaller than [k], plus the bits
   [mask] of key [k]. *)
let rec below_bits k mask t =
  match t with
  | Empty -> t
  | Node n ->
    if key n.kk < k then t
    else if key n.kk = k then
      rebuild t n.kk n.v n.l n.r (n.v land mask) n.l n.r
    else
      let m = mbit n.kk in
      if k land m <> 0
      then join n.kk (below_bits (k lxor m) mask n.l) n.r
      else below_bits k mask n.r

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
  if a == b then -1 else
  match a, b with
  | Empty, Empty -> -1
  | Node na, Empty -> highest (pre lor key na.kk) na.v
  | Empty, Node nb -> highest (pre lor key nb.kk) nb.v
  | Node na, Node nb ->
    let ka = key na.kk and kb = key nb.kk in
    if ka <> kb then
      if ka > kb then highest (pre lor ka) na.v
      else highest (pre lor kb) nb.v
    else if na.v <> nb.v then
      highest (pre lor ka) (na.v lxor nb.v)
    else
      let d = max_sym_diff (pre lor mbit na.kk) na.l nb.l in
      if d >= 0 then d else max_sym_diff pre na.r nb.r

let extract_shared_suffix s1 s2 =
  let x = max_sym_diff 0 s1 s2 in
  if x < 0 then s1, (empty, empty)
  else above x s1, (at_most x s1, at_most x s2)

(* [min_gt pre k t] is the least element of [t] whose key is greater than
   [k], or [-1]. *)
let rec min_gt pre k = function
  | Empty -> -1
  | Node n ->
    if key n.kk <= k then -1 else
    let m = mbit n.kk in
    let x =
      if k land m <> 0
      then min_gt (pre lor m) (k lxor m) n.l
      else
        let x = min_gt pre k n.r in
        if x >= 0 || n.l == Empty then x else min_in (pre lor m) n.l
    in
    if x >= 0 then x else lowest (pre lor key n.kk) n.v

(* [min_sym_diff pre a b] is the least element of the symmetric difference
   of [a] and [b], or [-1] if they are equal. *)
let rec min_sym_diff pre a b =
  if a == b then -1 else
  match a, b with
  | Empty, _ -> min_in pre b
  | _, Empty -> min_in pre a
  | Node na, Node nb ->
    let ka = key na.kk and kb = key nb.kk in
    if ka = kb then
      let x = min_sym_diff pre na.r nb.r in
      if x >= 0 then x else
      let x = min_sym_diff (pre lor mbit na.kk) na.l nb.l in
      if x >= 0 then x else
      let d = na.v lxor nb.v in
      if d = 0 then -1 else lowest (pre lor ka) d
    else if lt_msb ka kb then min_sym_diff_low pre a b
    else if lt_msb kb ka then min_sym_diff_low pre b a
    else if ka < kb then min_sym_diff_lt pre a b
    else min_sym_diff_lt pre b a

(* [a] lies in the lower half of [b]: all the upper half of [b] differs. *)
and min_sym_diff_low pre a b =
  match b with
  | Empty -> assert false
  | Node nb ->
    let x = min_sym_diff pre a nb.r in
    if x >= 0 then x
    else if nb.l != Empty then min_in (pre lor mbit nb.kk) nb.l
    else lowest (pre lor key nb.kk) nb.v

(* Same most significant bit [m], [ka < kb]. In the upper halves, [a] holds
   [a.l] and the key [ka - m], [b] holds [b.l] and the key [kb - m], which is
   above everything in [a]. *)
and min_sym_diff_lt pre a b =
  match a, b with
  | Node na, Node nb ->
    let x = min_sym_diff pre na.r nb.r in
    if x >= 0 then x else
    let m = mbit na.kk in
    let pre' = pre lor m in
    let ka = pre lor key na.kk in       (* absolute key of the root of [a] *)
    let x = min_sym_diff pre' na.l nb.l in
    if x >= 0 && key_of x < ka then x
    else if x < 0 || key_of x > ka then
      (* [b.l] does not have the key [ka]. *)
      lowest ka na.v
    else
      (* [b.l] has the key [ka] and [a.l] does not. *)
      let d = na.v lxor lookup (key na.kk lxor m) nb.l in
      if d <> 0 then lowest ka d
      else
        (* The next element of [b] above [ka] differs. *)
        let x = min_gt pre' (key na.kk lxor m) nb.l in
        if x >= 0 then x else lowest (pre lor key nb.kk) nb.v
  | _ -> assert false

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
    match t with
    | Empty -> ()
    | Node n ->
      let k = key n.kk in
      if n.v = 0 then fail "a node with an empty bitmap";
      if n.kk <> enc k then fail "bad cached msb for key %d" k;
      if bound >= 0 && k >= bound then
        fail "key %d is out of its bound %d" k bound;
      let m = mbit n.kk in
      check n.l (k - m);
      check n.r m
  in
  check t (-1)
