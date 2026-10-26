(* Sets of non-negative integers: the tries of PatSet (see PatSet.ml for the
   representation and the algorithms), with nodes that only have fields for
   their non-empty children.

   {2 Representation}

   A node is one of four constructors, depending on which of its children
   are non-empty: [Leaf] (none), [Left], [Right] or [Both]. A leaf takes 3
   words instead of 5, a node with a single child 4. The key and the bitmap
   are the fields [0] and [1] of every node, read with a single load ([kkey]
   and [bits]); the children are read with [left] and [right], which return
   [empty] for a missing child.

   The empty set is the leaf [empty = Leaf {kk = 0; v = 0}]. It is the only
   node with an empty bitmap and never occurs in a non-empty tree: [node]
   builds the constructor that fits the children it is given. Emptiness is
   tested on the bitmap ([is_empty]), so that a copy of [empty] (obtained by
   unmarshalling, for instance) is empty too; [left] and [right] return
   [empty] itself, so that the physical equality tests of the algorithms
   between chains of carried nodes see the missing children as equal.

   The algorithms are those of PatSet, written with these accessors and
   [node] in place of the patterns [Empty] and [Node n] and of the
   constructor [Node]. The operations that dominate the cost ([add],
   [remove], [union], [inter], [diff], [subset]) match on the constructors
   once per node instead, and skip the recursive calls on missing children;
   their recursions on children known to be non-empty go through variants
   ([union_ne], ...) that do not test it again. *)

(* The children are mutable only so that the operations of [Sharing] can
   replace a child with a structurally equal one. *)
type t =
  | Leaf of { kk: int; v: int }
  | Left of { kk: int; v: int; mutable l: t }
  | Right of { kk: int; v: int; mutable r: t }
  | Both of { kk: int; v: int; mutable l: t; mutable r: t }

type elt = int

let empty = Leaf {kk = 0; v = 0}

let msb = Bit_lib.extract_msb
let lt_msb = Bit_lib.lt_msb

(* -------------------------------------------------------------------------- *)
(* Encoding of keys (see PatSet). *)

let kbits = Sys.int_size - (if Sys.int_size > 32 then 6 else 5)
let kmask = 1 lsl kbits - 1

let[@inline] key kk = kk land kmask
let[@inline] mbit kk = (1 lsl (kk lsr kbits)) lsr 1
let[@inline] pack i k = k lor (i lsl kbits)
let[@inline] index kk = kk lsr kbits

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

let[@inline] lowest k v = base k + Bit_lib.lsb_index v
let[@inline] highest k v = base k + Bit_lib.msb_index v

let check_elt fn x =
  if x < 0 then invalid_arg ("PatSetCompact." ^ fn ^ ": negative element")

(* -------------------------------------------------------------------------- *)
(* Accessors and constructors. *)

(* The encoded key and the bitmap: the fields [0] and [1] of every
   constructor. *)
external kkey : t -> int = "%field0"
external bits : t -> int = "%field1"

let[@inline] tkey t = key (kkey t)
let[@inline] is_empty t = bits t = 0

let[@inline] left = function
  | Left {l; _} | Both {l; _} -> l
  | Leaf _ | Right _ -> empty

let[@inline] right = function
  | Right {r; _} | Both {r; _} -> r
  | Leaf _ | Left _ -> empty

(* [node kk v l r] is the node with encoded key [kk], bitmap [v <> 0] and
   children [l] and [r]. A missing child is [empty] itself: the children
   are read with [left], [right] or a match, or are results of operations
   on children, and only a whole set can be a copy of [empty] (which every
   operation tests with [is_empty] before using its children). *)
let[@inline] node kk v l r =
  if l == empty then
    if r == empty then Leaf {kk; v} else Right {kk; v; r}
  else if r == empty then Left {kk; v; l}
  else Both {kk; v; l; r}

let[@inline] with_v t v =
  match t with
  | Leaf {kk; _} -> Leaf {kk; v}
  | Left {kk; l; _} -> Left {kk; v; l}
  | Right {kk; r; _} -> Right {kk; v; r}
  | Both {kk; l; r; _} -> Both {kk; v; l; r}

let[@inline] with_r t r = node (kkey t) (bits t) (left t) r

let[@inline] leaf k v = Leaf {kk = enc k; v}

(* -------------------------------------------------------------------------- *)
(* Low-level operations on keys and bitmaps. *)

(* [lookup k t] is the bitmap associated with key [k] in [t] ([0] if none).
   On [empty], whose key is [0] and bitmap [0], it returns [0]. *)
let rec lookup k t =
  let kk = kkey t in
  if k >= key kk then
    if k = key kk then bits t else 0
  else
    let m = mbit kk in
    if k land m <> 0
    then lookup (k lxor m) (left t)
    else lookup k (right t)

(* [push k v t] adds the key [k] with bitmap [v <> 0] to [t], where [k] is
   greater than every key of [t]. *)
let rec push k v t =
  if is_empty t then leaf k v else
  let kk = enc k in
  let m = mbit kk in
  let kt = tkey t in
  if kt land m = 0 then Right {kk; v; r = t} else
  match t with
  | Leaf {v = vt; _} -> Left {kk; v; l = leaf (kt lxor m) vt}
  | Left {v = vt; l; _} -> Left {kk; v; l = push (kt lxor m) vt l}
  | Right {v = vt; r; _} -> Both {kk; v; l = leaf (kt lxor m) vt; r}
  | Both {v = vt; l; r; _} -> Both {kk; v; l = push (kt lxor m) vt l; r}

(* [join kk l r] is the tree obtained by removing the root of a node with
   encoded key [kk] and children [l] and [r]: the maximum of [l], if any, is
   promoted. *)
let rec join kk l r =
  if is_empty l then r else
  let kk' = pack (index kk) (mbit kk lor tkey l) in
  match l with
  | Leaf {v; _} -> node kk' v empty r
  | Left {kk = kl; v; l = ll} -> node kk' v (join kl ll empty) r
  | Right {v; r = lr; _} -> node kk' v lr r
  | Both {kk = kl; v; l = ll; r = lr} -> node kk' v (join kl ll lr) r

(* [rebuild t tl tr v l r] is the node [t], with children [tl] and [tr],
   with new bitmap [v] and children [l] and [r], reusing [t] when nothing
   changed, and removing the root if [v = 0]. *)
let[@inline] rebuild t tl tr v l r =
  if v = 0 then join (kkey t) l r
  else if v = bits t && l == tl && r == tr then t
  else node (kkey t) v l r

(* The operations below match on the constructors once per node, rather
   than reading each child with [left] and [right]. *)

(* [add_bits k v t] adds the bits [v <> 0] at key [k]. *)
let rec add_bits k v t =
  if is_empty t then leaf k v else
  let kk = kkey t in
  if k >= key kk then
    if k = key kk then
      let v' = bits t lor v in
      if v' = bits t then t else with_v t v'
    else
      push k v t
  else
    let m = mbit kk in
    if k land m <> 0 then
      let k = k lxor m in
      match t with
      | Leaf {v = vt; _} -> Left {kk; v = vt; l = leaf k v}
      | Right {v = vt; r; _} -> Both {kk; v = vt; l = leaf k v; r}
      | Left {v = vt; l; _} ->
        let l' = add_bits k v l in
        if l' == l then t else Left {kk; v = vt; l = l'}
      | Both {v = vt; l; r; _} ->
        let l' = add_bits k v l in
        if l' == l then t else Both {kk; v = vt; l = l'; r}
    else
      match t with
      | Leaf {v = vt; _} -> Right {kk; v = vt; r = leaf k v}
      | Left {v = vt; l; _} -> Both {kk; v = vt; l; r = leaf k v}
      | Right {v = vt; r; _} ->
        let r' = add_bits k v r in
        if r' == r then t else Right {kk; v = vt; r = r'}
      | Both {v = vt; l; r; _} ->
        let r' = add_bits k v r in
        if r' == r then t else Both {kk; v = vt; l; r = r'}

(* [remove_bits k v t] removes the bits [v] at key [k]. *)
let rec remove_bits k v t =
  if is_empty t then t else
  let kk = kkey t in
  if k >= key kk then
    if k = key kk then
      let v' = bits t land lnot v in
      if v' = bits t then t
      else if v' = 0 then
        match t with
        | Leaf _ -> empty
        | Left {l; _} -> join kk l empty
        | Right {r; _} -> r
        | Both {l; r; _} -> join kk l r
      else with_v t v'
    else
      t
  else
    let m = mbit kk in
    if k land m <> 0 then
      let k = k lxor m in
      match t with
      | Leaf _ | Right _ -> t
      | Left {v = vt; l; _} ->
        let l' = remove_bits k v l in
        if l' == l then t
        else if l' == empty then Leaf {kk; v = vt}
        else Left {kk; v = vt; l = l'}
      | Both {v = vt; l; r; _} ->
        let l' = remove_bits k v l in
        if l' == l then t
        else if l' == empty then Right {kk; v = vt; r}
        else Both {kk; v = vt; l = l'; r}
    else
      match t with
      | Leaf _ | Left _ -> t
      | Right {v = vt; r; _} ->
        let r' = remove_bits k v r in
        if r' == r then t
        else if r' == empty then Leaf {kk; v = vt}
        else Right {kk; v = vt; r = r'}
      | Both {v = vt; l; r; _} ->
        let r' = remove_bits k v r in
        if r' == r then t
        else if r' == empty then Left {kk; v = vt; l}
        else Both {kk; v = vt; l; r = r'}

(* -------------------------------------------------------------------------- *)
(* Elementary operations. *)

let is_singleton = function
  | Leaf {v; _} -> v <> 0 && v land (v - 1) = 0
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
    | Leaf {v; _} -> acc + Bit_lib.pop_count v
    | Left {v; l; _} -> loop l (acc + Bit_lib.pop_count v)
    | Right {v; r; _} -> loop r (acc + Bit_lib.pop_count v)
    | Both {v; l; r; _} -> loop r (loop l (acc + Bit_lib.pop_count v))
  in
  loop t 0

(* -------------------------------------------------------------------------- *)
(* Minimum and maximum. *)

(* [min_in pre t] is the least element of the non-empty tree [t], whose keys
   are relative to the prefix [pre]. *)
let rec min_in pre = function
  | Right {r; _} | Both {r; _} -> min_in pre r
  | Left {kk; l; _} -> min_in (pre lor mbit kk) l
  | Leaf {kk; v} -> if v = 0 then raise Not_found else lowest (pre lor key kk) v

let minimum t = min_in 0 t

let maximum t =
  if is_empty t then raise Not_found else highest (tkey t) (bits t)

let choose = maximum

let min_elt_opt t = if is_empty t then None else Some (min_in 0 t)

let max_elt_opt t =
  if is_empty t then None else Some (highest (tkey t) (bits t))

let compare_minimum a b =
  match is_empty a, is_empty b with
  | true, true -> 0
  | true, false -> -1
  | false, true -> 1
  | false, false -> Int.compare (min_in 0 a) (min_in 0 b)

(* -------------------------------------------------------------------------- *)
(* Carried keys and virtual trees (see PatSet). A chain [h .. s] follows the
   left children, with [left]: a missing left child is [empty], so a chain
   that ends at a missing child ends at [s = empty]. *)

let[@inline] vmax h s p mk =
  if h != s && not (is_empty h) then tkey h land mk
  else if p && not (is_empty s) then tkey s
  else -1

let[@inline] vtail s p = if p then s else empty

let rec materialize h s p mk =
  if h != s && not (is_empty h)
  then push (tkey h land mk) (bits h) (materialize (left h) s p mk)
  else vtail s p

let rec first_low m c s =
  if c != s && not (is_empty c) && tkey c land m <> 0
  then first_low m (left c) s
  else c

(* -------------------------------------------------------------------------- *)
(* Comparisons. *)

(* Two nodes with the same key and bitmap are equal if and only if their
   children are: a missing child of one is [empty], and the child of the
   other, if any, has a non-empty bitmap. *)
let rec equal a b =
  a == b ||
  (kkey a = kkey b && bits a = bits b &&
   equal (left a) (left b) && equal (right a) (right b))

let rec compare a b =
  if a == b then 0 else
  if is_empty a then (if is_empty b then 0 else -1) else
  if is_empty b then 1 else
  let c = Int.compare (tkey a) (tkey b) in
  if c <> 0 then c else
  let c = Int.compare (bits a) (bits b) in
  if c <> 0 then c else
  let c = compare (left a) (left b) in
  if c <> 0 then c else
  compare (right a) (right b)

(* In the operations below, the children of two nodes are combined by
   matching on the pair of constructors: a missing child is never read, so
   [empty] only appears as an argument of the outermost call, which tests it
   with [is_empty], and as a result. *)

let rec subset a b =
  a == b || is_empty a || (not (is_empty b) && subset_ne a b)

(* [subset_ne a b] is [subset a b] for non-empty [a] and [b]. *)
and subset_ne a b =
  let ka = tkey a and kb = tkey b in
  if ka > kb then false
  else if lt_msb ka kb then
    match b with
    | Right {r; _} | Both {r; _} -> a == r || subset_ne a r
    | Leaf _ | Left _ -> false
  else
    (if ka = kb then bits a land lnot (bits b) = 0
     else match b with
       | Left {l; _} | Both {l; _} ->
         bits a land lnot (lookup (ka lxor mbit (kkey b)) l) = 0
       | Leaf _ | Right _ -> false) &&
    match a, b with
    | Leaf _, _ -> true
    | Left {l = al; _}, Left {l = bl; _}
    | Left {l = al; _}, Both {l = bl; _} ->
      al == bl || subset_ne al bl
    | Right {r = ar; _}, Right {r = br; _}
    | Right {r = ar; _}, Both {r = br; _} ->
      ar == br || subset_ne ar br
    | Both {l = al; r = ar; _}, Both {l = bl; r = br; _} ->
      (al == bl || subset_ne al bl) && (ar == br || subset_ne ar br)
    | _ -> false

let rec disjoint a b =
  is_empty a || is_empty b ||
  (a != b &&
   let ka = tkey a and kb = tkey b in
   if ka = kb then
     bits a land bits b = 0 &&
     disjoint (left a) (left b) && disjoint (right a) (right b)
   else if lt_msb ka kb then
     disjoint a (right b)
   else if lt_msb kb ka then
     disjoint (right a) b
   else if ka < kb then
     bits a land lookup (ka lxor mbit (kkey b)) (left b) = 0 &&
     disjoint (left a) (left b) && disjoint (right a) (right b)
   else
     bits b land lookup (kb lxor mbit (kkey a)) (left a) = 0 &&
     disjoint (left a) (left b) && disjoint (right a) (right b))

let quick_subset a b =
  not (is_empty a) && lookup (tkey a) b land bits a <> 0

(* -------------------------------------------------------------------------- *)
(* Union. *)

let rec union a b =
  if a == b then b else
  if is_empty a then b else
  if is_empty b then a else
  union_ne a b

(* [union_ne a b] is [union a b] for non-empty [a] and [b]. *)
and union_ne a b =
  let ka = tkey a and kb = tkey b in
  if lt_msb ka kb then union_below a b
  else if lt_msb kb ka then union_below b a
  else if ka = kb then
    let kk = kkey a and va = bits a and vb = bits b in
    let v = va lor vb in
    match a, b with
    | Leaf _, Leaf _ ->
      if v = vb then b
      else if v = va then a
      else Leaf {kk; v}
    | Leaf _, Left {l = bl; _} ->
      if v = vb then b
      else Left {kk; v; l = bl}
    | Leaf _, Right {r = br; _} ->
      if v = vb then b
      else Right {kk; v; r = br}
    | Leaf _, Both {l = bl; r = br; _} ->
      if v = vb then b
      else Both {kk; v; l = bl; r = br}
    | Left {l = al; _}, Leaf _ ->
      if v = va then a
      else Left {kk; v; l = al}
    | Left {l = al; _}, Left {l = bl; _} ->
      let l = if al == bl then bl else union_ne al bl in
      if v = vb && l == bl then b
      else if v = va && l == al then a
      else Left {kk; v; l}
    | Left {l = al; _}, Right {r = br; _} ->
      Both {kk; v; l = al; r = br}
    | Left {l = al; _}, Both {l = bl; r = br; _} ->
      let l = if al == bl then bl else union_ne al bl in
      if v = vb && l == bl then b
      else Both {kk; v; l; r = br}
    | Right {r = ar; _}, Leaf _ ->
      if v = va then a
      else Right {kk; v; r = ar}
    | Right {r = ar; _}, Left {l = bl; _} ->
      Both {kk; v; l = bl; r = ar}
    | Right {r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then br else union_ne ar br in
      if v = vb && r == br then b
      else if v = va && r == ar then a
      else Right {kk; v; r}
    | Right {r = ar; _}, Both {l = bl; r = br; _} ->
      let r = if ar == br then br else union_ne ar br in
      if v = vb && r == br then b
      else Both {kk; v; l = bl; r}
    | Both {l = al; r = ar; _}, Leaf _ ->
      if v = va then a
      else Both {kk; v; l = al; r = ar}
    | Both {l = al; r = ar; _}, Left {l = bl; _} ->
      let l = if al == bl then bl else union_ne al bl in
      if v = va && l == al then a
      else Both {kk; v; l; r = ar}
    | Both {l = al; r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then br else union_ne ar br in
      if v = va && r == ar then a
      else Both {kk; v; l = al; r}
    | Both {l = al; r = ar; _}, Both {l = bl; r = br; _} ->
      let l = if al == bl then bl else union_ne al bl in
      let r = if ar == br then br else union_ne ar br in
      if v = vb && l == bl && r == br then b
      else if v = va && l == al && r == ar then a
      else Both {kk; v; l; r}
  else
    let al, ar = match a with
      | Leaf _ -> empty, empty
      | Left {l; _} -> l, empty
      | Right {r; _} -> empty, r
      | Both {l; r; _} -> l, r
    in
    let bl, br = match b with
      | Leaf _ -> empty, empty
      | Left {l; _} -> l, empty
      | Right {r; _} -> empty, r
      | Both {l; r; _} -> l, r
    in
    let r =
      if ar == empty then br else if br == empty then ar
      else if ar == br then br else union_ne ar br in
    let m = mbit (kkey a) in
    if ka > kb then
      let l = union_push false al b (bits b) bl (kb lxor m) in
      if l == al && r == ar then a else node (kkey a) (bits a) l r
    else
      let l = union_push true bl a (bits a) al (ka lxor m) in
      if l == bl && r == br then b else node (kkey b) (bits b) l r

(* [union_below a b], where the keys of [a] are below the msb of the key of
   [b]: [a] goes to the right of [b]. *)
and union_below a b =
  match b with
  | Leaf {kk; v} -> Right {kk; v; r = a}
  | Left {kk; v; l} -> Both {kk; v; l; r = a}
  | Right {kk; v; r = br} ->
    let r = union a br in
    if r == br then b else Right {kk; v; r}
  | Both {kk; v; l; r = br} ->
    let r = union a br in
    if r == br then b else Both {kk; v; l; r}

and union_o xb x y =
  if xb then union y x else union x y

and union_push xb x h v y k =
  if is_empty x then push k v y else
  let kx = tkey x in
  let xl, xr = match x with
    | Leaf _ -> empty, empty
    | Left {l; _} -> l, empty
    | Right {r; _} -> empty, r
    | Both {l; r; _} -> l, r
  in
  if kx = k then
    let m = mbit (kkey x) in
    let l, r =
      if not (is_empty y) && tkey y land m <> 0 then
        let yl, yr = match y with
          | Leaf _ -> empty, empty
          | Left {l; _} -> l, empty
          | Right {r; _} -> empty, r
          | Both {l; r; _} -> l, r
        in
        union_push xb xl y (bits y) yl (tkey y lxor m), union_o xb xr yr
      else xl, union_o xb xr y
    in
    let v' = bits x lor v in
    if v' = bits x && l == xl && r == xr then x
    else node (kkey x) v' l r
  else if lt_msb kx k then
    let kk = enc k in
    let m = mbit kk in
    if not (is_empty y) && tkey y land m <> 0 then
      let yl, yr = match y with
        | Leaf _ -> empty, empty
        | Left {l; _} -> l, empty
        | Right {r; _} -> empty, r
        | Both {l; r; _} -> l, r
      in
      node kk v (push (tkey y lxor m) (bits y) yl) (union_o xb x yr)
    else node kk v empty (union_o xb x y)
  else if lt_msb k kx then
    let r = union_push xb xr h v y k in
    if r == xr then x else with_r x r
  else
    let m = mbit (kkey x) in
    if not (is_empty y) && tkey y land m <> 0 then
      let yl, yr = match y with
        | Leaf _ -> empty, empty
        | Left {l; _} -> l, empty
        | Right {r; _} -> empty, r
        | Both {l; r; _} -> l, r
      in
      let r = union_o xb xr yr in
      if kx > k then
        let l =
          if xb then vunion h yl true xl xl true (m - 1)
          else vunion xl xl true h yl true (m - 1)
        in
        if l == xl && r == xr then x else node (kkey x) (bits x) l r
      else
        let l =
          if xb then vunion y yl true x xl true (m - 1)
          else vunion x xl true y yl true (m - 1)
        in
        node (pack (index (kkey x)) k) v l r
    else
      let r = union_o xb xr y in
      if kx > k then
        let l = add_bits (k lxor m) v xl in
        if l == xl && r == xr then x else node (kkey x) (bits x) l r
      else
        node (pack (index (kkey x)) k) v (push (kx lxor m) (bits x) xl) r

and vunion ha sa pa hb sb pb mk =
  if ha == sa then
    if hb == sb then union (vtail sa pa) (vtail sb pb)
    else if left hb == sb then
      if pb then union_push false (vtail sa pa) hb (bits hb) (left hb) (tkey hb land mk)
      else add_bits (tkey hb land mk) (bits hb) (vtail sa pa)
    else vunion_gen ha sa pa hb sb pb mk
  else if hb == sb && left ha == sa then
    if pa then union_push true (vtail sb pb) ha (bits ha) (left ha) (tkey ha land mk)
    else add_bits (tkey ha land mk) (bits ha) (vtail sb pb)
  else vunion_gen ha sa pa hb sb pb mk

and vunion_gen ha sa pa hb sb pb mk =
  let ka = vmax ha sa pa mk and kb = vmax hb sb pb mk in
  if ka < 0 then materialize hb sb pb mk
  else if kb < 0 then materialize ha sa pa mk
  else if lt_msb ka kb then vunion_below ha sa pa hb sb pb mk kb false
  else if lt_msb kb ka then vunion_below hb sb pb ha sa pa mk ka true
  else vunion_same ha sa pa hb sb pb ka kb

and vunion_below hx sx px hy sy py mk ky sw =
  if hy == sy then
    let yr = right hy in
    let r =
      if sw then vunion yr yr true hx sx px mk
      else vunion hx sx px yr yr true mk
    in
    if r == yr then hy else with_r hy r
  else
    let m = msb ky in
    let mk = m - 1 in
    let h = left hy in
    let c = first_low m h sy in
    let l, r =
      if c != sy then
        materialize h c false mk,
        (if sw then vunion c sy py hx sx px mk
         else vunion hx sx px c sy py mk)
      else if py && not (is_empty sy) && tkey sy land m <> 0 then
        materialize h (left sy) true mk,
        (let t = right sy in
         if sw then vunion t t true hx sx px mk
         else vunion hx sx px t t true mk)
      else
        materialize h c false mk,
        (let t = vtail sy py in
         if sw then vunion t t true hx sx px mk
         else vunion hx sx px t t true mk)
    in
    node (pack (enc_index m) ky) (bits hy) l r

and vunion_same ha sa pa hb sb pb ka kb =
  let k = if ka > kb then ka else kb in
  let m = msb k in
  let kk = pack (enc_index m) k in
  let uha, usa, upa, lha, lsa, lpa =
    if ka = k && ha == sa then
      let l = left sa and r = right sa in l, l, true, r, r, true
    else
      let h = if ka = k then left ha else ha in
      let c = first_low m h sa in
      if c != sa then h, c, false, c, sa, pa
      else if pa && not (is_empty sa) && tkey sa land m <> 0 then
        let r = right sa in h, left sa, true, r, r, true
      else h, sa, false, sa, sa, pa
  in
  let uhb, usb, upb, lhb, lsb, lpb =
    if kb = k && hb == sb then
      let l = left sb and r = right sb in l, l, true, r, r, true
    else
      let h = if kb = k then left hb else hb in
      let c = first_low m h sb in
      if c != sb then h, c, false, c, sb, pb
      else if pb && not (is_empty sb) && tkey sb land m <> 0 then
        let r = right sb in h, left sb, true, r, r, true
      else h, sb, false, sb, sb, pb
  in
  let v =
    if ka = kb then bits ha lor bits hb
    else if ka = k then bits ha
    else bits hb
  in
  let l = vunion uha usa upa uhb usb upb (m - 1) in
  let r = vunion lha lsa lpa lhb lsb lpb (m - 1) in
  if kb = k && hb == sb && v = bits sb && l == left sb && r == right sb then sb
  else if ka = k && ha == sa && v = bits sa && l == left sa && r == right sa then sa
  else node kk v l r

(* -------------------------------------------------------------------------- *)
(* Sharing. *)

(* As in PatSet, but the index of the field of a child depends on the
   constructor: the left child is always the field [2], the right child is
   the field [2] of a [Right] node and [3] of a [Both] node. Two equal
   children may be at different indices, hence [Bit_lib.share_fields]. A
   field is only unified with an equal, hence non-empty, child: the field
   exists. *)
module Sharing = struct
  let field_l = 2
  let[@inline] field_r = function Both _ -> 3 | _ -> 2

  let[@inline] share (a : t) i (b : t) j : t =
    Obj.obj (Bit_lib.share_fields (Obj.repr a) i (Obj.repr b) j)

  let rec equal a b =
    a == b ||
    (kkey a = kkey b && bits a = bits b &&
     (let al = left a and bl = left b in
      al == bl || (equal al bl && (ignore (share a field_l b field_l); true))) &&
     (let ar = right a and br = right b in
      ar == br ||
      (equal ar br && (ignore (share a (field_r a) b (field_r b)); true))))

  let rec compare a b =
    if a == b then 0 else
    if is_empty a then (if is_empty b then 0 else -1) else
    if is_empty b then 1 else
    let c = Int.compare (tkey a) (tkey b) in
    if c <> 0 then c else
    let c = Int.compare (bits a) (bits b) in
    if c <> 0 then c else
    let al = left a and bl = left b in
    let c = if al == bl then 0 else compare al bl in
    if c <> 0 then c else begin
      if al != bl then ignore (share a field_l b field_l);
      let ar = right a and br = right b in
      let c = if ar == br then 0 else compare ar br in
      if c = 0 && ar != br then ignore (share a (field_r a) b (field_r b));
      c
    end

  let rec compare_deep a b =
    if a == b then 0 else
    if is_empty a then (if is_empty b then 0 else -1) else
    if is_empty b then 1 else
    let al = left a and bl = left b in
    let cl = if al == bl then 0 else compare_deep al bl in
    if cl = 0 && al != bl then ignore (share a field_l b field_l);
    let ar = right a and br = right b in
    let cr = if ar == br then 0 else compare_deep ar br in
    if cr = 0 && ar != br then ignore (share a (field_r a) b (field_r b));
    if cl <> 0 then cl else
    if cr <> 0 then cr else
    let c = Int.compare (tkey a) (tkey b) in
    if c <> 0 then c else
      Int.compare (bits a) (bits b)

  (* Union: see PatSet.Sharing. The children are read again after a
     unification, which may have replaced them. *)

  let[@inline] same_root x y =
    bits x <> 0 && kkey x = kkey y && bits x = bits y &&
    left x == left y && right x == right y

  (* [share_if a i x b j y u]: [x] and [y] are the fields [i] of [a] and [j]
     of [b], and [u] is their union. *)
  let[@inline] share_if a i x b j y u =
    if x != y && (u == x || u == y) && same_root x y then share a i b j else u

  let rec union a b =
    if a == b then b else
    if is_empty a then b else
    if is_empty b then a else
    let ka = tkey a and kb = tkey b in
    if ka = kb then
      let al = left a and bl = left b in
      let l = share_if a field_l al b field_l bl (union al bl) in
      let ar = right a and br = right b in
      let r = share_if a (field_r a) ar b (field_r b) br (union ar br) in
      let v = bits a lor bits b in
      if v = bits b && l == left b && r == right b then b
      else if v = bits a && l == left a && r == right a then a
      else node (kkey a) v l r
    else if lt_msb ka kb then
      let br = right b in
      let r = union a br in
      if r == br then b else with_r b r
    else if lt_msb kb ka then
      let ar = right a in
      let r = union ar b in
      if r == ar then a else with_r a r
    else
      let m = mbit (kkey a) in
      let ar = right a and br = right b in
      let r = share_if a (field_r a) ar b (field_r b) br (union ar br) in
      if ka > kb then
        let al = left a in
        let l = union_push false al b (bits b) (left b) (kb lxor m) in
        if l == al && r == right a then a else node (kkey a) (bits a) l r
      else
        let bl = left b in
        let l = union_push true bl a (bits a) (left a) (ka lxor m) in
        if l == bl && r == right b then b else node (kkey b) (bits b) l r

  and union_o xb x y =
    if xb then union y x else union x y

  and union_push xb x h v y k =
    if is_empty x then push k v y else
    let kx = tkey x in
    if kx = k then
      let m = mbit (kkey x) in
      let l, r =
        if not (is_empty y) && tkey y land m <> 0 then
          let l = union_push xb (left x) y (bits y) (left y) (tkey y lxor m) in
          let xr = right x and yr = right y in
          l, share_if x (field_r x) xr y (field_r y) yr (union_o xb xr yr)
        else left x, union_o xb (right x) y
      in
      let v' = bits x lor v in
      if v' = bits x && l == left x && r == right x then x
      else node (kkey x) v' l r
    else if lt_msb kx k then
      let kk = enc k in
      let m = mbit kk in
      if not (is_empty y) && tkey y land m <> 0 then
        node kk v (push (tkey y lxor m) (bits y) (left y))
          (union_o xb x (right y))
      else node kk v empty (union_o xb x y)
    else if lt_msb k kx then
      let xr = right x in
      let r = union_push xb xr h v y k in
      if r == xr then x else with_r x r
    else
      let m = mbit (kkey x) in
      if not (is_empty y) && tkey y land m <> 0 then
        let xr = right x and yr = right y in
        let r = share_if x (field_r x) xr y (field_r y) yr (union_o xb xr yr) in
        let xl = left x and yl = left y in
        if kx > k then
          let l =
            if xb then vunion h yl true xl xl true (m - 1)
            else vunion xl xl true h yl true (m - 1)
          in
          if l == xl && r == right x then x else node (kkey x) (bits x) l r
        else
          let l =
            if xb then vunion y yl true x xl true (m - 1)
            else vunion x xl true y yl true (m - 1)
          in
          node (pack (index (kkey x)) k) v l r
      else
        let xl = left x and xr = right x in
        let r = union_o xb xr y in
        if kx > k then
          let l = add_bits (k lxor m) v xl in
          if l == xl && r == xr then x else node (kkey x) (bits x) l r
        else
          node (pack (index (kkey x)) k) v (push (kx lxor m) (bits x) xl) r

  and vunion ha sa pa hb sb pb mk =
    if ha == sa then
      if hb == sb then union (vtail sa pa) (vtail sb pb)
      else if left hb == sb then
        if pb then union_push false (vtail sa pa) hb (bits hb) (left hb) (tkey hb land mk)
        else add_bits (tkey hb land mk) (bits hb) (vtail sa pa)
      else vunion_gen ha sa pa hb sb pb mk
    else if hb == sb && left ha == sa then
      if pa then union_push true (vtail sb pb) ha (bits ha) (left ha) (tkey ha land mk)
      else add_bits (tkey ha land mk) (bits ha) (vtail sb pb)
    else vunion_gen ha sa pa hb sb pb mk

  and vunion_gen ha sa pa hb sb pb mk =
    let ka = vmax ha sa pa mk and kb = vmax hb sb pb mk in
    if ka < 0 then materialize hb sb pb mk
    else if kb < 0 then materialize ha sa pa mk
    else if lt_msb ka kb then vunion_below ha sa pa hb sb pb mk kb false
    else if lt_msb kb ka then vunion_below hb sb pb ha sa pa mk ka true
    else vunion_same ha sa pa hb sb pb ka kb

  and vunion_below hx sx px hy sy py mk ky sw =
    if hy == sy then
      let yr = right hy in
      let r =
        if sw then vunion yr yr true hx sx px mk
        else vunion hx sx px yr yr true mk
      in
      if r == yr then hy else with_r hy r
    else
      let m = msb ky in
      let mk = m - 1 in
      let h = left hy in
      let c = first_low m h sy in
      let l, r =
        if c != sy then
          materialize h c false mk,
          (if sw then vunion c sy py hx sx px mk
           else vunion hx sx px c sy py mk)
        else if py && not (is_empty sy) && tkey sy land m <> 0 then
          materialize h (left sy) true mk,
          (let t = right sy in
           if sw then vunion t t true hx sx px mk
           else vunion hx sx px t t true mk)
        else
          materialize h c false mk,
          (let t = vtail sy py in
           if sw then vunion t t true hx sx px mk
           else vunion hx sx px t t true mk)
      in
      node (pack (enc_index m) ky) (bits hy) l r

  and vunion_same ha sa pa hb sb pb ka kb =
    let k = if ka > kb then ka else kb in
    let m = msb k in
    let kk = pack (enc_index m) k in
    let uha, usa, upa, lha, lsa, lpa =
      if ka = k && ha == sa then
        let l = left sa and r = right sa in l, l, true, r, r, true
      else
        let h = if ka = k then left ha else ha in
        let c = first_low m h sa in
        if c != sa then h, c, false, c, sa, pa
        else if pa && not (is_empty sa) && tkey sa land m <> 0 then
          let r = right sa in h, left sa, true, r, r, true
        else h, sa, false, sa, sa, pa
    in
    let uhb, usb, upb, lhb, lsb, lpb =
      if kb = k && hb == sb then
        let l = left sb and r = right sb in l, l, true, r, r, true
      else
        let h = if kb = k then left hb else hb in
        let c = first_low m h sb in
        if c != sb then h, c, false, c, sb, pb
        else if pb && not (is_empty sb) && tkey sb land m <> 0 then
          let r = right sb in h, left sb, true, r, r, true
        else h, sb, false, sb, sb, pb
    in
    let v =
      if ka = kb then bits ha lor bits hb
      else if ka = k then bits ha
      else bits hb
    in
    let l = vunion uha usa upa uhb usb upb (m - 1) in
    let r = vunion lha lsa lpa lhb lsb lpb (m - 1) in
    if kb = k && hb == sb && v = bits sb && l == left sb && r == right sb then sb
    else if ka = k && ha == sa && v = bits sa && l == left sa && r == right sa then sa
    else node kk v l r
end

(* -------------------------------------------------------------------------- *)
(* Intersection. *)

let rec inter a b =
  if a == b then a else
  if is_empty a || is_empty b then empty else
  inter_ne a b

(* [inter_ne a b] is [inter a b] for non-empty [a] and [b]. *)
and inter_ne a b =
  let ka = tkey a and kb = tkey b in
  if lt_msb ka kb then
    match b with
    | Right {r; _} | Both {r; _} -> if a == r then a else inter_ne a r
    | Leaf _ | Left _ -> empty
  else if lt_msb kb ka then
    match a with
    | Right {r; _} | Both {r; _} -> if r == b then b else inter_ne r b
    | Leaf _ | Left _ -> empty
  else if ka = kb then
    let kk = kkey a and va = bits a and vb = bits b in
    let v = va land vb in
    match a, b with
    | Leaf _, Leaf _ ->
      if v = va then a
      else if v = vb then b
      else if v = 0 then empty
      else Leaf {kk; v}
    | Leaf _, Left _
    | Leaf _, Right _
    | Leaf _, Both _ ->
      if v = va then a
      else if v = 0 then empty
      else Leaf {kk; v}
    | Left _, Leaf _
    | Right _, Leaf _
    | Both _, Leaf _ ->
      if v = vb then b
      else if v = 0 then empty
      else Leaf {kk; v}
    | Left {l = al; _}, Left {l = bl; _} ->
      let l = if al == bl then al else inter_ne al bl in
      if v = va && l == al then a
      else if v = vb && l == bl then b
      else if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Left _, Right _
    | Right _, Left _ ->
      if v = 0 then empty
      else Leaf {kk; v}
    | Left {l = al; _}, Both {l = bl; _} ->
      let l = if al == bl then al else inter_ne al bl in
      if v = va && l == al then a
      else if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Right {r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then ar else inter_ne ar br in
      if v = va && r == ar then a
      else if v = vb && r == br then b
      else if v = 0 then r
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Right {r = ar; _}, Both {r = br; _} ->
      let r = if ar == br then ar else inter_ne ar br in
      if v = va && r == ar then a
      else if v = 0 then r
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Both {l = al; _}, Left {l = bl; _} ->
      let l = if al == bl then al else inter_ne al bl in
      if v = vb && l == bl then b
      else if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Both {r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then ar else inter_ne ar br in
      if v = vb && r == br then b
      else if v = 0 then r
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Both {l = al; r = ar; _}, Both {l = bl; r = br; _} ->
      let l = if al == bl then al else inter_ne al bl in
      let r = if ar == br then ar else inter_ne ar br in
      if v = va && l == al && r == ar then a
      else if v = vb && l == bl && r == br then b
      else if v = 0 then join kk l r
      else node kk v l r
  else if ka < kb then inter_lt a b
  else inter_lt b a

(* [inter_lt a b] is [inter a b] for non-empty [a] and [b] whose keys have
   the same msb, the key of [a] being lower. *)
and inter_lt a b =
  let kk = kkey a and va = bits a in
  let k = tkey a lxor mbit (kkey b) in
  match a, b with
    | _, Leaf _
    | Leaf _, Right _
    | Left _, Right _ ->
      empty
    | Leaf _, Left {l = bl; _}
    | Leaf _, Both {l = bl; _} ->
      let v = va land lookup k bl in
      if v = va then a
      else if v = 0 then empty
      else Leaf {kk; v}
    | Left {l = al; _}, Left {l = bl; _}
    | Left {l = al; _}, Both {l = bl; _} ->
      let v = va land lookup k bl in
      let l = if al == bl then al else inter_ne al bl in
      if v = va && l == al then a
      else if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Right _, Left {l = bl; _} ->
      let v = va land lookup k bl in
      if v = 0 then empty
      else Leaf {kk; v}
    | Right {r = ar; _}, Right {r = br; _}
    | Both {r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then ar else inter_ne ar br in
      r
    | Right {r = ar; _}, Both {l = bl; r = br; _} ->
      let v = va land lookup k bl in
      let r = if ar == br then ar else inter_ne ar br in
      if v = va && r == ar then a
      else if v = 0 then r
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Both {l = al; _}, Left {l = bl; _} ->
      let v = va land lookup k bl in
      let l = if al == bl then al else inter_ne al bl in
      if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Both {l = al; r = ar; _}, Both {l = bl; r = br; _} ->
      let v = va land lookup k bl in
      let l = if al == bl then al else inter_ne al bl in
      let r = if ar == br then ar else inter_ne ar br in
      if v = va && l == al && r == ar then a
      else if v = 0 then join kk l r
      else node kk v l r

(* -------------------------------------------------------------------------- *)
(* Difference. *)

let rec diff a b =
  if a == b || is_empty a then empty else
  if is_empty b then a else
  diff_ne a b

(* [diff_ne a b] is [diff a b] for non-empty [a] and [b]. *)
and diff_ne a b =
  let ka = tkey a and kb = tkey b in
  if lt_msb ka kb then
    match b with
    | Right {r; _} | Both {r; _} -> if a == r then empty else diff_ne a r
    | Leaf _ | Left _ -> a
  else if lt_msb kb ka then
    match a with
    | Leaf _ | Left _ -> a
    | Right {kk; v; r = ar} ->
      let r = if ar == b then empty else diff_ne ar b in
      if r == ar then a
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Both {kk; v; l; r = ar} ->
      let r = if ar == b then empty else diff_ne ar b in
      if r == ar then a
      else if r == empty then Left {kk; v; l}
      else Both {kk; v; l; r}
  else if ka = kb then
    let kk = kkey a and va = bits a and vb = bits b in
    let v = va land lnot vb in
    match a, b with
    | Leaf _, _ ->
      if v = va then a
      else if v = 0 then empty
      else Leaf {kk; v}
    | Left {l = al; _}, Leaf _
    | Left {l = al; _}, Right _ ->
      if v = va then a
      else if v = 0 then join kk al empty
      else Left {kk; v; l = al}
    | Left {l = al; _}, Left {l = bl; _}
    | Left {l = al; _}, Both {l = bl; _} ->
      let l = if al == bl then empty else diff_ne al bl in
      if v = va && l == al then a
      else if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Right {r = ar; _}, Leaf _
    | Right {r = ar; _}, Left _ ->
      if v = va then a
      else if v = 0 then ar
      else Right {kk; v; r = ar}
    | Right {r = ar; _}, Right {r = br; _}
    | Right {r = ar; _}, Both {r = br; _} ->
      let r = if ar == br then empty else diff_ne ar br in
      if v = va && r == ar then a
      else if v = 0 then r
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Both {l = al; r = ar; _}, Leaf _ ->
      if v = va then a
      else if v = 0 then join kk al ar
      else Both {kk; v; l = al; r = ar}
    | Both {l = al; r = ar; _}, Left {l = bl; _} ->
      let l = if al == bl then empty else diff_ne al bl in
      if v = va && l == al then a
      else if v = 0 then join kk l ar
      else if l == empty then Right {kk; v; r = ar}
      else Both {kk; v; l; r = ar}
    | Both {l = al; r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then empty else diff_ne ar br in
      if v = va && r == ar then a
      else if v = 0 then join kk al r
      else if r == empty then Left {kk; v; l = al}
      else Both {kk; v; l = al; r}
    | Both {l = al; r = ar; _}, Both {l = bl; r = br; _} ->
      let l = if al == bl then empty else diff_ne al bl in
      let r = if ar == br then empty else diff_ne ar br in
      if v = va && l == al && r == ar then a
      else if v = 0 then join kk l r
      else node kk v l r
  else if ka < kb then diff_lt a b
  else
    let al, ar = match a with
      | Leaf _ -> empty, empty
      | Left {l; _} -> l, empty
      | Right {r; _} -> empty, r
      | Both {l; r; _} -> l, r
    in
    let bl, br = match b with
      | Leaf _ -> empty, empty
      | Left {l; _} -> l, empty
      | Right {r; _} -> empty, r
      | Both {l; r; _} -> l, r
    in
    let r =
      if ar == empty || br == empty then ar
      else if ar == br then empty else diff_ne ar br in
    let l = diff_push al b (bits b) bl (kb lxor mbit (kkey a)) in
    if l == al && r == ar then a else node (kkey a) (bits a) l r

(* [diff_lt a b] is [diff a b] for non-empty [a] and [b] whose keys have the
   same msb, the key of [a] being lower. *)
and diff_lt a b =
  let kk = kkey a and va = bits a in
  let k = tkey a lxor mbit (kkey b) in
  match a, b with
    | _, Leaf _
    | Leaf _, Right _
    | Left _, Right _ ->
      a
    | Leaf _, Left {l = bl; _}
    | Leaf _, Both {l = bl; _} ->
      let v = va land lnot (lookup k bl) in
      if v = va then a
      else if v = 0 then empty
      else Leaf {kk; v}
    | Left {l = al; _}, Left {l = bl; _}
    | Left {l = al; _}, Both {l = bl; _} ->
      let v = va land lnot (lookup k bl) in
      let l = if al == bl then empty else diff_ne al bl in
      if v = va && l == al then a
      else if v = 0 then join kk l empty
      else if l == empty then Leaf {kk; v}
      else Left {kk; v; l}
    | Right {r = ar; _}, Left {l = bl; _} ->
      let v = va land lnot (lookup k bl) in
      if v = va then a
      else if v = 0 then ar
      else Right {kk; v; r = ar}
    | Right {r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then empty else diff_ne ar br in
      if r == ar then a
      else if r == empty then Leaf {kk; v = va}
      else Right {kk; v = va; r}
    | Right {r = ar; _}, Both {l = bl; r = br; _} ->
      let v = va land lnot (lookup k bl) in
      let r = if ar == br then empty else diff_ne ar br in
      if v = va && r == ar then a
      else if v = 0 then r
      else if r == empty then Leaf {kk; v}
      else Right {kk; v; r}
    | Both {l = al; r = ar; _}, Left {l = bl; _} ->
      let v = va land lnot (lookup k bl) in
      let l = if al == bl then empty else diff_ne al bl in
      if v = va && l == al then a
      else if v = 0 then join kk l ar
      else if l == empty then Right {kk; v; r = ar}
      else Both {kk; v; l; r = ar}
    | Both {l = al; r = ar; _}, Right {r = br; _} ->
      let r = if ar == br then empty else diff_ne ar br in
      if r == ar then a
      else if r == empty then Left {kk; v = va; l = al}
      else Both {kk; v = va; l = al; r}
    | Both {l = al; r = ar; _}, Both {l = bl; r = br; _} ->
      let v = va land lnot (lookup k bl) in
      let l = if al == bl then empty else diff_ne al bl in
      let r = if ar == br then empty else diff_ne ar br in
      if v = va && l == al && r == ar then a
      else if v = 0 then join kk l r
      else node kk v l r

and diff_push x h hv y k =
  if is_empty x then x else
  let kx = tkey x in
  if kx = k then
    let m = mbit (kkey x) in
    let xl, xr = match x with
      | Leaf _ -> empty, empty
      | Left {l; _} -> l, empty
      | Right {r; _} -> empty, r
      | Both {l; r; _} -> l, r
    in
    let l, r =
      if not (is_empty y) && tkey y land m <> 0 then
        let yl, yr = match y with
          | Leaf _ -> empty, empty
          | Left {l; _} -> l, empty
          | Right {r; _} -> empty, r
          | Both {l; r; _} -> l, r
        in
        diff_push xl y (bits y) yl (tkey y lxor m), diff xr yr
      else xl, diff xr y
    in
    rebuild x xl xr (bits x land lnot hv) l r
  else if lt_msb kx k then
    let m = msb k in
    if not (is_empty y) && tkey y land m <> 0 then diff x (right y)
    else diff x y
  else if lt_msb k kx then
    let xr = right x in
    let r = diff_push xr h hv y k in
    if r == xr then x else with_r x r
  else if kx < k then
    diff x y
  else
    let m = mbit (kkey x) in
    let xl, xr = match x with
      | Leaf _ -> empty, empty
      | Left {l; _} -> l, empty
      | Right {r; _} -> empty, r
      | Both {l; r; _} -> l, r
    in
    let l, r =
      if not (is_empty y) && tkey y land m <> 0 then
        let yl, yr = match y with
          | Leaf _ -> empty, empty
          | Left {l; _} -> l, empty
          | Right {r; _} -> empty, r
          | Both {l; r; _} -> l, r
        in
        vdiff xl h yl true (m - 1), diff xr yr
      else remove_bits (k lxor m) hv xl, diff xr y
    in
    if l == xl && r == xr then x else node (kkey x) (bits x) l r

and vdiff x hb sb pb mk =
  if hb == sb then diff x (vtail sb pb) else
  if left hb == sb then
    if pb then diff_push x hb (bits hb) (left hb) (tkey hb land mk)
    else remove_bits (tkey hb land mk) (bits hb) x
  else if is_empty x then x else
  let kb = tkey hb land mk and kx = tkey x in
  let xl, xr = match x with
    | Leaf _ -> empty, empty
    | Left {l; _} -> l, empty
    | Right {r; _} -> empty, r
    | Both {l; r; _} -> l, r
  in

  if kx = kb then
    let m = mbit (kkey x) in
    let h = left hb in
    let c = first_low m h sb in
    let l, r =
      if c != sb then
        vdiff xl h c false (m - 1), vdiff xr c sb pb (m - 1)
      else if pb && not (is_empty sb) && tkey sb land m <> 0 then
        vdiff xl h (left sb) true (m - 1), diff xr (right sb)
      else
        vdiff xl h sb false (m - 1), diff xr (vtail sb pb)
    in
    rebuild x xl xr (bits x land lnot (bits hb)) l r
  else if lt_msb kx kb then
    let m = msb kb in
    let c = first_low m hb sb in
    if c != sb then vdiff x c sb pb (m - 1)
    else if pb && not (is_empty sb) && tkey sb land m <> 0 then diff x (right sb)
    else diff x (vtail sb pb)
  else if lt_msb kb kx then
    let r = vdiff xr hb sb pb mk in
    if r == xr then x else with_r x r
  else if kx < kb then
    vdiff x (left hb) sb pb mk
  else
    let m = mbit (kkey x) in
    let c = first_low m hb sb in
    let l, r =
      if c != sb then
        vdiff xl hb c false (m - 1), vdiff xr c sb pb (m - 1)
      else if pb && not (is_empty sb) && tkey sb land m <> 0 then
        vdiff xl hb (left sb) true (m - 1), diff xr (right sb)
      else
        vdiff xl hb sb false (m - 1), diff xr (vtail sb pb)
    in
    if l == xl && r == xr then x else node (kkey x) (bits x) l r

(* -------------------------------------------------------------------------- *)
(* Iteration. The functions that only read the tree match on the four
   constructors; [empty] is a leaf with no bits. *)

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
  | Leaf {kk; v} -> iter_bits f (pre lor key kk) v
  | Left {kk; v; l} ->
    iter f (pre lor mbit kk) l;
    iter_bits f (pre lor key kk) v
  | Right {kk; v; r} ->
    iter f pre r;
    iter_bits f (pre lor key kk) v
  | Both {kk; v; l; r} ->
    iter f pre r;
    iter f (pre lor mbit kk) l;
    iter_bits f (pre lor key kk) v

let iter f t = iter f 0 t

let rec rev_iter f pre = function
  | Leaf {kk; v} -> rev_iter_bits f (pre lor key kk) v
  | Left {kk; v; l} ->
    rev_iter_bits f (pre lor key kk) v;
    rev_iter f (pre lor mbit kk) l
  | Right {kk; v; r} ->
    rev_iter_bits f (pre lor key kk) v;
    rev_iter f pre r
  | Both {kk; v; l; r} ->
    rev_iter_bits f (pre lor key kk) v;
    rev_iter f (pre lor mbit kk) l;
    rev_iter f pre r

let rev_iter f t = rev_iter f 0 t

let[@inline] fold_bits f k v acc =
  let b = base k in
  let v = ref v and acc = ref acc in
  while !v <> 0 do
    let i = Bit_lib.lsb_index !v in
    v := !v land (!v - 1);
    acc := f (b + i) !acc
  done;
  !acc

let rec fold f pre t acc =
  match t with
  | Leaf {kk; v} -> fold_bits f (pre lor key kk) v acc
  | Left {kk; v; l} ->
    fold_bits f (pre lor key kk) v (fold f (pre lor mbit kk) l acc)
  | Right {kk; v; r} ->
    fold_bits f (pre lor key kk) v (fold f pre r acc)
  | Both {kk; v; l; r} ->
    let acc = fold f pre r acc in
    fold_bits f (pre lor key kk) v (fold f (pre lor mbit kk) l acc)

let fold f t acc = fold f 0 t acc

let[@inline] fold_right_bits f k v acc =
  let b = base k in
  let v = ref v and acc = ref acc in
  while !v <> 0 do
    let i = Bit_lib.msb_index !v in
    v := !v lxor (1 lsl i);
    acc := f (b + i) !acc
  done;
  !acc

let rec fold_right f pre t acc =
  match t with
  | Leaf {kk; v} -> fold_right_bits f (pre lor key kk) v acc
  | Left {kk; v; l} ->
    fold_right f (pre lor mbit kk) l (fold_right_bits f (pre lor key kk) v acc)
  | Right {kk; v; r} ->
    fold_right f pre r (fold_right_bits f (pre lor key kk) v acc)
  | Both {kk; v; l; r} ->
    let acc = fold_right_bits f (pre lor key kk) v acc in
    fold_right f pre r (fold_right f (pre lor mbit kk) l acc)

let elements t = fold_right (fun x xs -> x :: xs) 0 t []

let find_bits p k v =
  let b = base k in
  let rec loop v =
    if v = 0 then -1 else
    let x = b + Bit_lib.lsb_index v in
    if p x then x else loop (v land (v - 1))
  in
  loop v

let rec find_first p pre = function
  | Leaf {kk; v} -> find_bits p (pre lor key kk) v
  | Left {kk; v; l} ->
    let x = find_first p (pre lor mbit kk) l in
    if x >= 0 then x else find_bits p (pre lor key kk) v
  | Right {kk; v; r} ->
    let x = find_first p pre r in
    if x >= 0 then x else find_bits p (pre lor key kk) v
  | Both {kk; v; l; r} ->
    let x = find_first p pre r in
    if x >= 0 then x else
    let x = find_first p (pre lor mbit kk) l in
    if x >= 0 then x else find_bits p (pre lor key kk) v

let find_first_opt p t =
  let x = find_first p 0 t in
  if x < 0 then None else Some x

let rec for_all p pre = function
  | Leaf {kk; v} -> find_bits (fun x -> not (p x)) (pre lor key kk) v < 0
  | Left {kk; v; l} ->
    for_all p (pre lor mbit kk) l &&
    find_bits (fun x -> not (p x)) (pre lor key kk) v < 0
  | Right {kk; v; r} ->
    for_all p pre r &&
    find_bits (fun x -> not (p x)) (pre lor key kk) v < 0
  | Both {kk; v; l; r} ->
    for_all p pre r &&
    for_all p (pre lor mbit kk) l &&
    find_bits (fun x -> not (p x)) (pre lor key kk) v < 0

let for_all p t = for_all p 0 t

let exists p t = find_first p 0 t >= 0

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
  if is_empty t then t else
  let tl = left t and tr = right t in
  let r = filter p pre tr in
  let l = filter p (pre lor mbit (kkey t)) tl in
  rebuild t tl tr (filter_bits p (pre lor tkey t) (bits t)) l r

let filter p t = filter p 0 t

let partition p t =
  let yes = filter p t in
  yes, diff t yes

(* -------------------------------------------------------------------------- *)
(* Sequences and lists. *)

type enum =
  | End
  | Tree of int * t * enum      (* a prefix and a non-empty tree *)
  | Bits of int * int * enum    (* an absolute key and a non-empty bitmap *)

let[@inline] tree pre t e = if is_empty t then e else Tree (pre, t, e)

let rec seq_of_enum e () =
  match e with
  | End -> Seq.Nil
  | Tree (pre, t, e) ->
    seq_of_enum
      (tree pre (right t)
         (tree (pre lor mbit (kkey t)) (left t)
            (Bits (pre lor tkey t, bits t, e)))) ()
  | Bits (k, v, e) ->
    let i = Bit_lib.lsb_index v in
    let v = v land (v - 1) in
    Seq.Cons (base k + i, seq_of_enum (if v = 0 then e else Bits (k, v, e)))

let to_seq t = seq_of_enum (tree 0 t End)

let add_seq s t = Seq.fold_left (fun t x -> add x t) t s
let of_seq s = add_seq s empty
let of_list xs = List.fold_left (fun t x -> add x t) empty xs

(* -------------------------------------------------------------------------- *)
(* Splitting. *)

let rec above_bits k mask t =
  if is_empty t then empty else
  let kt = tkey t in
  if kt < k then empty
  else if kt = k then
    let v = bits t land mask in
    if v = 0 then empty
    else match t with
      | Leaf _ when v = bits t -> t
      | _ -> Leaf {kk = kkey t; v}
  else
    let m = mbit (kkey t) in
    if k land m <> 0 then
      let tl = left t in
      let l = above_bits (k lxor m) mask tl in
      if l == tl && is_empty (right t) then t
      else node (kkey t) (bits t) l empty
    else
      let tr = right t in
      let r = above_bits k mask tr in
      if r == tr then t else with_r t r

let rec below_bits k mask t =
  if is_empty t then t else
  let kt = tkey t in
  if kt < k then t
  else if kt = k then
    let tl = left t and tr = right t in
    rebuild t tl tr (bits t land mask) tl tr
  else
    let m = mbit (kkey t) in
    if k land m <> 0
    then join (kkey t) (below_bits (k lxor m) mask (left t)) (right t)
    else below_bits k mask (right t)

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

let rec max_sym_diff pre a b =
  if a == b then -1 else
  match is_empty a, is_empty b with
  | true, true -> -1
  | false, true -> highest (pre lor tkey a) (bits a)
  | true, false -> highest (pre lor tkey b) (bits b)
  | false, false ->
    let ka = tkey a and kb = tkey b in
    if ka <> kb then
      if ka > kb then highest (pre lor ka) (bits a)
      else highest (pre lor kb) (bits b)
    else if bits a <> bits b then
      highest (pre lor ka) (bits a lxor bits b)
    else
      let d = max_sym_diff (pre lor mbit (kkey a)) (left a) (left b) in
      if d >= 0 then d else max_sym_diff pre (right a) (right b)

let extract_shared_suffix s1 s2 =
  let x = max_sym_diff 0 s1 s2 in
  if x < 0 then s1, (empty, empty)
  else above x s1, (at_most x s1, at_most x s2)

let rec min_gt pre k t =
  if is_empty t || tkey t <= k then -1 else
  let m = mbit (kkey t) in
  let x =
    if k land m <> 0
    then min_gt (pre lor m) (k lxor m) (left t)
    else
      let x = min_gt pre k (right t) in
      let tl = left t in
      if x >= 0 || is_empty tl then x else min_in (pre lor m) tl
  in
  if x >= 0 then x else lowest (pre lor tkey t) (bits t)

let rec min_sym_diff pre a b =
  if a == b then -1 else
  if is_empty a then (if is_empty b then -1 else min_in pre b) else
  if is_empty b then min_in pre a else
  let ka = tkey a and kb = tkey b in
  if ka = kb then
    let x = min_sym_diff pre (right a) (right b) in
    if x >= 0 then x else
    let x = min_sym_diff (pre lor mbit (kkey a)) (left a) (left b) in
    if x >= 0 then x else
    let d = bits a lxor bits b in
    if d = 0 then -1 else lowest (pre lor ka) d
  else if lt_msb ka kb then min_sym_diff_low pre a b
  else if lt_msb kb ka then min_sym_diff_low pre b a
  else if ka < kb then min_sym_diff_lt pre a b
  else min_sym_diff_lt pre b a

and min_sym_diff_low pre a b =
  let x = min_sym_diff pre a (right b) in
  if x >= 0 then x else
  let bl = left b in
  if not (is_empty bl) then min_in (pre lor mbit (kkey b)) bl
  else lowest (pre lor tkey b) (bits b)

and min_sym_diff_lt pre a b =
  let x = min_sym_diff pre (right a) (right b) in
  if x >= 0 then x else
  let m = mbit (kkey a) in
  let pre' = pre lor m in
  let ka = pre lor tkey a in
  let bl = left b in
  let x = min_sym_diff pre' (left a) bl in
  if x >= 0 && key_of x < ka then x
  else if x < 0 || key_of x > ka then
    lowest ka (bits a)
  else
    let d = bits a lxor lookup (tkey a lxor m) bl in
    if d <> 0 then lowest ka d
    else
      let x = min_gt pre' (tkey a lxor m) bl in
      if x >= 0 then x else lowest (pre lor tkey b) (bits b)

let extract_shared_prefix s1 s2 =
  let x = min_sym_diff 0 s1 s2 in
  if x < 0 then s1, (empty, empty)
  else below x s1, (at_least x s1, at_least x s2)

(* -------------------------------------------------------------------------- *)
(* Invariant checking. *)

let check t =
  let fail fmt = Printf.ksprintf failwith ("PatSetCompact.check: " ^^ fmt) in
  let rec check t bound =
    let k = tkey t in
    if bits t = 0 then fail "a node with an empty bitmap (or an empty child)";
    if kkey t <> enc k then fail "bad cached msb for key %d" k;
    if bound >= 0 && k >= bound then
      fail "key %d is out of its bound %d" k bound;
    let m = mbit (kkey t) in
    match t with
    | Leaf _ -> ()
    | Left {l; _} -> check l (k - m)
    | Right {r; _} -> check r m
    | Both {l; r; _} -> check l (k - m); check r m
  in
  match t with
  | Leaf {kk = 0; v = 0} -> ()
  | _ -> check t (-1)
