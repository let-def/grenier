(* The primitives of Maxtrie's private Bit_lib that this module uses; the C
   stubs are linked with the maxtrie library. *)
module Bit_lib = struct
  external pop_count : (int [@untagged]) -> (int [@untagged]) =
    "maxtrie_pop_count_tagged" "maxtrie_pop_count" [@@noalloc]
  external msb_index : (int [@untagged]) -> (int [@untagged]) =
    "maxtrie_msb_index_tagged" "maxtrie_msb_index" [@@noalloc]
  external lsb_index : (int [@untagged]) -> (int [@untagged]) =
    "maxtrie_lsb_index_tagged" "maxtrie_lsb_index" [@@noalloc]
  external extract_msb : (int [@untagged]) -> (int [@untagged]) =
    "maxtrie_extract_msb_tagged" "maxtrie_extract_msb" [@@noalloc]
  let[@inline] extract_lsb x = x land (-x)
end
[@@@warning "-a"]
type t = {
  k: int;
  v: int;
  l: t;
  r: t;
}

type elt = int

let rec empty = {k = 0; v = 0; l = empty; r = empty}

(* Extract the most significant bit of integer `x` *)
let extract_bit = Bit_lib.extract_msb

let rec equal t1 t2 =
  t1.k = t2.k && t1.v = t2.v &&
  (t1.l == t2.l || equal t1.l t2.l) &&
  (t1.r == t2.r || equal t1.r t2.r)

let equal t1 t2 = t1 == t2 || equal t1 t2

let rec compare t1 t2 =
  if t1 == t2 then 0 else
  let c = Int.compare t1.k t2.k in
  if c <> 0 then c else
    let c = Int.compare t1.v t2.v in
    if c <> 0 then c else
      let c = compare t1.l t2.l in
      if c <> 0 then c else (
        let c = compare t1.r t2.r in
        if c <> 0 then c else
          0
      )

let rec lookup k t =
  if k >= t.k then
    if k = t.k
    then t.v
    else 0
  else
    let k' = k land lnot (extract_bit t.k) in
    lookup k' (if k = k' then t.r else t.l)

let rec insert k v t =
  if k >= t.k then
    if k = t.k then
      match v lor t.v with
      | v when v = t.v -> t
      | v -> {t with v}
    else
      let m = extract_bit k in
      if m land t.k = m then
        {k; v; l = insert (t.k land lnot m) t.v t.l; r = t.r}
      else
        {k; v; l = empty; r = t}
  else
    let m = extract_bit k in
    if m > t.k lsr 1 then
      match insert (k land lnot m) v t.l with
      | l when t.l == l -> t
      | l -> {t with l}
    else
      match insert k v t.r with
      | r when t.r == r -> t
      | r -> {t with r}

let _low_level_insert_mask = insert

let rec join k l r =
  match l.v with
  | 0 -> r
  | v ->
    let l' = join l.k l.l l.r in
    {k = (extract_bit k) lor l.k; v; l = l'; r}

let rec remove k v t =
  if k >= t.k then
    if k = t.k then
      match t.v land lnot v with
      | 0 -> join t.k t.l t.r
      | v -> if v = t.v then t else {t with v}
    else
      t
  else
    let m = extract_bit k in
    if m > t.k lsr 1 then
      match remove (k land lnot m) v t.l with
      | l when t.l == l -> t
      | l -> {t with l}
    else
      match remove k v t.r with
      | r when t.r == r -> t
      | r -> {t with r}

let rec union a b =
  let a, b = if a.k > b.k then b, a else a, b in
  (* b.k >= a.k *)
  if a.v = 0 then b else
  if a.k = b.k then
    let l = union a.l b.l in
    let r = union a.r b.r in
    match a.v lor b.v with
    | v when v = b.v && l == b.l && r == b.r -> b
    | v -> {b with v; l; r}
  else
    let m = extract_bit a.k in
    if m > b.k lsr 1 then
      match
        (* TODO: insert_and_union? *)
        union_fringe (lnot m) a a.l b.l,
        union a.r b.r
      with
      | l, r when l == b.l && r == b.r -> b
      | l, r -> {b with l; r}
    else
      match union a b.r with
      | r when r == b.r -> b
      | r -> {b with r}

and union_fringe mask a0 a b =
  if a0 == a then union a b
  else
    let k = a0.k land mask in
    if k = b.k then
      (* b.k > a.k *)
      let m = extract_bit a.k in
      let v = a0.v lor b.v in
      let l, r =
        let mask = mask land lnot m in
        if m > b.k lsr 1 then
          union_fringe mask a0.l a.l b.l,
          union a.r b.r
        else
          insert_fringe mask a0.l a b.l,
          union a b.r
      in
      if v = b.v && l == b.l && r == b.r then
        b
      else
        {k; v; l; r}
    else if k > b.k then
      union_fringe_safe mask a0 a b
      (*let m = extract_bit k in
        let v = a0.v in
        if b.k land m = m (* same bit *) then
        {k; v; l = union_fringe_safe (mask land lnot m) a0.l a b.l; r = b.r}
        else
        {k; v; l = empty; r = union_fringe mask a0.l a b}*)
    else
      let m = extract_bit b.k in
      if a.k land m = m then
        (*same bit*)
        match
          union_fringe_safe (mask land lnot m) a0 a.l b.l,
          union a.r b.r
        with
        | l, r when l == b.l && r == b.r -> b
        | l, r -> {b with l; r}
      else
        let mask = mask land lnot m in
        let rec extract_fringe a0 a l =
          if a0.k land m = m then
            extract_fringe a0.l a (insert (a0.k land mask) a0.v l)
          else
            let r = union_fringe mask a0 a b.r in
            if l == b.l && r == b.r
            then b
            else {b with l; r}
        in
        extract_fringe a0 a b.l

and union_fringe_safe mask a0 a b =
  union a (insert_fringe mask a0 a b)

and insert_fringe mask a0 a b =
  if a0 == a then
    b
  else
    insert_fringe mask a0.l a (insert (a0.k land mask) a0.v b)

let diff_update a v l r =
  if v = 0 then
    join a.k l r
  else if v = a.v && a.l == l && a.r == r then
    a
  else
    {a with v; l; r}

let rec diff a b =
  if b.v = 0 then a else
  if a.k >= b.k then
    if a.k = b.k then
      diff_update a
        (a.v land lnot b.v)
        (diff a.l b.l)
        (diff a.r b.r)
    else
      let m = extract_bit a.k in
      if b.k land m = m then
        diff_update a a.v
          (* TODO: remove_and_diff?
             skip to b.k land m -1, remove b.l *)
          (diff (remove (b.k land lnot m) b.v a.l) b.l)
          (diff a.r b.r)
      else
        diff_update a a.v a.l (diff a.r b)
  else
    let m = extract_bit a.k in
    if m > b.k lsr 1 then
      diff_update a
        (a.v land lnot (lookup (a.k land lnot m) b.l))
        (diff a.l b.l)
        (diff a.r b.r)
    else
      diff a b.r

let rec inter a b =
  if a == b then
    a
  else if a.k > b.k then
    inter_right b a
  else
    inter_right a b

and inter_right a b =
  (* a.k <= b.k *)
  if b.v = 0 then empty else
  if a.k = b.k then
    let l = inter a.l b.l in
    let r = inter a.r b.r in
    match a.v land b.v with
    | 0 -> join a.k l r
    | v -> {k = a.k; v; l; r}
  else
    (* a.k < b.k *)
    let m = extract_bit a.k in
    if m = extract_bit b.k then
      (* same msb *)
      let l = inter a.l b.l in
      let r = inter a.r b.r in
      match lookup (a.k lxor m) b.l land a.v with
      | 0 -> join a.k l r
      | v -> {k = a.k; v; l; r}
    else
      (* a.k << b.k *)
      inter a b.r

let mask_size = Sys.word_size - 1

let encode x = (x / mask_size, 1 lsl (x mod mask_size))

let decode_base x = x * mask_size

let add x t =
  let k, v = encode x in
  insert k v t

let mem x t =
  let k, v = encode x in
  lookup k t land v <> 0

let remove x t =
  let k, v = encode x in
  remove k v t

let singleton x = add x empty

let rec iter f mask t =
  if t.v <> 0 then (
    iter f mask t.r;
    iter f (mask lor extract_bit t.k) t.l;
    let base = decode_base (mask lor t.k) in
    let v = ref t.v in
    for _ = 0 to Bit_lib.pop_count t.v - 1 do
      let index = Bit_lib.lsb_index !v in
      v := !v lxor (1 lsl index);
      f (base + index);
    done
  )

let iter f t = iter f 0 t

let rec fold f mask t acc =
  if t.v <> 0 then (
    let acc = fold f mask t.r acc in
    let acc = fold f (mask lor extract_bit t.k) t.l acc in
    let base = decode_base (mask lor t.k) in
    let v = ref t.v in
    let acc = ref acc in
    for _ = 0 to Bit_lib.pop_count t.v - 1 do
      let index = Bit_lib.lsb_index !v in
      v := !v lxor (1 lsl index);
      acc := f (base + index) !acc;
    done;
    !acc
  ) else
    acc

let fold f t acc = fold f 0 t acc

let rec rev_iter f mask t =
  if t.v <> 0 then (
    let base = decode_base (mask lor t.k) in
    let v = ref t.v in
    for _ = 0 to Bit_lib.pop_count t.v - 1 do
      let index = Bit_lib.msb_index !v in
      v := !v lxor (1 lsl index);
      f (base + index);
    done;
    rev_iter f (mask lor extract_bit t.k) t.l;
    rev_iter f mask t.r;
  )

let _rev_iter f t = rev_iter f 0 t

let is_empty t = t.v = 0

let is_singleton t =
  t.v <> 0 &&
  (t.v land (t.v - 1)) lor t.l.v lor t.r.v = 0

let check _ = ()

let elements s = fold (fun x xs -> x :: xs) s []

let sorted_union xs = List.fold_left union empty xs

let rec disjoint t1 t2 =
  t1.v = 0 || t2.v = 0 || (
    t1 != t2 &&
    if t1.k = t2.k then (
      t1.v land t2.v = 0 &&
      disjoint t1.l t2.l &&
      disjoint t1.r t2.r
    ) else
      let msb1 = extract_bit t1.k in
      let msb2 = extract_bit t2.k in
      if msb1 = msb2 then (
        if t1.k > t2.k then
          lookup t2.k t1 land t2.v = 0 &&
          disjoint t1.l t2.l &&
          disjoint t1.r t2.r
        else
          lookup t1.k t2 land t1.v = 0 &&
          disjoint t1.l t2.l &&
          disjoint t1.r t2.r
      ) else if t1.k > t2.k then
        disjoint t1.r t2
      else
        disjoint t1 t2.r
  )

let cardinal t =
  let rec loop t acc =
    if t.v = 0 then
      acc
    else
      let acc = acc + Bit_lib.pop_count t.v in
      let acc = loop t.l acc in
      let acc = loop t.r acc in
      acc
  in
  loop t 0

let rec minimum mask t =
  if t.r.v <> 0 then
    minimum mask t.r
  else if t.l.v <> 0 then
    minimum (mask lor extract_bit t.k) t.l
  else
    decode_base (mask lor t.k) + Bit_lib.lsb_index t.v

let minimum t =
  if t.v = 0 then
    raise Not_found;
  minimum 0 t

let maximum t =
  if t.v = 0 then
    raise Not_found;
  decode_base t.k + Bit_lib.msb_index t.v

let choose t = minimum t (*TODO: Later make it maximum, it is faster, but testsuite assumes minimum*)

let quick_subset a b =
  lookup a.k b land a.v <> 0

let compare_minimum a b =
  match a.v, b.v with
  | 0, 0 -> 0
  | 0, _ -> -1
  | _, 0 -> 1
  | _ -> Int.compare (minimum a) (minimum b)

let above x t =
  let k, v = encode x in
  let mask = -v lsl 1 (* -v: include x, -v lsl 1: exclude x *) in
  let rec loop k t =
    if t.v = 0 || t.k < k then
      empty
    else if t.k = k then
      match t.v land mask with
      | 0 -> empty
      | v -> {k; v; l = empty; r = empty}
    else
      (* t.k > k *)
      let msb = extract_bit t.k in
      if k land msb = msb then
        (* same msb *)
        let l = loop (k lxor msb) t.l in
        {t with l; r = empty}
      else
        (* lower msb *)
        {t with r = loop k t.r}
  in
  loop k t

let rec subset sub sup =
  sub.v = 0 || (
    sup.v <> 0 &&
    if sub.k = sup.k then
      sub.v land sup.v = sub.v &&
      subset sub.l sup.l &&
      subset sub.r sup.r
    else
      sub.k < sup.k &&
      let msb = extract_bit sup.k in
      if sub.k land msb = msb then
        (* Same msb *)
        lookup (sub.k lxor msb) sup.l land sub.v = sub.v &&
        subset sub.l sup.l &&
        subset sub.r sup.r
      else
        subset sub sup.r
  )

(*let rec find_first f mask t =
  match t.v with
  | 0 -> -1
  | v ->
    let base = decode_base (mask lor t.k) in
    let i = Bit_lib.lsb_index v in
    let x = base + i in
    if f x then
      match find_first f mask t.r with
      | -1 ->
        (* Try in the left *)
        begin match find_first f (mask lor (extract_bit t.k)) t.l with
          | -1 -> x
          | x' -> x'
        end
      | x' -> x'
    else
      (* Try in other elements of this node *)
      let j = Bit_lib.msb_index v in
      if f (base + j) then
        let v = ref (v lxor ((1 lsl i) lor (1 lsl j))) in
        let j = ref j in
        while !v <> 0 do
          let j' = Bit_lib.extract_msb !v in
          if f (base + j') then
            (j := j'; v := !v lxor (1 lsl j'))
          else
            v := 0
        done;
        base + !j
      else
        -1

let find_first_opt f t =
  match find_first f 0 t with
  | -1 -> None
  | x -> Some x*)

exception Found of int

let find_first_opt f t =
  try
    iter (fun x -> if f x then raise (Found x)) t;
    None
  with Found x -> Some x

(* TODO *)

let _above_inclusive x t =
  let k, v = encode x in
  let mask = -v in
  let rec loop k t =
    if t.v = 0 || t.k < k then
      empty
    else if t.k = k then
      match t.v land mask with
      | 0 -> empty
      | v -> {k; v; l = empty; r = empty}
    else
      (* t.k > k *)
      let msb = extract_bit t.k in
      if k land msb = msb then
        (* same msb *)
        let l = loop (k lxor msb) t.l in
        {t with l; r = empty}
      else
        (* lower msb *)
        {t with r = loop k t.r}
  in
  loop k t

let below x t =
  let k, v = encode x in
  let mask = v - 1 in
  let rec loop k t =
    if t.v = 0 || t.k < k then
      t
    else if t.k = k then
      match t.v land mask with
      | 0 -> join k t.l t.r
      | v -> {k; v; l = t.l; r = t.r}
    else
      (* t.k > k *)
      let msb = extract_bit t.k in
      if k land msb = msb then
        (* same msb *)
        let l = loop (k lxor msb) t.l in
        join k l t.r
      else
        (* lower msb *)
        loop k t.r
  in
  loop k t

let rec extract_unique_prefix1 mask k t =
  if t.v = 0 || t.k < k then
    (t, empty)
  else if t.k = k then
    match t.v land mask, t.v land lnot mask with
    | 0 , v1 -> (join k t.l t.r              , {k; v=v1; l = empty; r = empty})
    | v0, 0  -> ({k; v=v0; l = t.l; r = t.r} , empty                          )
    | v0, v1 -> ({k; v=v0; l = t.l; r = t.r} , {k; v=v1; l = empty; r = empty})
  else
    (* t.k > k *)
    let msb = extract_bit t.k in
    if k land msb = msb then
      (* same msb *)
      let lb, la = extract_unique_prefix1 mask (k lxor msb) t.l in
      (join k lb t.r, {t with l=la; r = empty})
    else
      (* lower msb *)
      let rb, ra = extract_unique_prefix1 mask k t.r in
      (rb, {t with r = ra})

let extract_unique_prefix s1 s2 =
  let rec loop s1 mask s2 =
    if s2.r.v <> 0 then
      loop s1 mask s2.r
    else if s2.l.v <> 0 then
      loop s1 (mask lor extract_bit s2.k) s2.l
    else
      let k = mask lor s2.k in
      let v = Bit_lib.extract_lsb s2.v in
      extract_unique_prefix1 (v - 1) k s1
  in
  loop s1 0 s2

let rec extract_suffix_to t1 k v =
  if t1.k > k then
    let msb = extract_bit t1.k in
    if k land msb = msb then
      (* same msb *)
      let l0, l1 = extract_suffix_to t1.l (k lxor msb) v in
      {t1 with l = l0; r = empty},
      join t1.k l1 t1.r
    else
      let suffix, t1' = extract_suffix_to t1.r k v in
      {t1 with r = suffix}, t1'
  else if t1.k = k then
    let msb = extract_bit v in
    match t1.v land lnot (msb lor (msb - 1)) with
    | 0 -> empty, t1
    | v ->
      let suffix = {k = t1.k; v; l = empty; r = empty} in
      match t1.v lxor v with
      | 0 -> suffix, join t1.k t1.l t1.r
      | v' -> suffix, {t1 with v = v'}
  else
    empty, t1

let extract_unique_suffix t1 t2 =
  extract_suffix_to t1 t2.k t2.v

let rec extract_shared_suffix t1 t2 =
  if t1.k <> t2.k then
    empty, (t1, t2)
  else if t1.v <> t2.v then
    let msb = extract_bit (t1.v lxor t2.v) in
    let mask = msb lor (msb - 1) in
    let suffix = match t1.v land lnot mask with
      | 0 -> empty
      | v -> {k = t1.k; v; l = empty; r = empty}
    in
    let t1 = match t1.v land mask with
      | 0 -> join t1.k t1.l t1.r
      | v -> {t1 with v}
    in
    let t2 = match t2.v land mask with
      | 0 -> join t2.k t2.l t2.r
      | v -> {t2 with v}
    in
    suffix, (t1, t2)
  else if t1.v = 0 then
    empty, (empty, empty)
  else
    let ls, (l1, l2) = extract_shared_suffix t1.l t2.l in
    let rs, (r1, r2) =
      if is_empty l1 && is_empty l2 then
        extract_shared_suffix t1.r t2.r
      else
        empty, (t1.r, t2.r)
    in
    {k = t1.k; v = t1.v; l = ls; r = rs},
    (join t1.k l1 r1, join t1.k l2 r2)

let extract_shared_prefix _s1 _s2 =
  failwith "TODO"

(*let rec extract_shared_prefix s1 s2 =
  if s1.k = s2.k then
    match extract_shared_prefix s1.r s2.r with
    | Some prefix -> Some prefix
    | None ->
      match extract_shared_prefix s1.l s2.l with
      | Some prefix -> Some (join s1.k prefix s1.r)
      | None ->
        if s1.v = s2.v then
          None
        else
          let mask =
            let s1' = s1.v land lnot s2.v in
            let s2' = s2.v land lnot s1.v in
            (Bit_lib.extract_lsb s1' - 1) land
            (Bit_lib.extract_lsb s2' - 1)
          in
          match s1.v land mask with
          | 0 -> Some (join s1.k s1.l s1.r)
          | v -> Some {s1 with v}
  else
    let m1 = extract_bit s1.k in
    let m2 = extract_bit s2.k in
    if m1 = m2 then (
      (* same msb *)
      if s2.k > s1.k then
        (* s2 > s1 *)

      else
        (* s1 > s2 *)
    ) else (

    )*)
