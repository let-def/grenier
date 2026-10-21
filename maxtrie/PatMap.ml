(* Maps whose keys are non-negative integers, represented as Patricia tries
   whose nodes hoist the maximum key of their subtree.

   This is the representation of [PatSet], with one binding per node instead
   of a bitmap of elements. A node [Node {k; v; l; r}] binds the key [k],
   which is the largest key of the tree, to [v]. With [m] the most
   significant bit of [k] ([0] if [k = 0]):
   - [r] holds the keys that are less than [m];
   - [l] holds the keys in the interval [\[m, k)], with their bit [m]
     cleared: keys are relative to the "frame" of their subtree.
   The absolute key of a node is obtained by or-ing the most significant
   bits of the ancestors at which a left branch was taken (the "prefix").

   The representation is canonical, every node holds a binding (a map of
   size [n] has exactly [n] nodes), and the depth is bounded by the number of
   bits of a key. See [PatSet] for a more detailed explanation; the
   algorithms below follow those of [PatSet]. *)

type key = int

type +'a t =
  | Empty
  | Node of { k: int; v: 'a; l: 'a t; r: 'a t }

let msb = Bit_lib.extract_msb
let lt_msb = Bit_lib.lt_msb

let check_key fn k =
  if k < 0 then invalid_arg ("PatMap." ^ fn ^ ": negative key")

let empty = Empty

let is_empty = function Empty -> true | Node _ -> false

let[@inline] leaf k v = Node {k; v; l = Empty; r = Empty}

let singleton k v =
  check_key "singleton" k;
  leaf k v

(* -------------------------------------------------------------------------- *)
(* Low-level operations. *)

(* [node_at k t] is the node of [t] whose key is [k], or [Empty]. *)
let rec node_at k t =
  match t with
  | Empty -> Empty
  | Node n ->
    if k >= n.k then
      if k = n.k then t else Empty
    else
      let m = msb n.k in
      if k land m <> 0
      then node_at (k lxor m) n.l
      else node_at k n.r

(* [push k v t] binds [k] to [v] in [t], where [k] is greater than every key
   of [t]. *)
let rec push k v t =
  match t with
  | Empty -> leaf k v
  | Node n ->
    let m = msb k in
    if n.k land m <> 0
    then Node {k; v; l = push (n.k lxor m) n.v n.l; r = n.r}
    else Node {k; v; l = Empty; r = t}

(* [join k l r] removes the root of a node with key [k] and children [l] and
   [r]: the maximum of [l], if any, is promoted. *)
let rec join k l r =
  match l with
  | Empty -> r
  | Node n -> Node {k = msb k lor n.k; v = n.v; l = join n.k n.l n.r; r}

(* [node t k v l r] is [Node {k; v; l; r}], reusing [t] if it is identical.
   [t] must be a node with key [k]. *)
let[@inline] node t k v l r =
  match t with
  | Node n when n.v == v && n.l == l && n.r == r -> t
  | _ -> Node {k; v; l; r}

(* [rebuild t k o l r] is the node [t] with key [k], the binding [o] (if
   any), and children [l] and [r]. *)
let[@inline] rebuild t k o l r =
  match o with
  | None -> join k l r
  | Some v -> node t k v l r

let[@inline] with_l t l =
  match t with
  | Node n when n.l != l -> Node {n with l}
  | _ -> t

let[@inline] with_r t r =
  match t with
  | Node n when n.r != r -> Node {n with r}
  | _ -> t

(* -------------------------------------------------------------------------- *)
(* Elementary operations. *)

let rec find k = function
  | Empty -> raise Not_found
  | Node n ->
    if k >= n.k then
      if k = n.k then n.v else raise Not_found
    else
      let m = msb n.k in
      if k land m <> 0 then find (k lxor m) n.l else find k n.r

let find k t = if k < 0 then raise Not_found else find k t

let find_opt k t =
  if k < 0 then None else
  match node_at k t with
  | Empty -> None
  | Node n -> Some n.v

let mem k t =
  k >= 0 && match node_at k t with Empty -> false | Node _ -> true

(* [add k v t]: if [k] is already bound to a physically equal value, [t] is
   returned. *)
let rec add k v t =
  match t with
  | Empty -> leaf k v
  | Node n ->
    if k >= n.k then
      if k = n.k then
        if n.v == v then t else Node {n with v}
      else
        push k v t
    else
      let m = msb n.k in
      if k land m <> 0
      then with_l t (add (k lxor m) v n.l)
      else with_r t (add k v n.r)

let add k v t =
  check_key "add" k;
  add k v t

let rec remove k t =
  match t with
  | Empty -> t
  | Node n ->
    if k >= n.k then
      if k = n.k then join n.k n.l n.r else t
    else
      let m = msb n.k in
      if k land m <> 0
      then with_l t (remove (k lxor m) n.l)
      else with_r t (remove k n.r)

let remove k t = if k < 0 then t else remove k t

let rec update k f t =
  match t with
  | Empty ->
    (match f None with None -> t | Some v -> leaf k v)
  | Node n ->
    if k >= n.k then
      if k = n.k then
        match f (Some n.v) with
        | None -> join n.k n.l n.r
        | Some v -> if v == n.v then t else Node {n with v}
      else
        match f None with
        | None -> t
        | Some v -> push k v t
    else
      let m = msb n.k in
      if k land m <> 0
      then with_l t (update (k lxor m) f n.l)
      else with_r t (update k f n.r)

let update k f t =
  check_key "update" k;
  update k f t

let cardinal t =
  let rec loop t acc =
    match t with
    | Empty -> acc
    | Node n -> loop n.r (loop n.l (acc + 1))
  in
  loop t 0

let is_singleton = function
  | Node {l = Empty; r = Empty; _} -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* Minimum and maximum. *)

let rec min_binding pre n =
  match n with
  | Empty -> assert false
  | Node {r = Node _ as r; _} -> min_binding pre r
  | Node {k; l = Node _ as l; _} -> min_binding (pre lor msb k) l
  | Node {k; v; _} -> (pre lor k, v)

let min_binding = function
  | Empty -> raise Not_found
  | t -> min_binding 0 t

let min_binding_opt = function
  | Empty -> None
  | t -> Some (min_binding t)

let max_binding = function
  | Empty -> raise Not_found
  | Node n -> (n.k, n.v)

let max_binding_opt = function
  | Empty -> None
  | Node n -> Some (n.k, n.v)

let choose = max_binding
let choose_opt = max_binding_opt

(* -------------------------------------------------------------------------- *)
(* Iteration. Bindings are enumerated in increasing order of keys: right
   child, then left child, then root. *)

let rec iter f pre = function
  | Empty -> ()
  | Node n ->
    iter f pre n.r;
    iter f (pre lor msb n.k) n.l;
    f (pre lor n.k) n.v

let iter f t = iter f 0 t

let rec fold f pre t acc =
  match t with
  | Empty -> acc
  | Node n ->
    let acc = fold f pre n.r acc in
    let acc = fold f (pre lor msb n.k) n.l acc in
    f (pre lor n.k) n.v acc

let fold f t acc = fold f 0 t acc

let rec fold_right f pre t acc =
  match t with
  | Empty -> acc
  | Node n ->
    let acc = f (pre lor n.k) n.v acc in
    let acc = fold_right f (pre lor msb n.k) n.l acc in
    fold_right f pre n.r acc

let bindings t = fold_right (fun k v acc -> (k, v) :: acc) 0 t []

let rec for_all p pre = function
  | Empty -> true
  | Node n ->
    for_all p pre n.r &&
    for_all p (pre lor msb n.k) n.l &&
    p (pre lor n.k) n.v

let for_all p t = for_all p 0 t

let rec exists p pre = function
  | Empty -> false
  | Node n ->
    exists p pre n.r ||
    exists p (pre lor msb n.k) n.l ||
    p (pre lor n.k) n.v

let exists p t = exists p 0 t

exception Found

(* [find_first p m] is the binding of [m] with the least key that satisfies
   [p]. Unlike [Stdlib.Map], [p] need not be monotonic. *)
let find_first_opt p t =
  let result = ref None in
  (try iter (fun k v -> if p k then (result := Some (k, v); raise Found)) t
   with Found -> ());
  !result

let find_first p t =
  match find_first_opt p t with
  | None -> raise Not_found
  | Some b -> b

let rec map f = function
  | Empty -> Empty
  | Node n ->
    let r = map f n.r in
    let l = map f n.l in
    Node {k = n.k; v = f n.v; l; r}

let rec mapi f pre = function
  | Empty -> Empty
  | Node n ->
    let r = mapi f pre n.r in
    let l = mapi f (pre lor msb n.k) n.l in
    Node {k = n.k; v = f (pre lor n.k) n.v; l; r}

let mapi f t = mapi f 0 t

let rec filter p pre t =
  match t with
  | Empty -> t
  | Node n ->
    let r = filter p pre n.r in
    let l = filter p (pre lor msb n.k) n.l in
    if p (pre lor n.k) n.v
    then node t n.k n.v l r
    else join n.k l r

let filter p t = filter p 0 t

let rec filter_map f pre = function
  | Empty -> Empty
  | Node n ->
    let r = filter_map f pre n.r in
    let l = filter_map f (pre lor msb n.k) n.l in
    match f (pre lor n.k) n.v with
    | None -> join n.k l r
    | Some v -> Node {k = n.k; v; l; r}

let filter_map f t = filter_map f 0 t

let rec partition p pre t =
  match t with
  | Empty -> t, t
  | Node n ->
    let r1, r2 = partition p pre n.r in
    let l1, l2 = partition p (pre lor msb n.k) n.l in
    if p (pre lor n.k) n.v
    then node t n.k n.v l1 r1, join n.k l2 r2
    else join n.k l1 r1, node t n.k n.v l2 r2

let partition p t = partition p 0 t

(* -------------------------------------------------------------------------- *)
(* Comparisons. Both are structural; [compare] is a total order on maps but
   not the lexicographic order on bindings. *)

let rec equal eq a b =
  a == b ||
  match a, b with
  | Node a, Node b ->
    a.k = b.k && eq a.v b.v && equal eq a.l b.l && equal eq a.r b.r
  | _ -> false

let rec compare cmp a b =
  if a == b then 0 else
  match a, b with
  | Empty, _ -> -1
  | _, Empty -> 1
  | Node a, Node b ->
    let c = Int.compare a.k b.k in
    if c <> 0 then c else
    let c = cmp a.v b.v in
    if c <> 0 then c else
    let c = compare cmp a.l b.l in
    if c <> 0 then c else
    compare cmp a.r b.r

(* -------------------------------------------------------------------------- *)
(* Union. *)

(* Carried keys: see [PatSet] for the explanation. A virtual tree is
   described by [h s p] with a mask [mk] shared by both operands: the chain of
   nodes [h], [h.l], ... up to [s] excluded, standing for the keys
   [c.k land mk], followed by the tail [s] if [p]. *)

let[@inline] vtail s p = if p then s else Empty

let[@inline] vmax h s p mk =
  if h != s then (match h with Node n -> n.k land mk | Empty -> -1)
  else if p then (match s with Node n -> n.k | Empty -> -1)
  else -1

let rec materialize h s p mk =
  if h == s then vtail s p
  else match h with
    | Node n -> push (n.k land mk) n.v (materialize n.l s p mk)
    | Empty -> assert false

let rec first_low m c s =
  if c == s then c
  else match c with
    | Node n when n.k land m <> 0 -> first_low m n.l s
    | _ -> c

let[@inline] joins m s p =
  p && match s with Node n -> n.k land m <> 0 | Empty -> false

let[@inline] left = function Node n -> n.l | Empty -> Empty
let[@inline] right = function Node n -> n.r | Empty -> Empty

(* [merge_o f sw key vx vy] merges the value [vx] of [x] with the value [vy]
   of the other operand, where [sw] tells whether [x] comes from the second
   argument of the toplevel [union]. *)
let[@inline] merge_o f sw key vx vy =
  if sw then f key vy vx else f key vx vy

(* [add_with f sw pre k v t] binds [k] to [v] in [t], merging with an
   existing binding (which comes from the side described by [sw]). *)
let rec add_with f sw pre k v t =
  match t with
  | Empty -> leaf k v
  | Node n ->
    if k >= n.k then
      if k = n.k then rebuild t k (merge_o f sw (pre lor k) n.v v) n.l n.r
      else push k v t
    else
      let m = msb n.k in
      if k land m <> 0
      then with_l t (add_with f sw (pre lor m) (k lxor m) v n.l)
      else with_r t (add_with f sw pre k v n.r)

(* [union f a b]: the bindings of [a] and [b], where [f k va vb] decides the
   binding of a key [k] present in both. If the result is equal to [b] (with
   physically equal values), then [b] is returned. *)
let rec union f pre a b =
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | Node na, Node nb ->
    let ka = na.k and kb = nb.k in
    if ka = kb then
      let m = msb ka in
      let l = union f (pre lor m) na.l nb.l in
      let r = union f pre na.r nb.r in
      rebuild b kb (f (pre lor ka) na.v nb.v) l r
    else if lt_msb ka kb then
      with_r b (union f pre a nb.r)
    else if lt_msb kb ka then
      with_r a (union f pre na.r b)
    else
      let m = msb ka in
      let r = union f pre na.r nb.r in
      if ka > kb then
        let l = union_push f false (pre lor m) na.l b (kb lxor m) in
        node a ka na.v l r
      else
        let l = union_push f true (pre lor m) nb.l a (ka lxor m) in
        node b kb nb.v l r

and union_o f sw pre x y =
  if sw then union f pre y x else union f pre x y

(* [union_push f sw pre x h k] is the union of [x] and the pennant made of the
   carried node [h], standing for the key [k], followed by [h.l]. *)
and union_push f sw pre x h k =
  match h with
  | Empty -> assert false
  | Node nh ->
  let v = nh.v and y = nh.l in
  match x with
  | Empty -> push k v y
  | Node nx ->
    let kx = nx.k in
    if kx = k then
      let m = msb k in
      let l, r =
        match y with
        | Node ny when ny.k land m <> 0 ->
          union_push f sw (pre lor m) nx.l y (ny.k lxor m),
          union_o f sw pre nx.r ny.r
        | _ ->
          nx.l, union_o f sw pre nx.r y
      in
      rebuild x k (merge_o f sw (pre lor k) nx.v v) l r
    else if lt_msb kx k then
      let m = msb k in
      match y with
      | Node ny when ny.k land m <> 0 ->
        Node {k; v; l = push (ny.k lxor m) ny.v ny.l;
              r = union_o f sw pre x ny.r}
      | _ ->
        Node {k; v; l = Empty; r = union_o f sw pre x y}
    else if lt_msb k kx then
      with_r x (union_push f sw pre nx.r h k)
    else
      let m = msb k in
      let pre' = pre lor m in
      match y with
      | Node ny when ny.k land m <> 0 ->
        (* Two keys are carried into the upper half. *)
        let r = union_o f sw pre nx.r ny.r in
        if kx > k then
          let l =
            if sw then vunion f pre' h ny.l true nx.l nx.l true (m - 1)
            else vunion f pre' nx.l nx.l true h ny.l true (m - 1)
          in
          node x kx nx.v l r
        else
          let l =
            if sw then vunion f pre' y ny.l true x nx.l true (m - 1)
            else vunion f pre' x nx.l true y ny.l true (m - 1)
          in
          Node {k; v; l; r}
      | _ ->
        let r = union_o f sw pre nx.r y in
        if kx > k then
          node x kx nx.v (add_with f sw pre' (k lxor m) v nx.l) r
        else
          Node {k; v; l = push (kx lxor m) nx.v nx.l; r}

(* [vunion f pre ha sa pa hb sb pb mk] is the union of two virtual trees,
   the first one coming from the first argument of the toplevel [union]. *)
and vunion f pre ha sa pa hb sb pb mk =
  if ha == sa then
    if hb == sb then union f pre (vtail sa pa) (vtail sb pb)
    else match hb with
      | Node nb when nb.l == sb ->
        if pb then union_push f false pre (vtail sa pa) hb (nb.k land mk)
        else add_with f false pre (nb.k land mk) nb.v (vtail sa pa)
      | _ -> vunion_gen f pre ha sa pa hb sb pb mk
  else if hb == sb then
    match ha with
    | Node na when na.l == sa ->
      if pa then union_push f true pre (vtail sb pb) ha (na.k land mk)
      else add_with f true pre (na.k land mk) na.v (vtail sb pb)
    | _ -> vunion_gen f pre ha sa pa hb sb pb mk
  else vunion_gen f pre ha sa pa hb sb pb mk

and vunion_gen f pre ha sa pa hb sb pb mk =
  let ka = vmax ha sa pa mk and kb = vmax hb sb pb mk in
  if ka < 0 then materialize hb sb pb mk
  else if kb < 0 then materialize ha sa pa mk
  else if lt_msb ka kb then vunion_below f pre ha sa pa hb sb pb mk kb false
  else if lt_msb kb ka then vunion_below f pre hb sb pb ha sa pa mk ka true
  else vunion_same f pre ha sa pa hb sb pb ka kb

(* [x] lies below the most significant bit of the maximum [ky] of [y]. *)
and vunion_below f pre hx sx px hy sy py mk ky sw =
  match hy with
  | Empty -> assert false
  | Node ny ->
  if hy == sy then
    let r =
      if sw then vunion f pre ny.r ny.r true hx sx px mk
      else vunion f pre hx sx px ny.r ny.r true mk
    in
    with_r sy r
  else
    let m = msb ky in
    let mk = m - 1 in
    let h = ny.l in
    let c = first_low m h sy in
    let j = c == sy && joins m sy py in
    let l = materialize h (if j then left sy else c) j mk in
    let r =
      if c != sy then
        if sw then vunion f pre c sy py hx sx px mk
        else vunion f pre hx sx px c sy py mk
      else
        let t = if j then right sy else vtail sy py in
        if sw then vunion f pre t t true hx sx px mk
        else vunion f pre hx sx px t t true mk
    in
    Node {k = ky; v = ny.v; l; r}

(* The maxima [ka] and [kb] share their most significant bit [m]. *)
and vunion_same f pre ha sa pa hb sb pb ka kb =
  let k = if ka > kb then ka else kb in
  let m = msb k in
  let uha, usa, upa, lha, lsa, lpa =
    if ka = k && ha == sa then
      left sa, left sa, true, right sa, right sa, true
    else
      let h = if ka = k then left ha else ha in
      let c = first_low m h sa in
      if c != sa then h, c, false, c, sa, pa
      else if joins m sa pa then h, left sa, true, right sa, right sa, true
      else h, sa, false, sa, sa, pa
  in
  let uhb, usb, upb, lhb, lsb, lpb =
    if kb = k && hb == sb then
      left sb, left sb, true, right sb, right sb, true
    else
      let h = if kb = k then left hb else hb in
      let c = first_low m h sb in
      if c != sb then h, c, false, c, sb, pb
      else if joins m sb pb then h, left sb, true, right sb, right sb, true
      else h, sb, false, sb, sb, pb
  in
  let value t = match t with Node n -> n.v | Empty -> assert false in
  let o =
    if ka = kb then f (pre lor k) (value ha) (value hb)
    else if ka = k then Some (value ha) else Some (value hb)
  in
  let l = vunion f (pre lor m) uha usa upa uhb usb upb (m - 1) in
  let r = vunion f pre lha lsa lpa lhb lsb lpb (m - 1) in
  match o with
  | None -> join k l r
  | Some v ->
    if kb = k && hb == sb then node sb k v l r
    else if ka = k && ha == sa then node sa k v l r
    else Node {k; v; l; r}

let union f a b = union f 0 a b

(* -------------------------------------------------------------------------- *)
(* Intersection and difference. *)

(* [inter f a b]: the keys present in both [a] and [b], where [f k va vb]
   decides the binding. *)
let rec inter f pre a b =
  match a, b with
  | Empty, _ | _, Empty -> Empty
  | Node na, Node nb ->
    let ka = na.k and kb = nb.k in
    if ka = kb then
      let l = inter f (pre lor msb ka) na.l nb.l in
      let r = inter f pre na.r nb.r in
      match f (pre lor ka) na.v nb.v with
      | None -> join ka l r
      | Some v -> Node {k = ka; v; l; r}
    else if lt_msb ka kb then inter f pre a nb.r
    else if lt_msb kb ka then inter f pre na.r b
    else if ka < kb then
      (* The root of [b] is not in [a]. *)
      let m = msb kb in
      let l = inter f (pre lor m) na.l nb.l in
      let r = inter f pre na.r nb.r in
      match node_at (ka lxor m) nb.l with
      | Node n ->
        (match f (pre lor ka) na.v n.v with
         | Some v -> Node {k = ka; v; l; r}
         | None -> join ka l r)
      | Empty -> join ka l r
    else
      let m = msb ka in
      let l = inter f (pre lor m) na.l nb.l in
      let r = inter f pre na.r nb.r in
      match node_at (kb lxor m) na.l with
      | Node n ->
        (match f (pre lor kb) n.v nb.v with
         | Some v -> Node {k = kb; v; l; r}
         | None -> join kb l r)
      | Empty -> join kb l r

let inter f a b = inter f 0 a b

(* [diff a b] removes from [a] the keys of [b]. If nothing is removed, then
   [a] is returned. *)
let rec diff a b =
  match a, b with
  | Empty, _ -> a
  | _, Empty -> a
  | Node na, Node nb ->
    let ka = na.k and kb = nb.k in
    if ka = kb then
      join ka (diff na.l nb.l) (diff na.r nb.r)
    else if lt_msb ka kb then diff a nb.r
    else if lt_msb kb ka then with_r a (diff na.r b)
    else
      let m = msb ka in
      let r = diff na.r nb.r in
      if ka > kb then
        node a ka na.v (diff_push na.l b (kb lxor m)) r
      else
        let l = diff na.l nb.l in
        match node_at (ka lxor m) nb.l with
        | Node _ -> join ka l r
        | Empty -> node a ka na.v l r

(* [diff_push x h k] removes from [x] the keys of the pennant made of the
   carried node [h], standing for the key [k], followed by [h.l]. *)
and diff_push x h k =
  match x, h with
  | Empty, _ | _, Empty -> x
  | Node nx, Node nh ->
    let y = nh.l in
    let kx = nx.k in
    let m = msb k in
    if kx = k then
      let l, r =
        match y with
        | Node ny when ny.k land m <> 0 -> diff_push nx.l y (ny.k lxor m), diff nx.r ny.r
        | _ -> nx.l, diff nx.r y
      in
      join k l r
    else if lt_msb kx k then
      diff x (match y with Node ny when ny.k land m <> 0 -> ny.r | _ -> y)
    else if lt_msb k kx then
      with_r x (diff_push nx.r h k)
    else if kx < k then
      (* The carried key is above every key of [x]: drop it. *)
      diff x y
    else
      match y with
      | Node ny when ny.k land m <> 0 ->
        let r = diff nx.r ny.r in
        node x kx nx.v (vdiff nx.l h ny.l true (m - 1)) r
      | _ ->
        let r = diff nx.r y in
        node x kx nx.v (remove (k lxor m) nx.l) r

(* [vdiff x hb sb pb mk] removes from [x] the keys of a virtual tree. *)
and vdiff x hb sb pb mk =
  if hb == sb then diff x (vtail sb pb) else
  match x, hb with
  | Empty, _ | _, Empty -> x
  | Node nx, Node nh ->
    if nh.l == sb then
      if pb then diff_push x hb (nh.k land mk)
      else remove (nh.k land mk) x
    else
    let kb = nh.k land mk and kx = nx.k in
    if kx = kb || (not (lt_msb kx kb) && not (lt_msb kb kx) && kx > kb) then
      (* Split the virtual tree at the most significant bit [m] of [kx],
         after popping its maximum if it is [kx]. *)
      let m = msb kx in
      let h = if kx = kb then nh.l else hb in
      let c = first_low m h sb in
      let l, r =
        if c != sb then vdiff nx.l h c false (m - 1), vdiff nx.r c sb pb (m - 1)
        else if joins m sb pb then
          vdiff nx.l h (left sb) true (m - 1), diff nx.r (right sb)
        else
          vdiff nx.l h sb false (m - 1), diff nx.r (vtail sb pb)
      in
      if kx = kb then join kx l r else node x kx nx.v l r
    else if lt_msb kx kb then
      let m = msb kb in
      let c = first_low m hb sb in
      if c != sb then vdiff x c sb pb (m - 1)
      else if joins m sb pb then diff x (right sb)
      else diff x (vtail sb pb)
    else if lt_msb kb kx then
      with_r x (vdiff nx.r hb sb pb mk)
    else
      (* [kx < kb]: the maximum of the virtual tree is above [x]. *)
      vdiff x nh.l sb pb mk

let disjoint a b =
  let rec disjoint : 'a 'b. 'a t -> 'b t -> bool = fun a b ->
    match a, b with
    | Empty, _ | _, Empty -> true
    | Node na, Node nb ->
      Obj.repr a != Obj.repr b &&
      let ka = na.k and kb = nb.k in
      if ka = kb then false
      else if lt_msb ka kb then disjoint a nb.r
      else if lt_msb kb ka then disjoint na.r b
      else if ka < kb then
        node_at (ka lxor msb kb) nb.l == Empty &&
        disjoint na.l nb.l && disjoint na.r nb.r
      else
        node_at (kb lxor msb ka) na.l == Empty &&
        disjoint na.l nb.l && disjoint na.r nb.r
  in
  disjoint a b

(* -------------------------------------------------------------------------- *)
(* Changes between two versions of a map. *)

(* [report f side pre t] calls [f] on every binding of [t], in increasing
   order of keys, as present only on the given side ([true]: the first map). *)
let rec report f side pre t =
  match t with
  | Empty -> ()
  | Node n ->
    report f side pre n.r;
    report f side (pre lor msb n.k) n.l;
    if side then f (pre lor n.k) (Some n.v) None
    else f (pre lor n.k) None (Some n.v)

(* [vreport f side pre h s p mk] does the same for a virtual tree: the tail
   first, then the chain, whose keys decrease. *)
let rec vreport f side pre h s p mk =
  if h == s then report f side pre (vtail s p)
  else match h with
    | Empty -> assert false
    | Node n ->
      vreport f side pre n.l s p mk;
      let k = pre lor (n.k land mk) in
      if side then f k (Some n.v) None else f k None (Some n.v)

(* [changes f pre a b] calls [f k x y], in increasing order of keys, for
   every key [k] whose bindings [x] in [a] and [y] in [b] are not physically
   equal. Physically equal subtrees are skipped. When the maxima of [a] and
   [b] differ but share their most significant bit, the smaller one is
   carried, as in [union]: this aligns again the subtrees that an update has
   shifted, so that they are still recognized as shared. *)
let rec changes f pre a b =
  if a != b then
  match a, b with
  | Empty, _ -> report f false pre b
  | _, Empty -> report f true pre a
  | Node na, Node nb ->
    let ka = na.k and kb = nb.k in
    if ka = kb then begin
      changes f pre na.r nb.r;
      changes f (pre lor msb ka) na.l nb.l;
      if na.v != nb.v then f (pre lor ka) (Some na.v) (Some nb.v)
    end
    else if lt_msb ka kb then begin
      (* [a] lies below the most significant bit of [kb]. *)
      changes f pre a nb.r;
      report f false (pre lor msb kb) nb.l;
      f (pre lor kb) None (Some nb.v)
    end
    else if lt_msb kb ka then begin
      changes f pre na.r b;
      report f true (pre lor msb ka) na.l;
      f (pre lor ka) (Some na.v) None
    end
    else begin
      let m = msb ka in
      changes f pre na.r nb.r;
      if ka > kb then begin
        vchanges f (pre lor m) na.l na.l true b nb.l true (m - 1);
        f (pre lor ka) (Some na.v) None
      end else begin
        vchanges f (pre lor m) a na.l true nb.l nb.l true (m - 1);
        f (pre lor kb) None (Some nb.v)
      end
    end

(* [vchanges] is [changes] on two virtual trees. *)
and vchanges f pre ha sa pa hb sb pb mk =
  if ha == sa && hb == sb then changes f pre (vtail sa pa) (vtail sb pb) else
  let ka = vmax ha sa pa mk and kb = vmax hb sb pb mk in
  if ka < 0 then vreport f false pre hb sb pb mk
  else if kb < 0 then vreport f true pre ha sa pa mk
  else if lt_msb ka kb then vchanges_below f pre ha sa pa hb sb pb kb false
  else if lt_msb kb ka then vchanges_below f pre hb sb pb ha sa pa ka true
  else vchanges_same f pre ha sa pa hb sb pb ka kb

(* [x] lies below the most significant bit [m] of the maximum [ky] of [y]:
   compare [x] with the lower part of [y], then report the upper part of [y]
   and its maximum. [sw] tells whether [x] is the second map. *)
and vchanges_below f pre hx sx px hy sy py ky sw =
  let m = msb ky in
  let vy = match hy with Node n -> n.v | Empty -> assert false in
  (* The lower part of [y] is the virtual tree [lh ls lp]. *)
  let lh, ls, lp, up =
    if hy == sy then
      (* Popping the root of a plain tree yields its two children. *)
      right sy, right sy, true, false
    else
      let c = first_low m (left hy) sy in
      if c != sy then c, sy, py, true
      else if joins m sy py then right sy, right sy, true, true
      else sy, sy, py, true
  in
  if sw then vchanges f pre lh ls lp hx sx px (m - 1)
  else vchanges f pre hx sx px lh ls lp (m - 1);
  if not up then report f sw (pre lor m) (left sy)
  else begin
    let h = left hy in
    let c = first_low m h sy in
    let j = c == sy && joins m sy py in
    vreport f sw (pre lor m) h (if j then left sy else c) j (m - 1)
  end;
  if sw then f (pre lor ky) (Some vy) None else f (pre lor ky) None (Some vy)

(* The maxima [ka] and [kb] share their most significant bit [m]: split
   both virtual trees at [m] (popping the largest maximum, or both), compare
   the lower parts, then the upper parts, then the maximum. *)
and vchanges_same f pre ha sa pa hb sb pb ka kb =
  let k = if ka > kb then ka else kb in
  let m = msb k in
  let uha, usa, upa, lha, lsa, lpa =
    if ka = k && ha == sa then
      left sa, left sa, true, right sa, right sa, true
    else
      let h = if ka = k then left ha else ha in
      let c = first_low m h sa in
      if c != sa then h, c, false, c, sa, pa
      else if joins m sa pa then h, left sa, true, right sa, right sa, true
      else h, sa, false, sa, sa, pa
  in
  let uhb, usb, upb, lhb, lsb, lpb =
    if kb = k && hb == sb then
      left sb, left sb, true, right sb, right sb, true
    else
      let h = if kb = k then left hb else hb in
      let c = first_low m h sb in
      if c != sb then h, c, false, c, sb, pb
      else if joins m sb pb then h, left sb, true, right sb, right sb, true
      else h, sb, false, sb, sb, pb
  in
  vchanges f pre lha lsa lpa lhb lsb lpb (m - 1);
  vchanges f (pre lor m) uha usa upa uhb usb upb (m - 1);
  match ha, hb with
  | Node na, Node nb ->
    if ka = kb then (if na.v != nb.v then f (pre lor k) (Some na.v) (Some nb.v))
    else if ka = k then f (pre lor k) (Some na.v) None
    else f (pre lor k) None (Some nb.v)
  | _ -> assert false

let changes f a b = changes f 0 a b

(* -------------------------------------------------------------------------- *)
(* Splitting. *)

(* [above k t] keeps the bindings whose keys are greater than [k]. *)
let rec above k t =
  match t with
  | Empty -> t
  | Node n ->
    if n.k <= k then Empty
    else
      let m = msb n.k in
      if k land m <> 0 then
        let l = above (k lxor m) n.l in
        (match n.r with
         | Empty when l == n.l -> t
         | _ -> Node {n with l; r = Empty})
      else
        with_r t (above k n.r)

(* [below k t] keeps the bindings whose keys are less than [k]. *)
let rec below k t =
  match t with
  | Empty -> t
  | Node n ->
    if n.k < k then t
    else if n.k = k then join k n.l n.r
    else
      let m = msb n.k in
      if k land m <> 0
      then join n.k (below (k lxor m) n.l) n.r
      else below k n.r

let split k t =
  if k < 0 then Empty, None, t
  else below k t, find_opt k t, above k t

(* -------------------------------------------------------------------------- *)
(* Sequences and lists. *)

type 'a enum =
  | End
  | More of int * 'a t * 'a enum      (* a prefix and a tree *)
  | Binding of int * 'a * 'a enum     (* an absolute key and its value *)

let rec seq_of_enum e () =
  match e with
  | End -> Seq.Nil
  | Binding (k, v, e) -> Seq.Cons ((k, v), seq_of_enum e)
  | More (_, Empty, e) -> seq_of_enum e ()
  | More (pre, Node n, e) ->
    seq_of_enum
      (More (pre, n.r,
             More (pre lor msb n.k, n.l, Binding (pre lor n.k, n.v, e)))) ()

let to_seq t = seq_of_enum (More (0, t, End))

let rec rev_seq pre t rest () =
  match t with
  | Empty -> rest ()
  | Node n ->
    Seq.Cons ((pre lor n.k, n.v),
              rev_seq (pre lor msb n.k) n.l (rev_seq pre n.r rest))

let to_rev_seq t = rev_seq 0 t Seq.empty

let add_seq s t = Seq.fold_left (fun t (k, v) -> add k v t) t s
let of_seq s = add_seq s empty
let of_list l = List.fold_left (fun t (k, v) -> add k v t) empty l

(* -------------------------------------------------------------------------- *)
(* Invariant checking. *)

let check t =
  let fail fmt = Printf.ksprintf failwith ("PatMap.check: " ^^ fmt) in
  let rec check t bound =
    match t with
    | Empty -> ()
    | Node n ->
      if n.k < 0 then fail "negative key %d" n.k;
      if bound >= 0 && n.k >= bound then
        fail "key %d is out of its bound %d" n.k bound;
      let m = msb n.k in
      check n.l (n.k - m);
      check n.r m
  in
  check t (-1)
