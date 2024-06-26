module type OrderedType = Stdlib.Set.OrderedType
module type S = sig
  include Stdlib.Set.S
  val of_bt1 : elt Bt1.t -> t option
  val rank : int -> t -> elt
  val rank_of : t -> elt -> int option
  val to_list : t -> elt list
end

type +'a balset = 'a Bt1.t = private
  | Leaf
  | Node of int * 'a balset * 'a * 'a balset

module Make (O : OrderedType) :
  S with type elt = O.t
     and type t = O.t balset =
struct
  type elt = O.t

  type t = O.t balset

  let empty = Bt1.leaf

  let is_empty = function Leaf -> true | Node _ -> false

  let rec mem k = function
    | Leaf -> false
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        mem k l
      else if c > 0 then
        mem k r
      else
        true

  let singleton k =
    Bt1.node Bt1.leaf k Bt1.leaf

  let rec add k = function
    | Leaf -> singleton k
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        Bt1.node (add k l) k' r
      else if c > 0 then
        Bt1.node l k' (add k r)
      else
        Bt1.node l k r

  let rec remove k = function
    | Leaf -> raise Not_found
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        Bt1.node (remove k l) k' r
      else if c > 0 then
        Bt1.node l k' (remove k r)
      else
        Bt1.join l r

  let remove k t =
    try remove k t
    with Not_found -> t

  let rec split k = function
    | Leaf -> Bt1.leaf, false, Bt1.leaf
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        let l', v', r' = split k l in
        l', v', Bt1.join r' r
      else if c > 0 then
        let l', v', r' = split k r in
        Bt1.join l l', v', r'
      else
        (l, true, r)

  let rec union t1 t2 =
    match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | t1, Node (_, l2, k2, r2) ->
      let l1, _, r1 = split k2 t1 in
      let l' = union l1 l2 in
      let r' = union r1 r2 in
      Bt1.node l' k2 r'

  let union3 l k r =
    union (add k l) r

  let cardinal = Bt1.size

  type enumeration =
    | End
    | More of elt * t * enumeration

  let rec cons_enum t e = match t with
    | Leaf -> e
    | Node (_, l, k, r) -> cons_enum l (More (k, r, e))

  let compare t1 t2 =
    if t1 == t2 then 0 else
      match Int.compare (cardinal t1) (cardinal t2) with
      | 0 ->
        let rec compare_aux = function
          | (End, End) -> 0
          | (End, _  ) | (_  , End) -> assert false
          | (More (k1, r1, e1), More (k2, r2, e2)) ->
            match O.compare k1 k2 with
            | 0 ->
              if r1 == r2 then
                compare_aux (e1, e2)
              else
                compare_aux (cons_enum r1 e1, cons_enum r2 e2)
            | n -> n
        in
        compare_aux (cons_enum t1 End, cons_enum t2 End)
      | n -> n

  let equal t1 t2 =
    t1 == t2 && Int.equal (cardinal t1) (cardinal t2) &&
    let rec equal_aux = function
      | (End, End) -> true
      | (End, _  ) | (_  , End) -> assert false
      | (More (k1, r1, e1), More (k2, r2, e2)) ->
        match O.compare k1 k2 with
        | 0 ->
          if r1 == r2 then
            equal_aux (e1, e2)
          else
            equal_aux (cons_enum r1 e1, cons_enum r2 e2)
        | _ -> false
    in
    equal_aux (cons_enum t1 End, cons_enum t2 End)

  let rec iter f = function
    | Leaf -> ()
    | Node (_, l, k, r) ->
      iter f l;
      f k;
      iter f r

  let rec fold f t acc =
    match t with
    | Leaf -> acc
    | Node (_, l, k, r) ->
      let acc = fold f l acc in
      let acc = f k acc in
      let acc = fold f r acc in
      acc

  let rec for_all f = function
    | Leaf -> true
    | Node (_, l, k, r) ->
      for_all f l &&
      f k &&
      for_all f r

  let rec exists f = function
    | Leaf -> false
    | Node (_, l, k, r) ->
      exists f l || f k || exists f r

  let rec filter f = function
    | Leaf -> Bt1.leaf
    | Node (_, l, k, r) ->
      let l' = filter f l in
      let keep = f k in
      let r' = filter f r in
      if keep
      then Bt1.node l' k r'
      else Bt1.join l' r'

  let rec filter_map f = function
    | Leaf -> Bt1.leaf
    | Node (_, l, k, r) ->
      let l' = filter_map f l in
      let k' = f k in
      let r' = filter_map f r in
      match k' with
      | None -> union l' r'
      | Some k' -> union3 l' k' r'

  let rec partition f = function
    | Leaf -> Bt1.leaf, Bt1.leaf
    | Node (_, l, k, r) ->
      let l1, l2 = partition f l in
      let side = f k in
      let r1, r2 = partition f r in
      if side then
        (Bt1.node l1 k r1, Bt1.join l2 r2)
      else
        (Bt1.join l1 r1, Bt1.node l2 k r2)

  let of_list l =
    List.fold_left (fun t a -> add a t) empty l

  let elements t =
    let rec aux t acc = match t with
      | Leaf -> acc
      | Node (_, l, k, r) ->
        aux l (k :: aux r acc)
    in
    aux t []

  let rec min_elt = function
    | Leaf -> raise Not_found
    | Node (_, Leaf, k, _) -> k
    | Node (_, l, _, _) -> min_elt l

  let rec min_elt_opt = function
    | Leaf -> None
    | Node (_, Leaf, k, _) -> Some k
    | Node (_, l, _, _) -> min_elt_opt l

  let rec max_elt = function
    | Leaf -> raise Not_found
    | Node (_, _, k, Leaf) -> k
    | Node (_, _, _, r) -> max_elt r

  let rec max_elt_opt = function
    | Leaf -> None
    | Node (_, _, k, Leaf) -> Some k
    | Node (_, _, _, r) -> max_elt_opt r

  let choose = min_elt
  let choose_opt = min_elt_opt

  let rec find k = function
    | Leaf -> raise Not_found
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        find k l
      else if c > 0 then
        find k r
      else
        k'

  let rec find_opt k = function
    | Leaf -> None
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        find_opt k l
      else if c > 0 then
        find_opt k r
      else
        Some k'

  let rec find_first_opt f = function
    | Leaf -> None
    | Node (_, l, k', r) ->
      if f k' then
        match find_first_opt f l with
        | None -> Some k'
        | Some _ as result -> result
      else
        find_first_opt f r

  let find_first f t =
    match find_first_opt f t with
    | None -> raise Not_found
    | Some kv -> kv

  let rec find_last_opt f = function
    | Leaf -> None
    | Node (_, l, k', r) ->
      if f k' then
        match find_last_opt f r with
        | None -> Some k'
        | Some _ as result -> result
      else
        find_last_opt f l

  let find_last f t =
    match find_last_opt f t with
    | None -> raise Not_found
    | Some kv -> kv

  let rec map f = function
    | Leaf -> Bt1.leaf
    | Node (_, l, k, r) ->
      let l' = map f l in
      let k' = f k in
      let r' = map f r in
      union3 l' k' r'

  let rec seq_enum enum () =
    match enum with
    | End -> Seq.Nil
    | More (k, r, e) ->
      Seq.Cons (k, seq_enum (cons_enum r e))

  let to_seq = function
    | Leaf -> Seq.empty
    | t -> seq_enum (cons_enum t End)

  let rec snoc_enum t e = match t with
    | Leaf -> e
    | Node (_, l, k, r) -> snoc_enum r (More (k, l, e))

  let to_rev_seq = function
    | Leaf -> Seq.empty
    | t ->
      let rec seq enum () =
        match enum with
        | End -> Seq.Nil
        | More (k, r, e) ->
          Seq.Cons (k, seq (snoc_enum r e))
      in
      seq (snoc_enum t End)

  let rec cons_from k e = function
    | Leaf -> e
    | Node (_, l, k', r) ->
      let c = O.compare k k' in
      if c < 0 then
        cons_from k (More (k', r, e)) l
      else if c > 0 then
        cons_from k e r
      else
        More (k', r, e)

  let to_seq_from k t =
    seq_enum (cons_from k End t)

  let add_seq seq t =
    Seq.fold_left (fun t k -> add k t) t seq

  let of_seq seq =
    add_seq seq empty

  let subset t1 t2 =
    cardinal t1 <= cardinal t2 &&
    let rec aux = function
      | Bt1.Leaf -> true
      | Bt1.Node (_, l, k, r) -> mem k t2 && aux l && aux r
    in
    aux t1

  let rec disjoint t1 t2 =
    match t1, t2 with
    | Leaf, _ | _, Leaf -> true
    | Node (_, l1, k1, r1), t2 ->
      let l2, k2, r2 = split k1 t2 in
      not k2 &&
      disjoint l1 l2 &&
      disjoint r1 r2

  let rec diff t1 t2 =
    match t1, t2 with
    | Leaf, _ -> Bt1.leaf
    | t1, Leaf -> t1
    | Node (_, l1, k, r1), t2 ->
      let l2, drop, r2 = split k t2 in
      let l = diff l1 l2 in
      let r = diff r1 r2 in
      if drop then
        Bt1.join l r
      else
        Bt1.node l k r

  let rec inter t1 t2 =
    match t1, t2 with
    | Leaf, _ | _, Leaf -> empty
    | Node (_, l1, k1, r1), t2 ->
      let l2, keep, r2 = split k1 t2 in
      let l = inter l1 l2 in
      let r = inter r1 r2 in
      if keep then
        Bt1.node l k1 r
      else
        Bt1.join l r

  let to_list = elements

  let of_bt1 t =
    let rec validate k = function
      | Leaf -> k
      | Node (_, l, k', r) ->
        let k = validate k l in
        if O.compare k k' > 0 then
          raise Exit;
        validate k' r
    in
    let rec validate_fringe = function
      | Leaf -> None
      | Node (_, l, k', r) ->

        match validate_fringe l with
        | None -> Some (validate k' r)
        | Some k ->
          if O.compare k k' > 0 then
            raise Exit;
          Some (validate k' r)
    in
    match validate_fringe t with
    | (_ : elt option) -> Some t
    | exception Exit -> None

  let rank = Bt1.rank

  let rank_of t k =
    let rec aux ofs k = function
      | Leaf -> None
      | Node (_, l, k', r) ->
        let c = O.compare k k' in
        if c < 0 then
          aux ofs k l
        else
          let ofs = cardinal l + ofs in
          if c > 0 then
            aux (ofs + 1) k r
          else
            Some ofs
    in
    aux 0 k t
end
