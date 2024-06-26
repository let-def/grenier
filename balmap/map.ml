module type OrderedType = Stdlib.Map.OrderedType

module type S = sig
  include Stdlib.Map.S
  val of_bt2 : (key, 'a) Bt2.t -> 'a t option
  val rank : int -> 'a t -> key * 'a
  val rank_of : 'a t -> key -> int option
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val add_to_list : key -> 'a -> 'a list t -> 'a list t
end

type (+'a, +'b) balmap = ('a, 'b) Bt2.t = private
  | Leaf
  | Node of int * ('a, 'b) balmap * 'a * 'b * ('a, 'b) balmap

module Make (O : OrderedType) :
  S with type key = O.t
     and type 'a t = (O.t, 'a) balmap =
struct
  type key = O.t

  type 'a t = (O.t, 'a) balmap

  let empty = Bt2.leaf

  let is_empty = function Leaf -> true | Node _ -> false

  let rec mem k = function
    | Leaf -> false
    | Node (_, l, k', _, r) ->
      let c = O.compare k k' in
      if c < 0 then
        mem k l
      else if c > 0 then
        mem k r
      else
        true

  let singleton k v =
    Bt2.node Bt2.leaf k v Bt2.leaf

  let rec add k v = function
    | Leaf -> singleton k v
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        Bt2.node (add k v l) k' v' r
      else if c > 0 then
        Bt2.node l k' v' (add k v r)
      else
        Bt2.node l k v r

  let maybe_node l k v r =
    match v with
    | None -> Bt2.join l r
    | Some v -> Bt2.node l k v r

  let rec update k f = function
    | Leaf ->
      begin match f None with
        | None -> Bt2.leaf
        | Some v -> singleton k v
      end
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        Bt2.node (update k f l) k' v' r
      else if c > 0 then
        Bt2.node l k' v' (update k f r)
      else
        maybe_node l k (f (Some v')) r

  let rec remove k = function
    | Leaf -> raise Not_found
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        Bt2.node (remove k l) k' v' r
      else if c > 0 then
        Bt2.node l k' v' (remove k r)
      else
        Bt2.join l r

  let remove k t =
    try remove k t
    with Not_found -> t

  let rec split k = function
    | Leaf -> Bt2.leaf, None, Bt2.leaf
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        let l', v', r' = split k l in
        l', v', Bt2.join r' r
      else if c > 0 then
        let l', v', r' = split k r in
        Bt2.join l l', v', r'
      else
        (l, Some v', r)

  let rec filter_map f = function
    | Leaf -> Bt2.leaf
    | Node (_, l, k, v, r) ->
      let l' = filter_map f l in
      let v' = f k v in
      let r' = filter_map f r in
      maybe_node l' k v' r'

  let rec merge f t1 t2 =
    match t1, t2 with
    | Leaf, t2 -> filter_map (fun k v -> f k None (Some v)) t2
    | t1, Leaf -> filter_map (fun k v -> f k (Some v) None) t1
    | t1, Node (_, l2, k2, v2, r2) ->
      let l1, v1, r1 = split k2 t1 in
      let l' = merge f l1 l2 in
      let v' = f k2 v1 (Some v2) in
      let r' = merge f r1 r2 in
      maybe_node l' k2 v' r'

  let rec union f t1 t2 =
    match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | t1, Node (_, l2, k2, v2, r2) ->
      let l1, v1, r1 = split k2 t1 in
      let l' = union f l1 l2 in
      match v1 with
      | None ->
        let r' = union f r1 r2 in
        Bt2.node l' k2 v2 r'
      | Some v1 ->
        let v' = f k2 v1 v2 in
        let r' = union f r1 r2 in
        maybe_node l' k2 v' r'

  let cardinal = Bt2.size

  type 'a enumeration =
    | End
    | More of key * 'a * 'a t * 'a enumeration

  let rec cons_enum t e = match t with
    | Leaf -> e
    | Node (_, l, k, v, r) -> cons_enum l (More (k, v, r, e))

  let compare f t1 t2 =
    if t1 == t2 then 0 else
      match Int.compare (cardinal t1) (cardinal t2) with
      | 0 ->
        let rec compare_aux = function
          | (End, End) -> 0
          | (End, _  ) | (_  , End) -> assert false
          | (More (k1, v1, r1, e1), More (k2, v2, r2, e2)) ->
            match O.compare k1 k2 with
            | 0 ->
              begin match f v1 v2 with
                | 0 ->
                  if r1 == r2 then
                    compare_aux (e1, e2)
                  else
                    compare_aux (cons_enum r1 e1, cons_enum r2 e2)
                | n -> n
              end
            | n -> n
        in
        compare_aux (cons_enum t1 End, cons_enum t2 End)
      | n -> n

  let equal f t1 t2 =
    t1 == t2 && Int.equal (cardinal t1) (cardinal t2) &&
    let rec equal_aux = function
      | (End, End) -> true
      | (End, _  ) | (_  , End) -> assert false
      | (More (k1, v1, r1, e1), More (k2, v2, r2, e2)) ->
        match O.compare k1 k2 with
        | 0 ->
          f v1 v2 &&
          if r1 == r2 then
            equal_aux (e1, e2)
          else
            equal_aux (cons_enum r1 e1, cons_enum r2 e2)
        | _ -> false
    in
    equal_aux (cons_enum t1 End, cons_enum t2 End)

  let rec iter f = function
    | Leaf -> ()
    | Node (_, l, k, v, r) ->
      iter f l;
      f k v;
      iter f r

  let rec fold f t acc =
    match t with
    | Leaf -> acc
    | Node (_, l, k, v, r) ->
      let acc = fold f l acc in
      let acc = f k v acc in
      let acc = fold f r acc in
      acc

  let rec for_all f = function
    | Leaf -> true
    | Node (_, l, k, v, r) ->
      for_all f l &&
      f k v &&
      for_all f r

  let rec exists f = function
    | Leaf -> false
    | Node (_, l, k, v, r) ->
      exists f l || f k v || exists f r

  let rec filter f = function
    | Leaf -> Bt2.leaf
    | Node (_, l, k, v, r) ->
      let l' = filter f l in
      let keep = f k v in
      let r' = filter f r in
      if keep then
        Bt2.node l' k v r'
      else
        Bt2.join l' r'

  let rec partition f = function
    | Leaf -> Bt2.leaf, Bt2.leaf
    | Node (_, l, k, v, r) ->
      let l1, l2 = partition f l in
      let side = f k v in
      let r1, r2 = partition f r in
      if side then
        (Bt2.node l1 k v r1, Bt2.join l2 r2)
      else
        (Bt2.join l1 r1, Bt2.node l2 k v r2)

  let bindings t =
    let rec aux t acc = match t with
      | Leaf -> acc
      | Node (_, l, k, v, r) ->
        aux l ((k, v) :: aux r acc)
    in
    aux t []

  let rec min_binding = function
    | Leaf -> raise Not_found
    | Node (_, Leaf, k, v, _) -> (k, v)
    | Node (_, l, _, _, _) -> min_binding l

  let rec min_binding_opt = function
    | Leaf -> None
    | Node (_, Leaf, k, v, _) -> Some (k, v)
    | Node (_, l, _, _, _) -> min_binding_opt l

  let rec max_binding = function
    | Leaf -> raise Not_found
    | Node (_, _, k, v, Leaf) -> (k, v)
    | Node (_, _, _, _, r) -> max_binding r

  let rec max_binding_opt = function
    | Leaf -> None
    | Node (_, _, k, v, Leaf) -> Some (k, v)
    | Node (_, _, _, _, r) -> max_binding_opt r

  let choose = min_binding
  let choose_opt = min_binding_opt

  let rec find k = function
    | Leaf -> raise Not_found
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        find k l
      else if c > 0 then
        find k r
      else
        v'

  let rec find_opt k = function
    | Leaf -> None
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        find_opt k l
      else if c > 0 then
        find_opt k r
      else
        Some v'

  let rec find_first_opt f = function
    | Leaf -> None
    | Node (_, l, k', v', r) ->
      if f k' then
        match find_first_opt f l with
        | None -> Some (k', v')
        | Some _ as result -> result
      else
        find_first_opt f r

  let find_first f t =
    match find_first_opt f t with
    | None -> raise Not_found
    | Some kv -> kv

  let rec find_last_opt f = function
    | Leaf -> None
    | Node (_, l, k', v', r) ->
      if f k' then
        match find_last_opt f r with
        | None -> Some (k', v')
        | Some _ as result -> result
      else
        find_last_opt f l

  let find_last f t =
    match find_last_opt f t with
    | None -> raise Not_found
    | Some kv -> kv

  let rec map f = function
    | Leaf -> Bt2.leaf
    | Node (_, l, k, v, r) ->
      let l' = map f l in
      let v' = f v in
      let r' = map f r in
      Bt2.node l' k v' r'

  let rec mapi f = function
    | Leaf -> Bt2.leaf
    | Node (_, l, k, v, r) ->
      let l' = mapi f l in
      let v' = f k v in
      let r' = mapi f r in
      Bt2.node l' k v' r'

  let rec seq_enum enum () =
    match enum with
    | End -> Seq.Nil
    | More (k, v, r, e) ->
      Seq.Cons ((k, v), seq_enum (cons_enum r e))

  let to_seq = function
    | Leaf -> Seq.empty
    | t -> seq_enum (cons_enum t End)

  let rec snoc_enum t e = match t with
    | Leaf -> e
    | Node (_, l, k, v, r) -> snoc_enum r (More (k, v, l, e))

  let to_rev_seq = function
    | Leaf -> Seq.empty
    | t ->
      let rec seq enum () =
        match enum with
        | End -> Seq.Nil
        | More (k, v, r, e) ->
          Seq.Cons ((k, v), seq (snoc_enum r e))
      in
      seq (snoc_enum t End)

  let rec cons_from k e = function
    | Leaf -> e
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        cons_from k (More (k', v', r, e)) l
      else if c > 0 then
        cons_from k e r
      else
        More (k', v', r, e)

  let to_seq_from k t =
    seq_enum (cons_from k End t)

  let add_seq seq t =
    Seq.fold_left (fun t (k, v) -> add k v t) t seq

  let of_seq seq =
    add_seq seq empty

  let rec add_to_list k v = function
    | Leaf -> singleton k [v]
    | Node (_, l, k', v', r) ->
      let c = O.compare k k' in
      if c < 0 then
        Bt2.node (add_to_list k v l) k' v' r
      else if c > 0 then
        Bt2.node l k' v' (add_to_list k v r)
      else
        Bt2.node l k (v :: v') r

  let to_list = bindings
  let of_list bs = List.fold_left (fun m (k, v) -> add k v m) empty bs

  let of_bt2 t =
    let rec validate k = function
      | Leaf -> k
      | Node (_, l, k', _, r) ->
        let k = validate k l in
        if O.compare k k' > 0 then
          raise Exit;
        validate k' r
    in
    let rec validate_fringe = function
      | Leaf -> None
      | Node (_, l, k', _, r) ->

        match validate_fringe l with
        | None -> Some (validate k' r)
        | Some k ->
          if O.compare k k' > 0 then
            raise Exit;
          Some (validate k' r)
    in
    match validate_fringe t with
    | (_ : key option) -> Some t
    | exception Exit -> None

  let rank = Bt2.rank

  let rank_of t k =
    let rec aux ofs k = function
      | Leaf -> None
      | Node (_, l, k', _, r) ->
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
