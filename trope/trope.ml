module O = Order_managed

include
  (struct
    type +'a cell = {offset: int; cursor: 'a}
    let make_cell offset cursor =
      assert (offset >= 0);
      {offset; cursor}
    let shift_cell n c =
      if n = 0 then c
      else make_cell (c.offset + n) c.cursor
  end : sig
    type +'a cell = private {offset: int; cursor: 'a}
    val make_cell : int -> 'a -> 'a cell
    val shift_cell : int -> 'a cell -> 'a cell
   end)

module T = Mbt.Make (struct
    type 'a measurable = 'a cell
    type measure = int
    let empty = 0
    let cat a b c = a + b.offset + c
  end)

type 'a t = {
  root: O.t;
  tree: 'a cursor T.t;
}

and 'a cursor = {
  content: 'a;
  position: O.t;
}

let create () = {
  root = O.root ();
  tree = T.leaf;
}

let content c = c.content

let validate t c msg =
  if not (O.same_order t.root c.position) then
    invalid_arg msg

let update t f =
  let tree, result = f t.tree in
  {t with tree}, result

let update' t f =
  {t with tree = f t.tree}

let clear t = {t with tree = T.leaf}

let compare a b =
  O.compare a.position b.position

let is_leaf = function
  | T.Leaf -> true
  | T.Node _ -> false

let is_empty t = is_leaf t.tree

let member t c =
  let rec aux = function
  | T.Leaf -> false
  | T.Node (_, l, cell, r, _) ->
    let o = compare c cell.cursor in
    if o = 0 then
      true
    else if o < 0 then
      aux l
    else
      aux r
  in
  aux t.tree

let position t c0 =
  let rec traverse n = function
    | T.Leaf -> raise Not_found
    | T.Node (_, l, cell, r, _) ->
      let o = compare c0 cell.cursor in
      if o < 0 then
        traverse n l
      else
        let n = n + T.measure l + cell.offset in
        if o > 0 then
          traverse n r
        else
          n
  in
  traverse 0 t.tree

let rec shift_tree n = function
  | T.Leaf -> T.leaf
  | T.Node (_, T.Leaf, cell, r, _) ->
    T.node T.leaf (shift_cell n cell) r
  | T.Node (_, l, cell, r, _) ->
    T.node (shift_tree n l) cell r

let shift_tree n tree = if n = 0 then tree else shift_tree n tree

let rem_cursor t c0 =
  let rec traverse = function
    | T.Leaf -> raise Not_found
    | T.Node (_, l, cell, r, _) ->
      let o = compare c0 cell.cursor in
      if o < 0 then
        let l, shift = traverse l in
        T.node l (shift_cell shift cell) r, 0
      else if o > 0 then
        let r, shift = traverse r in
        T.node l cell r, shift
      else if is_leaf r then
        l, cell.offset
      else
        T.join l (shift_tree cell.offset r), 0
  in
  fst (update t traverse)

let put_cursor t ~at content =
  if at < 0 then
    invalid_arg "Trope.put_cursor: [at] must be >= 0";
  let rec traverse before at = function
    | T.Leaf ->
      let cursor = {position = O.after before; content} in
      let cell = make_cell at cursor in
      T.node T.leaf cell T.leaf, cursor
    | T.Node (_, l, cell, r, _) ->
      let pos = T.measure l + cell.offset in
      if at < pos then
        let l, cursor = traverse before at l in
        T.node l (make_cell (pos - T.measure l) cell.cursor) r, cursor
      else
        let r, cursor = traverse cell.cursor.position (at - pos) r in
        T.node l cell r, cursor
  in
  update t (traverse t.root at)

let insert ?left_of t ~at ~len =
  if at < 0 then
    invalid_arg "Trope.insert: [at] must be >= 0";
  if len < 0 then
    invalid_arg "Trope.insert: [len] must be >= 0";
  let right = (left_of : unit option) = None in
  let rec aux n = function
    | T.Leaf -> T.leaf, len
    | T.Node (_, l, cell, r, _) ->
      let n0 = T.measure l + cell.offset in
      if (if right then n < n0 else n <= n0) then
        let l, shift = aux n l in
        T.node l (shift_cell shift cell) r, 0
      else
        let r, shift = aux (n - n0) r in
        T.node l cell r, shift
  in
  fst (update t (aux at))

let remove ?left_of t ~at ~len =
  if at < 0 then
    invalid_arg "Trope.remove: [at] must be >= 0";
  if len < 0 then
    invalid_arg "Trope.remove: [len] must be >= 0";
  if len = 0 then
    t
  else
    let rec rem len = function
      | T.Leaf -> T.leaf
      | T.Node (_, l, cell, r, _) ->
        let n0 = T.measure l + cell.offset in
        if len <= n0 then
          let l = rem len l in
          let cell = make_cell (n0 - len - T.measure l) cell.cursor in
          T.node l cell r
        else
          let len = len - n0 in
          let r = rem len r in
          r
    in
    let right = (left_of : unit option) = None in
    let rec aux n len = function
      | T.Leaf -> T.leaf
      | T.Node (_, l, cell, r, _) ->
        let n0 = T.measure l + cell.offset in
        if n + len <= n0 then
          let l = aux n len l in
          let cell = make_cell (n0 - len - T.measure l) cell.cursor in
          T.node l cell r
        else if (if right then n >= n0 else n > n0) then
          T.node l cell (aux (n - n0) len r)
        else (* Splitting case *)
          let l = aux n len l in
          let len = len - (n0 - n) in
          let r = shift_tree (n - T.measure l) (rem len r) in
          T.join l r
    in
    update' t (aux at len)

let remove_between t c1 c2 =
  validate t c1 "Trope.remove_between: cursor not in buffer";
  validate t c2 "Trope.remove_between: cursor not in buffer";
  if c1 == c2 then t
  else if compare c1 c2 > 0 then
    invalid_arg
      "Trope.remove_between: cursors must be in increaing order"
  else begin
    let rec cut_left = function
      | T.Leaf -> invalid_arg "Trope.remove_between: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let c1 = compare c1 cell.cursor in
        if c1 < 0 then
          cut_left l
        else if c1 > 0 then
          T.node l cell (cut_left r)
        else (* c1 = 0 *)
          T.node l cell T.leaf
    in
    let rec cut_right = function
      | T.Leaf -> invalid_arg "Trope.remove_between: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let c2 = compare c2 cell.cursor in
        if c2 > 0 then
          cut_right r
        else if c2 < 0 then
          T.node (cut_right l) cell r
        else (* c2 = 0 *)
          T.node T.leaf (make_cell 0 cell.cursor) r
    in
    let rec aux = function
      | T.Leaf -> invalid_arg "Trope.remove_between: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let c1 = compare c1 cell.cursor and c2 = compare c2 cell.cursor in
        if c2 < 0 then
          T.node (aux l) cell r
        else if c1 > 0 then
          T.node l cell (aux r)
        else if c1 = 0 then
          T.node l cell (cut_right r)
        else if c2 = 0 then
          T.node (cut_left l) (make_cell 0 cell.cursor) r
        else (* c1 < 0 && c2 > 0 *)
          T.join (cut_left l) (cut_right r)
    in
    update' t aux
  end

let remove_after t c len =
  validate t c "Trope.remove_after: cursor not in buffer";
  if len < 0 then
    invalid_arg "Trope.remove_after: len must be >= 0"
  else if len = 0 then
    t
  else
    let rec rem n = function
      | T.Leaf -> T.leaf, n
      | T.Node (_, l, cell, r, _) ->
        let n0 = T.measure l in
        let n1 = n0 + cell.offset in
        if n <= n0 then
          let l, shift = rem n l in
          T.node l (shift_cell (-shift) cell) r, 0
        else if n <= n1 then
          T.node T.leaf (make_cell (n1 - n) cell.cursor) r, 0
        else
          rem (n - n1) r
    in
    let rec seek = function
      | T.Leaf ->
        invalid_arg "Trope.remove_after: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let c = compare c cell.cursor in
        if c < 0 then
          let l, shift = seek l in
          if shift > cell.offset then
            let r, shift = rem (shift - cell.offset) r in
            T.join l r, shift
          else
            T.node l (shift_cell (-shift) cell) r, 0
        else
          let r, shift =
            if c = 0
            then rem len r
            else seek r
          in
          T.node l cell r, shift
    in
    fst (update t seek)

let remove_before t c len =
  validate t c "Trope.remove_before: cursor not in buffer";
  if len < 0 then
    invalid_arg "Trope.remove_before: len must be >= 0"
  else if len = 0 then
    t
  else
    let rec rem n = function
      | T.Leaf -> T.leaf
      | T.Node (_, l, cell, r, _) ->
        let n0 = T.measure r in
        if n <= n0 then
          let r = rem n r in
          T.node l cell r
        else
          let n = n - n0 - cell.offset in
          if n > 0 then
            rem n l
          else
            l
    in
    let rec seek = function
      | T.Leaf ->
        invalid_arg "Trope.remove_before: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let c = compare c cell.cursor in
        if c < 0 then
          T.node (seek l) cell r
        else if c = 0 then
          if len <= cell.offset then
            let cell = shift_cell (-len) cell in
            T.node l cell r
          else
            let len = len - cell.offset in
            let n0 = T.measure l in
            if len <= n0 then
              let l = rem len l in
              T.node l (make_cell (n0 - T.measure l - len) cell.cursor) r
            else
              T.node T.leaf (make_cell 0 cell.cursor) r
        else
          let n0 = T.measure r in
          let r = seek r in
          let len = len - (n0 - T.measure r) in
          assert (len >= 0);
          if len = 0 then
            T.node l cell r
          else if len <= cell.offset then
            T.join l (shift_tree (cell.offset - len) r)
          else
            let len = len - cell.offset in
            let n0 = T.measure l in
            if len <= n0 then
              let l = rem len l in
              T.join l (shift_tree (n0 - T.measure l - len) r)
            else
              r
    in
    update' t seek

let insert_before t c len =
  validate t c "Trope.insert_before: cursor not in buffer";
  if len < 0 then
    invalid_arg "Trope.insert_before: len must be >= 0"
  else if len = 0 then
    t
  else
    let rec aux = function
      | T.Leaf -> invalid_arg "Trope.insert_before: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let c = compare c cell.cursor in
        if c < 0 then
          T.node (aux l) cell r
        else if c > 0 then
          T.node l cell (aux r)
        else
          T.node l (shift_cell len cell) r
    in
    update' t aux

let insert_after t c len =
  validate t c "Trope.insert_after: cursor not in buffer";
  if len < 0 then
    invalid_arg "Trope.insert_after: len must be >= 0"
  else if len = 0 then
    t
  else
    let rec aux = function
      | T.Leaf -> invalid_arg "Trope.insert_after: cursor not in buffer"
      | T.Node (_, l, cell, r, _) as tree ->
        let c = compare c cell.cursor in
        if c < 0 then
          let l, shift = aux l in
          T.node l (shift_cell shift cell) r, 0
        else if c > 0 then
          let r, shift = aux r in
          T.node l cell r, shift
        else if is_leaf r then
          tree, len
        else
          T.node l cell (shift_tree len r), 0
    in
    fst (update t aux)

let put_before t c0 content =
  validate t c0 "Trope.put_before: cursor not in buffer";
  let aux t =
    let c = {position = O.before c0.position; content} in
    let rec aux = function
      | T.Leaf -> invalid_arg "Trope.put_before: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let i = compare c0 cell.cursor in
        if i = 0 then
          T.node
            (T.node l (make_cell cell.offset c) T.leaf)
            (make_cell 0 cell.cursor) r
        else if i < 0 then
          T.node (aux l) cell r
        else
          T.node l cell (aux r)
    in
    aux t, c
  in
  update t aux

let put_after t c0 content =
  validate t c0 "Trope.put_after: cursor not in buffer";
  let aux t =
    let c = {position = O.after c0.position; content} in
    let rec aux = function
      | T.Leaf -> invalid_arg "Trope.put_after: cursor not in buffer"
      | T.Node (_, l, cell, r, _) ->
        let i = compare c0 cell.cursor in
        if i = 0 then
          T.node (T.node l cell T.leaf) (make_cell 0 c) r
        else if i < 0 then
          T.node (aux l) cell r
        else
          T.node l cell (aux r)
    in
    aux t, c
  in
  update t aux

let put_back t c =
  validate t c "Trope.put_back: cursor not in buffer";
  let rec aux = function
    | T.Leaf -> T.node T.leaf (make_cell 0 c) T.leaf
    | T.Node (_, l, cell, r, _) ->
      let i = compare c cell.cursor in
      if i = 0 then raise Exit
      else if i < 0 then
        T.node (aux l) cell r
      else
        T.node l cell (aux r)
  in
  try update' t aux
  with Exit -> t

let find_before t n =
  let rec aux n = function
    | T.Leaf -> None
    | T.Node (_, l, cell, r, _) ->
      let n0 = T.measure l + cell.offset in
      if n < n0 then
        aux n l
      else match aux (n - n0) r with
        | Some _ as result -> result
        | None -> Some cell.cursor
  in
  aux n t.tree

let find_after t n =
  let rec aux n = function
    | T.Leaf -> None
    | T.Node (_, l, cell, r, _) ->
      let n0 = T.measure l in
      if n <= n0 && not (is_leaf l) then
        aux n l
      else
        let n1 = n0 + cell.offset in
        if n <= n1 then
          Some cell.cursor
        else
          aux (n - n1) r
  in
  aux n t.tree

let cursor_before t c =
  validate t c "Trope.cursor_before: cursor not in buffer";
  let rec aux = function
    | T.Leaf -> None
    | T.Node (_, l, cell, r, _) ->
      let c = compare c cell.cursor in
      if c <= 0 then
        aux l
      else match aux r with
        | Some _ as result -> result
        | None -> Some cell.cursor
  in
  aux t.tree

let cursor_after t c =
  validate t c "Trope.cursor_after: cursor not in buffer";
  let rec aux = function
    | T.Leaf -> None
    | T.Node (_, l, cell, r, _) ->
      let c = compare c cell.cursor in
      if c >= 0 then
        aux r
      else match aux l with
        | Some _ as result -> result
        | None -> Some cell.cursor
  in
  aux t.tree

let to_list t =
  let rec aux acc n = function
    | T.Leaf -> acc
    | T.Node (_, l, cell, r, _) ->
      let n' = n + T.measure l + cell.offset in
      let acc = aux acc n' r in
      aux ((n', cell.cursor) :: acc) n l
  in
  aux [] 0 t.tree
