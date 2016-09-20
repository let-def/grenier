module O = Order_managed

type cursor = O.t

type 'a t = {
  root: O.t;
  cursors: (int * cursor * 'a) list;
}

let create () = {
  root = O.root ();
  cursors = [];
}

let validate t c =
  assert (O.same_order t.root c)

let update t f =
  {t with cursors = f t.cursors}

let clear t = {t with cursors = []}

let is_empty t = t.cursors = []

let member t c = List.exists (fun (_,c',_) -> c == c') t.cursors

let find t c =
  let (_, _, v) = List.find (fun (_,c',_) -> c == c') t.cursors in
  v

let position t c =
  validate t c;
  let rec aux n = function
    | [] -> raise Not_found
    | (n',c',_) :: xs ->
      let n = n + n' in
      if c == c' then n
      else aux n xs
  in
  aux 0 t.cursors

let compare = O.compare

let remove ?left_of t ~at ~len =
  assert (at >= 0);
  assert (len >= 0);
  let rec skip_right shift len = function
    | [] -> []
    | (n, c, v) :: xs when len <= n ->
      (shift + n - len, c, v) :: xs
    | (n, c, _) :: xs ->
      skip_right shift (len - n) xs
  in
  let rec skip_left at = function
    | [] -> []
    | (n, c, v) :: xs when at = n && (left_of <> None) ->
      skip_right at len xs
    | (n, c, v) :: xs when at < n ->
      skip_right at len ((n - at, c, v) :: xs)
    | (n, _, _ as cell) :: xs ->
      cell :: skip_left (at - n) xs
  in
  update t (skip_left at)


let insert ?left_of t ~at ~len =
  assert (at >= 0);
  assert (len >= 0);
  let rec shift at = function
    | [] -> []
    | (n, c, v) :: xs when at < n || (at = n && left_of <> None) ->
      (n + len, c,v) :: xs
    | (n, _, _ as cell) :: xs ->
      cell :: shift (at - n) xs
  in
  update t (shift at)

let remove_between t c1 c2 =
  validate t c1;
  validate t c2;
  if c1 == c2 then t
  else begin
    assert (compare c1 c2 < 0);
    let rec drop = function
      | [] -> assert false
      | (_, c, v) :: xs when c == c2 ->
        (0, c, v) :: xs
      | (_, c, _) :: xs ->
        drop xs
    in
    let rec skip = function
      | [] -> assert false
      | (_, c, _ as cell) :: xs when c == c1 ->
        cell :: drop xs
      | x :: xs ->
        x :: skip xs
    in
    update t skip
  end

let remove_before t c len =
  validate t c;
  assert (len >= 0);
  let at = position t c - len in
  assert (member t c);
  let rec clean n = function
    | [] -> assert false
    | (_, c', v) :: xs when c == c' ->
      (max 0 n, c', v) :: xs
    | _ :: xs -> clean n xs
  in
  let rec seek pos = function
    | [] -> assert false
    | (n, c', _) :: _ as tail when pos < n || (c == c' && pos = n) ->
      clean pos tail
    | (n, _, _ as x) :: xs ->
      x :: seek (pos - n) xs
  in
  update t (seek at)

let remove_after t c len =
  validate t c;
  assert (len >= 0);
  let rec drop pos = function
    | [] -> []
    | (n, c', v) :: xs when pos <= n ->
      (n - pos, c', v) :: xs
    | (n, c', _) :: xs ->
      drop (pos - n) xs
  in
  let rec shift = function
    | [] -> assert false
    | (_, c', _ as cell) :: xs when c == c' ->
      cell :: drop len xs
    | cell :: xs -> cell :: shift xs
  in
  update t shift

let insert_before t c len =
  validate t c;
  assert (len >= 0);
  let rec shift = function
    | [] -> assert false
    | (n, c', v) :: xs when c == c' ->
      (n + len, c, v) :: xs
    | cell :: xs -> cell :: shift xs
  in
  update t shift

let insert_after t c len =
  validate t c;
  assert (len >= 0);
  let rec shift = function
    | [] -> assert false
    | (_, c', _ as cell) :: xs when c == c' ->
      cell :: (match xs with
          | [] -> []
          | (n, c'', v) :: xs' -> (n + len, c'', v) :: xs'
        )
    | cell :: xs -> cell :: shift xs
  in
  update t shift

let put_cursor t ~at value =
  assert (at >= 0);
  let rcursor = ref None in
  let rec aux p at = function
    | [] ->
      let cursor = O.after p in
      rcursor := Some cursor;
      [(at, cursor, value)]
    | (n, c, v) :: xs when at < n ->
      let cursor = O.after p in
      rcursor := Some cursor;
      (at, cursor, value) :: (n - at, c, v) :: xs
    | (n, c, _ as cell) :: xs ->
      cell :: aux c (at - n) xs
  in
  let t = update t (aux t.root at) in
  match !rcursor with
  | None -> assert false
  | Some c -> t, c

let rem_cursor t c0 =
  validate t c0;
  let rec aux = function
    | [] -> assert false
    | [(_, c, _)] when c == c0 -> []
    | (n, c, _) :: (n', c', v') :: xs when c == c0 ->
      (n + n', c', v') :: xs
    | x :: xs ->
      x :: aux xs
  in
  update t aux

let put_left t c0 v0 =
  validate t c0;
  let rec aux = function
    | [] -> [(0, c0, v0)]
    | ((n, c, _ as x) :: xs) as tail ->
      let o = O.compare c0 c in
      if o = 0 then (n, c, v0) :: xs
      else if o < 0 then
        (0, c0, v0) :: tail
      else
        x :: aux xs
  in
  update t aux

let put_right t c0 v0 =
  validate t c0;
  let rec aux = function
    | [] -> [(0, c0, v0)]
    | (n, c, v as x) :: xs ->
      let o = O.compare c0 c in
      if o = 0 then (n, c, v0) :: xs
      else if o < 0 then
        (n, c0, v0) :: (0, c, v) :: xs
      else
        x :: aux xs
  in
  update t aux

let cursor_after = O.after

let cursor_before = O.before

let cursor_at_origin t = O.after t.root

let find_before t n =
  let rec aux n c v = function
    | [] -> (c, v)
    | (n', _, _) :: _ when n' > n ->
      (c, v)
    | (n', c, v) :: xs ->
      aux (n - n') c v xs
  in
  match t.cursors with
  | (n', c, v) :: xs when n >= n' ->
    Some (aux (n - n') c v xs)
  | _ -> None

let find_after t n =
  let rec aux n = function
    | [] -> None
    | (n', c, v) :: _ when n <= n' -> Some (c, v)
    | (n', _, _) :: xs ->
      aux (n - n') xs
  in
  aux n t.cursors

let seek_before t c =
  validate t c;
  let rec aux = function
    | [] -> assert false
    | (_, c', v) :: (_, c0, _) :: _ when c == c0 -> Some (c', v)
    | _ :: xs -> aux xs
  in
  match t.cursors with
  | (_, c', _) :: _ when c == c' -> None
  | l -> aux l

let seek_after t c =
  validate t c;
  let rec aux = function
    | [] -> assert false
    | [(_, c0, _)] when c == c0 -> None
    | (_, c0, _) :: (_, c', v) :: _ when c == c0 -> Some (c', v)
    | _ :: xs -> aux xs
  in
  aux t.cursors

let to_list t =
  let rec aux n = function
    | [] -> []
    | (n', c, v) :: xs ->
      let n = n' + n in
      (n, c, v) :: aux n xs
  in
  aux 0 t.cursors
