module O = Order_managed

type 'a t = {
  root: O.t;
  cursors: (int * 'a cursor) list;
}

and 'a cursor = {
  content: 'a;
  position: O.t;
}

let create () = {
  root = O.root ();
  cursors = [];
}

let content c = c.content

let validate t c =
  assert (O.same_order t.root c.position)

let update t f =
  {t with cursors = f t.cursors}

let clear t = {t with cursors = []}

let is_empty t = t.cursors = []

let member t c = List.exists (fun (_,c') -> c == c') t.cursors

let position t c =
  validate t c;
  let rec aux n = function
    | [] -> raise Not_found
    | (n',c') :: xs ->
      let n = n + n' in
      if c == c' then n
      else aux n xs
  in
  aux 0 t.cursors

let compare c1 c2 =
  O.compare c1.position c2.position

let remove t ~at ~len =
  assert (at >= 0);
  assert (len >= 0);
  let rec skip_right shift len = function
    | [] -> []
    | (n, c) :: xs when len <= n ->
      (shift + n - len, c) :: xs
    | (n, c) :: xs ->
      skip_right shift (len - n) xs
  in
  let rec skip_left at = function
    | [] -> []
    | (n,c) :: xs when at < n ->
      skip_right at len ((n - at, c) :: xs)
    | (n,_ as cell) :: xs ->
      cell :: skip_left (at - n) xs
  in
  update t (skip_left at)


let insert t ~at ~len =
  assert (at >= 0);
  assert (len >= 0);
  let rec shift at = function
    | [] -> []
    | (n,c) :: xs when at < n ->
      (n + len, c) :: xs
    | (n,_ as cell) :: xs ->
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
      | (_, c) :: xs when c == c2 ->
        (0, c) :: xs
      | (_, c) :: xs ->
        drop xs
    in
    let rec skip = function
      | [] -> assert false
      | (n, c) :: xs when c == c1 ->
        (n, c) :: drop xs
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
    | (_, c') :: xs when c == c' ->
      (max 0 n, c') :: xs
    | _ :: xs -> clean n xs
  in
  let rec seek pos = function
    | [] -> assert false
    | (n,c') :: _ as tail when pos < n || (c == c' && pos = n) ->
      clean pos tail
    | x :: xs ->
      x :: seek (pos - fst x) xs
  in
  update t (seek at)

let remove_after t c len =
  validate t c;
  assert (len >= 0);
  let rec drop pos = function
    | [] -> []
    | (n, c') :: xs when pos <= n ->
      (n - pos, c') :: xs
    | (n, c') :: xs ->
      drop (pos - n) xs
  in
  let rec shift = function
    | [] -> assert false
    | (_,c' as cell) :: xs when c == c' ->
      cell :: drop len xs
    | cell :: xs -> cell :: shift xs
  in
  update t shift

let insert_before t c len =
  validate t c;
  assert (len >= 0);
  let rec shift = function
    | [] -> assert false
    | (n,c') :: xs when c == c' ->
      (n + len, c) :: xs
    | cell :: xs -> cell :: shift xs
  in
  update t shift

let insert_after t c len =
  validate t c;
  assert (len >= 0);
  let rec shift = function
    | [] -> assert false
    | (_,c' as cell) :: xs when c == c' ->
      cell :: (match xs with
          | [] -> []
          | (n, c'') :: xs' -> (n + len, c'') :: xs'
        )
    | cell :: xs -> cell :: shift xs
  in
  update t shift

let put_cursor t ~at content =
  assert (at >= 0);
  let rcursor = ref None in
  let rec aux p at = function
    | [] ->
      let cursor = {position = O.after p; content} in
      rcursor := Some cursor;
      [(at, cursor)]
    | (n, c) :: xs when at < n ->
      let cursor = {position = O.after p; content} in
      rcursor := Some cursor;
      (at, cursor) :: (n - at, c) :: xs
    | (n, c as cell) :: xs ->
      cell :: aux c.position (at - n) xs
  in
  let t = update t (aux t.root at) in
  match !rcursor with
  | None -> assert false
  | Some c -> t, c

let rem_cursor t c0 =
  validate t c0;
  let rec aux = function
    | [] -> assert false
    | [(_,c)] when c == c0 -> []
    | (n,c) :: (n',c') :: xs when c == c0 ->
      (n + n', c') :: xs
    | x :: xs ->
      x :: aux xs
  in
  update t aux

let put_before t c0 content =
  validate t c0;
  let c = {position = O.before c0.position; content} in
  let rec aux = function
    | [] -> assert false
    | (n, c0') :: xs when c0 == c0' ->
      (n, c) :: (0, c0') :: xs
    | x :: xs ->
      x :: aux xs
  in
  update t aux, c

let put_after t c0 content =
  validate t c0;
  let c = {position = O.after c0.position; content} in
  let rec aux = function
    | [] -> assert false
    | (_, c0' as cell) :: xs when c0 == c0' ->
      cell :: (0, c) :: xs
    | x :: xs ->
      x :: aux xs
  in
  update t aux, c

let find_before t n =
  let rec aux n c = function
    | [] -> c
    | (n', _) :: _ when n' > n ->
      c
    | (n', c) :: xs ->
      aux (n - n') c xs
  in
  match t.cursors with
  | (n', c) :: xs when n >= n' ->
    Some (aux (n - n') c xs)
  | _ -> None

let find_after t n =
  let rec aux n = function
    | [] -> None
    | (n', c) :: _ when n <= n' -> Some c
    | (n', _) :: xs ->
      aux (n - n') xs
  in
  aux n t.cursors

let cursor_before t c =
  validate t c;
  let rec aux = function
    | [] -> assert false
    | (_, c') :: (_, c0) :: _ when c == c0 -> Some c'
    | _ :: xs -> aux xs
  in
  match t.cursors with
  | (_, c') :: _ when c == c' -> None
  | l -> aux l

let cursor_after t c =
  validate t c;
  let rec aux = function
    | [] -> assert false
    | [(_, c0)] when c == c0 -> None
    | (_, c0) :: (_, c') :: _ when c == c0 -> Some c'
    | _ :: xs -> aux xs
  in
  aux t.cursors

let to_list t =
  let rec aux n = function
    | [] -> []
    | (n', c) :: xs ->
      let n = n' + n in
      (n, c) :: aux n xs
  in
  aux 0 t.cursors
