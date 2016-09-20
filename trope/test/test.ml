let () = Random.self_init ()

module type S = module type of Trope

module B0 : S = Trope
module B1 : S = Reference

let failed = ref false

let check b0 b1 =
  let rec check c0 c1 =
    match c0, c1 with
    | Some (c0, v0), None ->
      let p = B0.position b0 c0 in
      failed := true;
      Printf.eprintf "KO cursor %d at %d is missing in b1\n"
        (B0.find b0 c0) p
    | None, Some (c1, v1) ->
      let p = B1.position b1 c1 in
      failed := true;
      Printf.eprintf "KO cursor %d at %d is missing in b0\n"
        (B1.find b1 c1) p
    | None, None -> ()
    | Some (c0, v0), Some (c1, v1) ->
      let i0 = B0.find b0 c0 and i1 = B1.find b1 c1 in
      if i0 <> i1 then
        (failed := true;
         Printf.eprintf "KO found cursor %d in b0 and %d in b1\n" i0 i1)
      else
        let p0 = B0.position b0 c0 and p1 = B1.position b1 c1 in
        if p0 <> p1 then
          (failed := true;
           Printf.eprintf "KO cursor %d has position %d in b0 and %d in b1\n"
             (B0.find b0 c0) p0 p1)
        else
          check (B0.seek_after b0 c0) (B1.seek_after b1 c1)
  in
  check (B0.find_after b0 0) (B1.find_after b1 0);
  if !failed then
    let b0 = List.map (fun (n,c,v) -> B0.find b0 c, n, v) (B0.to_list b0) in
    let b1 = List.map (fun (n,c,v) -> B1.find b1 c, n, v) (B1.to_list b1) in
    let failed (i,p,i') = Printf.sprintf "[%d=%d?]:%d" i i' p in
    Printf.eprintf "B0: %s\nB1: %s\n"
      (String.concat " " (List.map failed b0))
      (String.concat " " (List.map failed b1))

module IntMap = Map.Make (struct
    type t = int
    let compare : int -> int -> int = compare
  end)

let cursor_table
  : (B0.cursor * B1.cursor) IntMap.t ref
  = ref IntMap.empty

let cursor_counter = ref 0

let find_cursor i =
  IntMap.find i !cursor_table

let pick_cursor () =
  let x = Random.int !cursor_counter in
  let l, x', r = IntMap.split x !cursor_table in
  match x' with
  | Some _ -> x
  | None ->
    try
      let (k, _) =
        if Random.bool () then
          IntMap.max_binding l
        else
          IntMap.min_binding l
      in
      k
    with Not_found ->
    try fst (IntMap.choose !cursor_table)
    with Not_found ->
      assert false

let fresh_cursors f =
  let sym = !cursor_counter in
  incr cursor_counter;
  let x = f sym in
  if !failed then
    Printf.eprintf "Created cursor [%d]\n" sym;
  cursor_table := IntMap.add sym (fst x) !cursor_table;
  x

type 'cursor op =
  | Clear
  | Remove     of int * int
  | Insert     of int * int
  | Is_member  of 'cursor
  | Compare    of 'cursor * 'cursor
  | Position   of 'cursor
  | Put_cursor of int
  | Put_left   of [`After | `Before] * 'cursor
  | Put_right  of [`After | `Before] * 'cursor
  | Rem_cursor of 'cursor
  | Remove_between of 'cursor * 'cursor
  | Remove_before  of 'cursor * int
  | Remove_after   of 'cursor * int
  | Insert_before  of 'cursor * int
  | Insert_after   of 'cursor * int
  | Find_before    of int
  | Find_after     of int
  | Seek_before  of 'cursor
  | Seek_after   of 'cursor

let size b0 b1 =
  match
    B0.find_before b0 max_int,
    B1.find_before b1 max_int
  with
  | None, None -> 0
  | Some (c, _), None ->
    failed := true;
    Printf.eprintf "KO b1 empty but not b0 (= %d)\n" (B0.position b0 c);
    0
  | None, Some (c, _) ->
    failed := true;
    Printf.eprintf "KO b0 empty but not b1 (= %d)\n" (B1.position b1 c);
    0
  | Some (c0, _), Some (c1, _) ->
    let i0 = B0.find b0 c0 and i1 = B1.find b1 c1 in
    if i0 <> i1 then
      (failed := true;
       Printf.eprintf "KO last cursor in b0 is %d but last in b1 is %d\n" i0 i1);
    let p0 = B0.position b0 c0 and p1 = B1.position b1 c1 in
    if p0 <> p1 then
      (failed := true;
       Printf.eprintf "KO cursor %d has position %d in b0 and %d in b1\n"
        (B0.find b0 c0) p0 p1);
    p0

let rand n = Random.int n

let rand_unlikely n =
  let r = int_of_float (Random.float (float n ** 2.0) ** (1.0/.2.0)) in
  if r >= n then n - 1 else r

let rand_relative () =
  if rand 2 = 0 then `After else `Before

let random_op b0 b1 =
  match size b0 b1 with
  | 0 -> Put_cursor (Random.int 256)
  | sz ->
    if !failed then
      Printf.eprintf "size: %d\n" sz;
    let sz = sz * 11 / 10 in
    match rand_unlikely 19 with
    | 0  -> Position       (pick_cursor ())
    | 1  -> Compare        (pick_cursor (), pick_cursor ())
    | 2  -> Is_member      (pick_cursor ())
    | 3  -> Seek_after   (pick_cursor ())
    | 4  -> Seek_before  (pick_cursor ())
    | 5  -> Find_after     (rand sz)
    | 6  -> Find_before    (rand sz)
    | 7  -> Insert_after   (pick_cursor (), rand sz)
    | 8  -> Remove_before  (pick_cursor (), rand sz)
    | 9  -> Insert_before  (pick_cursor (), rand sz)
    | 10 -> Remove_after   (pick_cursor (), rand sz)
    | 11 -> Remove_between (pick_cursor (), pick_cursor ())
    | 12 -> Rem_cursor     (pick_cursor ())
    | 13 -> Put_left       (rand_relative (), pick_cursor ())
    | 14 -> Put_right      (rand_relative (), pick_cursor ())
    | 15 -> Put_cursor     (rand sz)
    | 16 -> Insert (rand sz, rand_unlikely sz)
    | 17 -> Remove (rand sz, rand_unlikely sz)
    | 18 -> Clear
    | _ -> assert false

let str_rel = function
  | `After  -> "`After"
  | `Before -> "`Before"

let string_of_op = function
  | Clear                  -> "Clear"
  | Position       c       -> Printf.sprintf "Position ([%d])" c
  | Compare        (c1,c2) -> Printf.sprintf "Compare ([%d], [%d])" c1 c2
  | Is_member      c       -> Printf.sprintf "Is_member ([%d])" c
  | Seek_after     c       -> Printf.sprintf "Seek_after ([%d])" c
  | Seek_before    c       -> Printf.sprintf "Seek_before ([%d])" c
  | Find_after     n       -> Printf.sprintf "Find_after (%d)" n
  | Find_before    n       -> Printf.sprintf "Find_before (%d)" n
  | Insert_after   (c,n)   -> Printf.sprintf "Insert_after ([%d], %d)" c n
  | Remove_before  (c,n)   -> Printf.sprintf "Remove_before ([%d], %d)" c n
  | Insert_before  (c,n)   -> Printf.sprintf "Insert_before ([%d], %d)" c n
  | Remove_after   (c,n)   -> Printf.sprintf "Remove_after ([%d], %d)" c n
  | Remove_between (c1,c2) -> Printf.sprintf "Remove_between ([%d], [%d])" c1 c2
  | Rem_cursor     c       -> Printf.sprintf "Rem_cursor ([%d])" c
  | Put_left       (d,c)   -> Printf.sprintf "Put_left (%s,[%d])" (str_rel d) c
  | Put_right      (d,c)   -> Printf.sprintf "Put_right (%s,[%d])" (str_rel d) c
  | Put_cursor     n       -> Printf.sprintf "Put_cursor (%d)" n
  | Insert         (n1,n2) -> Printf.sprintf "Insert (%d,%d)" n1 n2
  | Remove         (n1,n2) -> Printf.sprintf "Remove (%d,%d)" n1 n2

let get_cursor b0 b1 c f =
  let c0, c1 = find_cursor c in
  match B0.member b0 c0, B1.member b1 c1 with
  | false, false ->
    cursor_table := IntMap.remove c !cursor_table;
    if !failed then
      Printf.eprintf "OK cursor [%d] vanished\n" c;
    None
  | true, true ->
    f c0 c1
  | false, true | true, false ->
    failed := true;
    Printf.eprintf "KO cursor [%d] only in one of the two buffers\n" c;
    None

let string_of_ord c =
  if c = 0 then
    "="
  else if c < 0 then
    "<"
  else
    ">"

let check_cursor b0 b1 ctx = function
  | None, None -> ()
  | Some c0, None ->
    failed := true;
    Printf.eprintf "KO %s: cursor [%d] exists only in b0\n" ctx (B0.find b0 c0)
  | None, Some c1 ->
    failed := true;
    Printf.eprintf "KO %s: cursor [%d] exists only in b1\n" ctx (B1.find b1 c1)
  | Some c0, Some c1 ->
    let i0 = B0.find b0 c0 and i1 = B1.find b1 c1 in
    if i0 <> i1 then
      (failed := true;
       Printf.eprintf "KO %s: cursor in b0 is %d but in b1 is %d\n" ctx i0 i1)
    else
      let p0 = B0.position b0 c0 and p1 = B1.position b1 c1 in
      if p0 <> p1 then
        (failed := true;
         Printf.eprintf "KO %s: cursor [%d] has position %d in b0 and %d in b1\n"
           ctx i0 p0 p1)

let check_cursor' b0 b1 ctx (c0, c1) =
  check_cursor b0 b1 ctx (Some c0, Some c1)

let check_cursor'' b0 b1 ctx (c0, c1) =
  let prj = function
    | None -> None
    | Some (c, v) -> Some c
  in
  check_cursor b0 b1 ctx (prj c0, prj c1)

let apply b0 b1 : int op -> _ = function
  | Clear                  ->
    Some (B0.clear b0, B1.clear b1)
  | Position       c       ->
    let c0, c1 = find_cursor c in
    let p0 = try Some (B0.position b0 c0) with Not_found -> None in
    let p1 = try Some (B1.position b1 c1) with Not_found -> None in
    begin match p0, p1 with
      | None, None -> ()
      | Some p0, None ->
        failed := true;
        Printf.eprintf "KO cursor [%d] exists only in b0 at position %d\n" c p0
      | None, Some p1 ->
        failed := true;
        Printf.eprintf "KO cursor [%d] exists only in b1 at position %d\n" c p1
      | Some p0, Some p1 ->
        if p0 <> p1 then
          (failed := true;
           Printf.eprintf "KO cursor [%d] has position %d in b0 and %d in b1\n" c p0 p1)
    end;
    None

  | Compare        (a,b) ->
    get_cursor b0 b1 a @@ fun a0 a1 ->
    check_cursor' b0 b1 "Compare (a, _)" (a0, a1);
    get_cursor b0 b1 b @@ fun c0 c1 ->
    check_cursor' b0 b1 "Compare (_, a)" (c0, c1);
    let e0 = string_of_ord (B0.compare a0 c0) in
    let e1 = string_of_ord (B1.compare a1 c1) in
    if e0 <> e1 then
      (failed := true;
       Printf.eprintf "KO [%d] %s [%d] in b0, but [%d] %s [%d] in b1\n"
         a e0 b a e1 b);
    None
  | Is_member      c       ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    None

  | Seek_after   c   ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor'' b0 b1 "Seek_after"
      (B0.seek_after b0 c0, B1.seek_after b1 c1);
    None

  | Seek_before  c   ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor'' b0 b1 "Seek_after"
      (B0.seek_before b0 c0, B1.seek_before b1 c1);
    None

  | Find_after     n   ->
    check_cursor'' b0 b1 "Find_after"
      (B0.find_after b0 n, B1.find_after b1 n);
    None

  | Find_before    n   ->
    check_cursor'' b0 b1 "Find_before"
      (B0.find_before b0 n, B1.find_before b1 n);
    None

  | Insert_after   (c,n)   ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Insert_after" (c0, c1);
    Some (B0.insert_after b0 c0 n,
          B1.insert_after b1 c1 n)

  | Remove_before  (c,n)   ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Remove_Before" (c0, c1);
    Some (B0.remove_before b0 c0 n,
          B1.remove_before b1 c1 n)

  | Insert_before  (c,n)   ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Insert_Before" (c0, c1);
    Some (B0.insert_before b0 c0 n,
          B1.insert_before b1 c1 n)

  | Remove_after   (c,n)   ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Remove_after" (c0, c1);
    Some (B0.remove_after b0 c0 n,
          B1.remove_after b1 c1 n)

  | Remove_between (a,b) ->
    get_cursor b0 b1 a @@ fun a0 a1 ->
    check_cursor' b0 b1 "Remove_between (a, _)" (a0, a1);
    get_cursor b0 b1 b @@ fun c0 c1 ->
    check_cursor' b0 b1 "Remove_between (_, a)" (c0, c1);
    if B0.compare a0 c0 <= 0 then
      Some (B0.remove_between b0 a0 c0,
            B1.remove_between b1 a1 c1)
    else
      None

  | Rem_cursor     c       ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Rem_cursor" (c0, c1);
    Some (B0.rem_cursor b0 c0,
          B1.rem_cursor b1 c1)

  | Put_left      (dir,c) ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Put_left" (c0, c1);
    let c0 = (if dir = `After then B0.cursor_after else B0.cursor_before) c0 in
    let c1 = (if dir = `After then B1.cursor_after else B1.cursor_before) c1 in
    let _, result = fresh_cursors @@ fun name ->
      let b0 = B0.put_left b0 c0 name in
      let b1 = B1.put_left b1 c1 name in
      (c0, c1), (b0, b1)
    in
    Some result

  | Put_right     (dir,c) ->
    get_cursor b0 b1 c @@ fun c0 c1 ->
    check_cursor' b0 b1 "Put_right" (c0, c1);
    let c0 = (if dir = `After then B0.cursor_after else B0.cursor_before) c0 in
    let c1 = (if dir = `After then B1.cursor_after else B1.cursor_before) c1 in
    let _, result = fresh_cursors @@ fun name ->
      let b0 = B0.put_right b0 c0 name in
      let b1 = B1.put_right b1 c1 name in
      (c0, c1), (b0, b1)
    in
    Some result

  | Put_cursor     n       ->
    let _, result = fresh_cursors @@ fun name ->
      let b0, c0 = B0.put_cursor b0 ~at:n name in
      let b1, c1 = B1.put_cursor b1 ~at:n name in
      (c0, c1), (b0, b1)
    in
    Some result

  | Insert         (n1,n2) ->
    Some (B0.insert b0 ~at:n1 ~len:n2,
          B1.insert b1 ~at:n1 ~len:n2)

  | Remove         (n1,n2) ->
    Some (B0.remove b0 ~at:n1 ~len:n2,
          B1.remove b1 ~at:n1 ~len:n2)

let max_steps =
  try
    if Array.length Sys.argv < 2 then raise Not_found;
    int_of_string Sys.argv.(1)
  with _ -> 0

let rec loop n b0 b1 =
  let failed' = !failed in
  if max_steps = 0 || (not failed' && n <> max_steps) then
    let op = random_op b0 b1 in
    if !failed then
      Printf.eprintf "STEP %d: applying %s\n" n (string_of_op op);
    match apply b0 b1 op with
    | None ->
      if !failed <> failed' then
        Printf.eprintf "STEP %d FAILED: applying %s\n" n (string_of_op op);
      loop (n + 1) b0 b1
    | Some (b0, b1) ->
      check b0 b1;
      if !failed <> failed' then
        Printf.eprintf "STEP %d FAILED: applying %s\n" n (string_of_op op);
      loop (n + 1) b0 b1

let () =
  loop 0 (B0.create ()) (B1.create ());
  if !failed then exit 1
