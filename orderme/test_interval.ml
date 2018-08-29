
let random_action () =
  match Random.int 12 with
  | 0 -> (* prerr_endline "Before";          *) `Before
  | 1 -> (* prerr_endline "After";           *) `After
  | 2 -> (* prerr_endline "Inside";          *) `Inside
  | 3 -> (* prerr_endline "Outside";         *) `Outside
  | 4 -> (* prerr_endline "Forget";          *) `Forget
  | 5 -> (* prerr_endline "Replace_before";  *) `Replace_before
  | 6 -> (* prerr_endline "Replace_after";   *) `Replace_after
  | 7 -> (* prerr_endline "Replace_inside";  *) `Replace_inside
  | 8 -> (* prerr_endline "Replace_outside"; *) `Replace_outside
  | 9 | 10 | 11 -> `None
  | _ -> assert false

module type S = module type of Order_managed_interval

type 'a tree = Node of 'a * 'a tree list

module Test (M : S) =
struct
  let process gc check name =
    let rec apply acc (Node (x,xs)) = function
      | `None   ->
        Node (x, sub xs) :: acc
      | `Before ->
        Node (x, sub xs) :: Node (M.before x, []) :: acc
      | `After  ->
        Node (M.after x, []) :: Node (x, sub xs) :: acc
      | `Inside ->
        Node (x, [Node (M.inside x, sub xs)]) :: acc
      | `Outside  ->
        Node (M.outside x, [Node (x, sub xs)]) :: acc
      | `Replace_before ->
        if gc then
          pass (Node (M.before x, []) :: acc) xs
        else begin
          let result = M.before x in
          M.forget x;
          pass (Node (result, []) :: acc) xs
        end
      | `Replace_after  ->
        if gc then
          Node (M.after x, []) :: pass acc xs
        else begin
          let result = M.after x in
          M.forget x;
          Node (result, []) :: pass acc xs
        end
      | `Replace_inside ->
        if gc then
          Node (M.inside x, sub xs) :: acc
        else begin
          let result = M.inside x in
          M.forget x;
          Node (result, sub xs) :: acc
        end
      | `Replace_outside  ->
        if gc then
          Node (M.outside x, sub xs) :: acc
        else begin
          let result = M.outside x in
          M.forget x;
          Node (result, sub xs) :: acc
        end
      | `Forget ->
        if not gc then M.forget x;
        acc

    and pass acc = function
      | [] -> acc
      | (Node (x, _) as x') :: xs ->
        if check then
          M.unsafe_check x name;
        pass (apply acc x' (random_action ())) xs

    and sub xs = List.rev (pass [] xs)

    in
    sub

  let total = ref 0

  let rec validate_order parent = function
    | (Node (x,xs)) :: ((Node (y, _) :: _) as rest) ->
      incr total;
      assert (M.compare x y = M.Before);
      assert (M.compare y x = M.After);
      begin match parent with
        | None -> ()
        | Some p ->
          assert (M.compare p x = M.Outside);
          assert (M.compare x p = M.Inside);
      end;
      validate_order (Some x) xs;
      validate_order parent rest
    | [Node (x, xs)] ->
      incr total;
      validate_order (Some x) xs;
      begin match parent with
        | None -> ()
        | Some p ->
          assert (M.compare p x = M.Outside);
          assert (M.compare x p = M.Inside);
      end
    | [] -> ()

  let validate_order xs =
    total := 0;
    validate_order None xs;
    !total

  let test ?(gc=false) ?(check=true) name count =
    Printf.eprintf "Testing %s\n" name;
    let items = ref [] in
    for i = 1 to count do
      if !items = [] then
        items := [Node (M.root (), [])];
      items := process gc check name !items;
      let expected = validate_order !items in
      let cardinal =
        match !items with
        | [] -> 0
        | (Node (x, _) :: _) -> M.cardinal x
      in
      if gc then begin
        Printf.eprintf "%s: Pass %d/%d succeded, %d intervals active, %d allocated\n%!"
          name i count expected cardinal;
      end else begin
        (*assert (cardinal = expected);*)
        Printf.eprintf "%s: Pass %d/%d succeded, %d intervals\n%!"
          name i count cardinal;
      end
    done

end

let t () = (Unix.times()).Unix.tms_utime

let once ?gc ?check ~pass state name (module M : S) =
  let module M = Test(M) in
  Random.set_state state;
  Gc.major (); Gc.major ();
  let t0 = t () in
  M.test ?gc ?check name pass;
  let dt = t () -. t0 in
  Printf.printf "%s check time: %.02f\n" name dt

let () =
  let () = Random.self_init () in
  let state = Random.get_state () in
  let pass = try int_of_string (Sys.argv.(1)) with _ -> 80 in
  once state "Order_interval" (module Order_interval) ~pass ~check:true ~gc:false;
  once state "Order_managed_interval" (module Order_managed_interval) ~pass ~check:true ~gc:true;
  once state "Order_interval" (module Order_interval) ~pass ~check:false ~gc:false;
  once state "Order_managed_interval" (module Order_managed_interval) ~pass ~check:false ~gc:true
