
let random_action () =
  match Random.int 8 with
  | 0 -> `Before
  | 1 -> `After
  | 2 -> `Forget
  | 3 -> `Replace_before
  | 4 -> `Replace_after
  | 5 | 6 | 7 -> `None
  | _ -> assert false

module type S = module type of Order_indir

module Test (M : S) =
struct
  let apply gc acc x = function
    | `None   ->
      x :: acc
    | `Before ->
      x :: M.before x :: acc
    | `After  ->
      M.after x :: x :: acc
    | `Replace_before ->
      if gc then
        M.before x :: acc
      else begin
        let result = M.before x in
        M.forget x;
        result :: acc
      end
    | `Replace_after  ->
      if gc then
        M.after x :: acc
      else begin
        let result = M.after x in
        M.forget x;
        result :: acc
      end
    | `Forget ->
      if not gc then M.forget x;
      acc

  let rec pass gc check name acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if check then
        M.unsafe_check x name;
      begin match xs with
        | [] -> ()
        | y :: _ -> assert (M.compare x y < 0)
      end;
      begin match acc with
        | [] -> ()
        | y :: _ -> assert (M.compare y x < 0)
      end;
      pass gc check name (apply gc acc x (random_action ())) xs

  let test ?(gc=false) ?(check=true) name count =
    Printf.eprintf "Testing %s\n" name;
    let items = ref [] in
    for i = 1 to count do
      if !items = [] then
        items := [M.root ()];
      items := pass gc check name [] !items;
      let expected = List.length !items in
      let cardinal =
        match !items with
        | [] -> 0
        | (x :: _) -> M.cardinal x
      in
      if gc then begin
        Printf.eprintf "%s: Pass %d/%d succeded, %d elements active, %d allocated\n%!"
          name i count expected cardinal;
      end else begin
        assert (cardinal = expected);
        Printf.eprintf "%s: Pass %d/%d succeded, %d elements\n%!"
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
  (*once state "Order_list"    (module Order_list)    ~pass ~check:true  ~gc:false;
    once state "Order_managed_list" (module Order_managed) ~pass ~check:true  ~gc:true;
    once state "Order_indir"   (module Order_indir)   ~pass ~check:true  ~gc:false;
    once state "Order_managed_indir"   (module Order_managed_indir)   ~pass ~check:true  ~gc:true;*)
  once state "Order_list"    (module Order_list)    ~pass ~check:false ~gc:false;
  once state "Order_managed_list" (module Order_managed) ~pass ~check:false ~gc:true;
  once state "Order_indir"   (module Order_indir)   ~pass ~check:false ~gc:false;
  once state "Order_managed_indir"   (module Order_managed_indir)  ~pass ~check:false  ~gc:true;
