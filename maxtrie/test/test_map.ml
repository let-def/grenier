(* Differential testing of [PatMap] against [Stdlib.Map.Make (Int)]. *)

(* test_map_sharing.ml is generated from this file by replacing the next two
   lines, so that the same tests run on [PatMapSharing]. *)
module C = Maxtrie.PatMap
module CS = struct let equal = C.equal let compare = C.compare end
module R = Map.Make (Int)

let name = "PatMap"

let failures = ref 0

let fail fmt =
  Printf.ksprintf (fun s ->
      incr failures;
      prerr_endline ("FAILURE: " ^ s);
      if !failures > 20 then exit 1) fmt

let show r =
  let l = List.filteri (fun i _ -> i < 30) (R.bindings r) in
  "[" ^ String.concat ";" (List.map (fun (k, v) -> Printf.sprintf "%d:%d" k v) l) ^ "]"

type v = { c : int C.t; r : int R.t }

let agree name { c; r } =
  (try C.check c with Failure msg -> fail "%s: invariant: %s" name msg);
  if C.bindings c <> R.bindings r then
    fail "%s: got %s, expected %s" name
      (show (R.of_seq (List.to_seq (C.bindings c)))) (show r)

let near_pow2 () =
  let i = Random.int (min 61 (Sys.int_size - 2)) in
  max 0 ((1 lsl i) + Random.int 3 - 1)

let gens : (unit -> int) list = [
  (fun () -> Random.int 16);
  (fun () -> Random.int 64);
  (fun () -> Random.int 300);
  (fun () -> Random.int 100_000);
  near_pow2;
  (fun () -> (Random.int 5 * 1_000_003_000) land max_int + Random.int 100);
  (fun () -> if Random.bool () then max_int - Random.int 100
    else (Random.bits () lor (Random.bits () lsl 30)) land max_int);
  (fun () -> match Random.int 3 with
     | 0 -> Random.int 64 | 1 -> near_pow2 () | _ -> max_int - Random.int 50);
]

let value () = Random.int 4

let random_map gen =
  let n = match Random.int 5 with
    | 0 -> 0 | 1 -> 1 | 2 -> Random.int 10 | 3 -> Random.int 100 | _ -> Random.int 1000
  in
  let c = ref C.empty and r = ref R.empty in
  for _ = 1 to n do
    let k = gen () and v = value () in
    c := C.add k v !c;
    r := R.add k v !r
  done;
  { c = !c; r = !r }

(* Merge functions: key-dependent and non-commutative, so that swapping the
   arguments or passing a wrong key is detected. *)
let merges : (string * (int -> int -> int -> int option)) list = [
  "left", (fun _ a _ -> Some a);
  "right", (fun _ _ b -> Some b);
  "mix", (fun k a b -> Some ((a * 7 + b * 3 + k) land 0xff));
  "drop-some", (fun k a b -> if (a + k) land 1 = 0 then None else Some (a - b));
  "drop-all", (fun _ _ _ -> None);
]

let pool_size = 48

let test_round gen ops =
  let pool = Array.init pool_size (fun _ -> random_map gen) in
  let pick () = pool.(Random.int pool_size) in
  let store v = pool.(Random.int pool_size) <- v in
  let key () =
    match Random.int 4 with
    | 0 ->
      let m = pick () in
      if R.is_empty m.r then gen () else
      let b = R.bindings m.r in
      fst (List.nth b (Random.int (List.length b)))
    | _ -> gen ()
  in
  for _ = 1 to ops do
    let a = pick () and b = pick () in
    let b = if Random.int 10 = 0 then a else b in
    match Random.int 24 with
    | 0 | 1 ->
      let k = key () and x = value () in
      let v = { c = C.add k x a.c; r = R.add k x a.r } in
      agree "add" v;
      if R.find_opt k a.r = Some x && v.c != a.c then fail "add: not physically equal";
      store v
    | 2 ->
      let k = key () in
      let v = { c = C.remove k a.c; r = R.remove k a.r } in
      agree "remove" v;
      if not (R.mem k a.r) && v.c != a.c then fail "remove: not physically equal";
      store v
    | 3 ->
      let k = key () in
      let f = function
        | None -> if Random.bool () then None else Some 9
        | Some x -> if x = 0 then None else if x = 1 then Some 1 else Some (x + 1)
      in
      let st = Random.get_state () in
      let c = C.update k f a.c in
      Random.set_state st;
      let r = R.update k f a.r in
      agree "update" { c; r };
      if R.equal ( = ) r a.r && R.find_opt k r = R.find_opt k a.r && c != a.c
         && (R.find_opt k a.r = None || R.find_opt k a.r = Some 1) then
        fail "update: not physically equal";
      store { c; r }
    | 4 | 5 | 6 ->
      let name, f = List.nth merges (Random.int (List.length merges)) in
      let v = { c = C.union f a.c b.c; r = R.union f a.r b.r } in
      agree (Printf.sprintf "union %s %s %s" name (show a.r) (show b.r)) v;
      if name = "right" && R.for_all (fun k _ -> R.mem k b.r) a.r && v.c != b.c then
        fail "union: not physically equal to the second argument";
      store v
    | 7 | 8 ->
      let name, f = List.nth merges (Random.int (List.length merges)) in
      let r = R.merge (fun k x y ->
          match x, y with Some x, Some y -> f k x y | _ -> None) a.r b.r in
      let v = { c = C.inter f a.c b.c; r } in
      agree (Printf.sprintf "inter %s %s %s" name (show a.r) (show b.r)) v;
      store v
    | 9 | 10 ->
      let r = R.filter (fun k _ -> not (R.mem k b.r)) a.r in
      let v = { c = C.diff a.c b.c; r } in
      agree (Printf.sprintf "diff %s %s" (show a.r) (show b.r)) v;
      if R.cardinal r = R.cardinal a.r && v.c != a.c then fail "diff: not physically equal";
      if C.disjoint a.c b.c <> R.for_all (fun k _ -> not (R.mem k b.r)) a.r then
        fail "disjoint";
      store v
    | 11 ->
      let p k x = (k + x) mod 3 <> 0 in
      let seen = ref [] in
      let v = { c = C.filter (fun k x -> seen := k :: !seen; p k x) a.c; r = R.filter p a.r } in
      agree "filter" v;
      if List.rev !seen <> List.map fst (R.bindings a.r) then fail "filter: order";
      if R.cardinal v.r = R.cardinal a.r && v.c != a.c then fail "filter: not physically equal";
      let fm k x = if (k + x) mod 3 = 0 then None else Some (x * 2 + k land 7) in
      agree "filter_map" { c = C.filter_map fm a.c; r = R.filter_map fm a.r };
      let c1, c2 = C.partition p a.c and r1, r2 = R.partition p a.r in
      agree "partition.1" { c = c1; r = r1 };
      agree "partition.2" { c = c2; r = r2 };
      agree "map" { c = C.map (fun x -> x + 1) a.c; r = R.map (fun x -> x + 1) a.r };
      let seen = ref [] in
      agree "mapi" { c = C.mapi (fun k x -> seen := k :: !seen; (k + x) land 15) a.c;
                     r = R.mapi (fun k x -> (k + x) land 15) a.r };
      if List.rev !seen <> List.map fst (R.bindings a.r) then fail "mapi: order";
      store v
    | 12 ->
      let k = key () in
      let rl, rd, rr = R.split k a.r and cl, cd, cr = C.split k a.c in
      agree "split.left" { c = cl; r = rl };
      agree "split.right" { c = cr; r = rr };
      if cd <> rd then fail "split.data";
      store (if Random.bool () then { c = cl; r = rl } else { c = cr; r = rr })
    | 13 ->
      let k = key () in
      if C.find_opt k a.c <> R.find_opt k a.r then fail "find_opt";
      if C.mem k a.c <> R.mem k a.r then fail "mem";
      (match C.find k a.c, R.find_opt k a.r with
       | x, Some y -> if x <> y then fail "find"
       | _, None -> fail "find: should raise"
       | exception Not_found -> if R.mem k a.r then fail "find: raised");
      if C.mem (-1 - k) a.c then fail "mem negative"
    | 14 ->
      if C.cardinal a.c <> R.cardinal a.r then fail "cardinal";
      if C.is_empty a.c <> R.is_empty a.r then fail "is_empty";
      if C.is_singleton a.c <> (R.cardinal a.r = 1) then fail "is_singleton";
      if C.min_binding_opt a.c <> R.min_binding_opt a.r then fail "min_binding";
      if C.max_binding_opt a.c <> R.max_binding_opt a.r then fail "max_binding";
      (match C.choose_opt a.c with
       | None -> if not (R.is_empty a.r) then fail "choose"
       | Some (k, v) -> if R.find_opt k a.r <> Some v then fail "choose")
    | 15 ->
      let l = ref [] in
      C.iter (fun k v -> l := (k, v) :: !l) a.c;
      if List.rev !l <> R.bindings a.r then fail "iter";
      if C.fold (fun k v acc -> (k, v) :: acc) a.c [] <> R.fold (fun k v acc -> (k, v) :: acc) a.r []
      then fail "fold";
      if List.of_seq (C.to_seq a.c) <> R.bindings a.r then fail "to_seq";
      if List.of_seq (C.to_rev_seq a.c) <> List.of_seq (R.to_rev_seq a.r) then fail "to_rev_seq";
      let p k v = (k + v) mod 5 = 0 in
      if C.for_all p a.c <> R.for_all p a.r then fail "for_all";
      if C.exists p a.c <> R.exists p a.r then fail "exists";
      let q k = k mod 3 = 1 in
      let expected = List.find_opt (fun (k, _) -> q k) (R.bindings a.r) in
      if C.find_first_opt q a.c <> expected then fail "find_first_opt"
    | 16 ->
      if C.equal ( = ) a.c b.c <> R.equal ( = ) a.r b.r then fail "equal";
      (* Sharing comparisons agree, and leave valid maps behind. *)
      let a' = { c = C.of_list (C.bindings a.c); r = a.r } in
      if CS.equal ( = ) a'.c b.c <> R.equal ( = ) a.r b.r then fail "Sharing.equal";
      if Int.compare (CS.compare Int.compare a'.c b.c) 0
         <> Int.compare (C.compare Int.compare a.c b.c) 0 then fail "Sharing.compare";
      agree "after Sharing" a'; agree "after Sharing" b;
      store a';
      let c = C.compare Int.compare a.c b.c in
      if (c = 0) <> R.equal ( = ) a.r b.r then fail "compare/equal";
      if Int.compare c 0 <> Int.compare 0 (C.compare Int.compare b.c a.c) then
        fail "compare: antisymmetry"
    | 17 ->
      let l = List.init (Random.int 30) (fun _ -> (key (), value ())) in
      agree "of_list" { c = C.of_list l; r = R.of_seq (List.to_seq l) };
      agree "add_seq" { c = C.add_seq (List.to_seq l) a.c; r = R.add_seq (List.to_seq l) a.r }
    | 18 | 19 ->
      (* [changes], on a random pair, or on [a] and a version derived from
         it by a few updates (which shifts spines when the maximum changes). *)
      let b =
        if Random.bool () then b else
        let b = ref a in
        for _ = 1 to 1 + Random.int 4 do
          let k = match Random.int 3 with
            | 0 -> (match R.max_binding_opt !b.r with
                | Some (k, _) ->
                  let k' = k + 1 + Random.int 1000 in
                  if Random.bool () || k' < 0 then k else k' 
                | None -> key ())
            | _ -> key ()
          in
          b := (match Random.int 3 with
              | 0 -> { c = C.remove k !b.c; r = R.remove k !b.r }
              | 1 -> (* rebinding to the same value changes nothing *)
                (match R.find_opt k !b.r with
                 | Some v -> { c = C.add k v !b.c; r = R.add k v !b.r }
                 | None -> !b)
              | _ -> let v = value () in { c = C.add k v !b.c; r = R.add k v !b.r })
        done;
        !b
      in
      let got = ref [] in
      C.changes (fun k x y -> got := (k, x, y) :: !got) a.c b.c;
      let expected =
        R.merge (fun _ x y -> if x = y then None else Some (x, y)) a.r b.r
        |> R.bindings |> List.map (fun (k, (x, y)) -> (k, x, y))
      in
      if List.rev !got <> expected then
        fail "changes %s %s: %d calls, expected %d" (show a.r) (show b.r)
          (List.length !got) (List.length expected);
      store b
    | _ -> store (random_map gen)
  done

let () =
  let seed =
    match Sys.getenv_opt "SEED" with
    | Some s -> int_of_string s
    | None -> Random.self_init (); Random.bits ()
  in
  let rounds = match Sys.getenv_opt "ROUNDS" with Some s -> int_of_string s | None -> 200 in
  Printf.printf "%s: seed %d, %d rounds\n%!" name seed rounds;
  Random.init seed;
  (match C.add (-1) 0 C.empty with
   | _ -> fail "add (-1) should fail"
   | exception Invalid_argument _ -> ());
  for i = 1 to rounds do
    test_round (List.nth gens (i mod List.length gens)) 400
  done;
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1)
  else Printf.printf "%s: OK\n" name
