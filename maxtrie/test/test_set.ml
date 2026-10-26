(* Differential testing of [PatSet] against [Stdlib.Set.Make (Int)]. *)

module C = Maxtrie.PatSet
let set_name = "PatSet"

(* Whether the children of the roots of two equal non-empty sets are
   physically equal: the fields from the index [2] on, whatever the layout of
   the nodes. *)
let children_unified (x : C.t) (y : C.t) =
  let x = Obj.repr x and y = Obj.repr y in
  let rec loop i = i >= Obj.size x || (Obj.field x i == Obj.field y i && loop (i + 1)) in
  Obj.size x = Obj.size y && loop 2
module R = Set.Make (Int)

let failures = ref 0

let fail fmt =
  Printf.ksprintf (fun s ->
      incr failures;
      prerr_endline ("FAILURE: " ^ s);
      if !failures > 20 then exit 1) fmt

let show_list xs =
  let xs = List.filteri (fun i _ -> i < 40) xs in
  "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

let show r = show_list (R.elements r)

(* A test value: a candidate set together with its reference. *)
type v = { c : C.t; r : R.t }

let agree name { c; r } =
  (try C.check c with Failure msg -> fail "%s: invariant: %s" name msg);
  let ce = C.elements c and re = R.elements r in
  if ce <> re then
    fail "%s: got %s, expected %s" name (show_list ce) (show_list re)

(* -------------------------------------------------------------------------- *)
(* Element distributions. *)

let word = Sys.word_size - 1

let near_pow2_key () =
  (* Keys at, just below, and just above powers of two stress the handling
     of most significant bits. The bound keeps [k * word] in range on 32-bit
     integers (js_of_ocaml), and is 40 on 64-bit. *)
  let i = Random.int (min 40 (Sys.int_size - 6)) in
  let k = (1 lsl i) + (Random.int 3 - 1) in
  max 0 k * word + Random.int word

let gens : (string * (unit -> int)) list = [
  "tiny", (fun () -> Random.int 64);
  (* One element per key, over small key universes: every tree shape and
     carried-key configuration occurs often. *)
  "keys32", (fun () -> Random.int 32 * word);
  "keys300", (fun () -> Random.int 300 * word + Random.int 2);
  "small", (fun () -> Random.int 700);
  "medium", (fun () -> Random.int 20_000);
  "wide", (fun () -> Random.int (1 lsl 24));
  "pow2", near_pow2_key;
  "clustered", (fun () ->
      let cluster = Random.int 6 in
      (cluster * 7_919 * 1_000_003) land max_int + Random.int 300);
  "huge", (fun () ->
      if Random.bool () then max_int - Random.int 500
      else (Random.bits () lor (Random.bits () lsl 30)) land max_int);
  "mixed", (fun () ->
      match Random.int 4 with
      | 0 -> Random.int 128
      | 1 -> near_pow2_key ()
      | 2 -> Random.int 5000
      | _ -> max_int - Random.int 200);
]

(* -------------------------------------------------------------------------- *)
(* Random sets. *)

let random_set gen =
  let n =
    match Random.int 6 with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2 + Random.int 5
    | 3 -> Random.int 40
    | 4 -> Random.int 300
    | _ -> Random.int 3000
  in
  let c = ref C.empty and r = ref R.empty in
  for _ = 1 to n do
    let x = gen () in
    c := C.add x !c;
    r := R.add x !r
  done;
  { c = !c; r = !r }

(* -------------------------------------------------------------------------- *)
(* Operations. *)

let pool_size = 64

let check_phys name ~result ~expect_equal_to:(arg : v) =
  (* If the result is mathematically equal to [arg], it must be [arg]. *)
  if R.equal (R.of_list (C.elements result)) arg.r && result != arg.c then
    fail "%s: result equal to %s but not physically equal" name (show arg.r)

let test_round gen_name gen ops =
  let pool = Array.init pool_size (fun _ -> random_set gen) in
  let pick () = pool.(Random.int pool_size) in
  let store v = pool.(Random.int pool_size) <- v in
  let elt () =
    match Random.int 5 with
    | 0 -> (* an element of some set, to hit existing keys *)
      let s = pick () in
      if R.is_empty s.r then gen () else
      let xs = R.elements s.r in
      List.nth xs (Random.int (List.length xs))
    | 1 -> (match R.max_elt_opt (pick ()).r with
        | Some x -> let y = x + Random.int 3 - 1 in if y < 0 then x else y
        | None -> gen ())
    | 2 -> (match R.min_elt_opt (pick ()).r with
        | Some x -> max 0 (x + Random.int 3 - 1) | None -> gen ())
    | _ -> gen ()
  in
  for _ = 1 to ops do
    let a = pick () and b = pick () in
    let b = if Random.int 10 = 0 then a else b in
    match Random.int 30 with
    | 0 ->
      let x = elt () in
      let v = { c = C.add x a.c; r = R.add x a.r } in
      agree "add" v;
      if R.mem x a.r && v.c != a.c then fail "add: not physically equal";
      store v
    | 1 ->
      let x = elt () in
      let v = { c = C.remove x a.c; r = R.remove x a.r } in
      agree "remove" v;
      if not (R.mem x a.r) && v.c != a.c then
        fail "remove: not physically equal";
      store v
    | 2 | 3 ->
      let v = { c = C.union a.c b.c; r = R.union a.r b.r } in
      agree (Printf.sprintf "union %s %s" (show a.r) (show b.r)) v;
      check_phys ("union(2) " ^ show a.r) ~result:v.c ~expect_equal_to:b;
      store v;
      (* Sharing.union on rebuilt copies, whose equal subtrees are physically
         distinct: same result, valid arguments afterwards. *)
      let rebuild (x : v) = { x with c = C.of_list (C.elements x.c) } in
      let a' = rebuild a and b' = rebuild b in
      let v' = { c = C.Sharing.union a'.c b'.c; r = v.r } in
      agree (Printf.sprintf "Sharing.union %s %s" (show a.r) (show b.r)) v';
      agree "after Sharing.union" a'; agree "after Sharing.union" b';
      check_phys "Sharing.union(2)" ~result:v'.c ~expect_equal_to:b';
      (* On equal copies, the children of the roots end up unified. *)
      let a'' = rebuild a in
      if C.Sharing.union a''.c a'.c != a'.c then
        fail "Sharing.union of equal sets: not physically equal";
      if not (C.is_empty a.c) then begin
        if not (children_unified a''.c a'.c) then
          fail "Sharing.union of equal sets: children not unified"
      end;
      store a''
    | 4 | 5 ->
      let v = { c = C.inter a.c b.c; r = R.inter a.r b.r } in
      agree (Printf.sprintf "inter %s %s" (show a.r) (show b.r)) v;
      check_phys "inter(1)" ~result:v.c ~expect_equal_to:a;
      store v
    | 6 | 7 ->
      let v = { c = C.diff a.c b.c; r = R.diff a.r b.r } in
      agree (Printf.sprintf "diff %s %s" (show a.r) (show b.r)) v;
      check_phys "diff" ~result:v.c ~expect_equal_to:a;
      store v
    | 8 ->
      let x = elt () in
      let below, present, above = R.split x a.r in
      let cb, cp, ca = C.split x a.c in
      agree "split.below" { c = cb; r = below };
      agree "split.above" { c = ca; r = above };
      if cp <> present then fail "split.mem";
      agree "at_least" { c = C.at_least x a.c; r = R.filter (fun y -> y >= x) a.r };
      agree "at_most" { c = C.at_most x a.c; r = R.filter (fun y -> y <= x) a.r };
      if Random.bool () then store { c = ca; r = above } else store { c = cb; r = below }
    | 9 ->
      let x = elt () in
      if C.mem x a.c <> R.mem x a.r then fail "mem %d" x;
      if C.mem (-1 - x) a.c then fail "mem negative"
    | 10 ->
      if C.cardinal a.c <> R.cardinal a.r then fail "cardinal";
      if C.is_empty a.c <> R.is_empty a.r then fail "is_empty";
      if C.is_singleton a.c <> (R.cardinal a.r = 1) then fail "is_singleton"
    | 11 ->
      if C.equal a.c b.c <> R.equal a.r b.r then fail "equal";
      (* Sharing comparisons must agree, and leave valid sets behind. *)
      let a' = { c = C.union C.empty (C.of_list (C.elements a.c)); r = a.r } in
      if C.Sharing.equal a'.c b.c <> R.equal a.r b.r then fail "Sharing.equal";
      if Int.compare (C.Sharing.compare a'.c b.c) 0 <> Int.compare (C.compare a.c b.c) 0
      then fail "Sharing.compare";
      agree "after Sharing" a'; agree "after Sharing" b;
      store a';
      (* Sharing.compare_deep: a total order compatible with equality, which
         unifies the children of equal sets. *)
      let a' = { c = C.of_list (C.elements a.c); r = a.r } in
      let d = pick () in
      let cab = C.Sharing.compare_deep a'.c b.c in
      if (cab = 0) <> R.equal a.r b.r then fail "compare_deep/equal";
      if Int.compare 0 cab <> Int.compare (C.Sharing.compare_deep b.c a'.c) 0 then
        fail "compare_deep: not antisymmetric";
      let cbd = C.Sharing.compare_deep b.c d.c in
      if cab < 0 && cbd < 0 && C.Sharing.compare_deep a'.c d.c >= 0 then
        fail "compare_deep: not transitive";
      agree "after compare_deep" a'; agree "after compare_deep" b;
      let a'' = { c = C.of_list (C.elements a.c); r = a.r } in
      if C.Sharing.compare_deep a''.c a'.c <> 0 then fail "compare_deep: not reflexive";
      if not (C.is_empty a.c) then begin
        if not (children_unified a''.c a'.c) then
          fail "compare_deep of equal sets: children not unified"
      end;
      agree "after compare_deep" a'';
      store a'';
      let c = C.compare a.c b.c in
      if (c = 0) <> R.equal a.r b.r then fail "compare/equal";
      if Int.compare 0 c <> Int.compare (C.compare b.c a.c) 0 then
        fail "compare: not antisymmetric";
      let d = pick () in
      let c2 = C.compare b.c d.c in
      if c < 0 && c2 < 0 && C.compare a.c d.c >= 0 then fail "compare: not transitive"
    | 12 ->
      if C.subset a.c b.c <> R.subset a.r b.r then
        fail "subset %s %s" (show a.r) (show b.r);
      let i = { c = C.inter a.c b.c; r = R.inter a.r b.r } in
      if not (C.subset i.c a.c) then fail "subset of inter"
    | 13 ->
      if C.disjoint a.c b.c <> R.disjoint a.r b.r then
        fail "disjoint %s %s" (show a.r) (show b.r)
    | 14 ->
      (* quick_subset, under its precondition. *)
      let s = if Random.bool () then R.inter a.r b.r else R.diff a.r b.r in
      let cs = if Random.bool () then C.inter a.c b.c else C.diff a.c b.c in
      if R.equal s (R.of_list (C.elements cs)) then begin
        let expected = not (R.disjoint s b.r) in
        if C.quick_subset cs b.c <> expected then fail "quick_subset"
      end
    | 15 ->
      (match R.min_elt_opt a.r with
       | None ->
         (match C.minimum a.c with _ -> fail "minimum" | exception Not_found -> ());
         (match C.maximum a.c with _ -> fail "maximum" | exception Not_found -> ());
         (match C.choose a.c with _ -> fail "choose" | exception Not_found -> ())
       | Some x ->
         if C.minimum a.c <> x then fail "minimum";
         if C.maximum a.c <> R.max_elt a.r then fail "maximum";
         if not (R.mem (C.choose a.c) a.r) then fail "choose");
      if C.min_elt_opt a.c <> R.min_elt_opt a.r then fail "min_elt_opt";
      if C.max_elt_opt a.c <> R.max_elt_opt a.r then fail "max_elt_opt"
    | 16 ->
      let l = ref [] in
      C.iter (fun x -> l := x :: !l) a.c;
      if List.rev !l <> R.elements a.r then fail "iter";
      let l = ref [] in
      C.rev_iter (fun x -> l := x :: !l) a.c;
      if !l <> R.elements a.r then fail "rev_iter";
      if C.fold (fun x acc -> x :: acc) a.c [] <> R.fold (fun x acc -> x :: acc) a.r []
      then fail "fold";
      if List.of_seq (C.to_seq a.c) <> R.elements a.r then fail "to_seq"
    | 17 ->
      let m = 2 + Random.int 9 in
      let p x = x mod m = 0 in
      let reference_first =
        R.fold (fun x acc -> if acc = None && p x then Some x else acc) a.r None in
      if C.find_first_opt p a.c <> reference_first then fail "find_first_opt";
      if C.for_all p a.c <> R.for_all p a.r then fail "for_all";
      if C.exists p a.c <> R.exists p a.r then fail "exists";
      let seen = ref [] in
      let v = { c = C.filter (fun x -> seen := x :: !seen; p x) a.c;
                r = R.filter p a.r } in
      agree "filter" v;
      if List.rev !seen <> R.elements a.r then fail "filter: order of calls";
      if R.equal v.r a.r && v.c != a.c then fail "filter: not physically equal";
      let c1, c2 = C.partition p a.c and r1, r2 = R.partition p a.r in
      agree "partition.1" { c = c1; r = r1 };
      agree "partition.2" { c = c2; r = r2 };
      store v
    | 18 ->
      let cm = C.compare_minimum a.c b.c in
      let rm =
        match R.min_elt_opt a.r, R.min_elt_opt b.r with
        | None, None -> 0 | None, _ -> -1 | _, None -> 1
        | Some x, Some y -> Int.compare x y
      in
      if Int.compare cm 0 <> Int.compare rm 0 then fail "compare_minimum"
    | 19 ->
      (* sorted_union on a list of sorted, non-overlapping, non-empty sets. *)
      let sets = List.init (1 + Random.int 5) (fun _ -> pick ()) in
      let sets = List.filter (fun s -> not (R.is_empty s.r)) sets in
      let sets = List.sort (fun s1 s2 -> Int.compare (R.min_elt s1.r) (R.min_elt s2.r)) sets in
      let rec cut lo = function
        | [] -> []
        | s :: rest ->
          let s' = { c = C.above lo s.c; r = R.filter (fun x -> x > lo) s.r } in
          if R.is_empty s'.r then cut lo rest
          else s' :: cut (R.max_elt s'.r) rest
      in
      let sets = cut (-1) sets in
      let v = { c = C.sorted_union (List.map (fun s -> s.c) sets);
                r = List.fold_left (fun acc s -> R.union acc s.r) R.empty sets } in
      agree "sorted_union" v;
      store v
    | 20 ->
      if not (R.is_empty b.r) then begin
        let x = R.min_elt b.r in
        let h, t = C.extract_unique_prefix a.c b.c in
        agree "extract_unique_prefix.head" { c = h; r = R.filter (fun y -> y < x) a.r };
        agree "extract_unique_prefix.tail" { c = t; r = R.filter (fun y -> y >= x) a.r };
        let x = R.max_elt b.r in
        let h, t = C.extract_unique_suffix a.c b.c in
        agree "extract_unique_suffix.head" { c = h; r = R.filter (fun y -> y > x) a.r };
        agree "extract_unique_suffix.tail" { c = t; r = R.filter (fun y -> y <= x) a.r }
      end
    | 21 | 22 ->
      (* Make [b] share a prefix or suffix with [a]. *)
      let b =
        if Random.bool () || R.is_empty a.r then b else
        let x = elt () in
        if Random.bool () then
          { c = C.union (C.below x a.c) (C.at_least x b.c);
            r = R.union (R.filter (fun y -> y < x) a.r) (R.filter (fun y -> y >= x) b.r) }
        else
          { c = C.union (C.above x a.c) (C.at_most x b.c);
            r = R.union (R.filter (fun y -> y > x) a.r) (R.filter (fun y -> y <= x) b.r) }
      in
      let shared_prefix r1 r2 =
        let rec go s1 s2 acc =
          match s1 (), s2 () with
          | Seq.Cons (x, s1), Seq.Cons (y, s2) when x = y -> go s1 s2 (R.add x acc)
          | _ -> acc
        in
        go (R.to_seq r1) (R.to_seq r2) R.empty
      in
      let shared_suffix r1 r2 =
        let rec go s1 s2 acc =
          match s1 (), s2 () with
          | Seq.Cons (x, s1), Seq.Cons (y, s2) when x = y -> go s1 s2 (R.add x acc)
          | _ -> acc
        in
        go (R.to_rev_seq r1) (R.to_rev_seq r2) R.empty
      in
      let name = Printf.sprintf "%s %s" (show a.r) (show b.r) in
      let h = shared_prefix a.r b.r in
      let ch, (ct1, ct2) = C.extract_shared_prefix a.c b.c in
      agree ("extract_shared_prefix.head " ^ name) { c = ch; r = h };
      agree "extract_shared_prefix.tail1" { c = ct1; r = R.diff a.r h };
      agree "extract_shared_prefix.tail2" { c = ct2; r = R.diff b.r h };
      let h = shared_suffix a.r b.r in
      let ch, (ct1, ct2) = C.extract_shared_suffix a.c b.c in
      agree ("extract_shared_suffix.head " ^ name) { c = ch; r = h };
      agree "extract_shared_suffix.tail1" { c = ct1; r = R.diff a.r h };
      agree "extract_shared_suffix.tail2" { c = ct2; r = R.diff b.r h };
      store b
    | 23 ->
      let xs = List.init (Random.int 50) (fun _ -> elt ()) in
      agree "of_list" { c = C.of_list xs; r = R.of_list xs };
      agree "of_seq" { c = C.add_seq (List.to_seq xs) a.c; r = R.add_seq (List.to_seq xs) a.r }
    | 24 ->
      let x = elt () in
      agree "singleton" { c = C.singleton x; r = R.singleton x }
    | _ ->
      (* Build a fresh random set, so the pool does not degenerate. *)
      store (random_set gen)
  done;
  ignore gen_name

let () =
  let seed =
    match Sys.getenv_opt "SEED" with
    | Some s -> int_of_string s
    | None -> Random.self_init (); Random.bits ()
  in
  let rounds =
    match Sys.getenv_opt "ROUNDS" with Some s -> int_of_string s | None -> 200
  in
  Printf.printf "%s: seed %d, %d rounds\n%!" set_name seed rounds;
  Random.init seed;
  (* A copy of the empty set, which is not physically [C.empty]. *)
  let e = (Marshal.from_string (Marshal.to_string C.empty []) 0 : C.t) in
  let s = C.of_list [1; 100; 10_000] in
  if not (C.is_empty e && C.equal e C.empty && C.compare e C.empty = 0
          && C.Sharing.equal e C.empty && C.cardinal e = 0 && C.elements e = []
          && C.union e s == s && C.union s e == s && C.is_empty (C.inter s e)
          && C.diff s e == s && C.is_empty (C.diff e s) && C.subset e s
          && not (C.mem 0 e) && C.min_elt_opt e = None && C.max_elt_opt e = None
          && C.equal (C.add 0 e) (C.singleton 0)
          && C.extract_shared_prefix e C.empty = (e, (C.empty, C.empty)))
  then fail "copy of the empty set";
  (try C.check e with Failure msg -> fail "copy of the empty set: %s" msg);
  (match C.add (-1) C.empty with
   | _ -> fail "add (-1) should fail"
   | exception Invalid_argument _ -> ());
  for i = 1 to rounds do
    let name, gen = List.nth gens (i mod List.length gens) in
    test_round name gen 400
  done;
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1)
  else Printf.printf "%s: OK\n" set_name
