(* Differential test of Balmap.Map / Balmap.Set against Stdlib, with the
   size, order, balance and depth invariants of the trees. *)
module K = Int
module BM = Balmap.Map.Make (K)
module SM = Stdlib.Map.Make (K)
module BS = Balmap.Set.Make (K)
module SS = Stdlib.Set.Make (K)

let fails = ref 0
let check name b = if not b then (incr fails; if !fails < 30 then Printf.printf "FAIL %s\n%!" name)

(* Structural invariants: sizes, order, depth. *)
let rec depth2 : _ Balmap.Map.balmap -> int = function
  | Leaf -> 0 | Node (_, l, _, _, r) -> 1 + max (depth2 l) (depth2 r)
let rec ok2 : _ Balmap.Map.balmap -> bool = function
  | Leaf -> true
  | Node (s, l, _, _, r) -> s = Bt2.size l + Bt2.size r + 1 && ok2 l && ok2 r
let rec depth1 : _ Balmap.Set.balset -> int = function
  | Leaf -> 0 | Node (_, l, _, r) -> 1 + max (depth1 l) (depth1 r)
let rec ok1 : _ Balmap.Set.balset -> bool = function
  | Leaf -> true
  | Node (s, l, _, r) -> s = Bt1.size l + Bt1.size r + 1 && ok1 l && ok1 r

let smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)
let disbalanced smin smax = smaller_ell smin (smax lsr 1)
let balanced sl sr = not (disbalanced sl sr) && not (disbalanced sr sl)
let rec bal2 : _ Balmap.Map.balmap -> bool = function
  | Leaf -> true
  | Node (_, l, _, _, r) -> balanced (Bt2.size l) (Bt2.size r) && bal2 l && bal2 r
let rec bal1 : _ Balmap.Set.balset -> bool = function
  | Leaf -> true
  | Node (_, l, _, r) -> balanced (Bt1.size l) (Bt1.size r) && bal1 l && bal1 r

(* Float.log2 requires OCaml 4.13. Going through frexp keeps the result
   exact on powers of two. *)
let log2 x =
  let m, e = frexp x in
  float (e - 1) +. log (2. *. m) /. log 2.

let max_depth n = if n = 0 then 0 else 3 * (1 + int_of_float (log2 (float n))) + 2

let inv_map name (b : int BM.t) =
  check (name ^ ": sizes") (ok2 b);
  check (name ^ ": balance") (bal2 b);
  check (name ^ ": depth") (depth2 b <= max_depth (BM.cardinal b));
  let l = BM.bindings b in
  check (name ^ ": sorted") (List.sort_uniq (fun (a, _) (b, _) -> compare a b) l = l)

let inv_set name (b : BS.t) =
  check (name ^ ": sizes") (ok1 b);
  check (name ^ ": balance") (bal1 b);
  check (name ^ ": depth") (depth1 b <= max_depth (BS.cardinal b));
  let l = BS.elements b in
  check (name ^ ": sorted") (List.sort_uniq compare l = l)

let same_map name b s =
  inv_map name b;
  check (name ^ ": bindings") (BM.bindings b = SM.bindings s)

let same_set name b s =
  inv_set name b;
  check (name ^ ": elements") (BS.elements b = SS.elements s)

let rand_key range = Random.int range

let test_map range =
  let n = Random.int 300 in
  let b = ref BM.empty and s = ref SM.empty in
  for _ = 1 to n do
    let k = rand_key range and v = Random.int 100 in
    (match Random.int 6 with
     | 0 -> b := BM.remove k !b; s := SM.remove k !s
     | 1 ->
       let f = function None -> if v land 1 = 0 then Some v else None
                        | Some x -> if x land 1 = 0 then None else Some (x + v) in
       b := BM.update k f !b; s := SM.update k f !s
     | 2 -> b := BM.add_to_list k v (BM.map (fun x -> [x]) !b) |> BM.map List.hd;
       s := SM.add k v !s
     | _ -> b := BM.add k v !b; s := SM.add k v !s);
  done;
  let b = !b and s = !s in
  same_map "build" b s;
  let bcopy = BM.of_list (SM.bindings s) in same_map "of_list" bcopy s;
  let k = rand_key range in
  check "mem" (BM.mem k b = SM.mem k s);
  check "find_opt" (BM.find_opt k b = SM.find_opt k s);
  check "cardinal" (BM.cardinal b = SM.cardinal s);
  check "is_empty" (BM.is_empty b = SM.is_empty s);
  check "min" (BM.min_binding_opt b = SM.min_binding_opt s);
  check "max" (BM.max_binding_opt b = SM.max_binding_opt s);
  check "find_first" (BM.find_first_opt (fun x -> x >= k) b = SM.find_first_opt (fun x -> x >= k) s);
  check "find_last" (BM.find_last_opt (fun x -> x <= k) b = SM.find_last_opt (fun x -> x <= k) s);
  let (bl, bv, br) = BM.split k b and (sl, sv, sr) = SM.split k s in
  same_map "split l" bl sl; same_map "split r" br sr; check "split v" (bv = sv);
  same_map "filter" (BM.filter (fun k v -> (k + v) land 1 = 0) b) (SM.filter (fun k v -> (k + v) land 1 = 0) s);
  same_map "filter_map" (BM.filter_map (fun k v -> if k land 1 = 0 then Some (v * 2) else None) b)
    (SM.filter_map (fun k v -> if k land 1 = 0 then Some (v * 2) else None) s);
  let (b1, b2) = BM.partition (fun k _ -> k land 2 = 0) b and (s1, s2) = SM.partition (fun k _ -> k land 2 = 0) s in
  same_map "partition 1" b1 s1; same_map "partition 2" b2 s2;
  same_map "map" (BM.map succ b) (SM.map succ s);
  same_map "mapi" (BM.mapi (+) b) (SM.mapi (+) s);
  check "fold" (BM.fold (fun k v a -> (k, v) :: a) b [] = SM.fold (fun k v a -> (k, v) :: a) s []);
  check "for_all" (BM.for_all (fun k _ -> k <> range / 2) b = SM.for_all (fun k _ -> k <> range / 2) s);
  check "exists" (BM.exists (fun k _ -> k = range / 3) b = SM.exists (fun k _ -> k = range / 3) s);
  check "to_seq" (List.of_seq (BM.to_seq b) = List.of_seq (SM.to_seq s));
  check "to_rev_seq" (List.of_seq (BM.to_rev_seq b) = List.of_seq (SM.to_rev_seq s));
  check "to_seq_from" (List.of_seq (BM.to_seq_from k b) = List.of_seq (SM.to_seq_from k s));
  for i = 0 to BM.cardinal b - 1 do
    let (rk, _) as r = BM.rank i b in
    check "rank" (r = List.nth (SM.bindings s) i);
    check "rank_of" (BM.rank_of b rk = Some i)
  done;
  check "rank_of absent" (SM.mem k s || BM.rank_of b k = None);
  (* binary operations *)
  let n2 = Random.int 300 in
  let pairs = List.init n2 (fun _ -> rand_key range, Random.int 100) in
  let c = List.fold_left (fun m (k, v) -> BM.add k v m) BM.empty pairs
  and t = List.fold_left (fun m (k, v) -> SM.add k v m) SM.empty pairs in
  let mf _ a b = match a, b with
    | Some a, Some b -> if a > b then Some (a - b) else None
    | Some a, None -> Some a | None, b -> b in
  same_map "merge" (BM.merge mf b c) (SM.merge mf s t);
  let uf _ a b = if a = b then None else Some (2 * a + b) in
  same_map "union" (BM.union uf b c) (SM.union uf s t);
  check "compare" (compare 0 (BM.compare compare b c) = compare 0 (match compare (SM.cardinal s) (SM.cardinal t) with 0 -> SM.compare compare s t | n -> n));
  check "compare self" (BM.compare compare b bcopy = 0);
  check "equal" (BM.equal (=) b c = SM.equal (=) s t);
  check "equal self" (BM.equal (=) b bcopy)

let test_set range =
  let n = Random.int 300 in
  let b = ref BS.empty and s = ref SS.empty in
  for _ = 1 to n do
    let k = rand_key range in
    if Random.int 4 = 0 then (b := BS.remove k !b; s := SS.remove k !s)
    else (b := BS.add k !b; s := SS.add k !s)
  done;
  let b = !b and s = !s in
  same_set "build" b s;
  let k = rand_key range in
  check "mem" (BS.mem k b = SS.mem k s);
  check "find_opt" (BS.find_opt k b = SS.find_opt k s);
  check "min" (BS.min_elt_opt b = SS.min_elt_opt s);
  check "max" (BS.max_elt_opt b = SS.max_elt_opt s);
  check "find_first" (BS.find_first_opt (fun x -> x >= k) b = SS.find_first_opt (fun x -> x >= k) s);
  check "find_last" (BS.find_last_opt (fun x -> x <= k) b = SS.find_last_opt (fun x -> x <= k) s);
  let (bl, bv, br) = BS.split k b and (sl, sv, sr) = SS.split k s in
  same_set "split l" bl sl; same_set "split r" br sr; check "split v" (bv = sv);
  same_set "filter" (BS.filter (fun x -> x land 1 = 0) b) (SS.filter (fun x -> x land 1 = 0) s);
  same_set "filter_map" (BS.filter_map (fun x -> if x land 1 = 0 then Some (x / 3) else None) b)
    (SS.filter_map (fun x -> if x land 1 = 0 then Some (x / 3) else None) s);
  same_set "map" (BS.map (fun x -> x / 2) b) (SS.map (fun x -> x / 2) s);
  same_set "map id" (BS.map (fun x -> x) b) (SS.map (fun x -> x) s);
  let (b1, b2) = BS.partition (fun x -> x land 2 = 0) b and (s1, s2) = SS.partition (fun x -> x land 2 = 0) s in
  same_set "partition 1" b1 s1; same_set "partition 2" b2 s2;
  check "to_seq_from" (List.of_seq (BS.to_seq_from k b) = List.of_seq (SS.to_seq_from k s));
  check "to_rev_seq" (List.of_seq (BS.to_rev_seq b) = List.of_seq (SS.to_rev_seq s));
  same_set "of_list" (BS.of_list (SS.elements s)) s;
  same_set "of_list shuffled" (BS.of_list (List.rev (SS.elements s) @ SS.elements s)) s;
  for i = 0 to BS.cardinal b - 1 do
    let x = BS.rank i b in
    check "rank" (x = List.nth (SS.elements s) i);
    check "rank_of" (BS.rank_of b x = Some i)
  done;
  let l2 = List.init (Random.int 300) (fun _ -> rand_key range) in
  let c = BS.of_list l2 and t = SS.of_list l2 in
  same_set "union" (BS.union b c) (SS.union s t);
  same_set "inter" (BS.inter b c) (SS.inter s t);
  same_set "diff" (BS.diff b c) (SS.diff s t);
  check "disjoint" (BS.disjoint b c = SS.disjoint s t);
  check "subset" (BS.subset b c = SS.subset s t);
  check "subset inter" (BS.subset (BS.inter b c) b);
  (* small subsets of a large set: subset by lookups *)
  let few = BS.filter (fun x -> x mod 37 = 0) b in
  check "subset few" (BS.subset few b);
  check "subset few'" (BS.subset (BS.add (range + 1) few) b = false);
  check "remove absent" (BS.remove (range + 1) b == b);
  check "subset few''" (BS.subset few c = SS.subset (SS.filter (fun x -> x mod 37 = 0) s) t);
  check "compare" (compare 0 (BS.compare b c) = compare 0 (match compare (SS.cardinal s) (SS.cardinal t) with 0 -> SS.compare s t | n -> n));
  check "equal self" (BS.equal b (BS.of_list (SS.elements s)));
  check "compare self" (BS.compare b (BS.of_list (List.rev (SS.elements s))) = 0);
  check "equal" (BS.equal b c = SS.equal s t)

let () =
  let seed = try int_of_string (Sys.getenv "SEED") with Not_found -> Random.self_init (); Random.bits () in
  Random.init seed;
  Printf.printf "balmap: seed %d\n%!" seed;
  for i = 1 to 1000 do
    let range = [| 10; 100; 1000; 1_000_000 |].(i land 3) in
    test_map range; test_set range
  done;
  (* Large, sequential and adversarial insertion orders: depth. *)
  let big = BS.of_list (List.init 100_000 Fun.id) in inv_set "seq set" big;
  let big = List.fold_left (fun s x -> BS.add x s) BS.empty (List.init 100_000 (fun i -> -i)) in inv_set "rev set" big;
  let m = List.fold_left (fun m x -> BM.add x x m) BM.empty (List.init 100_000 Fun.id) in inv_map "seq map" m;
  let u = List.fold_left (fun a i -> BS.union a (BS.singleton (i * 7919 mod 100_003))) BS.empty (List.init 50_000 Fun.id) in
  inv_set "unions" u;
  (* Depth under repeated filter / partition / union / diff. *)
  let s = ref (BS.of_list (List.init 50_000 (fun _ -> Random.bits ()))) in
  for _ = 1 to 20 do
    let p = 2 + Random.int 7 and q = Random.int 3 in
    s := BS.filter (fun x -> (x / 7) mod p <> q) !s; inv_set "filter" !s;
    let a, b = BS.partition (fun x -> x land (1 lsl Random.int 20) = 0) !s in
    inv_set "partition" a; inv_set "partition" b;
    s := BS.union a (BS.of_list (List.init 20_000 (fun _ -> Random.bits ()))); inv_set "union" !s;
    s := BS.diff !s (BS.filter (fun x -> x mod 5 = 0) !s); inv_set "diff" !s;
    let base = Random.bits () in
    s := BS.union !s (BS.of_list (List.init 10_000 (fun i -> base + i))); inv_set "union run" !s;
    s := BS.filter (fun x -> not (x > base && x < base + 9_000)) !s; inv_set "filter run" !s
  done;
  (* Elements equal for [compare] but distinct: [inter] keeps the ones of
     its first argument, whichever set is the larger. *)
  let module TS = Balmap.Set.Make (struct
      type t = int * int
      let compare (a, _) (b, _) = Int.compare a b
    end) in
  for _ = 1 to 200 do
    let tagged tag n = TS.of_list (List.init n (fun _ -> (Random.int 1000, tag))) in
    let a = tagged 1 (Random.int 500) and b = tagged 2 (Random.int 500) in
    let i = TS.inter a b in
    check "inter keys" (List.map fst (TS.elements i) =
                        List.filter (fun k -> TS.mem (k, 0) b) (List.map fst (TS.elements a)));
    check "inter elements" (TS.for_all (fun x -> TS.find x a == x) i);
    check "partition all" (fst (TS.partition (fun _ -> true) a) == a);
    check "partition none" (snd (TS.partition (fun _ -> false) a) == a)
  done;
  Printf.printf "balmap: %s (%d failures)\n" (if !fails = 0 then "OK" else "FAILED") !fails;
  if !fails > 0 then exit 1
