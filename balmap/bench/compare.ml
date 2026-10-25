(* Balmap vs Stdlib, and Baby when it is installed, over many workloads per
   function of Set or Map.

   Usage: compare.exe set|map [-o FILE] [FUNCTION...]

   Times are ns per call, the best of 5 runs of a loop calibrated to last at
   least 10ms. Per function, the summary gives the geometric mean of time
   ratios against Stdlib (and of Balmap against Baby, with the extreme
   ratios), and the words allocated by all workloads. [-o] writes the
   measures of each workload to FILE, tab-separated. The listed functions
   only are measured, all of them by default. Build with
   [dune build --profile release]. *)

let measure f =
  let iters = ref 1 in
  let run k = let t = Unix.gettimeofday () in
    for _ = 1 to k do ignore (Sys.opaque_identity (f ())) done;
    Unix.gettimeofday () -. t in
  while run !iters < 0.01 do iters := !iters * 2 done;
  let best = ref infinity in
  for _ = 1 to 5 do best := min !best (run !iters) done;
  !best /. float !iters *. 1e9

(* Words allocated by one call, on the minor and major heaps. *)
let alloc_words f =
  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  ignore (Sys.opaque_identity (f ()));
  let a2 = Gc.allocated_bytes () in
  (a2 -. a1 -. (a1 -. a0)) /. float (Sys.word_size / 8)

let selected = ref []

(* [record rows f d g] measures [g] as workload [d] of function [f]. *)
let record rows f d g =
  if !selected = [] || List.mem f !selected then
    rows := (f, d, measure g, alloc_words g) :: !rows

let range a b = List.init (max 0 (b - a)) (fun i -> a + i)
let rand seed n = Random.init seed; List.init n (fun _ -> Random.bits ())
let shuffle seed l = let a = Array.of_list l in
  Random.init seed;
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in let x = a.(i) in a.(i) <- a.(j); a.(j) <- x done;
  Array.to_list a
let rec take k = function [] -> [] | x :: l -> if k = 0 then [] else x :: take (k - 1) l
let sizes = [1_000; 100_000]

(* (shape, insertion order or None for of_list, elements, misses) *)
let shapes n =
  let r = rand 1 n in
  [ "random", Some r, r, List.map (fun x -> x lor (1 lsl 40)) (rand 2 1000);
    "sequential", Some (range 0 n), range 0 n, List.map (fun i -> n + i) (range 0 1000);
    "of_list", None, r, List.map (fun x -> x lor (1 lsl 40)) (rand 2 1000) ]

(* Pairs of operands of binary operations. *)
let pairs n =
  let ks = rand 1 n in
  let blocks b off = List.filter (fun x -> (x / b) mod 2 = off) (range 0 (2 * n)) in
  [ "random, half overlap", ks, take (n/2) ks @ rand 2 (n/2);
    "random, disjoint", ks, rand 2 n;
    "s vs half of s", ks, take (n/2) ks;
    "half of s vs s", take (n/2) ks, ks;
    "equal sets", ks, ks;
    "equal but one", ks, List.tl ks @ [42];
    "evens vs odds", List.map (( * ) 2) (range 0 n), List.map (fun x -> 2*x+1) (range 0 n);
    "evens vs [0,2n)", List.map (( * ) 2) (range 0 n), range 0 (2*n);
    "ranges, half overlap", range 0 n, range (n/2) (3*n/2);
    "blocks of 64", blocks 64 0, blocks 64 1;
    "16 keys vs s", rand 3 16, ks;
    "s vs 16 keys", ks, rand 3 16 ]

let run_set (module M : Sigs.SET) =
  let rows = ref [] in
  let r f d g = record rows f d g in
  let build l = List.fold_left (fun s k -> M.add k s) M.empty l in
  List.iter (fun n ->
      List.iter (fun (shape, order, elts, misses) ->
          let d = Printf.sprintf "%s %d" shape n in
          let s = match order with Some o -> build o | None -> M.of_list elts in
          let hits = take 1000 (shuffle 5 elts) in
          let sorted = List.sort compare elts in
          let descending = List.rev sorted and shuffled = shuffle 9 elts in
          let median = List.nth sorted (n / 2) in
          let many f l () = List.iter (fun k -> ignore (Sys.opaque_identity (f k))) l in
          (match order with
           | Some o ->
             r "add" d (fun () -> build o);
             r "add" (d ^ " descending") (fun () -> build descending)
           | None -> ());
          let interior = List.filter (fun k -> not (M.mem k s)) (List.map (fun x -> x lxor 1) hits) in
          let interior_r f g = if interior <> [] then r f (d ^ " interior misses") (many g interior) in
          r "remove" (d ^ " hits") (many (fun k -> M.remove k s) hits);
          r "remove" (d ^ " misses above max") (many (fun k -> M.remove k s) misses);
          interior_r "remove" (fun k -> M.remove k s);
          r "mem" (d ^ " hits") (many (fun k -> M.mem k s) hits);
          r "mem" (d ^ " misses above max") (many (fun k -> M.mem k s) misses);
          interior_r "mem" (fun k -> M.mem k s);
          r "find_opt" (d ^ " hits") (many (fun k -> M.find_opt k s) hits);
          r "split" (d ^ " hits") (many (fun k -> M.split k s) hits);
          r "split" (d ^ " misses above max") (many (fun k -> M.split k s) misses);
          interior_r "split" (fun k -> M.split k s);
          r "find_first_opt" d (many (fun k -> M.find_first_opt (fun x -> x >= k) s) hits);
          r "to_seq_from" d (many (fun k -> Seq.fold_left (+) 0 (Seq.take 10 (M.to_seq_from k s))) hits);
          r "min/max_elt" d (fun () -> M.min_elt s + M.max_elt s);
          r "cardinal" d (fun () -> M.cardinal s);
          r "filter" (d ^ " all") (fun () -> M.filter (fun _ -> true) s);
          r "filter" (d ^ " parity") (fun () -> M.filter (fun x -> x land 1 = 0) s);
          r "filter" (d ^ " prefix") (fun () -> M.filter (fun x -> x < median) s);
          r "filter" (d ^ " none") (fun () -> M.filter (fun _ -> false) s);
          r "partition" (d ^ " all") (fun () -> M.partition (fun _ -> true) s);
          r "partition" (d ^ " parity") (fun () -> M.partition (fun x -> x land 1 = 0) s);
          r "partition" (d ^ " prefix") (fun () -> M.partition (fun x -> x < median) s);
          r "map" (d ^ " succ") (fun () -> M.map succ s);
          r "map" (d ^ " identity") (fun () -> M.map (fun x -> x) s);
          r "map" (d ^ " negate") (fun () -> M.map (fun x -> - x) s);
          r "map" (d ^ " x/4") (fun () -> M.map (fun x -> x / 4) s);
          r "filter_map" (d ^ " parity") (fun () -> M.filter_map (fun x -> if x land 1 = 0 then Some x else None) s);
          r "filter_map" (d ^ " x/4") (fun () -> M.filter_map (fun x -> Some (x / 4)) s);
          r "fold" d (fun () -> M.fold (+) s 0);
          r "iter" d (fun () -> let c = ref 0 in M.iter (fun x -> c := !c + x) s; !c);
          r "for_all" d (fun () -> M.for_all (fun x -> x >= 0) s);
          r "exists" d (fun () -> M.exists (fun x -> x < 0) s);
          r "elements" d (fun () -> M.elements s);
          r "to_seq" d (fun () -> Seq.fold_left (+) 0 (M.to_seq s));
          r "to_rev_seq" d (fun () -> Seq.fold_left (+) 0 (M.to_rev_seq s));
          r "of_list" (d ^ " shuffled") (fun () -> M.of_list shuffled);
          r "of_list" (d ^ " sorted") (fun () -> M.of_list sorted);
          r "of_seq" d (fun () -> M.of_seq (List.to_seq elts)))
        (shapes n);
      List.iter (fun (name, a, b) ->
          let d = Printf.sprintf "%s %d" name n in
          let x = build (shuffle 7 a) and y = build (shuffle 8 b) in
          r "union" d (fun () -> M.union x y);
          r "inter" d (fun () -> M.inter x y);
          r "diff" d (fun () -> M.diff x y);
          r "disjoint" d (fun () -> M.disjoint x y);
          r "subset" d (fun () -> M.subset x y);
          r "equal" d (fun () -> M.equal x y);
          r "compare" d (fun () -> M.compare x y))
        (pairs n))
    sizes;
  List.rev !rows

let run_map (module M : Sigs.MAP) =
  let rows = ref [] in
  let r f d g = record rows f d g in
  let build l = List.fold_left (fun m (k, v) -> M.add k v m) M.empty l in
  let bindings l = List.map (fun k -> k, k) l in
  List.iter (fun n ->
      let d s = Printf.sprintf "%s %d" s n in
      let kv = bindings (rand 1 n) in
      let m = build kv in
      let sorted = List.sort compare kv in
      let duplicated = List.concat_map (fun b -> [b; b]) sorted in
      let sequential = bindings (range 0 n) in
      let hits = take 1000 (shuffle 5 kv) in
      let many f l () = List.iter (fun (k, _) -> ignore (Sys.opaque_identity (f k))) l in
      r "add" (d "random") (fun () -> build kv);
      r "add" (d "sequential") (fun () -> build sequential);
      r "remove" (d "hits") (many (fun k -> M.remove k m) hits);
      r "find_opt" (d "hits") (many (fun k -> M.find_opt k m) hits);
      r "split" (d "hits") (many (fun k -> M.split k m) hits);
      r "min/max_binding" (d "random") (fun () -> fst (M.min_binding m) + fst (M.max_binding m));
      r "filter" (d "parity") (fun () -> M.filter (fun k _ -> k land 1 = 0) m);
      r "map" (d "succ") (fun () -> M.map succ m);
      r "fold" (d "random") (fun () -> M.fold (fun k v a -> k + v + a) m 0);
      r "of_list" (d "shuffled") (fun () -> M.of_list kv);
      r "of_list" (d "sorted") (fun () -> M.of_list sorted);
      r "of_list" (d "sorted, duplicated") (fun () -> M.of_list duplicated);
      r "of_seq" (d "shuffled") (fun () -> M.of_seq (List.to_seq kv));
      r "of_seq" (d "sorted") (fun () -> M.of_seq (List.to_seq sorted));
      let left _ a _ = Some a in
      let merge_left _ a b = match a with Some _ -> a | None -> b in
      List.iter (fun (name, a, b) ->
          let x = build (bindings (shuffle 7 a)) and y = build (bindings (shuffle 8 b)) in
          r "union" (d name) (fun () -> M.union left x y);
          r "merge" (d name) (fun () -> M.merge merge_left x y))
        (pairs n))
    sizes;
  List.rev !rows

let geomean l =
  if l = [] then nan
  else exp (List.fold_left (fun a x -> a +. log x) 0. l /. float (List.length l))

(* [report impls] prints the summary of the measures of [impls], a list of
   (name, rows) starting with Stdlib, then Balmap. *)
let report output impls =
  let names = List.map fst impls in
  let rows = List.map snd impls in
  let rows = List.map (fun l -> Array.of_list l) rows in
  let n = Array.length (List.hd rows) in
  let ns i k = let (_, _, t, _) = (List.nth rows k).(i) in t in
  let words i k = let (_, _, _, w) = (List.nth rows k).(i) in w in
  let others = List.tl names in
  let has_baby = List.length names > 2 in
  Option.iter (fun file ->
      let oc = open_out file in
      Printf.fprintf oc "function\tworkload";
      List.iter (fun name -> Printf.fprintf oc "\t%s ns" name) names;
      List.iter (fun name -> Printf.fprintf oc "\t%s words" name) names;
      Printf.fprintf oc "\n";
      for i = 0 to n - 1 do
        let (f, d, _, _) = (List.hd rows).(i) in
        Printf.fprintf oc "%s\t%s" f d;
        List.iteri (fun k _ -> Printf.fprintf oc "\t%.1f" (ns i k)) names;
        List.iteri (fun k _ -> Printf.fprintf oc "\t%.0f" (words i k)) names;
        Printf.fprintf oc "\n"
      done;
      close_out oc) output;
  Printf.printf "%-16s %5s |" "function" "cases";
  List.iter (fun name -> Printf.printf " %8s" name) others;
  if has_baby then Printf.printf " %8s %7s %7s" "bal/baby" "best" "worst";
  Printf.printf " |";
  List.iter (fun name -> Printf.printf " %11s" (name ^ " w")) names;
  Printf.printf "\n";
  let funcs = Array.fold_left (fun acc (f, _, _, _) ->
      if List.mem f acc then acc else acc @ [f]) [] (List.hd rows) in
  List.iter (fun f ->
      let cases = List.filter (fun i ->
          let (f', _, _, _) = (List.hd rows).(i) in f = f') (List.init n Fun.id) in
      let ratio a b = List.map (fun i -> ns i a /. ns i b) cases in
      Printf.printf "%-16s %5d |" f (List.length cases);
      List.iteri (fun k _ -> Printf.printf " %7.2fx" (geomean (ratio (k + 1) 0))) others;
      if has_baby then begin
        let r = ratio 1 2 in
        Printf.printf " %7.2fx %6.2fx %6.2fx" (geomean r)
          (List.fold_left min infinity r) (List.fold_left max 0. r)
      end;
      Printf.printf " |";
      List.iteri (fun k _ ->
          Printf.printf " %11.0f" (List.fold_left (fun a i -> a +. words i k) 0. cases))
        names;
      Printf.printf "\n")
    funcs

let () =
  let kind = ref None and output = ref None in
  let usage = "compare.exe set|map [-o FILE] [FUNCTION...]" in
  Arg.parse ["-o", Arg.String (fun f -> output := Some f), "FILE  write the measures of each workload to FILE"]
    (fun a -> if !kind = None then kind := Some a else selected := a :: !selected)
    usage;
  let impls run std balmap baby =
    ("stdlib", run std) :: ("balmap", run balmap) ::
    (match baby with Some b -> ["baby", run b] | None -> [])
  in
  match !kind with
  | Some "set" ->
    report !output (impls run_set
                      (module Set.Make (Int) : Sigs.SET)
                      (module Balmap.Set.Make (Int) : Sigs.SET)
                      Baby_impl.set)
  | Some "map" ->
    report !output (impls run_map
                      (module Map.Make (Int) : Sigs.MAP)
                      (module Balmap.Map.Make (Int) : Sigs.MAP)
                      Baby_impl.map)
  | _ -> Arg.usage [] usage; exit 2
