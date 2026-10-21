(* Benchmarks stressing comparisons of many equal (or nearly equal) maps and
   sets that were built independently, so that they are physically distinct:
   Stdlib (Map.Make / Set.Make) vs PatMap/PatSet (plain comparisons) vs
   PatMapSharing/PatSet.Sharing.

   For each structure, N values are built from K distinct contents, each one
   independently, by inserting its elements in a random order. We then:
   - sort them with [compare] (pass 1, then again: pass 2);
   - check [equal] on every adjacent pair of the sorted list (2 passes);
   and report the live heap after building and after the first pass. *)

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  r, (Unix.gettimeofday () -. t0) *. 1000.

let live_mb () =
  Gc.full_major ();
  float (Gc.quick_stat ()).Gc.live_words *. 8. /. 1e6

module type S = sig
  type t
  val name : string
  val build : (int * int) array -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let maps : (module S) list = [
  (module struct
    type t = int IntMap.t
    let name = "Stdlib Map"
    let build a = Array.fold_left (fun m (k, v) -> IntMap.add k v m) IntMap.empty a
    let compare = IntMap.compare Int.compare
    let equal = IntMap.equal Int.equal
  end);
  (module struct
    type t = int Maxtrie.PatMap.t
    let name = "PatMap"
    let build a = Array.fold_left (fun m (k, v) -> Maxtrie.PatMap.add k v m) Maxtrie.PatMap.empty a
    let compare = Maxtrie.PatMap.compare Int.compare
    let equal = Maxtrie.PatMap.equal Int.equal
  end);
  (module struct
    type t = int Maxtrie.PatMapSharing.t
    let name = "PatMapSharing"
    let build a = Array.fold_left (fun m (k, v) -> Maxtrie.PatMapSharing.add k v m) Maxtrie.PatMapSharing.empty a
    let compare = Maxtrie.PatMapSharing.Sharing.compare Int.compare
    let equal = Maxtrie.PatMapSharing.Sharing.equal Int.equal
  end);
]

let sets : (module S) list = [
  (module struct
    type t = IntSet.t
    let name = "Stdlib Set"
    let build a = Array.fold_left (fun s (k, _) -> IntSet.add k s) IntSet.empty a
    let compare = IntSet.compare
    let equal = IntSet.equal
  end);
  (module struct
    type t = Maxtrie.PatSet.t
    let name = "PatSet"
    let build a = Array.fold_left (fun s (k, _) -> Maxtrie.PatSet.add k s) Maxtrie.PatSet.empty a
    let compare = Maxtrie.PatSet.compare
    let equal = Maxtrie.PatSet.equal
  end);
  (module struct
    type t = Maxtrie.PatSet.t
    let name = "PatSet.Sharing"
    let build a = Array.fold_left (fun s (k, _) -> Maxtrie.PatSet.add k s) Maxtrie.PatSet.empty a
    let compare = Maxtrie.PatSet.Sharing.compare
    let equal = Maxtrie.PatSet.Sharing.equal
  end);
]

let shuffle a =
  let a = Array.copy a in
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
  done;
  a

(* [contents ~k ~size ~near gen] is [k] distinct binding arrays; with
   [near], they all derive from one base by changing one binding. *)
let contents ~k ~size ~near gen =
  let base () = Array.init size (fun _ -> let x = gen () in (x, x land 0xff)) in
  if near then
    let b = base () in
    Array.init k (fun i ->
        let a = Array.copy b in
        a.(Random.int size) <- (gen (), i);
        a)
  else Array.init k (fun _ -> base ())

let run title impls ~n ~k ~size ~near gen =
  Random.init 99;
  let cs = contents ~k ~size ~near gen in
  (* The same random insertion orders for every implementation. *)
  let orders = Array.init n (fun i -> shuffle cs.(i mod k)) in
  Printf.printf "\n== %s (n=%d values, k=%d distinct, %d bindings each%s)\n"
    title n k size (if near then ", nearly equal" else "");
  Printf.printf "%-16s %9s %9s %9s %9s %9s %9s\n"
    "" "sort #1" "sort #2" "equal #1" "equal #2" "live MB" "after #1";
  List.iter (fun (module X : S) ->
      Gc.compact ();
      let values = Array.map X.build orders in
      let live0 = live_mb () in
      let sort () = List.sort X.compare (Array.to_list values) in
      let equal_chain l =
        let rec go c = function
          | a :: (b :: _ as rest) -> go (if X.equal a b then c + 1 else c) rest
          | _ -> c
        in
        go 0 l
      in
      let l1, t_sort1 = time sort in
      let live1 = live_mb () in
      let _, t_sort2 = time sort in
      let e1, t_eq1 = time (fun () -> equal_chain l1) in
      let e2, t_eq2 = time (fun () -> equal_chain l1) in
      assert (e1 = e2);
      Printf.printf "%-16s %9.2f %9.2f %9.2f %9.2f %9.1f %9.1f   (%d equal pairs)\n"
        X.name t_sort1 t_sort2 t_eq1 t_eq2 live0 live1 e1;
      ignore (Sys.opaque_identity values))
    impls

let () =
  let sparse () = Random.bits () land (1 lsl 30 - 1) in
  let dense () = Random.int 20_000 in
  print_endline "Times in ms. 'live MB' is the live heap after building all values,";
  print_endline "'after #1' after the first sort (sharing lets duplicates be reclaimed).";
  run "maps, sparse keys" maps ~n:400 ~k:10 ~size:5000 ~near:false sparse;
  run "maps, sparse keys" maps ~n:400 ~k:10 ~size:5000 ~near:true sparse;
  run "maps, dense keys" maps ~n:400 ~k:10 ~size:10000 ~near:true dense;
  run "sets, sparse elements" sets ~n:400 ~k:10 ~size:5000 ~near:false sparse;
  run "sets, sparse elements" sets ~n:400 ~k:10 ~size:5000 ~near:true sparse;
  run "sets, dense elements" sets ~n:400 ~k:10 ~size:10000 ~near:true dense
