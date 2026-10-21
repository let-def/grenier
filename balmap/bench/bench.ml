(* Balmap vs Stdlib, Map and Set over int keys. Times are the best of
   [runs] runs, in ms. *)

let runs = 5

let time f =
  let best = ref infinity in
  for _ = 1 to runs do
    Gc.compact ();
    let t = Unix.gettimeofday () in
    ignore (Sys.opaque_identity (f ()));
    best := min !best ((Unix.gettimeofday () -. t) *. 1000.)
  done;
  !best

module type MAP = sig
  type 'a t
  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val find_opt : int -> 'a t -> 'a option
  val remove : int -> 'a t -> 'a t
  val union : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val merge : (int -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val filter : (int -> 'a -> bool) -> 'a t -> 'a t
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
  val split : int -> 'a t -> 'a t * 'a option * 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val of_seq : (int * 'a) Seq.t -> 'a t
  val to_seq : 'a t -> (int * 'a) Seq.t
end

module type SET = sig
  type t
  val empty : t
  val add : int -> t -> t
  val mem : int -> t -> bool
  val remove : int -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val filter : (int -> bool) -> t -> t
  val subset : t -> t -> bool
  val equal : t -> t -> bool
  val elements : t -> int list
  val of_list : int list -> t
end

let results : (string, (string * float) list) Hashtbl.t = Hashtbl.create 16
let order = ref []
let record impl name t =
  if not (Hashtbl.mem results name) then order := name :: !order;
  Hashtbl.replace results name ((impl, t) :: (try Hashtbl.find results name with Not_found -> []))

let keys n = Array.init n (fun _ -> Random.bits ())

module Map_bench (M : MAP) = struct
  let run impl n =
    let r name f = record impl (Printf.sprintf "map %-7d %s" n name) (time f) in
    Random.init 1;
    let ks = keys n and others = keys n in
    let build a = Array.fold_left (fun m k -> M.add k k m) M.empty a in
    let m = build ks and m2 = build others in
    let half = build (Array.sub ks 0 (n / 2)) in
    let reps = max 1 (1_000_000 / n) in
    r "add random" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (build ks)) done);
    r "add sequential" (fun () -> for _ = 1 to reps do
                            let m = ref M.empty in for i = 1 to n do m := M.add i i !m done;
                            ignore (Sys.opaque_identity !m) done);
    r "find hit" (fun () -> for _ = 1 to reps do Array.iter (fun k -> ignore (Sys.opaque_identity (M.find_opt k m))) ks done);
    r "find miss" (fun () -> for _ = 1 to reps do Array.iter (fun k -> ignore (Sys.opaque_identity (M.find_opt k m))) others done);
    r "remove all" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (Array.fold_left (fun m k -> M.remove k m) m ks)) done);
    let reps = max 1 (100_000 / n) in
    r "union disjoint" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.union (fun _ a _ -> Some a) m m2)) done);
    r "union overlap" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.union (fun _ a _ -> Some a) m half)) done);
    r "merge" (fun () -> for _ = 1 to reps do
                   ignore (Sys.opaque_identity (M.merge (fun _ a b -> match a, b with Some x, Some _ -> Some x | _ -> None) m half)) done);
    r "filter" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.filter (fun k _ -> k land 3 <> 0) m)) done);
    r "split x100" (fun () -> for _ = 1 to reps do for i = 0 to 99 do ignore (Sys.opaque_identity (M.split ks.(i * n / 100) m)) done done);
    r "map" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.map succ m)) done);
    r "fold" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.fold (fun k v a -> k + v + a) m 0)) done);
    r "to_seq" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (Seq.fold_left (fun a (k, _) -> a + k) 0 (M.to_seq m))) done);
    let sorted = List.sort compare (Array.to_list ks) in
    r "of_seq sorted" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.of_seq (List.to_seq (List.map (fun k -> k, k) sorted)))) done);
    let m' = build (Array.of_list (List.rev (Array.to_list ks))) in
    r "equal" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (M.equal (=) m m')) done)
end

module Set_bench (S : SET) = struct
  let run impl n =
    let r name f = record impl (Printf.sprintf "set %-7d %s" n name) (time f) in
    Random.init 2;
    let ks = keys n and others = keys n in
    let build a = Array.fold_left (fun s k -> S.add k s) S.empty a in
    let s = build ks and s2 = build others in
    let half = build (Array.sub ks 0 (n / 2)) in
    let reps = max 1 (1_000_000 / n) in
    r "add random" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (build ks)) done);
    r "mem hit" (fun () -> for _ = 1 to reps do Array.iter (fun k -> ignore (Sys.opaque_identity (S.mem k s))) ks done);
    r "remove all" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (Array.fold_left (fun s k -> S.remove k s) s ks)) done);
    let reps = max 1 (100_000 / n) in
    r "union disjoint" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.union s s2)) done);
    r "union overlap" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.union s half)) done);
    r "inter" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.inter s half)) done);
    r "diff" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.diff s half)) done);
    r "filter" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.filter (fun k -> k land 3 <> 0) s)) done);
    r "subset" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.subset half s)) done);
    let s' = build (Array.of_list (List.rev (Array.to_list ks))) in
    r "equal" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.equal s s')) done);
    let l = Array.to_list ks in
    r "of_list" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.of_list l)) done);
    r "elements" (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (S.elements s)) done)
end

module SM = Map_bench (Stdlib.Map.Make (Int))
module BM = Map_bench (Balmap.Map.Make (Int))
module SS = Set_bench (Stdlib.Set.Make (Int))
module BS = Set_bench (Balmap.Set.Make (Int))

let () =
  List.iter (fun n ->
      SM.run "stdlib" n; BM.run "balmap" n;
      SS.run "stdlib" n; BS.run "balmap" n) [1_000; 100_000];
  Printf.printf "%-32s %10s %10s %8s\n" "" "stdlib ms" "balmap ms" "ratio";
  List.iter (fun name ->
      let l = Hashtbl.find results name in
      let s = List.assoc "stdlib" l and b = List.assoc "balmap" l in
      Printf.printf "%-32s %10.2f %10.2f %7.2fx\n" name s b (b /. s))
    (List.rev !order)
