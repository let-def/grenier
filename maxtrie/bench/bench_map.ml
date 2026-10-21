(* Micro-benchmarks: PatMap vs Stdlib.Map. *)

module type M = sig
  type 'a t
  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val remove : int -> 'a t -> 'a t
  val find_opt : int -> 'a t -> 'a option
  val union : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

let time f =
  let best = ref infinity in
  for _ = 1 to 5 do
    Gc.full_major ();
    let t0 = Sys.time () in
    ignore (Sys.opaque_identity (f ()));
    best := Float.min !best (Sys.time () -. t0)
  done;
  !best *. 1000.

module Bench (X : M) = struct
  let of_array a = Array.fold_left (fun m k -> X.add k k m) X.empty a
  let run xs ys qs =
    let m1 = of_array xs and m2 = of_array ys in
    [ "build", time (fun () -> of_array xs);
      "find", time (fun () -> Array.fold_left (fun c q -> match X.find_opt q m1 with Some _ -> c + 1 | None -> c) 0 qs);
      "remove", time (fun () -> Array.fold_left (fun m k -> X.remove k m) m1 xs);
      "union", time (fun () -> X.union (fun _ a b -> Some (a + b)) m1 m2);
      "fold", time (fun () -> X.fold (fun _ v acc -> v + acc) m1 0) ]
end

module P = Bench (Maxtrie.PatMap)
module S = Bench (Map.Make (Int))

let () =
  Random.init 7;
  List.iter (fun (name, gen) ->
      let n = 100_000 in
      let xs = Array.init n (fun _ -> gen ()) and ys = Array.init n (fun _ -> gen ()) in
      let qs = Array.init 200_000 (fun _ -> gen ()) in
      Printf.printf "\n== %s\n%-8s %10s %10s %8s\n" name "op (ms)" "PatMap" "Stdlib" "ratio";
      List.iter2 (fun (op, a) (_, b) -> Printf.printf "%-8s %10.2f %10.2f %8.2f\n" op a b (a /. b))
        (P.run xs ys qs) (S.run xs ys qs))
    [ "dense 100k in [0,200k)", (fun () -> Random.int 200_000);
      "sparse 100k in [0,2^40)", (fun () -> Random.bits () lor (Random.bits () lsl 10) land (1 lsl 40 - 1)) ]
