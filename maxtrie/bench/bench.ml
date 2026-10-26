(* Micro-benchmarks: new PatSet vs PatSetCompact vs the original PatSet vs
   Stdlib.Set. *)

module type S = sig
  type t
  val empty : t
  val add : int -> t -> t
  val remove : int -> t -> t
  val mem : int -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
end

let time f =
  (* Best of a few runs, in milliseconds. *)
  let best = ref infinity in
  for _ = 1 to 5 do
    Gc.full_major ();
    let t0 = Sys.time () in
    ignore (Sys.opaque_identity (f ()));
    let t = Sys.time () -. t0 in
    if t < !best then best := t
  done;
  !best *. 1000.

module Bench (X : S) = struct
  let of_array a = Array.fold_left (fun s x -> X.add x s) X.empty a

  let run (xs : int array) (ys : int array) (qs : int array) =
    let s1 = of_array xs and s2 = of_array ys in
    let small = of_array (Array.sub ys 0 (min 50 (Array.length ys))) in
    [
      "build", time (fun () -> of_array xs);
      "mem", time (fun () -> Array.fold_left (fun c q -> if X.mem q s1 then c + 1 else c) 0 qs);
      "remove", time (fun () -> Array.fold_left (fun s x -> X.remove x s) s1 xs);
      "union", time (fun () -> for _ = 1 to 10 do ignore (Sys.opaque_identity (X.union s1 s2)) done);
      "union small", time (fun () -> for _ = 1 to 1000 do ignore (Sys.opaque_identity (X.union small s1)) done);
      "inter", time (fun () -> for _ = 1 to 10 do ignore (Sys.opaque_identity (X.inter s1 s2)) done);
      "diff", time (fun () -> for _ = 1 to 10 do ignore (Sys.opaque_identity (X.diff s1 s2)) done);
      "subset", time (fun () -> for _ = 1 to 10 do ignore (Sys.opaque_identity (X.subset (X.inter s1 s2) s1)) done);
      "fold", time (fun () -> for _ = 1 to 10 do ignore (Sys.opaque_identity (X.fold (+) s1 0)) done);
      "kwords", float (Obj.reachable_words (Obj.repr s1)) /. 1000.;
    ]
end

module New = Bench (Maxtrie.PatSet)
module Cmp = Bench (Maxtrie.PatSetCompact)
module Old = Bench (struct include Orig let subset = subset end)
module Std = Bench (Set.Make (Int))

let () =
  Random.init 42;
  let profiles = [
    "dense 100k in [0,400k)", 100_000, (fun () -> Random.int 400_000);
    "sparse 100k in [0,2^40)", 100_000, (fun () -> Random.bits () lor (Random.bits () lsl 10) land (1 lsl 40 - 1));
    "clustered 100k", 100_000, (fun () -> (Random.int 64 lsl 30) + Random.int 20_000);
    "dense 1k in [0,4k)", 1_000, (fun () -> Random.int 4_000);
  ] in
  List.iter (fun (name, n, gen) ->
      let xs = Array.init n (fun _ -> gen ()) in
      let ys = Array.init n (fun _ -> gen ()) in
      let qs = Array.init 200_000 (fun _ -> gen ()) in
      let rn = New.run xs ys qs and rc = Cmp.run xs ys qs in
      let ro = Old.run xs ys qs and rs = Std.run xs ys qs in
      Printf.printf "\n== %s\n%-12s %10s %10s %10s %10s %8s %8s\n" name "op (ms)"
        "new" "compact" "orig" "Stdlib" "new/orig" "cmp/new";
      List.iteri (fun i (op, tn) ->
          let tc = snd (List.nth rc i) and to_ = snd (List.nth ro i)
          and ts = snd (List.nth rs i) in
          Printf.printf "%-12s %10.2f %10.2f %10.2f %10.2f %8.2f %8.2f\n"
            op tn tc to_ ts (tn /. to_) (tc /. tn))
        rn)
    profiles
