(* PatSet.union vs PatSet.Sharing.union.

   Overhead: unions of independent random sets, where there is next to
   nothing to share.

   Benefit: unions of sets built independently from (nearly) the same
   elements, so that their equal subtrees are physically distinct. For each
   scenario we report the first union (on fresh copies), the next union of
   the same arguments (after Sharing.union has unified them), and the words
   reachable from both arguments and the result. *)

module S = Maxtrie.PatSet

let best_of n f =
  (* Best of [n] runs of [f ()], which returns the time of its measured part,
     in milliseconds. *)
  let best = ref infinity in
  for _ = 1 to n do
    let t = f () in
    if t < !best then best := t
  done;
  !best

let timed g =
  Gc.full_major ();
  let t0 = Sys.time () in
  ignore (Sys.opaque_identity (g ()));
  (Sys.time () -. t0) *. 1000.

let shuffle a =
  let a = Array.copy a in
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
  done;
  a

let build a = Array.fold_left (fun s x -> S.add x s) S.empty (shuffle a)

let profiles = [
  "dense 100k in [0,400k)", 100_000, (fun () -> Random.int 400_000);
  "sparse 100k in [0,2^40)", 100_000,
  (fun () -> Random.bits () lor (Random.bits () lsl 10) land (1 lsl 40 - 1));
  "clustered 100k", 100_000, (fun () -> (Random.int 64 lsl 30) + Random.int 20_000);
  "dense 1k in [0,4k)", 1_000, (fun () -> Random.int 4_000);
]

let overhead () =
  print_endline "== Overhead: independent sets (ms, best of 5)";
  Printf.printf "%-26s %-12s %9s %9s %7s\n" "" "op" "plain" "Sharing" "ratio";
  List.iter (fun (name, n, gen) ->
      let s1 = build (Array.init n (fun _ -> gen ())) in
      let s2 = build (Array.init n (fun _ -> gen ())) in
      let small = build (Array.init 50 (fun _ -> gen ())) in
      let row op reps x y =
        let run u = best_of 5 (fun () ->
            timed (fun () -> for _ = 1 to reps do ignore (Sys.opaque_identity (u x y)) done))
        in
        let tp = run S.union and ts = run S.Sharing.union in
        Printf.printf "%-26s %-12s %9.2f %9.2f %7.2f\n" name op tp ts (ts /. tp)
      in
      row "union" 10 s1 s2;
      row "union small" 1000 small s1)
    profiles

(* Scenarios: two element arrays, from which the arguments are rebuilt. *)
let scenarios gen n =
  let base = Array.init n (fun _ -> gen ()) in
  let extra k = Array.init k (fun _ -> gen ()) in
  [
    "equal", base, base;
    "+10 each", Array.append base (extra 10), Array.append base (extra 10);
    "+1000 each", Array.append base (extra 1000), Array.append base (extra 1000);
    "subset (-10)", Array.sub base 10 (n - 10), base;
  ]

let benefit () =
  print_endline "\n== Benefit: independently built, (nearly) equal sets";
  print_endline "   first: union of fresh copies (ms, best of 5); next: 10 more unions";
  print_endline "   of the same arguments (ms); kwords: reachable from (a, b, a U b)";
  Printf.printf "%-26s %-13s %8s %8s %8s %8s %8s %8s\n"
    "" "" "first" "first" "next" "next" "kwords" "kwords";
  Printf.printf "%-26s %-13s %8s %8s %8s %8s %8s %8s\n"
    "" "scenario" "plain" "Sharing" "plain" "Sharing" "plain" "Sharing";
  List.iter (fun (name, n, gen) ->
      List.iter (fun (sc, xa, xb) ->
          let measure u =
            let first = best_of 5 (fun () ->
                let a = build xa and b = build xb in
                timed (fun () -> u a b))
            in
            let a = build xa and b = build xb in
            let r = u a b in
            let next = best_of 5 (fun () ->
                timed (fun () -> for _ = 1 to 10 do ignore (Sys.opaque_identity (u a b)) done))
            in
            let words = Obj.reachable_words (Obj.repr (a, b, r)) in
            first, next, float words /. 1000.
          in
          let fp, np, wp = measure S.union in
          let fs, ns, ws = measure S.Sharing.union in
          Printf.printf "%-26s %-13s %8.2f %8.2f %8.2f %8.2f %8.0f %8.0f\n"
            name sc fp fs np ns wp ws)
        (scenarios gen n))
    profiles

let () =
  Random.init 42;
  overhead ();
  benefit ()
