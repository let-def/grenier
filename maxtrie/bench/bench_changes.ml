(* PatMap.changes between a map and a version derived from it by a few
   updates: the cost should depend on the updates, not on the size. *)

module M = Maxtrie.PatMap

let time_us f =
  let reps = ref 0 and t0 = Unix.gettimeofday () in
  while Unix.gettimeofday () -. t0 < 0.2 do f (); incr reps done;
  (Unix.gettimeofday () -. t0) /. float !reps *. 1e6

let () =
  Random.init 5;
  let n = 1_000_000 in
  let keys = Array.init n (fun _ -> Random.bits () land (1 lsl 40 - 1)) in
  (* Boxed values: unchanged bindings are physically shared. *)
  let a = Array.fold_left (fun m k -> M.add k (ref k) m) M.empty keys in
  let maxk = fst (M.max_binding a) in
  let count = ref 0 in
  let f _ _ _ = incr count in
  let scenarios = [
    "10 rebindings of existing keys", (fun m ->
        let m = ref m in
        for i = 0 to 9 do m := M.add keys.(i * 977) (ref 0) !m done; !m);
    "1 new maximum (shifts the root spine)", (fun m -> M.add (maxk + 1) (ref 0) m);
    "remove the maximum", (fun m -> M.remove maxk m);
    "10 new maxima", (fun m ->
        let m = ref m in
        for i = 1 to 10 do m := M.add (maxk + i * 1000) (ref 0) !m done; !m);
    "10 removals + 10 insertions", (fun m ->
        let m = ref m in
        for i = 0 to 9 do
          m := M.remove keys.(i * 7919) !m;
          m := M.add (Random.bits () land (1 lsl 40 - 1)) (ref 0) !m
        done; !m);
  ] in
  Printf.printf "map of %d bindings\n" (M.cardinal a);
  Printf.printf "  full fold (baseline): %10.1f us\n" (time_us (fun () -> ignore (M.fold (fun _ _ c -> c + 1) a 0)));
  List.iter (fun (name, edit) ->
      let b = edit a in
      count := 0;
      M.changes f a b;
      let calls = !count in
      let t = time_us (fun () -> M.changes f a b) in
      Printf.printf "  %-38s %8.2f us  (%d changes reported)\n" name t calls)
    scenarios
