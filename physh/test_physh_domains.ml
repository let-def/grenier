(* Multi-domain stress tests (OCaml 5 only).

   Minor collections are stop-the-world: when another domain allocates a lot,
   collections can happen inside the allocations performed by physh stubs,
   and the minor collection counter is incremented before this domain is
   stopped. A domain calling Gc.compact similarly moves objects under our
   feet. Tables are only used by the domain that created them. *)

module P = Physh

let failures = Atomic.make 0

let check label cond =
  if not cond then begin
    Atomic.incr failures;
    Printf.printf "  [FAIL] %s\n%!" label;
    failwith label
  end

(* Allocate continuously, forcing frequent minor collections on all domains;
   optionally compact the heap from time to time. *)
let churn ~compact stop () =
  let n = ref 0 in
  while not (Atomic.get stop) do
    for _ = 1 to 1000 do ignore (Sys.opaque_identity (Array.make 200 0)) done;
    incr n;
    if compact && !n mod 50 = 0 then Gc.compact ()
  done

(* Random adds and overwrites on a map and a set, with immediate and boxed
   keys, checked against a shadow array. Some tables are kept alive across
   rounds and re-checked later. *)
let worker ~rounds ~seed () =
  let rng = Random.State.make [|seed|] in
  let survivors = ref [] in
  for r = 1 to rounds do
    let m = P.Map.create () and s = P.Set.create () in
    let n = 50 + Random.State.int rng 2000 in
    let ks = Array.init n (fun i ->
        if i mod 7 = 0 then Obj.magic i else Obj.repr (ref i)) in
    let vs = Array.make n (-1) in
    Array.iteri (fun i k ->
        (* fresh key: the prior state is "absent" *)
        let fresh =
          if i land 1 = 0
          then P.Map.add_if_new m k i
          else P.Map.add_overwrite m k i
        in
        check "map: fresh key present" (fresh = None);
        vs.(i) <- i;
        check "set: fresh key present" (not (P.Set.add_if_new s k));
        (* existing key: the prior state is its current binding *)
        let j = Random.State.int rng (i + 1) in
        if Random.State.bool rng then begin
          check "map: wrong prior binding"
            (P.Map.add_overwrite m ks.(j) (j + r) = Some vs.(j));
          vs.(j) <- j + r
        end else
          check "map: wrong prior binding"
            (P.Map.add_if_new m ks.(j) (-1) = Some vs.(j));
        check "set: existing key absent" (P.Set.add_if_new s ks.(j));
        if Random.State.int rng 100 = 0 then
          ignore (P.Map.find m ks.(Random.State.int rng (i + 1))))
      ks;
    (* remove a fifth of the keys *)
    let removed i = i mod 5 = 1 in
    Array.iteri (fun i k ->
        if removed i then (P.Map.remove m k; P.Set.remove s k)) ks;
    let n = n - (n + 3) / 5 in
    let check_tables () =
      Array.iteri (fun i k ->
          if removed i then begin
            check "map: removed key" (not (P.Map.mem m k));
            check "set: removed key" (not (P.Set.mem s k))
          end else begin
            check "map: wrong value" (P.Map.find_opt m k = Some vs.(i));
            check "set: key lost" (P.Set.mem s k)
          end)
        ks;
      check "map: length" (P.Map.length m = n);
      check "set: length" (P.Set.length s = n);
      check "map: fold" (P.Map.fold (fun _ _ c -> c + 1) m 0 = n);
      check "set: fold" (P.Set.fold (fun _ c -> c + 1) s 0 = n);
      check "set: spurious mem" (not (P.Set.mem s (Obj.repr (ref 0))))
    in
    check_tables ();
    survivors :=
      check_tables :: (if List.length !survivors > 20 then [] else !survivors);
    if r mod 10 = 0 then List.iter (fun f -> f ()) !survivors
  done

let run label ~compact ~workers ~rounds =
  let stop = Atomic.make false in
  let churner = Domain.spawn (churn ~compact stop) in
  let ds = List.init workers (fun i ->
      Domain.spawn (fun () ->
          try worker ~rounds ~seed:i ()
          with exn ->
            if Atomic.get failures = 0 then Atomic.incr failures;
            Printf.printf "  [FAIL] %s: %s\n%!" label (Printexc.to_string exn)))
  in
  List.iter Domain.join ds;
  Atomic.set stop true;
  Domain.join churner;
  if Atomic.get failures = 0 then Printf.printf "  [PASS] %s\n%!" label

let () =
  Printf.printf "=== PHYSH MULTI-DOMAIN TESTS ===\n%!";
  run "one worker, minor GCs from another domain"
    ~compact:false ~workers:1 ~rounds:500;
  run "one worker, compactions from another domain"
    ~compact:true ~workers:1 ~rounds:500;
  run "three workers, compactions from another domain"
    ~compact:true ~workers:3 ~rounds:150;
  if Atomic.get failures > 0 then exit 2
