let run_test error seed count =
  let hll = Hll.make ~error in
  Printf.printf "counting %d elements using HLL with %.02f%% error rate (seed = %Ld)\n"
    count (error *. 100.0) seed;
  for i = 0 to count - 1 do
    Hll.add hll (Hll.hash_int64 (Int64.(add seed (of_int i))))
  done;
  let card = Hll.card hll in
  assert (card = Hll.card (Hll.of_string (Hll.to_string hll)));
  let m1 = max card (float count) and m2 = min card (float count) in
  let err = (m1 -. m2) /. m2 in
  Printf.printf "estimated cardinal: %.02f (error: %.02f%%)\n" card (err *. 100.0);
  (* The standard error is 1.04 / sqrt m: fail beyond 5 standard errors. *)
  let sigma = 1.04 /. sqrt (float (Hll.estimate_memory ~error)) in
  if err > 5. *. sigma then begin
    Printf.printf "FAILED: error above 5 x %.02f%%\n" (sigma *. 100.);
    exit 1
  end

let () =
  Random.self_init ();
  let seed () = Random.int64 Int64.max_int in
  run_test 0.05 (seed ()) 100000;
  run_test 0.05 (seed ()) 2000000;
  run_test 0.001 (seed ()) 100000;
  run_test 0.001 (seed ()) 2000000
