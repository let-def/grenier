module P = Physh
module R = Ref

(*let () = Gc.set { Gc.get () with Gc.minor_heap_size = 1024 * 512 }*)

let test_count = ref 0
let pass_count = ref 0
let fail_count  = ref 0

let with_label label body =
  incr test_count;
  try
    body ();
    incr pass_count;
    Printf.printf "  [PASS] %s\n%!" label
  with exn ->
    incr fail_count;
    Printf.printf "  [FAIL] %s -- %s: %s\n%!" label (Printexc.to_string exn) (Printexc.get_backtrace ())

let gc_minor () =
  let _ = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096 in
  Gc.minor ()

let gc_major () =
  Gc.full_major ()

let gc_both () =
  gc_minor ();
  gc_major ()

let gc_noise () =
  let _ = Array.init 1024 (fun i -> String.make i 'x') in
  Gc.minor ()

type box = Box of box option

let rec make_cycle n =
  if n <= 0 then () else
  let r = ref None in
  r := Some (Box !r);
  make_cycle (n - 1)


let () =
  Printf.printf "=== PHYSH STRESS TESTS ===\n%!";

  (* ---- SET TESTS ---- *)

  Printf.printf "\n--- Set: basic sanity ---\n%!" ;

  with_label "set empty length" (fun () ->
    let s = P.Set.create () in
    assert (P.Set.length s = 0)
  );

  with_label "set add + mem" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 in
    P.Set.add s x;
    assert (P.Set.mem s x);
    assert (P.Set.length s = 1)
  );

  with_label "set duplicate add" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 in
    P.Set.add s x;
    P.Set.add s x;
    assert (P.Set.length s = 1)
  );

  with_label "set physical not structural" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 in
    let y = ref 1 in
    P.Set.add s x;
    assert (P.Set.mem s x);
    assert (not (P.Set.mem s y))
  );

  Printf.printf "\n--- Set: GC stress ---\n%!" ;

  with_label "set gc after add, mem still true" (fun () ->
    let s = P.Set.create () in
    let x = ref 42 in
    P.Set.add s x;
    gc_both ();
    assert (P.Set.mem s x);
    assert (P.Set.length s = 1)
  );

  with_label "set gc between add and mem for many elements" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 200 (fun i -> ref i) in
    Array.iter (P.Set.add s) xs;
    gc_both ();
    Array.iteri (fun i x ->
      assert (P.Set.mem s x);
      if i mod 50 = 0 then gc_noise ()
    ) xs;
    assert (P.Set.length s = 200)
  );

  with_label "set interleaved add + gc_minor" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i x ->
      P.Set.add s x;
      if i mod 10 = 0 then gc_minor ()
    ) xs;
    Array.iter (fun x -> assert (P.Set.mem s x)) xs;
    assert (P.Set.length s = 500)
  );

  with_label "set interleaved add + gc_major" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i x ->
      P.Set.add s x;
      if i mod 25 = 0 then gc_major ()
    ) xs;
    Array.iter (fun x -> assert (P.Set.mem s x)) xs;
    assert (P.Set.length s = 500)
  );

  with_label "set gc during mem query" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 300 (fun i -> ref i) in
    Array.iter (P.Set.add s) xs;
    gc_both ();
    Array.iteri (fun i x ->
      if i mod 20 = 0 then gc_both ();
      assert (P.Set.mem s x)
    ) xs
  );

  with_label "set aggressive gc noise" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 1000 (fun i -> ref i) in
    Array.iteri (fun i x ->
      P.Set.add s x;
      if i mod 5 = 0 then gc_noise ()
    ) xs;
    gc_both ();
    assert (P.Set.length s = 1000);
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  with_label "set repeated create-fill-gc cycles" (fun () ->
    for c = 1 to 50 do
      let s = P.Set.create () in
      let xs = Array.init 200 (fun i -> ref (c, i)) in
      Array.iter (P.Set.add s) xs;
      gc_both ();
      Array.iter (fun x -> assert (P.Set.mem s x)) xs;
      assert (P.Set.length s = 200)
    done
  );

  Printf.printf "\n--- Set: large-scale stress ---\n%!" ;

  with_label "set 5000 elements with gc" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 5000 (fun i -> ref i) in
    Array.iteri (fun i x ->
      P.Set.add s x;
      if i mod 50 = 0 then gc_minor ()
    ) xs;
    assert (P.Set.length s = 5000);
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  with_label "set 10000 elements" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 10000 (fun i -> ref i) in
    Array.iteri (fun i x ->
      P.Set.add s x;
      if i mod 100 = 0 then gc_both ()
    ) xs;
    assert (P.Set.length s = 10000);
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  Printf.printf "\n--- Set: cyclic / shared structures ---\n%!" ;

  with_label "set cyclic structures as elements" (fun () ->
    let s = P.Set.create () in
    let r = ref None in
    r := Some (Box !r);
    let node = !r in
    P.Set.add s (Obj.repr node);
    gc_both ();
    assert (P.Set.mem s (Obj.repr node));
    assert (P.Set.length s = 1)
  );

  with_label "set many cyclic structures" (fun () ->
    let s = P.Set.create () in
    let cycles = Array.init 200 (fun _ ->
      let r = ref None in
      r := Some (Box !r);
      Obj.repr !r
    ) in
    Array.iteri (fun i c ->
      P.Set.add s c;
      if i mod 20 = 0 then gc_minor ()
    ) cycles;
    gc_both ();
    Array.iter (fun c -> assert (P.Set.mem s c)) cycles;
    assert (P.Set.length s = 200)
  );

  with_label "set shared substructures" (fun () ->
    let s = P.Set.create () in
    let shared = ref 999 in
    let xs = Array.init 300 (fun i -> (ref i, shared)) in
    Array.iter (fun (a, _) -> P.Set.add s (Obj.repr a)) xs;
    gc_both ();
    Array.iter (fun (a, _) -> assert (P.Set.mem s (Obj.repr a))) xs
  );

  Printf.printf "\n--- Set: long-lived table ---\n%!" ;

  with_label "set long-lived table under gc" (fun () ->
    let s = P.Set.create () in
    let all = ref [] in
    for i = 1 to 5000 do
      let x = ref i in
      P.Set.add s x;
      all := x :: !all;
      if i mod 50 = 0 then (
        gc_both ();
        List.iter (fun v -> assert (P.Set.mem s v)) !all
      )
    done;
    assert (P.Set.length s = 5000)
  );

  Printf.printf "\n--- Set: ref comparison ---\n%!" ;

  with_label "set matches ref: sequential add" (fun () ->
    let ps = P.Set.create () in
    let rs = R.Set.create () in
    let xs = Array.init 500 (fun i -> ref i) in
    Array.iter (fun x -> P.Set.add ps x; R.Set.add rs x) xs;
    gc_both ();
    Array.iter (fun x ->
      assert (P.Set.mem ps x = R.Set.mem rs x)
    ) xs;
    assert (P.Set.length ps = R.Set.length rs)
  );

  with_label "set matches ref: interleaved gc" (fun () ->
    let ps = P.Set.create () in
    let rs = R.Set.create () in
    let xs = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i x ->
      P.Set.add ps x;
      R.Set.add rs x;
      if i mod 10 = 0 then gc_both ()
    ) xs;
    gc_both ();
    Array.iter (fun x ->
      assert (P.Set.mem ps x = R.Set.mem rs x)
    ) xs;
    assert (P.Set.length ps = R.Set.length rs)
  );

  (* ---- MAP TESTS ---- *)

  Printf.printf "\n--- Map: basic sanity ---\n%!" ;

  with_label "map empty length" (fun () ->
    let m = P.Map.create () in
    assert (P.Map.length m = 0)
  );

  with_label "map add + find" (fun () ->
    let m = P.Map.create () in
    let k = ref 1 in
    P.Map.add m k "hello";
    assert (P.Map.find m k = "hello");
    assert (P.Map.length m = 1)
  );

  with_label "map overwrite value" (fun () ->
    let m = P.Map.create () in
    let k = ref 1 in
    P.Map.add m k "first";
    P.Map.add m k "second";
    assert (P.Map.find m k = "second");
    assert (P.Map.length m = 1)
  );

  with_label "map physical not structural keys" (fun () ->
    let m = P.Map.create () in
    let k1 = ref 1 in
    let k2 = ref 1 in
    P.Map.add m k1 "a";
    assert (P.Map.find m k1 = "a");
    try
      ignore (P.Map.find m k2);
      assert false
    with Not_found -> ()
  );

  with_label "map Not_found for missing key" (fun () ->
    let m = P.Map.create () in
    let k = ref 99 in
    try
      ignore (P.Map.find m k);
      assert false
    with Not_found -> ()
  );

  Printf.printf "\n--- Map: GC stress ---\n%!" ;

  with_label "map gc after add, find still works" (fun () ->
    let m = P.Map.create () in
    let k = ref 42 in
    P.Map.add m k "val";
    gc_both ();
    assert (P.Map.find m k = "val")
  );

  with_label "map many entries with gc between adds" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add m k i;
      if i mod 10 = 0 then gc_minor ()
    ) keys;
    gc_both ();
    Array.iteri (fun i k ->
      assert (P.Map.find m k = i)
    ) keys;
    assert (P.Map.length m = 500)
  );

  with_label "map gc_major during adds" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add m k i;
      if i mod 25 = 0 then gc_major ()
    ) keys;
    gc_both ();
    Array.iteri (fun i k ->
      assert (P.Map.find m k = i)
    ) keys
  );

  with_label "map gc during find" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 300 (fun i -> ref i) in
    Array.iteri (fun i k -> P.Map.add m k i) keys;
    gc_both ();
    Array.iteri (fun i k ->
      if i mod 20 = 0 then gc_both ();
      assert (P.Map.find m k = i)
    ) keys
  );

  with_label "map aggressive gc noise" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 1000 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add m k i;
      if i mod 5 = 0 then gc_noise ()
    ) keys;
    gc_both ();
    assert (P.Map.length m = 1000);
    Array.iteri (fun i k -> assert (P.Map.find m k = i)) keys
  );

  with_label "map repeated create-fill-gc cycles" (fun () ->
    for c = 1 to 50 do
      let m = P.Map.create () in
      let keys = Array.init 200 (fun i -> ref (c, i)) in
      Array.iteri (fun i k -> P.Map.add m k (c * 1000 + i)) keys;
      gc_both ();
      Array.iteri (fun i k ->
        assert (P.Map.find m k = c * 1000 + i)
      ) keys;
      assert (P.Map.length m = 200)
    done
  );

  Printf.printf "\n--- Map: large-scale stress ---\n%!" ;

  with_label "map 5000 entries with gc" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 5000 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add m k i;
      if i mod 50 = 0 then gc_minor ()
    ) keys;
    assert (P.Map.length m = 5000);
    gc_both ();
    Array.iteri (fun i k -> assert (P.Map.find m k = i)) keys
  );

  with_label "map 10000 entries" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 10000 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add m k i;
      if i mod 100 = 0 then gc_both ()
    ) keys;
    assert (P.Map.length m = 10000);
    gc_both ();
    Array.iteri (fun i k -> assert (P.Map.find m k = i)) keys
  );

  Printf.printf "\n--- Map: value types stress ---\n%!" ;

  with_label "map large values as data" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 200 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add m k (Array.make (i + 100) i);
      if i mod 20 = 0 then gc_minor ()
    ) keys;
    gc_both ();
    Array.iteri (fun i k ->
      let v = P.Map.find m k in
      assert (Array.length v = i + 100)
    ) keys
  );

  with_label "map cyclic values" (fun () ->
    let m = P.Map.create () in
    let k = ref 1 in
    let r = ref None in
    r := Some (Box !r);
    P.Map.add m k !r;
    gc_both ();
    assert (P.Map.find m k == !r)
  );

  Printf.printf "\n--- Map: cyclic keys ---\n%!" ;

  with_label "map cyclic structures as keys" (fun () ->
    let m = P.Map.create () in
    let r = ref None in
    let node = Obj.new_block 1 1 in
    r := Some (Obj.repr node);
    P.Map.add m (Obj.repr node) "cyclic";
    gc_both ();
    assert (P.Map.find m (Obj.repr node) = "cyclic")
  );

  with_label "map many cyclic keys" (fun () ->
    let m = P.Map.create () in
    let cycles = Array.init 200 (fun _ ->
      Obj.new_block 1 1
    ) in
    Array.iteri (fun i c ->
      P.Map.add m c i;
      if i mod 20 = 0 then gc_minor ()
    ) cycles;
    gc_both ();
    Array.iteri (fun i c ->
      assert (P.Map.find m c = i)
    ) cycles;
    assert (P.Map.length m = 200)
  );

  Printf.printf "\n--- Map: long-lived table ---\n%!" ;

  with_label "map long-lived table under gc" (fun () ->
    let m = P.Map.create () in
    let all_keys = ref [] in
    for i = 1 to 5000 do
      let k = ref i in
      P.Map.add m k i;
      all_keys := k :: !all_keys;
      if i mod 50 = 0 then (
        gc_both ();
        List.iter (fun k -> assert (P.Map.find m k = !k)) !all_keys
      )
    done;
    assert (P.Map.length m = 5000)
  );

  with_label "map long-lived, overwrite under gc" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 500 (fun i -> ref i) in
    Array.iter (fun k -> P.Map.add m k 0) keys;
    for round = 1 to 20 do
      gc_both ();
      Array.iter (fun k -> P.Map.add m k round) keys;
      Array.iter (fun k -> assert (P.Map.find m k = round)) keys
    done
  );

  Printf.printf "\n--- Map: ref comparison ---\n%!" ;

  with_label "map matches ref: sequential add" (fun () ->
    let pm = P.Map.create () in
    let rm = R.Map.create () in
    let keys = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add pm k i;
      R.Map.add rm k i
    ) keys;
    gc_both ();
    Array.iter (fun k ->
      assert (P.Map.find pm k = R.Map.find rm k)
    ) keys;
    assert (P.Map.length pm = R.Map.length rm)
  );

  with_label "map matches ref: interleaved gc" (fun () ->
    let pm = P.Map.create () in
    let rm = R.Map.create () in
    let keys = Array.init 500 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add pm k i;
      R.Map.add rm k i;
      if i mod 10 = 0 then gc_both ()
    ) keys;
    gc_both ();
    Array.iter (fun k ->
      assert (P.Map.find pm k = R.Map.find rm k)
    ) keys;
    assert (P.Map.length pm = R.Map.length rm)
  );

  with_label "map matches ref: overwrites + gc" (fun () ->
    let pm = P.Map.create () in
    let rm = R.Map.create () in
    let keys = Array.init 300 (fun i -> ref i) in
    Array.iteri (fun i k ->
      P.Map.add pm k i;
      R.Map.add rm k i;
      P.Map.add pm k (-i);
      R.Map.add rm k (-i)
    ) keys;
    gc_both ();
    Array.iter (fun k ->
      assert (P.Map.find pm k = R.Map.find rm k)
    ) keys;
    assert (P.Map.length pm = R.Map.length rm)
  );

  (* ---- MIXED STRESS ---- *)

  Printf.printf "\n--- Mixed: sets and maps together ---\n%!" ;

  with_label "mixed set + map interleaved with gc" (fun () ->
    let s = P.Set.create () in
    let m = P.Map.create () in
    let keys = Array.init 1000 (fun i -> ref i) in
    Array.iteri (fun i k ->
      if i mod 3 = 0 then (
        P.Set.add s k;
        gc_minor ()
      ) else if i mod 3 = 1 then (
        P.Map.add m k i;
        gc_minor ()
      ) else (
        gc_major ()
      )
    ) keys;
    gc_both ();
    Array.iteri (fun i k ->
      if i mod 3 = 0 then assert (P.Set.mem s k);
      if i mod 3 = 1 then assert (P.Map.find m k = i)
    ) keys
  );

  Printf.printf "\n--- Summary ---\n%!";
  Printf.printf "  Passed: %d / %d  (failed: %d)\n%!"
    !pass_count !test_count !fail_count;
  if !fail_count > 0 then exit 2
