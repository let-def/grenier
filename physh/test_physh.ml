module P = Physh
module R = Ref

module D : sig
  type 'a t
  type 'a view = Int of int | Obj of 'a
  val pack : 'a view -> 'a t
  val unpack : 'a t -> 'a view
end = struct
  type 'a view = Int of int | Obj of 'a
  type 'a t = 'a view

  let pack = function
    | Int i -> Obj.magic i
    | Obj _ as x -> x

  let unpack t =
    if Obj.is_int (Obj.magic t)
    then Int (Obj.magic t)
    else t
end

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
    Printf.printf "  [FAIL] %s -- %s: %s\n%!"
      label (Printexc.to_string exn) (Printexc.get_backtrace ())

let gc_minor () =
  let _ = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096 in
  Gc.minor ()

let gc_major () = Gc.full_major ()
let gc_both  () = gc_minor (); gc_major ()

let gc_noise () =
  let _ = Array.init 1024 (fun i -> String.make i 'x') in
  Gc.minor ()

let iteri_check a ~f =
  Array.iteri (fun i _ ->
    if i mod 20 = 0 then gc_both ();
    f i
  ) a

(* ---- SET TESTS ---- *)

let () =
  Printf.printf "=== PHYSH STRESS TESTS ===\n%!";

  Printf.printf "\n--- Set: basic ---\n%!" ;

  with_label "set empty" (fun () ->
    assert (P.Set.length (P.Set.create ()) = 0)
  );

  with_label "set add + mem + length" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 in
    P.Set.add s x;
    assert (P.Set.mem s x);
    assert (P.Set.length s = 1)
  );

  with_label "set duplicate add" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 in
    P.Set.add s x; P.Set.add s x;
    assert (P.Set.length s = 1)
  );

  with_label "set physical equality" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 and y = ref 1 in
    P.Set.add s x;
    assert (P.Set.mem s x);
    assert (not (P.Set.mem s y))
  );

  with_label "set int keys (D)" (fun () ->
    let s = P.Set.create () in
    let k1 = D.pack (D.Int 42) in
    let k2 = D.pack (D.Int 42) in
    let k3 = D.pack (D.Int 99) in
    P.Set.add s k1;
    assert (P.Set.mem s k1);
    assert (P.Set.mem s k2);
    assert (not (P.Set.mem s k3));
    assert (P.Set.length s = 1)
  );

  with_label "set obj keys (D)" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 and y = ref 1 in
    let k1 = D.pack (D.Obj x) in
    let k2 = D.pack (D.Obj y) in
    P.Set.add s k1;
    assert (P.Set.mem s k1);
    assert (not (P.Set.mem s k2))
  );

  Printf.printf "\n--- Set: GC stress (obj keys) ---\n%!" ;

  with_label "set gc after add" (fun () ->
    let s = P.Set.create () in
    let x = ref 42 in
    P.Set.add s x;
    gc_both ();
    assert (P.Set.mem s x);
    assert (P.Set.length s = 1)
  );

  with_label "set interleaved add + minor gc" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 2000 (fun i -> ref i) in
    Array.iter (P.Set.add s) xs;
    (* simpler: just iterate with gc *)
    let s = P.Set.create () in
    Array.iteri (fun i _ ->
      P.Set.add s xs.(i);
      if i mod 10 = 0 then gc_minor ()
    ) xs;
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs;
    assert (P.Set.length s = 2000)
  );

  with_label "set interleaved add + major gc" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 2000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Set.add s xs.(i);
      if i mod 25 = 0 then gc_major ()
    ) xs;
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs;
    assert (P.Set.length s = 2000)
  );

  with_label "set gc during mem queries" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 2000 (fun i -> ref i) in
    Array.iter (P.Set.add s) xs;
    gc_both ();
    iteri_check xs ~f:(fun i -> assert (P.Set.mem s xs.(i)))
  );

  with_label "set aggressive gc noise" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 5000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Set.add s xs.(i);
      if i mod 5 = 0 then gc_noise ();
      if i mod 25 = 0 then gc_minor ();
      if i mod 50 = 0 then gc_major ()
    ) xs;
    gc_both ();
    assert (P.Set.length s = 5000);
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  with_label "set create-fill-gc cycles" (fun () ->
    for _c = 1 to 100 do
      let s = P.Set.create () in
      let xs = Array.init 500 (fun i -> ref i) in
      Array.iter (P.Set.add s) xs;
      gc_both ();
      Array.iter (fun x -> assert (P.Set.mem s x)) xs;
      assert (P.Set.length s = 500)
    done
  );

  Printf.printf "\n--- Set: GC stress (int keys via D) ---\n%!" ;

  with_label "set int keys interleaved gc" (fun () ->
    let s = P.Set.create () in
    let ks = Array.init 5000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      P.Set.add s ks.(i);
      if i mod 10 = 0 then gc_minor ();
      if i mod 50 = 0 then gc_major ();
      if i mod 100 = 0 then gc_noise ()
    ) ks;
    gc_both ();
    Array.iter (fun k -> assert (P.Set.mem s k)) ks;
    assert (P.Set.length s = 5000)
  );

  with_label "set int keys gc during mem" (fun () ->
    let s = P.Set.create () in
    let ks = Array.init 5000 (fun i -> D.pack (D.Int i)) in
    Array.iter (P.Set.add s) ks;
    gc_both ();
    iteri_check ks ~f:(fun i -> assert (P.Set.mem s ks.(i)))
  );

  Printf.printf "\n--- Set: large-scale (obj keys) ---\n%!" ;

  with_label "set 20000 elements" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 20000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Set.add s xs.(i);
      if i mod 50 = 0 then gc_minor ();
      if i mod 200 = 0 then gc_major ();
      if i mod 500 = 0 then gc_noise ()
    ) xs;
    assert (P.Set.length s = 20000);
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  with_label "set 50000 elements" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 50000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Set.add s xs.(i);
      if i mod 100 = 0 then gc_minor ();
      if i mod 500 = 0 then gc_major ();
      if i mod 1000 = 0 then gc_noise ()
    ) xs;
    assert (P.Set.length s = 50000);
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  with_label "set 100000 elements" (fun () ->
    let s = P.Set.create () in
    let xs = Array.init 100000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Set.add s xs.(i);
      if i mod 500 = 0 then gc_minor ();
      if i mod 2000 = 0 then gc_major ();
      if i mod 5000 = 0 then gc_noise ()
    ) xs;
    assert (P.Set.length s = 100000);
    gc_both ();
    Array.iter (fun x -> assert (P.Set.mem s x)) xs
  );

  Printf.printf "\n--- Set: large-scale (int keys via D) ---\n%!" ;

  with_label "set int 50000" (fun () ->
    let s = P.Set.create () in
    let ks = Array.init 50000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      P.Set.add s ks.(i);
      if i mod 100 = 0 then gc_minor ();
      if i mod 500 = 0 then gc_major ();
      if i mod 1000 = 0 then gc_noise ()
    ) ks;
    assert (P.Set.length s = 50000);
    gc_both ();
    Array.iter (fun k -> assert (P.Set.mem s k)) ks
  );

  with_label "set int 100000" (fun () ->
    let s = P.Set.create () in
    let ks = Array.init 100000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      P.Set.add s ks.(i);
      if i mod 500 = 0 then gc_minor ();
      if i mod 2000 = 0 then gc_major ();
      if i mod 5000 = 0 then gc_noise ()
    ) ks;
    assert (P.Set.length s = 100000);
    gc_both ();
    Array.iter (fun k -> assert (P.Set.mem s k)) ks
  );

  Printf.printf "\n--- Set: long-lived table ---\n%!" ;

  with_label "set long-lived incremental" (fun () ->
    let s = P.Set.create () in
    let all = ref [] in
    for i = 1 to 10000 do
      let x = ref i in
      P.Set.add s x;
      all := x :: !all;
      if i mod 100 = 0 then (
        gc_both ();
        List.iter (fun v -> assert (P.Set.mem s v)) !all
      )
    done;
    assert (P.Set.length s = 10000)
  );

  Printf.printf "\n--- Set: ref comparison ---\n%!" ;

  with_label "set vs ref: obj keys 10000" (fun () ->
    let ps = P.Set.create () and rs = R.Set.create () in
    let xs = Array.init 10000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      let x = xs.(i) in
      P.Set.add ps x; R.Set.add rs x;
      if i mod 50 = 0 then gc_minor ();
      if i mod 200 = 0 then gc_major ();
      if i mod 500 = 0 then gc_noise ()
    ) xs;
    gc_both ();
    Array.iter (fun x ->
      assert (P.Set.mem ps x = R.Set.mem rs x)
    ) xs;
    assert (P.Set.length ps = R.Set.length rs)
  );

  with_label "set vs ref: int keys 10000" (fun () ->
    let ps = P.Set.create () and rs = R.Set.create () in
    let ks = Array.init 10000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      let k = ks.(i) in
      P.Set.add ps k; R.Set.add rs k;
      if i mod 50 = 0 then gc_minor ();
      if i mod 200 = 0 then gc_major ();
      if i mod 500 = 0 then gc_noise ()
    ) ks;
    gc_both ();
    Array.iter (fun k ->
      assert (P.Set.mem ps k = R.Set.mem rs k)
    ) ks;
    assert (P.Set.length ps = R.Set.length rs)
  );

  (* ---- MAP TESTS ---- *)

  Printf.printf "\n--- Map: basic ---\n%!" ;

  with_label "map empty" (fun () ->
    assert (P.Map.length (P.Map.create ()) = 0)
  );

  with_label "map add + find + length" (fun () ->
    let m = P.Map.create () in
    let k = ref 1 in
    P.Map.add m k 42;
    assert (P.Map.find m k = 42);
    assert (P.Map.length m = 1)
  );

  with_label "map overwrite" (fun () ->
    let m = P.Map.create () in
    let k = ref 1 in
    P.Map.add m k 1; P.Map.add m k 2;
    assert (P.Map.find m k = 2);
    assert (P.Map.length m = 1)
  );

  with_label "map physical keys" (fun () ->
    let m = P.Map.create () in
    let k1 = ref 1 and k2 = ref 1 in
    P.Map.add m k1 "a";
    assert (P.Map.find m k1 = "a");
    try ignore (P.Map.find m k2); assert false
    with Not_found -> ()
  );

  with_label "map Not_found" (fun () ->
    let m = P.Map.create () in
    try ignore (P.Map.find m (ref 99)); assert false
    with Not_found -> ()
  );

  with_label "map int keys (D)" (fun () ->
    let m = P.Map.create () in
    let k1 = D.pack (D.Int 42) in
    let k2 = D.pack (D.Int 42) in
    let k3 = D.pack (D.Int 99) in
    P.Map.add m k1 "ok";
    assert (P.Map.find m k1 = "ok");
    assert (P.Map.find m k2 = "ok");
    try ignore (P.Map.find m k3); assert false
    with Not_found -> ()
  );

  Printf.printf "\n--- Map: GC stress (obj keys) ---\n%!" ;

  with_label "map gc after add" (fun () ->
    let m = P.Map.create () in
    let k = ref 42 in
    P.Map.add m k "v";
    gc_both ();
    assert (P.Map.find m k = "v")
  );

  with_label "map interleaved add + minor gc" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 2000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 10 = 0 then gc_minor ()
    ) ks;
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks;
    assert (P.Map.length m = 2000)
  );

  with_label "map interleaved add + major gc" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 2000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 25 = 0 then gc_major ()
    ) ks;
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  with_label "map gc during find" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 2000 (fun i -> ref i) in
    Array.iteri (fun i _ -> P.Map.add m ks.(i) i) ks;
    gc_both ();
    iteri_check ks ~f:(fun i -> assert (P.Map.find m ks.(i) = i))
  );

  with_label "map aggressive gc noise" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 5000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 5 = 0 then gc_noise ();
      if i mod 25 = 0 then gc_minor ();
      if i mod 50 = 0 then gc_major ()
    ) ks;
    gc_both ();
    assert (P.Map.length m = 5000);
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  with_label "map create-fill-gc cycles" (fun () ->
    for _c = 1 to 100 do
      let m = P.Map.create () in
      let ks = Array.init 500 (fun i -> ref i) in
      Array.iteri (fun i _ -> P.Map.add m ks.(i) i) ks;
      gc_both ();
      Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks;
      assert (P.Map.length m = 500)
    done
  );

  Printf.printf "\n--- Map: GC stress (int keys via D) ---\n%!" ;

  with_label "map int keys interleaved gc" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 5000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 10 = 0 then gc_minor ();
      if i mod 50 = 0 then gc_major ();
      if i mod 100 = 0 then gc_noise ()
    ) ks;
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks;
    assert (P.Map.length m = 5000)
  );

  Printf.printf "\n--- Map: large-scale (obj keys) ---\n%!" ;

  with_label "map 20000 entries" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 20000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 50 = 0 then gc_minor ();
      if i mod 200 = 0 then gc_major ();
      if i mod 500 = 0 then gc_noise ()
    ) ks;
    assert (P.Map.length m = 20000);
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  with_label "map 50000 entries" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 50000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 100 = 0 then gc_minor ();
      if i mod 500 = 0 then gc_major ();
      if i mod 1000 = 0 then gc_noise ()
    ) ks;
    assert (P.Map.length m = 50000);
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  with_label "map 100000 entries" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 100000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 500 = 0 then gc_minor ();
      if i mod 2000 = 0 then gc_major ();
      if i mod 5000 = 0 then gc_noise ()
    ) ks;
    assert (P.Map.length m = 100000);
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  Printf.printf "\n--- Map: large-scale (int keys via D) ---\n%!" ;

  with_label "map int 50000" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 50000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 100 = 0 then gc_minor ();
      if i mod 500 = 0 then gc_major ();
      if i mod 1000 = 0 then gc_noise ()
    ) ks;
    assert (P.Map.length m = 50000);
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  with_label "map int 100000" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 100000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      P.Map.add m ks.(i) i;
      if i mod 500 = 0 then gc_minor ();
      if i mod 2000 = 0 then gc_major ();
      if i mod 5000 = 0 then gc_noise ()
    ) ks;
    assert (P.Map.length m = 100000);
    gc_both ();
    Array.iteri (fun i _ -> assert (P.Map.find m ks.(i) = i)) ks
  );

  Printf.printf "\n--- Map: long-lived table ---\n%!" ;

  with_label "map long-lived incremental" (fun () ->
    let m = P.Map.create () in
    let all_keys = ref [] in
    for i = 1 to 10000 do
      let k = ref i in
      P.Map.add m k i;
      all_keys := k :: !all_keys;
      if i mod 100 = 0 then (
        gc_both ();
        List.iter (fun v -> assert (P.Map.find m v = !v)) !all_keys
      )
    done;
    assert (P.Map.length m = 10000)
  );

  with_label "map long-lived overwrite under gc" (fun () ->
    let m = P.Map.create () in
    let ks = Array.init 2000 (fun i -> ref i) in
    Array.iter (fun k -> P.Map.add m k 0) ks;
    for round = 1 to 50 do
      gc_both ();
      Array.iter (fun k -> P.Map.add m k round) ks;
      Array.iter (fun k -> assert (P.Map.find m k = round)) ks
    done
  );

  Printf.printf "\n--- Map: ref comparison ---\n%!" ;

  with_label "map vs ref: obj keys 10000" (fun () ->
    let pm = P.Map.create () and rm = R.Map.create () in
    let ks = Array.init 10000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      let k = ks.(i) in
      P.Map.add pm k i; R.Map.add rm k i;
      if i mod 50 = 0 then gc_minor ();
      if i mod 200 = 0 then gc_major ();
      if i mod 500 = 0 then gc_noise ()
    ) ks;
    gc_both ();
    Array.iter (fun k ->
      assert (P.Map.find pm k = R.Map.find rm k)
    ) ks;
    assert (P.Map.length pm = R.Map.length rm)
  );

  with_label "map vs ref: int keys 10000" (fun () ->
    let pm = P.Map.create () and rm = R.Map.create () in
    let ks = Array.init 10000 (fun i -> D.pack (D.Int i)) in
    Array.iteri (fun i _ ->
      let k = ks.(i) in
      P.Map.add pm k i; R.Map.add rm k i;
      if i mod 50 = 0 then gc_minor ();
      if i mod 200 = 0 then gc_major ();
      if i mod 500 = 0 then gc_noise ()
    ) ks;
    gc_both ();
    Array.iter (fun k ->
      assert (P.Map.find pm k = R.Map.find rm k)
    ) ks;
    assert (P.Map.length pm = R.Map.length rm)
  );

  with_label "map vs ref: overwrites + gc" (fun () ->
    let pm = P.Map.create () and rm = R.Map.create () in
    let ks = Array.init 5000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      let k = ks.(i) in
      P.Map.add pm k i; R.Map.add rm k i;
      P.Map.add pm k (-i); R.Map.add rm k (-i)
    ) ks;
    gc_both ();
    Array.iteri (fun i _ ->
      assert (P.Map.find pm ks.(i) = R.Map.find rm ks.(i))
    ) ks;
    assert (P.Map.length pm = R.Map.length rm)
  );

  (* ---- MIXED ---- *)

  Printf.printf "\n--- Mixed ---\n%!" ;

  with_label "mixed set + map + gc" (fun () ->
    let s = P.Set.create () and m = P.Map.create () in
    let ks = Array.init 10000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      if i mod 3 = 0 then (P.Set.add s ks.(i); gc_minor ());
      if i mod 3 = 1 then (P.Map.add m ks.(i) i; gc_minor ());
      if i mod 3 = 2 then gc_major ()
    ) ks;
    gc_both ();
    Array.iteri (fun i _ ->
      if i mod 3 = 0 then assert (P.Set.mem s ks.(i));
      if i mod 3 = 1 then assert (P.Map.find m ks.(i) = i)
    ) ks
  );

  Printf.printf "\n--- Summary ---\n%!";
  Printf.printf "  Passed: %d / %d  (failed: %d)\n%!"
    !pass_count !test_count !fail_count;
  if !fail_count > 0 then exit 2
