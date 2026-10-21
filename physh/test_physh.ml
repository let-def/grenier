module P = Physh
module R = Ref

module D : sig
  type 'a t
  type 'a view = Int of int | Obj of 'a
  val int : int -> 'a t
  val obj : 'a -> 'a t
  val unpack : 'a t -> 'a view
end = struct
  type 'a view = Int of int | Obj of 'a
  type 'a t = 'a view

  let int : int -> 'a t = Obj.magic
  let obj x = Obj x

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

(* ---- Generic GC stress ----

   Set and map tests share the same scenarios: [add k i] inserts key number
   [i], [check k i] asserts that it is still present (bound to [i] for maps). *)

type table = {
  add : Obj.t -> int -> unit;
  check : Obj.t -> int -> unit;
  length : unit -> int;
}

let set_table () =
  let s = P.Set.create () in
  { add = (fun k _ -> P.Set.add s k);
    check = (fun k _ -> assert (P.Set.mem s k));
    length = (fun () -> P.Set.length s) }

let map_table () =
  let m = P.Map.create () in
  { add = (fun k i -> P.Map.add m k i);
    check = (fun k i -> assert (P.Map.find m k = i));
    length = (fun () -> P.Map.length m) }

(* Immediate keys are integers. Old keys are allocated (and mostly promoted)
   before the test, fresh keys right before being inserted. *)
type keys = Immediate | Old | Fresh

let keys_name = function
  | Immediate -> "int keys" | Old -> "old keys" | Fresh -> "fresh keys"

let key_maker kind n =
  match kind with
  | Immediate -> Obj.repr
  | Old -> let ks = Array.init n (fun i -> Obj.repr (ref i)) in Array.get ks
  | Fresh -> fun i -> Obj.repr (ref i)

(* GC periods, in insertions (0: never) *)
type schedule = { minor : int; major : int; noise : int }

let no_gc = { minor = 0; major = 0; noise = 0 }

let tick s i =
  let every p = p > 0 && i mod p = 0 in
  if every s.noise then gc_noise ();
  if every s.minor then gc_minor ();
  if every s.major then gc_major ()

(* Insert [n] keys following [schedule], then check all of them.
   [gc_on_check]: full GC every 20 checks.
   [recheck]: every [recheck] insertions, full GC and check all keys so far. *)
let stress ?(gc_on_check = false) ?(recheck = 0) ~schedule ~kind ~n make_table =
  let t = make_table () in
  let key = key_maker kind n in
  let ks = Array.make n (Obj.repr 0) in
  for i = 0 to n - 1 do
    ks.(i) <- key i;
    t.add ks.(i) i;
    tick schedule i;
    if recheck > 0 && (i + 1) mod recheck = 0 then begin
      gc_both ();
      for j = 0 to i do t.check ks.(j) j done
    end
  done;
  gc_both ();
  assert (t.length () = n);
  Array.iteri (fun i k ->
      if gc_on_check && i mod 20 = 0 then gc_both ();
      t.check k i) ks

let scenarios = [
  "1 key", 1, no_gc;
  "2000, minor gc every 10", 2000, { no_gc with minor = 10 };
  "2000, major gc every 25", 2000, { no_gc with major = 25 };
  "5000, aggressive gc", 5000, { noise = 5; minor = 25; major = 50 };
  "10k", 10_000, { minor = 50; major = 200; noise = 500 };
  "20k", 20_000, { minor = 50; major = 200; noise = 500 };
  "50k", 50_000, { minor = 100; major = 500; noise = 1000 };
  "100k", 100_000, { minor = 500; major = 2000; noise = 5000 };
]

let stress_matrix name make_table =
  Printf.printf "\n--- %s: GC stress ---\n%!" (String.capitalize_ascii name);
  List.iter (fun kind ->
      let label fmt = Printf.ksprintf (fun s ->
          Printf.sprintf "%s %s: %s" name (keys_name kind) s) fmt in
      List.iter (fun (desc, n, schedule) ->
          with_label (label "%s" desc) (fun () ->
              stress ~schedule ~kind ~n make_table))
        scenarios;
      with_label (label "gc during lookups") (fun () ->
          stress ~gc_on_check:true ~schedule:no_gc ~kind ~n:2000 make_table);
      with_label (label "full gc and recheck every 100") (fun () ->
          stress ~recheck:100 ~schedule:no_gc ~kind ~n:10_000 make_table);
      with_label (label "100 create-fill-gc cycles") (fun () ->
          for _ = 1 to 100 do
            stress ~schedule:no_gc ~kind ~n:500 make_table
          done))
    [Immediate; Old; Fresh]

(* ---- BASIC TESTS ---- *)

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
    let k1 = D.int 42 in
    let k2 = D.int 42 in
    let k3 = D.int 99 in
    P.Set.add s k1;
    assert (P.Set.mem s k1);
    assert (P.Set.mem s k2);
    assert (not (P.Set.mem s k3));
    assert (P.Set.length s = 1)
  );

  with_label "set obj keys (D)" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 and y = ref 1 in
    let k1 = D.obj x in
    let k2 = D.obj y in
    P.Set.add s k1;
    assert (P.Set.mem s k1);
    assert (not (P.Set.mem s k2))
  );

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
    let k1 = D.int 42 in
    let k2 = D.int 42 in
    let k3 = D.int 99 in
    P.Map.add m k1 "ok";
    assert (P.Map.find m k1 = "ok");
    assert (P.Map.find m k2 = "ok");
    try ignore (P.Map.find m k3); assert false
    with Not_found -> ()
  );

  Printf.printf "\n--- Map: overwrites ---\n%!" ;

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

  stress_matrix "set" set_table;
  stress_matrix "map" map_table;

  (* ---- MIXED ---- *)

  Printf.printf "\n--- Mixed ---\n%!" ;

  with_label "mixed set + map + gc" (fun () ->
    let s = P.Set.create () and m = P.Map.create () in
    let ks = Array.init 10000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      if i mod 3 = 0 then (P.Set.add s ks.(i); gc_minor ());
      if i mod 3 = 1 then (P.Map.add m ks.(i) i; gc_minor ());
      if i mod 243 = 2 then gc_major ()
    ) ks;
    gc_both ();
    Array.iteri (fun i _ ->
      if i mod 3 = 0 then assert (P.Set.mem s ks.(i));
      if i mod 3 = 1 then assert (P.Map.find m ks.(i) = i)
    ) ks
  );

  Printf.printf "\n--- Chaotic stress: mixed add/find/overwrite + random alloc ---\n%!";

  with_label "set chaotic: add + mem + random alloc" (fun () ->
    let s = P.Set.create () in
    let r = R.Set.create () in
    let keys = Array.init 20000 (fun i -> ref i) in
    for _r = 0 to 500 do
      let i = Random.int 20000 in
      let k = keys.(i) in
      let d = Random.int 10 in
      if d < 7 then (P.Set.add s k; R.Set.add r k)
      else if d < 9 then assert (P.Set.mem s k = R.Set.mem r k)
      else ignore (Buffer.create (Random.int 4096))
    done;
    gc_both ();
    Array.iter (fun k -> assert (P.Set.mem s k = R.Set.mem r k)) keys;
    assert (P.Set.length s = R.Set.length r)
  );

  with_label "map chaotic: add + find + overwrite + random alloc" (fun () ->
    let m = P.Map.create () in
    let r = R.Map.create () in
    let keys = Array.init 20000 (fun i -> ref i) in
    Array.iteri (fun i _ -> P.Map.add m keys.(i) i;
                            R.Map.add r keys.(i) i) keys;
    for _r = 0 to 10000 do
      let i = Random.int 20000 in
      let k = keys.(i) in
      let d = Random.int 2000 in
      if d < 800 then
        let v = Random.int 1_000_000 in
        (P.Map.add m k v; R.Map.add r k v)
      else if d < 1400 then assert (P.Map.find m k = R.Map.find r k)
      else if d < 1999 then
        ignore (Buffer.create (Random.int 4096))
      else gc_major ()
    done;
    gc_both ();
    Array.iter (fun k -> assert (R.Map.find r k = P.Map.find m k)) keys;
    assert (P.Map.length m = R.Map.length r)
  );

  with_label "map chaotic large: 100k entries" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 100000 (fun i -> ref i) in
    Array.iteri (fun i _ ->
      P.Map.add m keys.(i) i;
      if i mod 1000 = 0 then (
        ignore (Buffer.create (Random.int 4096))
      )
    ) keys;
    for _r = 0 to 2000 do
      let i = Random.int 100000 in
      let k = keys.(i) in
      let d = Random.int 2000 in
      if d < 666 then P.Map.add m k (Random.int 1_000_000)
      else if d < 1333 then ignore (P.Map.find m k)
      else if d < 1999 then
        ignore (Buffer.create (Random.int 4096))
      else gc_major ()
    done;
    gc_both ();
    Array.iter (fun k -> ignore (P.Map.find m k)) keys;
    assert (P.Map.length m = 100000)
  );

  with_label "int keys chaotic: map add + find + overwrite + random alloc" (fun () ->
    let m = P.Map.create () in
    let keys = Array.init 50000 D.int in
    Array.iteri (fun i _ -> P.Map.add m keys.(i) i) keys;
    for _r = 0 to 500 do
      let i = Random.int 50000 in
      let k = keys.(i) in
      let d = Random.int 500 in
      if d < 200 then P.Map.add m k (Random.int 1_000_000)
      else if d < 350 then ignore (P.Map.find m k)
      else if d < 499 then
        ignore (Buffer.create (Random.int 4096))
      else gc_major ()
    done;
    gc_both ();
    Array.iter (fun k -> ignore (P.Map.find m k)) keys;
    assert (P.Map.length m = 50000)
  );

  (* ---- ADD_IF_NEW / ADD_OVERWRITE ---- *)

  Printf.printf "\n--- add_if_new / add_overwrite ---\n%!";

  with_label "set add_if_new basic" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 and y = ref 1 in
    assert (not (P.Set.add_if_new s x));
    assert (P.Set.add_if_new s x);
    assert (not (P.Set.add_if_new s y));
    assert (P.Set.length s = 2);
    let si = P.Set.create () in
    assert (not (P.Set.add_if_new si (D.int 3)));
    assert (P.Set.add_if_new si (D.int 3));
    assert (P.Set.length si = 1)
  );

  with_label "map add_if_new / add_overwrite basic" (fun () ->
    let m = P.Map.create () in
    let k = ref 0 in
    assert (P.Map.add_if_new m k "a" = None);
    assert (P.Map.add_if_new m k "b" = Some "a");
    assert (P.Map.find m k = "a");
    assert (P.Map.add_overwrite m k "c" = Some "a");
    assert (P.Map.find m k = "c");
    let k' = ref 0 in
    assert (P.Map.add_overwrite m k' "d" = None);
    assert (P.Map.find m k' = "d");
    assert (P.Map.length m = 2)
  );

  (* Random operations on young, old and immediate keys, checked against a
     model of the prior state of each key. *)
  let random_keys n =
    let old = Array.init (n / 2) (fun i -> ref i) in
    gc_both ();
    Array.init n (fun i ->
        if i mod 5 = 0 then D.int i
        else if i mod 2 = 0 then D.obj old.(i / 2)
        else D.obj (ref i))
  in

  with_label "set add_if_new vs model under gc" (fun () ->
    let s = P.Set.create () in
    let ks = random_keys 20_000 in
    let present = Array.make (Array.length ks) false in
    for r = 1 to 60_000 do
      let i = Random.int (Array.length ks) in
      assert (P.Set.add_if_new s ks.(i) = present.(i));
      present.(i) <- true;
      if r mod 1000 = 0 then gc_noise ();
      if r mod 5000 = 0 then gc_major ()
    done;
    Array.iteri (fun i k -> assert (P.Set.mem s k = present.(i))) ks;
    let n = Array.fold_left (fun n b -> if b then n + 1 else n) 0 present in
    assert (P.Set.length s = n)
  );

  with_label "map add_if_new / add_overwrite vs model under gc" (fun () ->
    let m = P.Map.create () in
    let ks = random_keys 20_000 in
    let model = Array.make (Array.length ks) None in
    for r = 1 to 60_000 do
      let i = Random.int (Array.length ks) in
      let v = Random.int 1000 in
      if Random.bool () then begin
        assert (P.Map.add_if_new m ks.(i) v = model.(i));
        if model.(i) = None then model.(i) <- Some v
      end else begin
        assert (P.Map.add_overwrite m ks.(i) v = model.(i));
        model.(i) <- Some v
      end;
      if r mod 1000 = 0 then gc_noise ();
      if r mod 5000 = 0 then gc_major ()
    done;
    Array.iteri (fun i k ->
        match model.(i) with
        | None -> (try ignore (P.Map.find m k); assert false with Not_found -> ())
        | Some v -> assert (P.Map.find m k = v)) ks
  );

  with_label "set add_if_new: count nodes of a fresh DAG" (fun () ->
    (* 2^k paths, k+1 distinct nodes: sharing must be detected *)
    let rec build k = if k = 0 then `Leaf else let t = build (k - 1) in `Node (t, t) in
    let t = build 30 in
    let s = P.Set.create () in
    let rec visit t =
      if not (P.Set.add_if_new s t) then
        match t with `Leaf -> () | `Node (a, b) -> visit a; visit b
    in
    visit t;
    assert (P.Set.length s = 31)
  );

  (* ---- REMOVE / ITERATION / CLEAR ---- *)

  Printf.printf "\n--- remove, iter, fold, clear, reset ---\n%!";

  with_label "set remove basic" (fun () ->
    let s = P.Set.create () in
    let x = ref 1 and y = ref 1 in
    P.Set.remove s x;
    P.Set.add s x; P.Set.add s y; P.Set.add s (D.int 3 |> Obj.magic);
    P.Set.remove s x;
    assert (not (P.Set.mem s x));
    assert (P.Set.mem s y);
    P.Set.remove s x;
    assert (P.Set.length s = 2)
  );

  with_label "map remove, find_opt, mem, is_empty" (fun () ->
    let m = P.Map.create () in
    let k = ref 0 and k' = ref 0 in
    assert (P.Map.is_empty m);
    assert (P.Map.find_opt m k = None);
    P.Map.add m k "a"; P.Map.add m k' "b";
    assert (P.Map.find_opt m k = Some "a");
    assert (P.Map.mem m k && not (P.Map.is_empty m));
    P.Map.remove m k;
    assert (P.Map.find_opt m k = None && not (P.Map.mem m k));
    assert (P.Map.find m k' = "b");
    P.Map.remove m k';
    assert (P.Map.is_empty m);
    P.Map.add m k "c";
    assert (P.Map.find m k = "c")
  );

  (* Dense immediate keys make long clusters: exercise backward shifts *)
  with_label "remove in clusters (int keys)" (fun () ->
    let s = P.Set.create () and m = P.Map.create () in
    let n = 10_000 in
    for i = 0 to n - 1 do P.Set.add s (D.int i); P.Map.add m (D.int i) i done;
    for i = 0 to n - 1 do
      if i mod 3 <> 0 then (P.Set.remove s (D.int i); P.Map.remove m (D.int i))
    done;
    for i = 0 to n - 1 do
      assert (P.Set.mem s (D.int i) = (i mod 3 = 0));
      assert (P.Map.find_opt m (D.int i) = if i mod 3 = 0 then Some i else None)
    done;
    assert (P.Set.length s = (n + 2) / 3 && P.Map.length m = (n + 2) / 3)
  );

  with_label "remove the witness, then minor gc" (fun () ->
    let s = P.Set.create () in
    let x = ref 0 in
    P.Set.add s x;
    P.Set.remove s x;
    let y = ref 1 and z = ref 2 in
    P.Set.add s y; P.Set.add s z;
    P.Set.remove s y;
    gc_minor ();
    assert (P.Set.mem s z && not (P.Set.mem s y) && not (P.Set.mem s x));
    assert (P.Set.length s = 1)
  );

  with_label "set add/remove vs model under gc" (fun () ->
    let s = P.Set.create () in
    let ks = random_keys 20_000 in
    let present = Array.make (Array.length ks) false in
    for r = 1 to 100_000 do
      let i = Random.int (Array.length ks) in
      (match Random.int 3 with
       | 0 -> P.Set.remove s ks.(i); present.(i) <- false
       | 1 -> P.Set.add s ks.(i); present.(i) <- true
       | _ -> assert (P.Set.mem s ks.(i) = present.(i)));
      if r mod 1000 = 0 then gc_noise ();
      if r mod 5000 = 0 then gc_major ()
    done;
    Array.iteri (fun i k -> assert (P.Set.mem s k = present.(i))) ks;
    let n = Array.fold_left (fun n b -> if b then n + 1 else n) 0 present in
    assert (P.Set.length s = n)
  );

  with_label "map add/remove vs model under gc" (fun () ->
    let m = P.Map.create () in
    let ks = random_keys 20_000 in
    let model = Array.make (Array.length ks) None in
    for r = 1 to 100_000 do
      let i = Random.int (Array.length ks) in
      (match Random.int 3 with
       | 0 -> P.Map.remove m ks.(i); model.(i) <- None
       | 1 -> let v = Random.int 1000 in P.Map.add m ks.(i) v; model.(i) <- Some v
       | _ -> assert (P.Map.find_opt m ks.(i) = model.(i)));
      if r mod 1000 = 0 then gc_noise ();
      if r mod 5000 = 0 then gc_major ()
    done;
    Array.iteri (fun i k -> assert (P.Map.find_opt m k = model.(i))) ks;
    let n = Array.fold_left (fun n b -> if b <> None then n + 1 else n) 0 model in
    assert (P.Map.length m = n)
  );

  (* DFS cycle detection: the nodes of the current path *)
  with_label "set remove: cycle detection" (fun () ->
    let rec has_cycle path visited (n : int list ref array) i =
      P.Set.mem path n.(i) ||
      (not (P.Set.add_if_new visited n.(i)) &&
       begin
         P.Set.add path n.(i);
         let r = List.exists (has_cycle path visited n) !(n.(i)) in
         P.Set.remove path n.(i);
         r
       end)
    in
    let graph edges = Array.map ref edges in
    let check g expected =
      let path = P.Set.create () and visited = P.Set.create () in
      assert (has_cycle path visited g 0 = expected);
      assert (P.Set.is_empty path)
    in
    check (graph [| [1; 2]; [3]; [3]; [] |]) false;
    check (graph [| [1; 2]; [3]; [3]; [2] |]) true;
    check (graph [| [0] |]) true
  );

  (* Each element once, with GCs and lookups (that sync the table) in the
     callback *)
  with_label "set iter / fold under gc" (fun () ->
    let s = P.Set.create () in
    let ks = random_keys 5000 in
    Array.iteri (fun i k -> if i mod 4 <> 0 then P.Set.add s k) ks;
    (* young keys in the minor table, old ones in the major table *)
    let young = Array.init 100 (fun i -> D.obj (ref i)) in
    Array.iter (P.Set.add s) young;
    let seen = R.Set.create () in
    let count = ref 0 in
    P.Set.iter (fun k ->
        assert (not (R.Set.mem seen k));
        R.Set.add seen k;
        incr count;
        if !count mod 50 = 0 then gc_minor ();
        if !count mod 1000 = 0 then gc_major ();
        assert (P.Set.mem s k)) s;
    assert (!count = P.Set.length s);
    Array.iteri (fun i k -> assert (R.Set.mem seen k = (i mod 4 <> 0))) ks;
    Array.iter (fun k -> assert (R.Set.mem seen k)) young;
    assert (P.Set.fold (fun _ n -> n + 1) s 0 = P.Set.length s);
    assert (P.Set.fold (fun _ n -> n + 1) (P.Set.create ()) 0 = 0)
  );

  with_label "map iter / fold under gc" (fun () ->
    let m = P.Map.create () in
    let ks = random_keys 5000 in
    Array.iteri (fun i k -> P.Map.add m k i) ks;
    let young = Array.init 100 (fun i -> D.obj (ref i)) in
    Array.iteri (fun i k -> P.Map.add m k (- i - 1)) young;
    let seen = R.Map.create () and sum = ref 0 in
    let mem k = match R.Map.find seen k with _ -> true | exception Not_found -> false in
    P.Map.iter (fun k v ->
        assert (not (mem k));
        R.Map.add seen k v;
        sum := !sum + v;
        if R.Map.length seen mod 50 = 0 then gc_minor ();
        if R.Map.length seen mod 1000 = 0 then gc_major ();
        assert (P.Map.find m k = v)) m;
    assert (R.Map.length seen = P.Map.length m);
    Array.iteri (fun i k -> assert (R.Map.find seen k = i)) ks;
    Array.iteri (fun i k -> assert (R.Map.find seen k = - i - 1)) young;
    assert (P.Map.fold (fun _ v acc -> acc + v) m 0 = !sum)
  );

  with_label "clear / reset and reuse under gc" (fun () ->
    let s = P.Set.create () and m = P.Map.create () in
    for round = 1 to 30 do
      let ks = Array.init (100 * round) (fun i -> ref i) in
      Array.iteri (fun i k ->
          P.Set.add s k; P.Map.add m k i;
          if i mod 500 = 0 then gc_minor ()) ks;
      if round mod 3 = 0 then gc_major ();
      Array.iteri (fun i k ->
          assert (P.Set.mem s k && P.Map.find m k = i)) ks;
      assert (P.Set.length s = Array.length ks);
      if round mod 2 = 0 then (P.Set.clear s; P.Map.clear m)
      else (P.Set.reset s; P.Map.reset m);
      assert (P.Set.is_empty s && P.Map.is_empty m);
      Array.iter (fun k ->
          assert (not (P.Set.mem s k) && not (P.Map.mem m k))) ks;
      assert (P.Set.fold (fun _ n -> n + 1) s 0 = 0)
    done
  );

  (* ---- COMPACTION ---- *)

  Printf.printf "\n--- Compaction ---\n%!";

  (* Promote many objects, keep a sparse subset in the tables, drop the rest
     and compact: the survivors are moved, the tables must be rehashed. *)
  let sparse_survivors () =
    let all = Array.init 400_000 (fun i -> ref i) in
    gc_both ();
    let keep = Array.init (400_000 / 64) (fun i -> all.(i * 64)) in
    Array.fill all 0 400_000 (ref 0);
    keep
  in

  with_label "map lookup after compaction" (fun () ->
    let keep = sparse_survivors () in
    let m = P.Map.create () in
    Array.iter (fun k -> P.Map.add m k !k) keep;
    ignore (P.Map.find m keep.(0));
    gc_major ();
    Gc.compact ();
    Array.iter (fun k -> assert (P.Map.find m k = !k)) keep;
    assert (P.Map.length m = Array.length keep)
  );

  with_label "set lookup after compaction" (fun () ->
    let keep = sparse_survivors () in
    let s = P.Set.create () in
    Array.iter (P.Set.add s) keep;
    ignore (P.Set.mem s keep.(0));
    gc_major ();
    Gc.compact ();
    Array.iter (fun k -> assert (P.Set.mem s k)) keep;
    assert (not (P.Set.mem s (ref 0)));
    assert (P.Set.length s = Array.length keep)
  );

  with_label "add after compaction" (fun () ->
    let keep = sparse_survivors () in
    let m = P.Map.create () in
    let half = Array.length keep / 2 in
    Array.iteri (fun i k -> if i < half then P.Map.add m k !k) keep;
    gc_major ();
    Gc.compact ();
    (* overwrite existing keys (must not duplicate them) and add new ones *)
    Array.iter (fun k -> P.Map.add m k (- !k)) keep;
    Array.iter (fun k -> assert (P.Map.find m k = - !k)) keep;
    assert (P.Map.length m = Array.length keep)
  );

  with_label "remove and iter after compaction" (fun () ->
    let keep = sparse_survivors () in
    let m = P.Map.create () in
    Array.iter (fun k -> P.Map.add m k !k) keep;
    gc_major ();
    Gc.compact ();
    let sum = P.Map.fold (fun k v acc -> assert (!k = v); acc + 1) m 0 in
    assert (sum = Array.length keep);
    Array.iteri (fun i k -> if i mod 2 = 0 then P.Map.remove m k) keep;
    Array.iteri (fun i k ->
        assert (P.Map.find_opt m k = if i mod 2 = 0 then None else Some !k)) keep;
    assert (P.Map.length m = Array.length keep / 2)
  );

  Printf.printf "\n--- Summary ---\n%!";
  Printf.printf "  Passed: %d / %d  (failed: %d)\n%!"
    !pass_count !test_count !fail_count;
  if !fail_count > 0 then exit 2
