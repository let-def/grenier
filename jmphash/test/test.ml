(* Jump consistent hash: reference algorithm (Lamping & Veach), uniformity,
   and minimal movement when the number of buckets grows. *)
let fails = ref 0
let check name b = if not b then (incr fails; if !fails < 20 then Printf.printf "FAIL %s\n%!" name)

let reference key n =
  let b = ref (-1L) and j = ref 0L and key = ref key in
  while Int64.compare !j (Int64.of_int n) < 0 do
    b := !j;
    key := Int64.(add (mul !key 2862933555777941757L) 1L);
    let k = Int64.(to_float (shift_right_logical !key 33)) +. 1. in
    j := Int64.of_float (Int64.to_float (Int64.succ !b) *. (2147483648. /. k))
  done;
  Int64.to_int !b

let () =
  Random.self_init ();
  let mism = ref 0 in
  for _ = 1 to 200_000 do
    let key = Random.int64 Int64.max_int and n = 1 + Random.int 5000 in
    let h = Jmphash.host ~hosts:n key in
    check "range" (0 <= h && h < n);
    if h <> reference key n then incr mism
  done;
  Printf.printf "jmphash: %d/200000 differ from the float reference\n" !mism;
  (* monotonicity *)
  for _ = 1 to 20_000 do
    let key = Random.int64 Int64.max_int and n = 1 + Random.int 1000 in
    let a = Jmphash.host ~hosts:n key and b = Jmphash.host ~hosts:(n + 1) key in
    check "monotone" (a = b || b = n)
  done;
  (* uniformity: chi2 over 100 buckets *)
  let n = 100 and samples = 1_000_000 in
  let c = Array.make n 0 in
  for i = 1 to samples do
    let h = Jmphash.host ~hosts:n (Int64.of_int i) in c.(h) <- c.(h) + 1
  done;
  let e = float samples /. float n in
  let chi2 = Array.fold_left (fun a o -> a +. (float o -. e) ** 2. /. e) 0. c in
  Printf.printf "jmphash: chi2 = %.1f over 99 dof (sequential keys)\n" chi2;
  check "chi2" (chi2 < 160.);
  check "negative keys" (let h = Jmphash.host ~hosts:10 (-5L) in 0 <= h && h < 10);
  Printf.printf "jmphash: %s (%d failures)\n" (if !fails = 0 then "OK" else "FAILED") !fails;
  if !fails > 0 then exit 1
