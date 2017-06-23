type sticky_sampler = {
  mutable error_tolerance  : float;
  mutable support          : float;
  mutable s                : (string, int) Hashtbl.t;
  mutable r                : int;
  mutable failure_prob     : float;
  mutable n                : int;
  mutable t                : float;
  mutable required_samples : int;
}

let new_sampler ~support ~error_tolerance ~failure_prob =
  let t = 2.0 /. error_tolerance *. log (1.0 /. (support *. failure_prob)) in
  {
    error_tolerance;
    support;
    failure_prob;
    r = 1; n = 0;
    t;
    required_samples = int_of_float t;
    s = Hashtbl.create 7;
  }

let prune t =
  let filter key value =
    let value = ref value in
    while Random.int 2 <> 0 && !value > 0 do
      decr value
    done;
    let value = !value in
    if value <= 0 then
      None
    else
      Some value
  in
  let x = Hashtbl.length t.s in
  Hashtbl.filter_map_inplace filter t.s;
  Printf.eprintf "prune([%d elements]) = [%d elements]\n%!"
    x (Hashtbl.length t.s)

(* ItemsAboveThreshold returns a list of items that occur more than threshold, along
   with their frequencies. threshold is in the range [0,1] *)
let items_above_threshold t tresh =
  let select k f acc =
    let f = float f in
    if f >= (tresh -. t.error_tolerance) *. float t.n then
      (k, f /. float t.n +. t.support) :: acc
    else
      acc
  in
  Hashtbl.fold select t.s

(* Observe records a new sample *)
let observe t key =
  t.n <- t.n + 1;
  if float t.n > t.t then (
    t.t <- t.t *. 2.0;
    t.r <- t.r * 2;
    prune t
  );
  match Hashtbl.find t.s key with
  | exception Not_found ->
    Hashtbl.add t.s key 1
  | n ->
    if Random.float (float t.r) <= 1.0 then
      (Hashtbl.replace t.s key (n + 1))
