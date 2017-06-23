type delta_pair = {
  mutable f     : float;
  mutable delta : float;
}

type t = {
  support : float;
  error_tolerance : float;
  d : (string, delta_pair) Hashtbl.t;
  mutable n: int;
  bucket_width: int;
}

let new_lossy_counter ~support ~error_tolerance =
  {
    support;
    error_tolerance;
    d = Hashtbl.create 7;
    bucket_width = int_of_float (ceil (1.0 /. error_tolerance));
    n = 0;
  }

let prune t bucket =
  let filter k v =
    if v.f +. v.delta <= bucket then
      None
    else
      Some v
  in
  Hashtbl.filter_map_inplace filter t.d

(* ItemsAboveThreshold returns a list of items that occur more than threshold, along
   with their frequencies. threshold is in the range [0,1] *)
let items_above_threshold t ~threshold =
  let select k v acc =
    let f = v.f /. float t.n in
    if f >= t.support -. v.delta && f > threshold -. t.support then
      (k, f +. t.support) :: acc
    else
      acc
  in
  Hashtbl.fold select t.d []

(* Observe records a new sample *)
let observe t key =
  t.n <- t.n + 1;
  let bucket = float t.n /. float t.bucket_width in
  begin match Hashtbl.find t.d key with
  | v -> v.f <- v.f +. 1.0
  | exception Not_found ->
    Hashtbl.add t.d key { f = 1.0; delta = bucket -. 1.0 }
  end ;
	if t.n mod t.bucket_width = 0 then
   prune t bucket
