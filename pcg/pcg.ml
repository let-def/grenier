module State : sig
  type t
  val create : unit -> t
  val get : t -> int64
  val set : t -> int64 -> unit
  val inc_get : t -> int64
  val inc_set : t -> int64 -> unit
end = struct
  type t = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create () : t = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout 2

  let get (t : t) = Bigarray.Array1.unsafe_get t 0
  let set (t : t) v = Bigarray.Array1.unsafe_set t 0 v

  let inc_get (t : t) = Bigarray.Array1.unsafe_get t 1
  let inc_set (t : t) v = Bigarray.Array1.unsafe_set t 1 v
end

let pure_advance state inc =
  Int64.(add (mul state 6364136223846793005L) inc)

let pure_peek s =
  let (lsl) = Int64.shift_left and (lsr)  = Int64.shift_right_logical
  and (lor) = Int64.logor      and (lxor) = Int64.logxor in
  let x = ((s lsr 18) lxor s) lsr 27 and r = Int64.to_int (s lsr 59) in
  (x lsr r) lor (x lsl ((-r) land 31))

let advance t =
  State.set t (pure_advance (State.get t) (State.inc_get t))

let reseed t ~state ~seq =
  State.set t 0L;
  State.inc_set t Int64.(logor (shift_left seq 1) 1L);
  advance t;
  State.set t (Int64.add (State.get t) seq);
  advance t

let create ~state ~seq =
  let t = State.create () in
  reseed t ~state ~seq;
  t

let get_int32 t =
  let s = State.get t in
  advance t;
  Int64.to_int32 (pure_peek s)

let get_int t =
  let s = State.get t in
  advance t;
  Int64.to_int (pure_peek s)

let hash_int64 s =
  let inc = Int64.logor (Int64.shift_left s 1) 1L in
  let s0 = Int64.add (pure_advance 0L inc) s in
  let s1 = pure_advance s0 inc in
  let s2 = pure_advance s1 inc in
  Int64.(logor
           (logand (pure_peek s1) 0xFFFFFFFFL)
           (shift_left (pure_peek s2) 32))
