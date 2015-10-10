let sqr x = x *. x

type t = {
  alpha : float;
  p     : int;
  m     : int;
  ms    : bytes;
}

(** Building a new hll *)

let get_alpha = function
  | p when not (4 <= p && p <= 16) -> assert false
  | 4 -> 0.673
  | 5 -> 0.697
  | 6 -> 0.709
  | p -> 0.7213 /. (1.0 +. 1.079 /. float_of_int (1 lsl p))

let estimate_memory ~error =
  let p = int_of_float (ceil (log (sqr (1.04 /. error)))) in
  (1 lsl p)

let make ~error =
  assert (0. < error && error < 1.);
  let p = int_of_float (ceil (log (sqr (1.04 /. error)))) in
  let m = 1 lsl p in
  { p; m; alpha = get_alpha p; ms = Bytes.make m '\000'}

let clear {ms} =
  Bytes.fill ms 0 (Bytes.length ms) '\000'

(** Adding an element to the hll *)

let first_setbit n =
  (* Reverse mapping of B(2,6)
     See http://chessprogramming.wikispaces.com/De+Bruijn+sequence *)
  let db26 =
    "\x00\x01\x02\x35\x03\x07\x36\x1b\x04\x26\x29\x08\x22\x37\x30\x1c\x3e\x05\x27\x2e\x2c\x2a\x16\x09\x18\x23\x3b\x38\x31\x12\x1d\x0b\x3f\x34\x06\x1a\x25\x28\x21\x2f\x3d\x2d\x2b\x15\x17\x3a\x11\x0a\x33\x19\x24\x20\x3c\x14\x39\x10\x32\x1f\x13\x0f\x1e\x0e\x0d\x0c" in
  (* Isolate lsb
     See http://aggregate.org/MAGIC/#Least%20Significant%201%20Bit *)
  let n = Int64.logand n (Int64.neg n) in
  (* Get index in B(2,6) *)
  let n = Int64.mul n 0x022fdd63cc95386dL in
  let n = Int64.shift_right_logical n 58 in
  Char.code db26.[Int64.to_int n]

let get_rho w =
  if w = 0L then
    64
  else 1 + first_setbit w

let add {p;m;ms} x =
  let j = Int64.to_int x land (m - 1) in
  let w = Int64.shift_right_logical x p in
  Bytes.set ms j (Char.chr (max (Char.code (Bytes.get ms j)) (get_rho w)))

(** Merging and copying hlls *)

let copy t = {t with ms = Bytes.copy t.ms}

let merge ~into:{ms; m} {ms = ms'; m = m'} =
  if m' <> m then
    invalid_arg "update: counters precision should be equal";
  for i = 0 to Bytes.length ms - 1 do
    Bytes.set ms i (max (Bytes.get ms i) (Bytes.get ms' i))
  done

(** Estimating cardinality *)

let get_threshold p = Hll_consts.threshold.(p - 4)

let get_nearest_neighbors e vec =
  let distance = Array.mapi (fun idx v -> sqr (e -. v), idx) vec in
  Array.sort (fun ((a : float),_) (b,_) -> compare a b) distance;
  Array.init 6 (fun i -> let _, idx = distance.(i) in idx)

let estimate_bias e p =
  let bias_vector = Hll_consts.bias_data.(p - 4) in
  let nearest_neighbors = get_nearest_neighbors e Hll_consts.raw_estimated_data.(p - 4) in
  let sum = ref 0. in
  for i = 0 to Array.length nearest_neighbors - 1 do
    sum := !sum +. bias_vector.(nearest_neighbors.(i))
  done;
  !sum /. float_of_int (Array.length nearest_neighbors)

let ep {alpha;p;m;ms}  =
  let sum = ref 0. in
  for i = 0 to m - 1 do
    sum := !sum +. 2. ** float_of_int (- Char.code (Bytes.get ms i))
  done;
  let e = alpha *. sqr (float_of_int m) /. !sum in
  if e <= 5. *. float_of_int m then
    e -. estimate_bias e p
  else
    e

let card t =
  let nulls = ref 0 in
  for i = 0 to t.m -1 do
    if Bytes.get t.ms i = '\000' then
      incr nulls
  done;
  let nulls = !nulls in
  if nulls > 0 then
    let m = float_of_int t.m in
    let h = m *. log (m /. float_of_int nulls) in
    if h <= get_threshold t.p then
      h
    else
      ep t
  else
    ep t

let hash_int64 key =
  let open Int64 in
  let (lsr) = shift_right_logical in
  let (lsl) = shift_left in
  let not = lognot in
  let xor = logxor in

  (* Thomas Wang 64-bit integer hashing *)
  let key = add (not key) (key lsl 21) in
	let key = xor key (key lsr 24) in
	let key = add (add key (key lsl 3)) (key lsl 8) in
	let key = xor key (key lsr 14) in
	let key = add (add key (key lsl 2)) (key lsl 4) in
	let key = xor key (key lsr 28) in
	let key = add key (key lsl 31) in
  key
