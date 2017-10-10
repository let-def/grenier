let sqr x = x *. x

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

type t = bytes

(** Building a new hll *)

let validate t = (1 lsl Char.code (Bytes.get t 0) + 1 = Bytes.length t)

let estimate_memory ~error =
  let p = int_of_float (ceil (log (sqr (1.04 /. error)))) in
  (1 lsl p)

let make ~error =
  assert (0. < error && error < 1.);
  let p = int_of_float (ceil (log (sqr (1.04 /. error)))) in
  let t = Bytes.make (1 lsl p + 1) '\000' in
  Bytes.set t 0 (Char.chr p);
  assert (validate t);
  t

let clear t =
  Bytes.fill t 1 (Bytes.length t - 1) '\000';
  assert (validate t)

(** Adding an element to the hll *)

let get_rho w =
  if w = 0L then
    64
  else 1 + first_setbit w

let add t x =
  let p = Char.code (Bytes.get t 0) in
  let m = 1 lsl p in
  let j = Int64.to_int x land (m - 1) + 1 in
  let w = Int64.shift_right_logical x p in
  Bytes.set t j (Char.chr (max (Char.code (Bytes.get t j)) (get_rho w)))
  (* assert (validate t): micro benchmark shows that validating in an add loop
     has a 10% overhead, not necessary. *)

(** Merging and copying hlls *)

let copy t = Bytes.copy t

let merge ~into:t t' =
  let length = Bytes.length t in
  if length <> Bytes.length t' then
    invalid_arg "update: counters precision should be equal";
  for i = 1 to length - 1 do
    Bytes.set t i (max (Bytes.get t i) (Bytes.get t' i))
  done;
  assert (validate t)

(** Estimating cardinality, HyperLogLog *)

let count_nulls t =
  let nulls = ref 0 in
  for i = 1 to Bytes.length t - 1 do
    if Bytes.get t i = '\000' then
      incr nulls
  done;
  !nulls

let get_alpha = function
  | p when not (4 <= p && p <= 16) -> assert false
  | 4 -> 0.673
  | 5 -> 0.697
  | 6 -> 0.709
  | p -> 0.7213 /. (1.0 +. 1.079 /. float (1 lsl p))

let hll_estimation precision t =
  let p = Char.code (Bytes.get t 0) in
  let m = 1 lsl p in
  let sum = ref 0. in
  for i = 1 to m do
    sum := !sum +. 2. ** float (- min (precision-p) (Char.code (Bytes.get t i)))
  done;
  get_alpha p *. sqr (float m) /. !sum

let linear_counting m nulls =
  let m = float m and nulls = float nulls in
  (m *. log (m /. nulls))

let card_hll t =
  let e = hll_estimation 32 t in
  let p = Char.code (Bytes.get t 0) in
  let m = 1 lsl p in
  if e <= (5.0 /. 2.0) *. float m then (
    (* Small range *)
    match count_nulls t with
    | 0 -> e
    | nulls -> linear_counting m nulls
  ) else if e <= (2.0 ** 32.0) /. 30.0 then (
    (* Normal range *)
    e
  ) else (
    (* Large range *)
    (-. (2.0 ** 32.0) *. log (1.0 -. e /. (2.0 ** 32.0)))
  )

(** Estimating cardinality, HyperLogLog++ *)

let get_threshold p = Hll_consts.threshold.(p - 4)

let get_nearest_neighbors e vec =
  let distance = Array.mapi (fun idx v -> sqr (e -. v), idx) vec in
  Array.sort (fun ((a : float),_) (b,_) -> compare a b) distance;
  Array.init 6 (fun i -> let _, idx = distance.(i) in idx)

let estimate_bias e p =
  let bias_vector = Hll_consts.bias_data.(p - 4) in
  let nearest_neighbors =
    get_nearest_neighbors e Hll_consts.raw_estimated_data.(p - 4) in
  let sum = ref 0. in
  for i = 0 to Array.length nearest_neighbors - 1 do
    sum := !sum +. bias_vector.(nearest_neighbors.(i))
  done;
  !sum /. float (Array.length nearest_neighbors)

let ep t =
  let p = Char.code (Bytes.get t 0) in
  let m = float (1 lsl p) in
  let e = hll_estimation 64 t in
  if e <= 5. *. m then
    e -. estimate_bias e p
  else
    e

let card_hllpp t =
  assert (validate t);
  let p = Char.code (Bytes.get t 0) in
  let m = (1 lsl p) in
  match count_nulls t with
  | 0 -> ep t
  | nulls ->
    let h = linear_counting m nulls in
    if h <= get_threshold p then
      h
    else
      ep t

let card = card_hllpp

(* Thomas Wang 64-bit integer hashing *)

let hash_int64 key =
  let open Int64 in
  let (lsr) = shift_right_logical in
  let (lsl) = shift_left in
  let not = lognot in
  let xor = logxor in
  let key = add (not key) (key lsl 21) in
	let key = xor key (key lsr 24) in
	let key = add (add key (key lsl 3)) (key lsl 8) in
	let key = xor key (key lsr 14) in
	let key = add (add key (key lsl 2)) (key lsl 4) in
	let key = xor key (key lsr 28) in
	let key = add key (key lsl 31) in
  key

let to_string t =
  assert (1 lsl Char.code (Bytes.get t 0) + 1 = Bytes.length t);
  Bytes.to_string t

let of_string s =
  let t = Bytes.of_string s in
  (* t.[0] = 1 lsl length s + 1.
     Also, it as to be small, so higher bits must be null and could be used to
     store versioning information in the future. *)
  if not (validate t) then
    raise (Invalid_argument "Hll.of_string");
  t
