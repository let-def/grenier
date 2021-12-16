(**
Copyright (c) 2013, Frédéric Bour <frederic.bour (at) lakaban.net>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of the {organization} nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**)
type t = {hi: float; lo: float}

let t hi lo = {hi; lo}

let float_floor = floor
let int_abs = abs

(* Constants *)
let pi      = t 3.141592653589793116e+00 1.224646799147353207e-16
let two_pi  = t 6.283185307179586232e+00 2.449293598294706414e-16
let pi_2    = t 1.570796326794896558e+00 6.123233995736766036e-17
let e       = t 2.718281828459045091e+00 1.445646891729250158e-16
let nan     = t nan nan
let zero    = t 0. 0.
let one     = t 1. 0.
let ten     = t 10. 0.

let k_eps   = 1.23259516440783e-32
let k_split = 134217729.0

(* Conversion *)
let of_float f = t f 0.
let to_float {hi; lo} = hi +. lo
let of_int i = of_float (float_of_int i)
let to_int {hi; _} = int_of_float hi

(* Predicates *)
let is_nan {hi; _} = hi <> hi
let is_zero t = t = zero
let is_negative {hi; lo} = hi < 0. || (hi = 0. && lo < 0.)
let is_positive {hi; lo} = hi > 0. || (hi = 0. && lo > 0.)

(* Ordering *)
let eq {hi = h1; lo = l1} {hi = h2; lo = l2} =
  h1 = h2 && l1 = l2
let ne {hi = h1; lo = l1} {hi = h2; lo = l2} =
  h1 <> h2 || l1 <> l2
let gt {hi = h1; lo = l1} {hi = h2; lo = l2} =
  h1 > h2 || (h1 = h2 && l1 > l2)
let ge {hi = h1; lo = l1} {hi = h2; lo = l2} =
  h1 > h2 || (h1 = h2 && l1 >= l2)
let lt {hi = h1; lo = l1} {hi = h2; lo = l2} =
  h1 < h2 || (h1 = h2 && l1 < l2)
let le {hi = h1; lo = l1} {hi = h2; lo = l2} =
  h1 < h2 || (h1 = h2 && l1 <= l2)

let compare t1 t2 = match t1, t2 with
  | {hi = h1;_}, {hi = h2;_} when h1 < h2 -> -1
  | {hi = h1;_}, {hi = h2;_} when h1 > h2 -> 1
  | {lo = l1;_}, {lo = l2;_} when l1 < l2 -> -1
  | {lo = l1;_}, {lo = l2;_} when l1 > l2 -> 1
  | _, _ -> 0

let signum = function
  | t when is_positive t -> 1
  | t when is_negative t -> -1
  | _ -> 0

(* Standard operations *)
let add {hi = h1; lo = l1} {hi = h2; lo = l2} =
  (* S = hi + y.hi; T = lo + y.lo *)
  let s' = h1 +. h2 and t' = l1 +. l2 in
  (* e = S - hi; f = T - lo *)
  let e  = s' -. h1 and f  = t' -. l1 in
  (* s = S-e; t = T-f *)
  let s  = s' -. e  and t  = t' -. f  in
 	(* s = (y.hi-e)+(hi-s) *)
  let s  = (h2 -. e) +. (h1 -. s)    in
  (* t = (y.lo-f)+(lo-t) *)
  let t  = (l2 -. f) +. (l1 -. t)    in
  (* e = s+T; H = S+e; h = e+(S-H); e = t+h; *)
  let e  = s  +. t' in
  let h' = s' +. e  in
  let h  = e  +. (s' -. h') in
  let e  = t  +. h in
  (* hi = H + e *)
  let hi = h' +. e in
  (* lo = e + (H - hi) *)
  let lo = e  +. (h' -. hi) in
  {hi; lo}

let neg {hi; lo} = {hi = -.hi; lo = -.lo}
let sub t1 t2 = add t1 (neg t2)

(* Disable fused-multiply-add optimization
   See https://github.com/ocaml/ocaml/issues/10323. *)
let ( *. ) x y = Sys.opaque_identity (x *. y)

let mul {hi = h1; lo = l1} {hi = h2; lo = l2} =
  (*C = SPLIT * hi; hx = C-hi; c = SPLIT * y.hi;*)
  let c' = k_split *. h1 in
  let hx = c' -. h1      in
  let c  = k_split *. h2 in
  (*hx = C-hx; tx = hi-hx; hy = c-y.hi; *)
  let hx = c' -. hx in
  let tx = h1 -. hx in
  let hy = c  -. h2 in
  (*C = hi*y.hi; hy = c-hy; ty = y.hi-hy;*)
  let c' = h1 *. h2 in
  let hy = c  -. hy in
  let ty = h2 -. hy in
	(*c = ((((hx*hy-C)+hx*ty)+tx*hy)+tx*ty)+(hi*y.lo+lo*y.hi);*)
  let c  = ((((hx *. hy -. c') +. hx *. ty) +. tx *. hy) +. tx *. ty)
           +. (h1 *. l2 +. l1 *. h2)
  in
  let hi = c' +. c in
  let hx = c' -. hi in
  let lo = c  +. hx in
  {hi; lo}

let div {hi = h1; lo = l1} {hi = h2; lo = l2} =
  (* C = hi/y.hi; c = SPLIT*C; hc =c-C;  u = SPLIT*y.hi; hc = c-hc;*)
  let c' = h1 /. h2      in
  let c  = k_split *. c' in
  let hc = c -. c'       in
  let u  = k_split *. h2 in
  let hc = c -. hc       in
  (* tc = C-hc; hy = u-y.hi; U = C * y.hi; hy = u-hy; ty = y.hi-hy;*)
  let tc = c' -. hc in
  let hy = u  -. h2 in
  let u' = c' *. h2 in
  let hy = u  -. hy in
  let ty = h2 -. hy in
  (* u = (((hc*hy-U)+hc*ty)+tc*hy)+tc*ty*)
  let u = (((hc *. hy -. u') +. hc *. ty) +. tc *. hy) +. tc *. ty in
  (* c = ((((hi-U)-u)+lo)-C*y.lo)/y.hi;*)
  let c = ((((h1 -. u') -. u) +. l1) -. c' *. l2) /. h2 in
   (* u = C+c; *)
  let hi = c' +. c in
  let lo = (c' -. hi) +. c in
  {hi; lo}

let inv {hi; lo} =
  (*C = 1.0/hi; *)
  let c' = 1. /. hi in
  (*c = SPLIT*C; *)
  let c  = k_split *. c' in
  (*hc =c-C;  *)
  let hc = c -. c' in
  (*u = SPLIT*hi;*)
  let u  = k_split *. hi in
  (*hc = c-hc; tc = C-hc; hy = u-hi; U = C*hi; hy = u-hy; ty = hi-hy;*)
  let hc = c  -. hc in
  let tc = c' -. hc in
  let hy = u  -. hi in
  let u' = c' *. hi in
  let hy = u  -. hy in
  let ty = hi -. hy in
  (*u = (((hc*hy-U)+hc*ty)+tc*hy)+tc*ty;*)
  let u = (((hc *. hy -. u') +. hc *. ty) +. tc *. hy) +. tc *. ty in
  (*c = ((((1.0-U)-u))-C*lo)/hi;*)
  let c = ((((1.0 -. u') -. u)) -. c' *. lo) /. hi in
  let hi = c' +. c in
  let lo = (c' -. hi) +. c in
  {hi; lo}

let floor {hi; lo} =
  let hi' = floor hi in
  let lo' = if hi' = hi then floor lo else 0. in
  t hi' lo'

let ceil {hi; lo} =
  let hi' = ceil hi in
  let lo' = if hi' = hi then ceil lo else 0. in
  t hi' lo'

let abs t   = if is_negative t then neg t   else t
let trunc t = if is_positive t then floor t else ceil t

let sqr t = mul t t

let sqrt = function
  | {hi = 0.; lo = 0.}   -> zero
  | t when is_negative t -> nan
  | {hi; _} as t ->
    let x  = 1. /. sqrt hi      in
    let ax = of_float (hi *. x) in
    let d  = sub t (sqr ax)     in
    let d2 = d.hi *. (x *. 0.5) in
    add ax (of_float d2)

let rec pow acc t n =
  if n = 0
  then acc
  else if n mod 2 = 1
  then pow (mul t acc) (sqr t) (n / 2)
  else pow acc (sqr t) (n / 2)

let pow t = function
  | 0 -> one
  | n when n < 0 ->
    let t = inv t in
    pow t t (-n - 1)
  | n -> pow t t (n - 1)

(* Output functions *)

let max_print_digits = 32

let dump {hi; lo} = Printf.sprintf "{hi = %f; lo = %f}" hi lo

let magnitude x =
  let x_abs = abs_float x in
  let x_log = log10 x_abs in
  let x_mag = float_floor x_log in
  if x_mag = neg_infinity
  then -10000
  else
    let x_apx = 10. ** x_mag in
    if 10. *. x_apx <= x_abs
    then int_of_float x_mag + 1
    else int_of_float x_mag

let c0 = int_of_char '0'

let significant_digits t ~insert_point =
  let t = abs t in
  let mag = magnitude t.hi in
  let scale = pow ten mag in
  let t = div t scale in
  let t, mag =
    if gt t ten
    then div t ten, succ mag
    else if lt t one
    then mul t ten, pred mag
    else t, mag
  in
  let point_pos = succ mag in
  let num_digits = max_print_digits - 1 in
  let buf = Buffer.create max_print_digits in
  let rec aux i y =
    if insert_point && i = point_pos then
      Buffer.add_char buf '.';
    let digit = int_of_float y.hi in
    if digit < 0
    then ()
    else
      let char, rebias =
        if digit > 9
        then '0', true
        else char_of_int (c0 + digit), false
      in
      Buffer.add_char buf char;
      let y = mul (sub y (of_int digit)) ten in
      let y = if rebias then add y ten else y in
      let mag' = magnitude y.hi in
      (*Printf.eprintf "digit: %d, mag:%d, i:%d\n%!" digit mag' i;*)
      if (mag' < 0 && int_abs mag' >= num_digits - i) ||
         (i >= num_digits)
      then ()
      else aux (succ i) y
  in
  aux 0 t;
  mag, Buffer.contents buf

let to_string_std = function
  | t when is_nan t -> "NaN "
  | t when is_zero t -> "0.0"
  | t ->
    let mag, digits = significant_digits t ~insert_point:true in
    let point_pos = mag + 1 in
    let digits =
      if digits.[0] = '.'
      then "0" ^ digits
      else if point_pos < 0
      then "0." ^ String.make (- point_pos) '0' ^ digits
      else
        try ignore (String.index digits '.'); digits
        with Not_found ->
          let zeroes = point_pos - String.length digits in
          digits ^ String.make zeroes '0' ^ ".0"
    in
    if is_negative t
    then "-" ^ digits
    else digits

let to_string_sci = function
  | t when is_nan t -> "NaN "
  | t when is_zero t -> "0.0E0"
  | t ->
    let mag, digits = significant_digits t ~insert_point:true in
    let exp_suffix = "E" ^ string_of_int mag in
    assert (digits.[0] <> '0');
    let digits = Bytes.unsafe_of_string ("." ^ digits) in
    Bytes.set digits 0 (Bytes.get digits 1);
    Bytes.set digits 1 '.';
    let digits = Bytes.unsafe_to_string digits in
    let digits =
      if is_negative t
      then "-" ^ digits
      else digits
    in
    digits ^ exp_suffix

let to_string t =
  let mag = magnitude t.hi in
  if mag >= -3 && mag <= 20
  then to_string_std t
  else to_string_sci t

let of_string s =
  let len = String.length s in
  let pos = ref (-1) in
  let get () =
    incr pos;
    if !pos < len
    then Some s.[!pos]
    else None
  in
  (* Drop whitespaces *)
  while (match get () with Some ' ' | Some '\t' -> true | _ -> false)
  do () done;
  decr pos;
  (* Test sign *)
  let is_negative =
    match get () with
    | Some '-' -> true
    | Some '+' -> false
    | _ -> decr pos; false
  in
  let get_point num_digits = function None -> num_digits | Some p -> p in
  let rec loop pos_point num_digits t =
    match get () with
    | Some ch when ch >= '0' && ch <= '9' ->
      loop pos_point (succ num_digits)
           (add (mul t ten) (of_int (int_of_char ch - c0)))
    | Some '.' when pos_point = None ->
      loop (Some num_digits) num_digits t
    | Some '.' -> invalid_arg "Doubledouble.of_string: two '.'"
    | Some ('e'|'E') ->
      let exp =
        try int_of_string (String.sub s !pos (len - !pos))
        with Invalid_argument _ ->
             invalid_arg "Doubledouble.of_string: invalid exponent"
      in
      num_digits, get_point num_digits pos_point, exp, t
    | None ->
      num_digits, get_point num_digits pos_point, 0, t
    | Some c ->
      Printf.ksprintf invalid_arg
        "Doubledouble.of_string: unknown character %C" c
  in
  let digits, point, exp, t = loop None 0 zero in
  let t =
    match digits - point - exp with
    | 0 -> t
    | n -> mul t (pow ten (-n))
  in
  if is_negative then neg t else t

let protect1 f a = if is_nan a then nan else f a
let protect2 f a b = if is_nan a || is_nan b then nan else f a b

let add   = protect2 add
let sub   = protect2 sub
let mul   = protect2 mul
let div   = protect2 div
let neg   = protect1 neg
let inv   = protect1 inv
let floor = protect1 floor
let ceil  = protect1 ceil
let abs   = protect1 abs
let trunc = protect1 trunc
let sqr   = protect1 sqr
let sqrt  = protect1 sqrt
let pow t n = if is_nan t then nan else pow t n

module Infix = struct
	let ( +..  ) = add
	let ( -..  ) = sub
	let ( ~-.. ) = neg
 	let ( *..  ) = mul
  let ( /..  ) = div
	let ( **.. ) = pow
  let ( <..  ) = lt
  let ( >..  ) = gt
  let ( =..  ) = eq
  let ( <>.. ) = ne
  let ( <=.. ) = le
  let ( >=.. ) = ge
end
