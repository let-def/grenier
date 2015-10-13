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
module DD = Doubledouble
open DD.Infix

let delta x y = DD.abs (x -.. y)

let check_epsilon a b eps =
  let delta = delta a b in
  let err = DD.(to_float delta) in
  let ok = err <= eps in
  if not ok then 
    Printf.eprintf "%f <= %f: %b (%s)\n%!" err eps (err <= eps) (DD.to_string delta);
  assert ok

let check_add_mult2 a =
  check_epsilon (a +.. a) (a *.. DD.of_int 2) 0.

let check_mult_div a b eps =
  check_epsilon a ((a *.. b) /.. b) eps

let check_div_mult a b eps =
  check_epsilon a ((a /.. b) *.. b) eps

let check_inv x eps =
  check_epsilon x DD.(inv (inv x)) eps

let check_sqrt d eps =
  let d' = DD.sqrt d in
  check_epsilon d (DD.sqr d') eps

let check_trunc x expect =
  assert (DD.trunc x =.. expect)

let check_binomial_square a b =
  let a, b = DD.(of_float a, of_float b) in
  (* a^2 + ̂b^2 + 2*a*b *)
  let sum = DD.(sqr b +.. of_int 2 *.. a *.. b) in
  (* (a + b)^2 - a^2 *)
  let diff  = DD.(sqr (a +.. b) -.. sqr a) in
  let delta = diff -.. sum in
  assert (diff =.. sum);
  assert (DD.is_zero delta)

let check_binomial2 a b =
  (* (a + b)^2 *)
  let a, b  = DD.(of_float a, of_float b) in 
  let sum   = (a +.. b) *.. (a -.. b)     in 
  let diff  = ~-.. (sum -.. (a *.. a))    in 
  let delta = diff -..  (b *.. b)         in 
  assert (diff =.. (b *.. b));
  assert (DD.is_zero delta)

let check_inv d eps =
  check_epsilon DD.(inv (inv d)) d eps

let slowpow x exp =
  if exp = 0
  then DD.one
  else
    let x = if exp < 0 then DD.inv x else x in
    let pow = ref x in
    for i = 2 to abs exp do
      pow := DD.mul x !pow
    done;
    !pow

let check_pow x exp eps =
  let p1 = DD.pow x exp in
  let p2 = slowpow x exp in
  check_epsilon p1 p2 eps

let () = (* Test NaN *)
  assert (DD.is_nan (DD.of_int 1 /.. DD.of_int 0));
  assert (DD.is_nan (DD.of_int 1 *.. DD.nan))

let () = (* Test Add Mult 2 *)
  check_add_mult2 (DD.of_int 3);
  check_add_mult2 (DD.pi)

let () = (* Test Mult Div *)
  check_mult_div DD.pi     DD.e 1e-30;
  check_mult_div DD.two_pi DD.e 1e-30;
  check_mult_div DD.pi_2   DD.e 1e-30;
  check_mult_div (DD.of_float 39.4) (DD.of_int 10) 1e-30
 
let () = (* Test Div Mult *)
  check_div_mult DD.pi DD.e 1e-30;
  check_div_mult (DD.of_float 39.4) (DD.of_int 10) 1e-30

let () = (* Test reciprocal *)
  let tests = [
    3.0        , 0.;
    99.0       , 1.e-29;
    999.0      , 0.;
    314159269.0, 0.;
  ] in
  List.iter (fun (x,eps) -> check_inv (DD.of_float x) eps) tests

let () = (* Test binomial square & binomial 2 *)
  let tests = [
    100.0    , 1.;
    1000.0   , 1.;
    10000.0  , 1.;
    100000.0 , 1.;
    1000000.0, 1.;
    1e8      , 1.;
    1e10     , 1.;
    1e14     , 1.;
  	(* Following call will fail, because it requires 32 digits of precision
    1e16     , 1.; *)
    1e14     , 291.;
    5e14     , 291.;
    5e14     , 345291.;
  ] in
  List.iter (fun (x,eps) -> check_binomial_square x eps) tests;
  List.iter (fun (x,eps) -> check_binomial2 x eps) tests

let () = (* Test Pow *)
  let tests = [
    0.     , 3 , 16. *. DD.k_eps;
    14.    , 3 , 16. *. DD.k_eps;
    3.     , -5, 16. *. DD.k_eps;
    -3.    , 5 , 16. *. DD.k_eps;
    -3.    , -5, 16. *. DD.k_eps;
    0.12345, -5, 1e5 *. DD.k_eps;
  ] in
  List.iter (fun (x,exp,eps) -> check_pow (DD.of_float x) exp eps) tests;
