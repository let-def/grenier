#load "doubledouble.cma";;
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
open Doubledouble.Infix

(* e = 1 + 1 + 1/2! + 1/3! + 1/4! + ... *)
let e_by_taylor_series () =
  let rec aux s t i =
    if Doubledouble.to_float t > Doubledouble.k_eps
    then let t = t /.. Doubledouble.of_int i in
         aux (s +.. t) t (succ i)
    else s
  in
  Doubledouble.(aux (of_int 2) (of_int 1) 2)

(* arctan(x) = x - x^3 / 3 + x^5 / 5 - x^7 / 7 + ... *)
let arctan_by_taylor_series x =
  let x2 = Doubledouble.sqr x in
  let rec aux ~t ~at ~d ~sign =
    let at = if sign then at +.. Doubledouble.(pow x d /.. of_int d)
                     else at -.. Doubledouble.(pow x d /.. of_int d)
    in
    if Doubledouble.to_float t > Doubledouble.k_eps
    then aux ~t:(t *.. x2)  ~at
        ~d:(d + 2) ~sign:(not sign)
    else at
  in
  aux ~t:x ~at:Doubledouble.zero ~d:1 ~sign:true

(* Pi / 4  =  4 * arctan(1/5) - arctan(1/239) *)
let pi_by_machin () =
  let t1 = Doubledouble.(one /.. of_int 5) in
  let t2 = Doubledouble.(one /.. of_int 239) in
  let d4  = Doubledouble.of_int 4 in
  let pi4 = d4 *.. arctan_by_taylor_series t1
               -.. arctan_by_taylor_series t2 in
  pi4 *.. d4

let () = (* Test E expansion *)
  let e = e_by_taylor_series () in
  let err = abs_float (Doubledouble.to_float (e -.. Doubledouble.e)) in
  assert (err < 64. *. Doubledouble.k_eps)

let () = (* Test pi by Machin *)
  let pi = pi_by_machin () in
  let err = abs_float (Doubledouble.to_float (pi -.. Doubledouble.pi)) in
  prerr_endline (Doubledouble.to_string pi);
  prerr_endline Doubledouble.(to_string pi);
  Printf.eprintf "%.37f\n%!" err;
  assert (err < 8. *. Doubledouble.k_eps)
