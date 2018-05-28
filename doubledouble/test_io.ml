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

let check_std_dd d str =
  let str' = Doubledouble.to_string_std d in
  Printf.eprintf "%s = %s? %b\n%!" str' str (str' = str);
  assert (str' = str)

let check_std_f f str =
  check_std_dd (Doubledouble.of_float f) str

let check_sci_dd d str =
  let str' = Doubledouble.to_string_sci d in
  Printf.eprintf "%s = %s? %b\n%!" str' str (str' = str);
  assert (str' = str)

let check_sci_f f str =
  check_std_dd (Doubledouble.of_float f) str

let check_parse str dd err =
  let dd' = Doubledouble.of_string str in
  let err' = Doubledouble.to_float (dd' -.. dd) in
  assert (err' <= err)

let check_parse_error str =
  assert (try ignore (Doubledouble.of_string str); false
          with _ -> true)

let () = (* Test standard notation *)
  check_std_f  1.0 "1.0";
  check_std_f  0.0 "0.0";

  (* cases where hi is a power of 10 and lo is negative *)
  check_std_dd Doubledouble.(of_float 1e12 -.. one) "999999999999.0";
  check_std_dd Doubledouble.(of_float 1e14 -.. one) "99999999999999.0";
  check_std_dd Doubledouble.(of_float 1e16 -.. one) "9999999999999999.0";

  check_std_dd Doubledouble.(of_int (-379363639) /.. of_int 100000000) "-3.79363639";

  check_std_dd Doubledouble.(t (-3.79363639) (8.039137357367426E-17))
  		                      "-3.7936363900000000000000000";

  check_std_dd Doubledouble.(of_int 34 /.. of_int 1000) "0.034";
  check_std_f  1.05e3 "1050.0";
  check_std_f  0.34 "0.34000000000000002442490654175344";
  check_std_dd Doubledouble.(of_int 34 /.. of_int 100) "0.34";
  check_std_f  14. "14.0"
