(*
 * Copyright (c) 2022 Frédéric Bour <frederic.bour@lakaban.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Generic code to reveal sharing in user-defined data structures *)

(** Representation of a graph with nodes of type 'a *)
type 'a graph = 'a Fastdom.graph = {
  memoize: 'b. ('a -> 'b) -> ('a -> 'b);
  (** Memoize a function on nodes *)

  successors: 'b. ('b -> 'a -> 'b) -> 'b -> 'a -> 'b;
  (** Fold over successors of a node *)
}

(** Rewrite a (possibly cyclic) directed graph by introducing
    let-binders at dominating nodes *)

type ('term, 'var) binding_structure = {

  (* Rewrite subterms of a term with a custom function *)
  map_subterms: ('term -> 'term) -> 'term -> 'term;

  (* Produce a fresh variable for a term *)
  name_term: 'term -> 'var;

  (* Injection from variable to terms *)
  var_term: 'var -> 'term;

  (* [introduce_let ~recursive bindings body] create a possibly recursive
     let-binder term that binds the names in [bindings] in the scope of [body]
  *)
  introduce_let: recursive:bool -> ('var * 'term) list -> 'term -> 'term;
}

val explicit_sharing : 'a graph -> ('a, 'b) binding_structure -> 'a -> 'a
