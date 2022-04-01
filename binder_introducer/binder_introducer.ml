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

(* Generic code to reveal sharing in a cyclic graph *)

type 'a graph = 'a Fastdom.graph = {
  memoize: 'b. ('a -> 'b) -> ('a -> 'b);
  successors: 'b. ('b -> 'a -> 'b) -> 'b -> 'a -> 'b;
}

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

type occurrence = {
  mutable min_scope: int;
  cursor: int ref;
}

let explicit_sharing
    (type a b) (gr : a Fastdom.graph) (bs : (a, b) binding_structure) t =
  let postorder, dominance = Fastdom.dominance gr t in
  let count = Array.length postorder in
  let bindings = Array.make count [] in
  let var_name = Array.make count None in
  let share tag = match Fastdom.predecessors tag with
    | [] -> false
    | [_] -> Fastdom.node tag == t
    | _ :: _ :: _ -> true
  in
  for i = count - 1 downto 0 do
    let tag = postorder.(i) in
    if share tag then begin
      let node = Fastdom.node tag in
      let var = bs.name_term node in
      var_name.(i) <- Some (bs.var_term var);
      let dominator = Fastdom.dominator tag in
      let index = Fastdom.postorder_index dominator in
      bindings.(index) <- (var, tag) :: bindings.(index)
    end
  done;
  let null_occurrence = {min_scope = 0; cursor = ref 0} in
  let rec_occurrences = Array.make count null_occurrence in
  let rec traverse ~is_binding t =
    let cursor = ref max_int in
    let bindings, t =
      let tag = dominance t in
      let id = Fastdom.postorder_index tag in
      if id = -1 then
        ([], t)
      else
        match var_name.(id) with
        | Some name when not is_binding ->
          let occ = rec_occurrences.(id) in
          if !(occ.cursor) < occ.min_scope then
            occ.min_scope <- !(occ.cursor);
          ([], name)
        | _ ->
          match bindings.(id) with
          | [] -> ([], t)
          | bindings' ->
            bindings.(id) <- [];
            let init_occurrence (_, tag) =
              rec_occurrences.(Fastdom.postorder_index tag) <- {
                min_scope = max_int;
                cursor;
              }
            in
            List.iter init_occurrence bindings';
            (bindings', t)
    in
    let t = bs.map_subterms traverse_child t in
    match List.mapi traverse_binding bindings with
    | [] -> t
    | bindings ->
      let normalize_scope (_, occ, _) min_scope =
        if min_scope < occ.min_scope then (
          occ.min_scope <- min_scope;
          min_scope
        ) else
          occ.min_scope
      in
      ignore (List.fold_right normalize_scope bindings max_int : int);
      let let_ ~recursive group body =
        match group with
        | [] -> body
        | bindings ->
          bs.introduce_let ~recursive
            (if recursive then bindings else List.rev bindings)
            body
      in
      let rec nonrec_bindings group ~scope_limit ~index = function
        | [] ->
          let_ ~recursive:false group t
        | (var, occ, t') :: bindings when occ.min_scope > index ->
          if index >= scope_limit then (
            let_ ~recursive:false group
              (nonrec_bindings [var, t']
                 ~scope_limit:occ.min_scope
                 ~index:(index + 1) bindings)
          ) else
            nonrec_bindings
              ((var, t') :: group)
              ~scope_limit:(min occ.min_scope scope_limit)
              ~index:(index + 1) bindings
        | bindings ->
          let_ ~recursive:false group (rec_bindings [] index bindings)
      and rec_bindings group index = function
        | (var, occ, t') :: bindings when occ.min_scope <= index ->
          rec_bindings ((var, t') :: group) (index + 1) bindings
        | bindings ->
          let_ ~recursive:true group
            (nonrec_bindings [] ~scope_limit:max_int ~index bindings)
      in
      nonrec_bindings [] ~scope_limit:max_int ~index:0 bindings
  and traverse_child t =
    traverse ~is_binding:false t
  and traverse_binding index (var, tag) =
    let occ = rec_occurrences.(Fastdom.postorder_index tag) in
    occ.cursor := index;
    (var, occ, traverse ~is_binding:true (Fastdom.node tag))
  in
  traverse ~is_binding:true t
