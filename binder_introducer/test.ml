type var = int

type sexp_with_sharing =
  | Atom of string
  | List of sexp_with_sharing list
  | Var of var
  | Let of (var * sexp_with_sharing) list * sexp_with_sharing

let graph = {
  Binder_introducer.
  memoize = (fun f ->
      let table = Hashtbl.create 7 in
      fun x ->
        match Hashtbl.find_opt table x with
        | Some y -> y
        | None ->
          let y = f x in
          Hashtbl.add table x y;
          y
    );
  successors = (fun f acc x ->
      match x with
      | Atom _ -> acc
      | List sexps ->
        List.fold_left f acc sexps
      (* The traversal stops at existing binders *)
      | Var _ | Let _ -> acc
    );
}

let binding_structure () =
  let id = ref 0 in
  {
    Binder_introducer.
    (* Rewrite subterms of a term with a custom function *)
    map_subterms = begin fun f sexp ->
      match sexp with
      | Atom _  | Var _ | Let _ as t -> t
      | List sexps -> List (List.map f sexps)
    end;

    (* Produce a fresh variable for a term *)
    name_term = (fun _ -> let var = !id in incr id; var);

    (* Injection from variable to terms *)
    var_term = (fun var -> Var var);

    (* [introduce_let ~recursive bindings body] create a possibly recursive
       let-binder term that binds the names in [bindings] in the scope of [body]
    *)
    introduce_let = (fun ~recursive bindings body ->
        assert (not recursive);
        Let (bindings, body)
      );
  }

let explicit_sharing term =
  Binder_introducer.explicit_sharing graph (binding_structure ()) term

let () =
  assert (explicit_sharing (List [Atom "a"; Atom "a"]) =
          Let ([0, Atom "a"], List [Var 0; Var 0]))
