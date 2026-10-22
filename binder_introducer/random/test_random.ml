(* Random (possibly cyclic) graphs: check that explicit_sharing produces a
   well-scoped term, without duplication, that unfolds back to the graph. *)

type term =
  | Node of int                       (* input: a graph node *)
  | T of int * term list              (* output: a node with its children *)
  | Var of int
  | Let of bool * (int * term) list * term

let random_graph () =
  let n = 1 + Random.int 12 in
  let density = Random.float 2.5 in
  let cyclic = Random.bool () in
  Array.init n (fun i ->
      let deg = int_of_float (Random.float density) in
      List.init deg (fun _ ->
          if cyclic then Random.int n
          else if i = n - 1 then -1
          else i + 1 + Random.int (n - i - 1))
      |> List.filter (fun j -> j >= 0))

let print_graph g =
  Array.iteri (fun i succs ->
      Printf.printf "  %d -> [%s]\n" i
        (String.concat ";" (List.map string_of_int succs))) g

let rec print_term = function
  | Node i -> Printf.sprintf "Node %d" i
  | T (i, []) -> string_of_int i
  | T (i, ts) ->
    Printf.sprintf "%d(%s)" i (String.concat ", " (List.map print_term ts))
  | Var v -> Printf.sprintf "v%d" v
  | Let (r, bs, body) ->
    Printf.sprintf "let%s %s in %s" (if r then " rec" else "")
      (String.concat " and "
         (List.map (fun (v, t) -> Printf.sprintf "v%d = %s" v (print_term t)) bs))
      (print_term body)

let failures = ref 0

let check g =
  let n = Array.length g in
  let graph = {
    Binder_introducer.
    memoize = (fun f ->
        let table = Hashtbl.create 7 in
        fun x -> match Hashtbl.find_opt table x with
          | Some y -> y
          | None -> let y = f x in Hashtbl.add table x y; y);
    successors = (fun f acc -> function
        | Node i -> List.fold_left (fun acc j -> f acc (Node j)) acc g.(i)
        | _ -> acc);
  } in
  let counter = ref 0 in
  let bs = {
    Binder_introducer.
    map_subterms = (fun f -> function
        | Node i -> T (i, List.map (fun j -> f (Node j)) g.(i))
        | t -> t);
    name_term = (fun _ -> let v = !counter in incr counter; v);
    var_term = (fun v -> Var v);
    introduce_let = (fun ~recursive bindings body ->
        Let (recursive, bindings, body));
  } in
  let result = Binder_introducer.explicit_sharing graph bs (Node 0) in
  let fail fmt = Printf.ksprintf (fun msg ->
      incr failures;
      Printf.printf "FAILURE: %s\n" msg;
      print_graph g;
      print_endline (print_term result);
      raise Exit) fmt
  in
  (* Scoping, unique binders, collect definitions *)
  let defs = Hashtbl.create 7 in
  let uses = Hashtbl.create 7 in
  let rec scope env = function
    | Node i -> fail "unexpanded Node %d" i
    | T (_, ts) -> List.iter (scope env) ts
    | Var v ->
      if not (List.mem v env) then fail "v%d used out of scope" v;
      Hashtbl.replace uses v ()
    | Let (recursive, bindings, body) ->
      if bindings = [] then fail "empty let";
      let vars = List.map fst bindings in
      List.iter (fun (v, t) ->
          if Hashtbl.mem defs v then fail "v%d bound twice" v;
          Hashtbl.add defs v t) bindings;
      let env' = vars @ env in
      List.iter (fun (_, t) -> scope (if recursive then env' else env) t)
        bindings;
      scope env' body
  in
  scope [] result;
  Hashtbl.iter (fun v _ ->
      if not (Hashtbl.mem uses v) then fail "v%d is never used" v) defs;
  (* Unfold up to a bounded depth *)
  let rec unfold depth node = function
    | Var v -> unfold depth node (Hashtbl.find defs v)
    | Let (_, _, body) -> unfold depth node body
    | T (i, ts) ->
      if i <> node then fail "found node %d, expected %d" i node;
      if List.length ts <> List.length g.(i) then
        fail "node %d: wrong arity" i;
      if depth > 0 then List.iter2 (unfold (depth - 1)) g.(i) ts
    | Node _ -> assert false
  in
  unfold 6 0 result;
  (* Each reachable node occurs exactly once *)
  let occurrences = Array.make n 0 in
  let rec count = function
    | Node _ | Var _ -> ()
    | T (i, ts) -> occurrences.(i) <- occurrences.(i) + 1; List.iter count ts
    | Let (_, bindings, body) ->
      List.iter (fun (_, t) -> count t) bindings; count body
  in
  count result;
  let reachable = Array.make n false in
  let rec visit i =
    if not reachable.(i) then (reachable.(i) <- true; List.iter visit g.(i)) in
  visit 0;
  Array.iteri (fun i r ->
      let expected = if r then 1 else 0 in
      if occurrences.(i) <> expected then
        fail "node %d occurs %d times, expected %d" i occurrences.(i) expected)
    reachable

let () =
  let seed = try int_of_string Sys.argv.(1) with _ -> 42 in
  let iterations = try int_of_string Sys.argv.(2) with _ -> 20000 in
  Random.init seed;
  for _ = 1 to iterations do
    (try check (random_graph ()) with Exit -> ());
    if !failures >= 3 then exit 1
  done;
  if !failures > 0 then exit 1;
  Printf.printf "binder_introducer: %d random graphs OK\n" iterations
