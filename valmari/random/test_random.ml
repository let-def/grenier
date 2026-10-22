(* Differential test: compare Valmari minimization against a naive Moore
   partition refinement on random partial DFAs. *)

open Strong

type dfa = {
  n : int;                       (* states *)
  k : int;                       (* labels *)
  trans : (int * int * int) array; (* source, label, target *)
  initials : int list;
  finals : int list;
  refinements : int list list;
}

let random_dfa () =
  let n = Random.int 12 in
  let k = 1 + Random.int 3 in
  let density = Random.float 1.0 in
  let trans = ref [] in
  if n > 0 then
    for s = 0 to n - 1 do
      for l = 0 to k - 1 do
        if Random.float 1.0 < density then
          trans := (s, l, Random.int n) :: !trans
      done
    done;
  let trans = Array.of_list !trans in
  (* Shuffle so transitions are not sorted by source *)
  for i = Array.length trans - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = trans.(i) in trans.(i) <- trans.(j); trans.(j) <- t
  done;
  let subset p = if n = 0 then [] else
      List.filter (fun _ -> Random.float 1.0 < p) (List.init n Fun.id) in
  let initials =
    if n = 0 then [] else Random.int n :: subset 0.1 in
  let finals = subset (Random.float 0.6) in
  let refinements = List.init (Random.int 3) (fun _ -> subset 0.4) in
  { n; k; trans; initials; finals; refinements }

let print_dfa d =
  Printf.printf "states:%d labels:%d\n" d.n d.k;
  Array.iter (fun (s, l, t) -> Printf.printf "  %d -%d-> %d\n" s l t) d.trans;
  let pl name l =
    Printf.printf "%s: [%s]\n" name
      (String.concat ";" (List.map string_of_int l)) in
  pl "initials" d.initials;
  pl "finals" d.finals;
  List.iter (pl "refinement") d.refinements

(* Reference: trim then Moore refinement *)

let reference d =
  let succ = Array.make d.n [] and pred = Array.make d.n [] in
  Array.iter (fun (s, _, t) ->
      succ.(s) <- t :: succ.(s); pred.(t) <- s :: pred.(t)) d.trans;
  let closure roots next =
    let seen = Array.make d.n false in
    let rec visit s =
      if not seen.(s) then (seen.(s) <- true; List.iter visit next.(s)) in
    List.iter visit roots;
    seen
  in
  let reach = closure d.initials succ and coreach = closure d.finals pred in
  let live = Array.init d.n (fun s -> reach.(s) && coreach.(s)) in
  let delta = Array.make_matrix d.n d.k (-1) in
  Array.iter (fun (s, l, t) -> delta.(s).(l) <- t) d.trans;
  let is_final = Array.make d.n false in
  List.iter (fun s -> is_final.(s) <- true) d.finals;
  let cls = Array.make d.n 0 in
  let refine signature =
    let tbl = Hashtbl.create 16 in
    let count = ref 0 in
    let keys = Array.init d.n (fun s -> if live.(s) then Some (signature s) else None) in
    for s = 0 to d.n - 1 do
      if live.(s) then begin
        let key = Option.get keys.(s) in
        let c = match Hashtbl.find_opt tbl key with
          | Some c -> c
          | None -> let c = !count in incr count; Hashtbl.add tbl key c; c
        in
        cls.(s) <- c
      end else cls.(s) <- -1
    done;
    !count
  in
  let base s =
    is_final.(s) :: List.map (List.mem s) d.refinements in
  let count = ref (refine (fun s -> (base s, []))) in
  let continue = ref true in
  while !continue do
    let target s l =
      let t = delta.(s).(l) in if t >= 0 && live.(t) then cls.(t) else -1 in
    let count' =
      refine (fun s -> (base s, cls.(s) :: List.init d.k (target s))) in
    continue := count' <> !count;
    count := count'
  done;
  let edges = ref [] in
  for s = 0 to d.n - 1 do
    if live.(s) then
      for l = 0 to d.k - 1 do
        let t = delta.(s).(l) in
        if t >= 0 && live.(t) then
          edges := (cls.(s), l, cls.(t)) :: !edges
      done
  done;
  (!count, Array.copy cls, List.sort_uniq compare !edges)

let failures = ref 0

let check d =
  let fail fmt =
    Printf.ksprintf (fun msg ->
        incr failures;
        Printf.printf "FAILURE: %s\n" msg;
        print_dfa d;
        raise Exit) fmt
  in
  let ref_count, ref_cls, ref_edges = reference d in
  let module In = struct
    module States = Natural.Nth(struct let n = d.n end)
    type states = States.n
    let states = States.n
    module Transitions = Natural.Nth(struct let n = Array.length d.trans end)
    type transitions = Transitions.n
    let transitions = Transitions.n
    let get t = d.trans.((t : transitions Finite.elt :> int))
    let label t = let (_, l, _) = get t in l
    let source t = let (s, _, _) = get t in Finite.Elt.of_int states s
    let target t = let (_, _, s) = get t in Finite.Elt.of_int states s
    let iter l f = List.iter (fun s -> f (Finite.Elt.of_int states s)) l
    let initials f = iter d.initials f
    let finals f = iter d.finals f
    let refinements ~refine =
      List.iter (fun l -> refine ~iter:(iter l)) d.refinements
  end in
  let module M = Valmari.Minimize(Int)(In) in
  let count = Finite.Set.cardinal M.states in
  if count <> ref_count then
    fail "state count %d, expected %d" count ref_count;
  (* Same partition as the reference, up to renaming *)
  let rename = Array.make (max ref_count 1) (-1) in
  for s = 0 to d.n - 1 do
    let st = Finite.Elt.of_int In.states s in
    match M.transport_state st, ref_cls.(s) with
    | None, -1 -> ()
    | None, _ -> fail "live state %d discarded" s
    | Some _, -1 -> fail "dead state %d kept" s
    | Some m, c ->
      let m = (m :> int) in
      if rename.(c) = -1 then rename.(c) <- m
      else if rename.(c) <> m then
        fail "state %d mapped to %d, expected %d" s m rename.(c)
  done;
  let seen = Array.make (max count 1) false in
  Array.iter (fun m -> if m >= 0 then seen.(m) <- true) rename;
  Finite.Set.iter M.states (fun m ->
      if not seen.((m :> int)) then fail "minimized state %d unused" (m :> int);
      let r = M.represent_state m in
      match M.transport_state r with
      | Some m' when m' = m -> ()
      | _ -> fail "represent_state %d does not transport back" (m :> int));
  (* Transitions *)
  let cls_of m =
    let r = ref (-1) in
    Array.iteri (fun c m' -> if m' = m then r := c) rename; !r in
  let edges = Finite.Set.fold_left M.transitions (fun acc t ->
      let src = (M.source t :> int) and tgt = (M.target t :> int) in
      let tr = M.represent_transition t in
      if In.label tr <> M.label t then fail "represent_transition: label";
      if M.transport_state (In.source tr) <> Some (M.source t) ||
         M.transport_state (In.target tr) <> Some (M.target t) then
        fail "represent_transition: endpoints";
      if M.transport_transition tr <> Some t then
        fail "transport_transition (represent_transition t) <> t";
      (cls_of src, M.label t, cls_of tgt) :: acc) [] in
  let sorted = List.sort compare edges in
  if List.length (List.sort_uniq compare edges) <> List.length edges then
    fail "duplicate transitions";
  if sorted <> ref_edges then
    fail "transitions differ (%d vs %d expected)"
      (List.length sorted) (List.length ref_edges);
  Finite.Set.iter In.transitions (fun tr ->
      match M.transport_transition tr with
      | None -> ()
      | Some t -> if M.represent_transition t <> tr then
          fail "transport_transition inconsistent");
  (* Initials and finals *)
  let transported l =
    List.sort_uniq compare (List.filter_map (fun s ->
        Option.map (fun m -> (m : _ Finite.elt :> int))
          (M.transport_state (Finite.Elt.of_int In.states s))) l) in
  let sorted_arr a =
    List.sort compare (List.map (fun m -> (m : _ Finite.elt :> int))
                         (Array.to_list a)) in
  if sorted_arr M.initials <> transported d.initials then fail "initials differ";
  if sorted_arr M.finals <> transported d.finals then fail "finals differ"

let () =
  Printexc.record_backtrace true;
  let seed = try int_of_string Sys.argv.(1) with _ -> 42 in
  let iterations = try int_of_string Sys.argv.(2) with _ -> 20000 in
  Random.init seed;
  for _ = 1 to iterations do
    let d = random_dfa () in
    try check d with
    | Exit -> if !failures >= 5 then exit 1
    | exn ->
      incr failures;
      Printf.printf "EXCEPTION: %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ());
      print_dfa d;
      if !failures >= 5 then exit 1
  done;
  if !failures > 0 then exit 1;
  Printf.printf "valmari: %d random DFAs OK\n" iterations
