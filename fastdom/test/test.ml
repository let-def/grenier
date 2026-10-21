(* Fastdom against a naive iterative dominator computation. *)
let fails = ref 0
let check name b = if not b then (incr fails; if !fails < 20 then Printf.printf "FAIL %s\n%!" name)

let test n edges entry =
  let succ = Array.make n [] in
  List.iter (fun (a, b) -> succ.(a) <- b :: succ.(a)) edges;
  let graph = { Fastdom.
    memoize = (fun f -> let tbl = Hashtbl.create 16 in
                fun x -> match Hashtbl.find_opt tbl x with
                  | Some v -> v
                  | None -> let v = f x in Hashtbl.add tbl x v; v);
    successors = (fun f acc x -> List.fold_left f acc succ.(x)) } in
  let info, map = Fastdom.dominance graph entry in
  (* reachability *)
  let reach = Array.make n false in
  let rec dfs x = if not reach.(x) then (reach.(x) <- true; List.iter dfs succ.(x)) in
  dfs entry;
  (* naive dominators: dom sets as bool arrays *)
  let dom = Array.init n (fun x -> if x = entry then Array.init n (( = ) entry) else Array.make n true) in
  let preds = Array.make n [] in
  List.iter (fun (a, b) -> if reach.(a) then preds.(b) <- a :: preds.(b)) edges;
  let changed = ref true in
  while !changed do
    changed := false;
    for x = 0 to n - 1 do
      if x <> entry && reach.(x) then begin
        let nd = Array.init n (fun y -> y = x || (preds.(x) <> [] && List.for_all (fun p -> dom.(p).(y)) preds.(x))) in
        if nd <> dom.(x) then (dom.(x) <- nd; changed := true)
      end
    done
  done;
  let card a = Array.fold_left (fun c b -> if b then c + 1 else c) 0 a in
  let idom x =
    (* strict dominator with the most dominators *)
    let best = ref (-1) in
    Array.iteri (fun y d -> if d && y <> x && (!best < 0 || card dom.(y) > card dom.(!best)) then best := y) dom.(x);
    !best in
  check "info length" (Array.length info = Array.fold_left (fun c b -> if b then c + 1 else c) 0 reach);
  Array.iteri (fun i t -> check "postorder index" (Fastdom.postorder_index t = i)) info;
  for x = 0 to n - 1 do
    let t = map x in
    check "node" (Fastdom.node t = x);
    check "reachable" (Fastdom.is_reachable t = reach.(x));
    if reach.(x) then begin
      check "info" (info.(Fastdom.postorder_index t) == t);
      if x = entry then check "entry dom" (Fastdom.dominator t == t)
      else check (Printf.sprintf "idom of %d" x) (Fastdom.node (Fastdom.dominator t) = idom x);
      let ps = List.sort compare (List.map Fastdom.node (Fastdom.predecessors t)) in
      check "predecessors" (ps = List.sort compare preds.(x))
    end else check "unreachable index" (Fastdom.postorder_index t = -1)
  done

let () =
  let seed = try int_of_string (Sys.getenv "SEED") with Not_found -> Random.self_init (); Random.bits () in
  Random.init seed; Printf.printf "fastdom: seed %d\n%!" seed;
  for _ = 1 to 3000 do
    let n = 1 + Random.int 30 in
    let m = Random.int (3 * n) in
    let edges = List.init m (fun _ -> Random.int n, Random.int n) in
    test n edges (Random.int n)
  done;
  (* A long chain with back edges only: the dominator of i is i - 1. *)
  let n = 100_000 in
  let succ = Array.init n (fun i -> if i + 1 < n then [i + 1] else []) in
  for _ = 1 to 1000 do
    let a = Random.int n in let b = Random.int (a + 1) in succ.(a) <- b :: succ.(a)
  done;
  let graph = { Fastdom.
    memoize = (fun f -> let tbl = Hashtbl.create 16 in
                fun x -> match Hashtbl.find_opt tbl x with
                  | Some v -> v
                  | None -> let v = f x in Hashtbl.add tbl x v; v);
    successors = (fun f acc x -> List.fold_left f acc succ.(x)) } in
  let _, map = Fastdom.dominance graph 0 in
  for i = 1 to n - 1 do
    check "chain idom" (Fastdom.node (Fastdom.dominator (map i)) = i - 1)
  done;
  Printf.printf "fastdom: %s (%d failures)\n" (if !fails = 0 then "OK" else "FAILED") !fails;
  if !fails > 0 then exit 1
