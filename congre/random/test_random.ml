(* Congre against a naive congruence closure, with snapshots. *)
let fails = ref 0
let check name b = if not b then (incr fails; if !fails < 20 then Printf.printf "FAIL %s\n%!" name)

type eq = Eq of int * int | App of int * int * int

let naive n eqs =
  let p = Array.init n Fun.id in
  let rec find x = if p.(x) = x then x else (let r = find p.(x) in p.(x) <- r; r) in
  let union a b = let a = find a and b = find b in if a <> b then (p.(a) <- b; true) else false in
  List.iter (function Eq (a, b) -> ignore (union a b) | App _ -> ()) eqs;
  let apps = List.filter_map (function App (f, x, y) -> Some (f, x, y) | Eq _ -> None) eqs in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (f1, x1, y1) -> List.iter (fun (f2, x2, y2) ->
        if find f1 = find f2 && find x1 = find x2 && union y1 y2 then changed := true) apps) apps
  done;
  find, apps

let () =
  let seed = try int_of_string (Sys.getenv "SEED") with Not_found -> Random.self_init (); Random.bits () in
  Random.init seed; Printf.printf "congre: seed %d\n%!" seed;
  for _ = 1 to 2000 do
    let n = 2 + Random.int 25 in
    let gr = Congre.make () in
    let nodes = Array.init n (fun i -> Congre.fresh gr i) in
    let eqs = ref [] in
    let snaps = ref [] in
    for _ = 1 to Random.int 40 do
      (match Random.int 10 with
       | 0 | 1 | 2 -> let a = Random.int n and b = Random.int n in
         Congre.assume_equal nodes.(a) nodes.(b); eqs := Eq (a, b) :: !eqs
       | 3 | 4 | 5 | 6 -> let f = Random.int n and x = Random.int n and y = Random.int n in
         Congre.assume_application nodes.(f) nodes.(x) ~equal:nodes.(y); eqs := App (f, x, y) :: !eqs
       | 7 when Sys.getenv_opt "NOSNAP" = None -> snaps := (Congre.snapshot gr, !eqs) :: !snaps
       | 7 -> ()
       | _ when Sys.getenv_opt "NOSNAP" <> None -> ()
       | _ -> (match !snaps with
           | [] -> ()
           | l -> let k = Random.int (List.length l) in
             let (s, e) = List.nth l k in
             Congre.restore s; eqs := e;
             (* snapshots taken after s are invalidated *)
             snaps := List.filteri (fun i _ -> i >= k) l));
      if Random.int 3 = 0 then begin
        let find, apps = naive n !eqs in
        for a = 0 to n - 1 do for b = 0 to n - 1 do
            check "same" (Congre.same nodes.(a) nodes.(b) = (find a = find b))
          done done;
        let f = Random.int n and x = Random.int n in
        let expected = List.find_opt (fun (f', x', _) -> find f' = find f && find x' = find x) apps in
        (match Congre.find_app nodes.(f) nodes.(x), expected with
         | None, None -> ()
         | Some z, Some (_, _, y) -> check "find_app class" (find (Congre.get_tag z) = find y)
         | Some _, None -> check "find_app spurious" false
         | None, Some _ -> check "find_app missing" false)
      end
    done
  done;
  Printf.printf "congre: %s (%d failures)\n" (if !fails = 0 then "OK" else "FAILED") !fails;
  if !fails > 0 then exit 1
