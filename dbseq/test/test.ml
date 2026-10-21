let fails = ref 0
let check name b = if not b then (incr fails; if !fails < 20 then Printf.printf "FAIL %s\n%!" name)

let same name (d : int Dbseq.t) (l : int list) =
  check (name ^ " length") (Dbseq.length d = List.length l);
  check (name ^ " is_empty") (Dbseq.is_empty d = (l = []));
  check (name ^ " to_seq") (List.of_seq (Dbseq.to_seq d) = l);
  check (name ^ " to_rev_seq") (List.of_seq (Dbseq.to_rev_seq d) = List.rev l);
  List.iteri (fun i x -> check (name ^ " get") (Dbseq.get i d = x)) l;
  check (name ^ " get oob") (match Dbseq.get (List.length l) d with _ -> false | exception Not_found -> true);
  check (name ^ " get neg") (match Dbseq.get (-1) d with _ -> false | exception Not_found -> true | exception Invalid_argument _ -> true)

let rec drop n l = if n <= 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t

let () =
  let seed = try int_of_string (Sys.getenv "SEED") with Not_found -> Random.self_init (); Random.bits () in
  Random.init seed; Printf.printf "dbseq: seed %d\n%!" seed;
  for _ = 1 to 2000 do
    let d = ref Dbseq.empty and l = ref [] in
    for _ = 1 to Random.int 400 do
      let len = List.length !l in
      (match Random.int 10 with
       | 0 when len > 0 -> let i = Random.int len and v = Random.int 1000 in
         d := Dbseq.set i v !d; l := List.mapi (fun j x -> if j = i then v else x) !l
       | 1 when len > 0 -> let i = Random.int len in
         d := Dbseq.update !d i succ; l := List.mapi (fun j x -> if j = i then succ x else x) !l
       | 2 -> (match Dbseq.uncons !d, !l with
           | None, [] -> ()
           | Some (x, d'), y :: l' -> check "uncons" (x = y); d := d'; l := l'
           | _ -> check "uncons shape" false)
       | 3 -> let n = Random.int (len + 3) in d := Dbseq.drop n !d; l := drop n !l
       | _ -> let v = Random.int 1000 in d := Dbseq.cons v !d; l := v :: !l)
    done;
    same "random" !d !l
  done;
  let big = ref Dbseq.empty in
  for i = 0 to 1_000_000 do big := Dbseq.cons i !big done;
  check "big length" (Dbseq.length !big = 1_000_001);
  check "big get" (Dbseq.get 0 !big = 1_000_000 && Dbseq.get 1_000_000 !big = 0 && Dbseq.get 500_000 !big = 500_000);
  check "big drop" (Dbseq.length (Dbseq.drop 999_999 !big) = 2);
  Printf.printf "dbseq: %s (%d failures)\n" (if !fails = 0 then "OK" else "FAILED") !fails;
  if !fails > 0 then exit 1
