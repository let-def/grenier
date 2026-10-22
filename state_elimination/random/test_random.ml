(* Check that the regexes produced by state elimination denote the same
   languages as random NFAs, on all words up to a bounded length. *)

open Strong

let max_len = 5

module Words = Set.Make(struct
    type t = int list
    let compare = compare
  end)

type node =
  | Eps
  | Chr of int
  | Cat of t * t
  | Alt of t * t
  | Star of t
and t = { node : node; mutable lang : Words.t option }

let mk node = { node; lang = None }

module Regex = struct
  type nonrec t = t
  let epsilon = mk Eps
  let ( ^. ) a b = mk (Cat (a, b))
  let ( |. ) a b = mk (Alt (a, b))
  let star a = mk (Star a)
end

let concat a b =
  Words.fold (fun u acc ->
      let lu = List.length u in
      Words.fold (fun v acc ->
          if lu + List.length v <= max_len then Words.add (u @ v) acc else acc)
        b acc) a Words.empty

(* Words of length <= max_len, memoized on the (shared) regex nodes *)
let rec lang r =
  match r.lang with
  | Some l -> l
  | None ->
    let l = match r.node with
      | Eps -> Words.singleton []
      | Chr c -> Words.singleton [c]
      | Cat (a, b) -> concat (lang a) (lang b)
      | Alt (a, b) -> Words.union (lang a) (lang b)
      | Star a ->
        let la = lang a in
        let rec fix acc =
          let acc' = Words.union acc (concat acc la) in
          if Words.equal acc acc' then acc else fix acc'
        in
        fix (Words.singleton [])
    in
    r.lang <- Some l;
    l

let rec to_string r = match r.node with
  | Eps -> "ε"
  | Chr c -> string_of_int c
  | Cat (a, b) -> "(" ^ to_string a ^ "." ^ to_string b ^ ")"
  | Alt (a, b) -> "(" ^ to_string a ^ "|" ^ to_string b ^ ")"
  | Star a -> to_string a ^ "*"

type nfa = {
  n : int;
  k : int;
  trans : (int * int option * int) array; (* None is an epsilon transition *)
  initials : int array;
  finals : int array;
}

let random_nfa () =
  let n = 1 + Random.int 7 in
  let k = 1 + Random.int 2 in
  let trans = Array.init (Random.int (3 * n)) (fun _ ->
      let l = if Random.int 10 = 0 then None else Some (Random.int k) in
      (Random.int n, l, Random.int n)) in
  let initials = Array.init (1 + Random.int 2) (fun _ -> Random.int n) in
  let finals = Array.init (Random.int 3) (fun _ -> Random.int n) in
  { n; k; trans; initials; finals }

let print_nfa a =
  Printf.printf "states:%d labels:%d\n" a.n a.k;
  Array.iter (fun (s, l, t) ->
      Printf.printf "  %d -%s-> %d\n" s
        (match l with None -> "ε" | Some l -> string_of_int l) t) a.trans;
  let pa name l =
    Printf.printf "%s: [%s]\n" name
      (String.concat ";" (Array.to_list (Array.map string_of_int l))) in
  pa "initials" a.initials;
  pa "finals" a.finals

(* All words up to max_len *)
let all_words k =
  let rec go len =
    if len = 0 then [[]] else
      let shorter = go (len - 1) in
      [] :: List.concat_map (fun w ->
          if List.length w = len - 1 then List.init k (fun c -> c :: w) else [])
        shorter
      @ List.filter (fun w -> w <> []) shorter
  in
  List.sort_uniq compare (go max_len)

(* Reference: subset simulation with epsilon closure *)
let accepts a src dst word =
  let closure set =
    let set = Array.copy set in
    let changed = ref true in
    while !changed do
      changed := false;
      Array.iter (fun (s, l, t) ->
          if l = None && set.(s) && not set.(t) then
            (set.(t) <- true; changed := true)) a.trans
    done;
    set
  in
  let start = Array.make a.n false in
  start.(src) <- true;
  let final = List.fold_left (fun set c ->
      let next = Array.make a.n false in
      Array.iter (fun (s, l, t) -> if l = Some c && set.(s) then next.(t) <- true)
        a.trans;
      closure next) (closure start) word in
  final.(dst)

let failures = ref 0

let check a =
  let module NFA = struct
    module States = Natural.Nth(struct let n = a.n end)
    module Transitions = Natural.Nth(struct let n = Array.length a.trans end)
    let get t = a.trans.((t : Transitions.n Finite.elt :> int))
    let label t = match get t with
      | (_, None, _) -> Regex.epsilon
      | (_, Some c, _) -> mk (Chr c)
    let source t = let (s, _, _) = get t in Finite.Elt.of_int States.n s
    let target t = let (_, _, s) = get t in Finite.Elt.of_int States.n s
    module Initials = Finite.Array.Of_array(struct
        type a = States.n Finite.elt
        let table = Array.map (Finite.Elt.of_int States.n) a.initials
      end)
    module Finals = Finite.Array.Of_array(struct
        type a = States.n Finite.elt
        let table = Array.map (Finite.Elt.of_int States.n) a.finals
      end)
  end in
  let module R = State_elimination.Convert(Regex)(NFA) in
  let words = all_words a.k in
  let result = Finite.Array.to_array R.result in
  Array.iteri (fun i finals ->
      Array.iteri (fun j _ ->
          let regexes =
            List.concat_map (fun (j', res) ->
                if (j' : _ Finite.elt :> int) = j then res else [])
              finals
          in
          let l = List.fold_left (fun acc r -> Words.union acc (lang r))
              Words.empty regexes in
          List.iter (fun w ->
              let expected = accepts a a.initials.(i) a.finals.(j) w in
              if Words.mem w l <> expected then begin
                incr failures;
                Printf.printf "FAILURE: initial #%d final #%d, word [%s]: \
                               regex %s, nfa %s\n"
                  i j (String.concat ";" (List.map string_of_int w))
                  (if expected then "rejects" else "accepts")
                  (if expected then "accepts" else "rejects");
                print_nfa a;
                List.iter (fun r -> print_endline ("  " ^ to_string r)) regexes;
                raise Exit
              end) words)
        a.finals) result

let () =
  let seed = try int_of_string Sys.argv.(1) with _ -> 42 in
  let iterations = try int_of_string Sys.argv.(2) with _ -> 5000 in
  Random.init seed;
  for _ = 1 to iterations do
    let a = random_nfa () in
    (try check a with Exit -> ());
    if !failures >= 3 then exit 1
  done;
  if !failures > 0 then exit 1;
  Printf.printf "state_elimination: %d random NFAs OK\n" iterations
