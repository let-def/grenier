open Strong

let ic = Scanf.Scanning.from_file Sys.argv.(1)

let () =
  Scanf.bscanf ic "%d %d %d %d\n" @@
  fun state_count transition_count initial_state final_state_count ->

  Printf.eprintf
    "state_count:%d transition_count:%d initial_state:%d final_state_count:%d\n"
    state_count transition_count initial_state final_state_count;

  let module Regex = struct
    type t = string list

    let epsilon : t = [""]

    let to_string = function
      | [] -> assert false
      | xs -> String.concat "|"
                (List.filter (function "" -> false | _ -> true) xs)

    (* Concatenation *)
    let (^.) la lb : t =
      match la, lb with
      | [], _ | _, [] -> []
      | [a], [b] -> [a ^ b]
      | [a], xs -> List.map (fun x -> a ^ x) xs
      | xs, [b] -> List.map (fun x -> x ^ b) xs
      | xs, ys -> ["(" ^ to_string xs ^ ")(" ^ to_string ys ^ ")"]

    (* Disjunction *)
    let (|.) a b = a @ b

    (* Kleene star *)
    let star t =
      match to_string t with
      | "" -> [""]
      | s -> ["(" ^ s ^ ")*"]
  end in
  let module DFA = struct

    module States = Natural.Nth(struct let n = state_count end)

    module Transitions = Natural.Nth(struct let n = transition_count end)

    let transitions = Array.init transition_count (fun _i ->
        Scanf.bscanf ic "%d %d %d\n" @@ fun from_state input to_state ->
        (Finite.Elt.of_int States.n from_state,
         input,
         Finite.Elt.of_int States.n to_state)
      )

    let label t =
      let (_, l, _) = transitions.((t : Transitions.n Finite.elt :> int)) in
      [string_of_int l]

    let source t =
      let (s, _, _) = transitions.((t : Transitions.n Finite.elt :> int)) in
      s

    let target t =
      let (_, _, d) = transitions.((t : Transitions.n Finite.elt :> int)) in
      d

    module Initials = Finite.Array.Of_array(struct
        type a = States.n Finite.elt

        let table = [|Finite.Elt.of_int States.n initial_state|]
    end)

    module Finals = Finite.Array.Of_array(struct
        type a = States.n Finite.elt

        let table = Array.init final_state_count
            (fun _i -> Scanf.bscanf ic "%d\n"
                (Finite.Elt.of_int States.n))
      end)
  end in
  let module Result = State_elimination.Convert(Regex)(DFA) in
  Finite.Array.iter (fun res ->
      List.iter (fun (_, re) ->
          print_endline (String.concat " | " (List.map Regex.to_string re))
        ) res
    ) Result.result
