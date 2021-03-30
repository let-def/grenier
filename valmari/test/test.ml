open Strong

let ic = Scanf.Scanning.from_file Sys.argv.(1)

let () =
  Scanf.bscanf ic "%d %d %d %d\n" @@
  fun state_count transition_count initial_state final_state_count ->

  Printf.eprintf
    "state_count:%d transition_count:%d initial_state:%d final_state_count:%d\n"
    state_count transition_count initial_state final_state_count;

  let module Label = struct
    type t = int
    let compare : t -> t -> int = compare
  end in
  let module DFA = struct

    module States = Natural.Nth(struct let n = state_count end)
    type states = States.n
    let states = States.n

    module Transitions = Natural.Nth(struct let n = transition_count end)
    type transitions = Transitions.n
    let transitions = Transitions.n

    let trans_table = Array.init transition_count (fun _i ->
        Scanf.bscanf ic "%d %d %d\n" @@ fun from_state input to_state ->
        (Finite.Elt.of_int States.n from_state,
         input,
         Finite.Elt.of_int States.n to_state)
      )

    let label t =
      let (_, l, _) = trans_table.((t : Transitions.n Finite.elt :> int)) in
      l

    let source t =
      let (s, _, _) = trans_table.((t : Transitions.n Finite.elt :> int)) in
      s

    let target t =
      let (_, _, d) = trans_table.((t : Transitions.n Finite.elt :> int)) in
      d

    let initials =
      [|Finite.Elt.of_int States.n initial_state|]

    let finals = Array.init final_state_count
        (fun _i -> Scanf.bscanf ic "%d\n"
            (Finite.Elt.of_int States.n))

    let refinements ~refine:_ = ()
  end in
  let module MDFA = Valmari.Minimize(Label)(DFA) in
  Printf.printf
    "%d %d %d %d\n"
    (Finite.Set.cardinal MDFA.states)
    (Finite.Set.cardinal MDFA.transitions)
    (MDFA.initials.(0) :> int)
    (Array.length MDFA.finals);

  Finite.Set.iter MDFA.transitions
    (fun t ->
       Printf.printf "%d %d %d\n"
         (MDFA.source t :> int)
         (MDFA.label t :> int)
         (MDFA.target t :> int));

  Array.iter
    (fun t -> Printf.printf "%d\n" (t : _ Finite.elt :> int))
    MDFA.finals
