let ic = Scanf.Scanning.from_file Sys.argv.(1)

let () =
  Scanf.bscanf ic "%d %d %d %d\n" @@
  fun state_count transition_count initial_state final_state_count ->

  Printf.eprintf
    "state_count:%d transition_count:%d initial_state:%d final_state_count:%d\n"
    state_count transition_count initial_state final_state_count;

  let module DFA = struct

    module States = Finite.Set(struct let n = state_count end)

    module Transitions = Finite.Set(struct let n = transition_count end)

    module Label = struct
      type t = int
      let compare : t -> t -> int = compare
    end

    let initial_state =
      Finite.Element.of_int (module States) initial_state

    let transitions = Array.init transition_count (fun _i ->
        Scanf.bscanf ic "%d %d %d\n" @@ fun from_state input to_state ->
        (Finite.Element.of_int (module States) from_state,
         input,
         Finite.Element.of_int (module States) to_state)
      )

    let label t =
      let (_, l, _) = transitions.((t : Transitions.element :> int)) in
      l

    let source t =
      let (s, _, _) = transitions.((t : Transitions.element :> int)) in
      s

    let target t =
      let (_, _, d) = transitions.((t : Transitions.element :> int)) in
      d

    module Final = Finite.Map_of_array(struct
        type codomain = States.element

        let table = Array.init final_state_count
            (fun _i -> Scanf.bscanf ic "%d\n"
                (Finite.Element.of_int (module States)))
      end)
  end in
  let module MDFA = Valmari.Minimize(DFA) in
  Printf.printf
    "%d %d %d %d\n"
    MDFA.States.n
    MDFA.Transitions.n
    (MDFA.initial_state :> int)
    MDFA.Final.Domain.n;

  Finite.Element.iter (module MDFA.Transitions)
    (fun t ->
       Printf.printf "%d %d %d\n"
         (MDFA.source t :> int)
         (MDFA.label t :> int)
         (MDFA.target t :> int));

  Finite.iter_map (module MDFA.Final)
    (fun t -> Printf.printf "%d\n" (t :> int))
