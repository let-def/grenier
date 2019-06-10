module type DFA = sig
  module States : Finite.Set
  module Transitions : Finite.Set
  module Label : Map.OrderedType

  val label  : Transitions.element -> Label.t
  val source : Transitions.element -> States.element
  val target : Transitions.element -> States.element

  val initial_state : States.element
  module Final : Finite.Map with type codomain = States.element
end

let index_transitions (type state) (type transition)
    (states : state Finite.set)
    (transitions : transition Finite.set)
    (target : transition Finite.element -> state Finite.element)
  : state Finite.element -> transition Finite.element Finite.map
  =
  let f = Array.make (Finite.cardinal states + 1) 0 in
  Finite.Element.iter transitions (fun t ->
      let state = (target t :> int)
      in f.(state) <- f.(state) + 1
    );
  for i = 0 to Finite.cardinal states - 1 do
    f.(i + 1) <- f.(i + 1) + f.(i)
  done;
  let a = Array.make (Finite.cardinal transitions)
      (Finite.Element.of_int transitions 0)
  in
  Finite.Element.rev_iter transitions (fun t ->
    let state = (target t :> int) in
    let index = f.(state) - 1 in
    f.(state) <- index;
    a.(index) <- t
    );
  (fun st ->
     let st = (st : state Finite.element :> int) in
     let offset = f.(st) in
     let n = f.(st + 1) - offset in
     (module struct
       module Domain = Finite.Set(struct let n = n end)
       type codomain = transition Finite.element
       let element n = a.(offset + (n : Domain.element :> int))
     end))

let discard_unreachable
    (type state) (type transition)
    (blocks : state Partition.t)
    (transitions_of : state Finite.element -> transition Finite.element Finite.map)
    (target : transition Finite.element -> state Finite.element)
  =
  Partition.iter_marked_elements blocks 0 (fun state ->
      Finite.iter_map (transitions_of state)
        (fun transition -> Partition.mark blocks (target transition));
    );
  Partition.discard_unmarked blocks

module Minimize (DFA: DFA) : sig
  include DFA with module Label = DFA.Label

  val transport_state :
    DFA.States.t Finite.element -> States.t Finite.element option
  val transport_transition :
    DFA.Transitions.t Finite.element -> Transitions.t Finite.element option

  val represent_state :
    States.t Finite.element -> DFA.States.t Finite.element
  val represent_transition :
    Transitions.t Finite.element -> DFA.Transitions.t Finite.element
end = struct

  (* State partition *)
  let blocks = Partition.create (module DFA.States)

  (* Remove states unreachable from initial state *)
  let () =
    Partition.mark blocks DFA.initial_state;
    let transitions_source =
      index_transitions (module DFA.States) (module DFA.Transitions) DFA.source
    in
    discard_unreachable blocks transitions_source DFA.target

  (* Index the set of transitions targeting a state *)
  let transitions_targeting =
    index_transitions (module DFA.States) (module DFA.Transitions) DFA.target

  (* Remove states unreachable from final states *)
  let () =
    Finite.iter_map (module DFA.Final) (Partition.mark blocks);
    discard_unreachable blocks transitions_targeting DFA.source

  (* Split final states *)
  let () =
    Finite.iter_map (module DFA.Final) (Partition.mark blocks);
    Partition.split blocks

  (* Transition partition *)
  let cords = Partition.create
      (module DFA.Transitions)
      ~partition:(fun t1 t2 ->
          let l1 = DFA.label t1 in
          let l2 = DFA.label t2 in
          DFA.Label.compare l1 l2
        )

  let () =
    Partition.discard cords
      (fun t ->
         Partition.set_of blocks (DFA.source t) = -1 ||
         Partition.set_of blocks (DFA.target t) = -1)

  (* Main loop, split the sets *)
  let () =
    let block_set = ref 1 in
    let cord_set = ref 0 in
    while !cord_set < Partition.set_count cords do
      Partition.iter_elements cords !cord_set
        (fun transition -> Partition.mark blocks (DFA.source transition));
      Partition.split blocks;
      while !block_set < Partition.set_count blocks do
        Partition.iter_elements blocks !block_set (fun state ->
            Finite.iter_map (transitions_targeting state) (Partition.mark cords)
          );
        Partition.split cords;
        incr block_set;
      done;
      incr cord_set;
    done

  module States = Finite.Set(struct let n = Partition.set_count blocks end)
  module Transitions = Finite.Set(struct let n = Partition.set_count cords end)
  module Label = DFA.Label

  let transport_state_unsafe state =
    Finite.Element.of_int
      (module States) (Partition.set_of blocks state)

  let represent_state state =
    Partition.choose blocks
      (state : States.t Finite.element :> int)

  let represent_transition transition =
    Partition.choose cords
      (transition : Transitions.t Finite.element :> int)

  let label transition : Label.t =
    DFA.label (represent_transition transition)

  let source transition =
    transport_state_unsafe (DFA.source (represent_transition transition))

  let target transition =
    transport_state_unsafe (DFA.target (represent_transition transition))

  let initial_state =
    Finite.Element.of_int (module States)
      (Partition.set_of blocks DFA.initial_state)

  module Final = Finite.Map_of_array(struct
      type codomain = States.t Finite.element

      let table =
        Finite.iter_map (module DFA.Final) (Partition.mark blocks);
        let sets = Partition.marked_sets blocks in
        Partition.clear_marks blocks;
        Array.map (Finite.Element.of_int (module States)) (Array.of_list sets)
    end)

  let transport_state state =
    match Partition.set_of blocks state with
    | -1 -> None
    | n -> Some (Finite.Element.of_int (module States) n)

  let transport_transition transition =
    match Partition.set_of cords transition with
    | -1 -> None
    | n -> Some (Finite.Element.of_int (module Transitions) n)

end
