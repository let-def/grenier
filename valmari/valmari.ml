open Strong

module type DFA = sig
  module States : Finite.Set
  module Transitions : Finite.Set
  module Label : Map.OrderedType

  val label  : Transitions.n Finite.elt -> Label.t
  val source : Transitions.n Finite.elt -> States.n Finite.elt
  val target : Transitions.n Finite.elt -> States.n Finite.elt

  val initial_state : States.n Finite.elt
  module Final : Finite.Map with type codomain = States.n Finite.elt
end

let index_transitions (type state) (type transition)
    (states : state Finite.set)
    (transitions : transition Finite.set)
    (target : transition Finite.elt -> state Finite.elt)
  : state Finite.elt -> transition Finite.elt Finite.map
  =
  let f = Array.make (Finite.cardinal states + 1) 0 in
  Finite.iter_set transitions (fun t ->
      let state = (target t :> int)
      in f.(state) <- f.(state) + 1
    );
  for i = 0 to Finite.cardinal states - 1 do
    f.(i + 1) <- f.(i + 1) + f.(i)
  done;
  let a = Array.make (Finite.cardinal transitions)
      (Finite.elt_of_int transitions 0)
  in
  Finite.rev_iter_set transitions (fun t ->
    let state = (target t :> int) in
    let index = f.(state) - 1 in
    f.(state) <- index;
    a.(index) <- t
    );
  (fun st ->
     let st = (st : state Finite.elt :> int) in
     let offset = f.(st) in
     let n = f.(st + 1) - offset in
     let module Domain = Natural.Nth(struct let n = n end) in
     (module struct
       type domain = Domain.n
       let domain = Domain.n
       type codomain = transition Finite.elt
       let get n = a.(offset + (n : Domain.n Finite.elt :> int))
     end))

let discard_unreachable
    (type state) (type transition)
    (blocks : state Partition.t)
    (transitions_of : state Finite.elt -> transition Finite.elt Finite.map)
    (target : transition Finite.elt -> state Finite.elt)
  =
  Partition.iter_marked_elements blocks 0 (fun state ->
      Finite.iter_map (transitions_of state)
        (fun transition -> Partition.mark blocks (target transition));
    );
  Partition.discard_unmarked blocks

module Minimize (DFA: DFA) : sig
  include DFA with module Label = DFA.Label

  val transport_state :
    DFA.States.n Finite.elt -> States.n Finite.elt option
  val transport_transition :
    DFA.Transitions.n Finite.elt -> Transitions.n Finite.elt option

  val represent_state :
    States.n Finite.elt -> DFA.States.n Finite.elt
  val represent_transition :
    Transitions.n Finite.elt -> DFA.Transitions.n Finite.elt
end = struct

  (* State partition *)
  let blocks = Partition.create DFA.States.n

  (* Remove states unreachable from initial state *)
  let () =
    Partition.mark blocks DFA.initial_state;
    let transitions_source =
      index_transitions DFA.States.n DFA.Transitions.n DFA.source
    in
    discard_unreachable blocks transitions_source DFA.target

  (* Index the set of transitions targeting a state *)
  let transitions_targeting =
    index_transitions DFA.States.n DFA.Transitions.n DFA.target

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
      DFA.Transitions.n
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

  module States = Natural.Nth(struct let n = Partition.set_count blocks end)
  module Transitions = Natural.Nth(struct let n = Partition.set_count cords end)
  module Label = DFA.Label

  let transport_state_unsafe state =
    Finite.elt_of_int
      States.n (Partition.set_of blocks state)

  let represent_state state =
    Partition.choose blocks
      (state : States.n Finite.elt :> int)

  let represent_transition transition =
    Partition.choose cords
      (transition : Transitions.n Finite.elt :> int)

  let label transition : Label.t =
    DFA.label (represent_transition transition)

  let source transition =
    transport_state_unsafe (DFA.source (represent_transition transition))

  let target transition =
    transport_state_unsafe (DFA.target (represent_transition transition))

  let initial_state =
    Finite.elt_of_int States.n
      (Partition.set_of blocks DFA.initial_state)

  module Final = Finite.Map_of_array(struct
      type codomain = States.n Finite.elt

      let table =
        Finite.iter_map (module DFA.Final) (Partition.mark blocks);
        let sets = Partition.marked_sets blocks in
        Partition.clear_marks blocks;
        Array.map (Finite.elt_of_int States.n) (Array.of_list sets)
    end)

  let transport_state state =
    match Partition.set_of blocks state with
    | -1 -> None
    | n -> Some (Finite.elt_of_int States.n n)

  let transport_transition transition =
    match Partition.set_of cords transition with
    | -1 -> None
    | n -> Some (Finite.elt_of_int Transitions.n n)

end
