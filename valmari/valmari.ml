module Fin = Strong.Finite

module type DFA = sig
  type states
  val states : states Fin.set
  type transitions
  val transitions : transitions Fin.set

  type label
  val label  : transitions Fin.elt -> label
  val source : transitions Fin.elt -> states Fin.elt
  val target : transitions Fin.elt -> states Fin.elt

  val initial : states Fin.elt
  val finals : states Fin.elt array
end

let index_transitions (type state) (type transition)
    (states : state Fin.set)
    (transitions : transition Fin.set)
    (target : transition Fin.elt -> state Fin.elt)
  : state Fin.elt -> (transition Fin.elt -> unit) -> unit
  =
  let f = Array.make (Fin.Set.cardinal states + 1) 0 in
  Fin.Set.iter transitions (fun t ->
      let state = (target t :> int)
      in f.(state) <- f.(state) + 1
    );
  for i = 0 to Fin.Set.cardinal states - 1 do
    f.(i + 1) <- f.(i + 1) + f.(i)
  done;
  let a = Array.make (Fin.Set.cardinal transitions)
      (Fin.Elt.of_int transitions 0)
  in
  Fin.Set.rev_iter transitions (fun t ->
    let state = (target t :> int) in
    let index = f.(state) - 1 in
    f.(state) <- index;
    a.(index) <- t
    );
  (fun st fn ->
     let st = (st : state Fin.elt :> int) in
     for i = f.(st) to f.(st + 1) - 1 do fn a.(i) done
  )


let discard_unreachable
    (type state) (type transition)
    (blocks : state Partition.t)
    (transitions_of : state Fin.elt -> (transition Fin.elt -> unit) -> unit)
    (target : transition Fin.elt -> state Fin.elt)
  =
  Partition.iter_marked_elements blocks 0 (fun state ->
      transitions_of state
        (fun transition -> Partition.mark blocks (target transition))
    );
  Partition.discard_unmarked blocks

module Minimize
    (Label : Map.OrderedType)
    (In: DFA with type label := Label.t) :
sig
  include DFA with type label = Label.t

  val transport_state :
    In.states Fin.elt -> states Fin.elt option
  val transport_transition :
    In.transitions Fin.elt -> transitions Fin.elt option

  val represent_state :
    states Fin.elt -> In.states Fin.elt
  val represent_transition :
    transitions Fin.elt -> In.transitions Fin.elt
end = struct

  (* State partition *)
  let blocks = Partition.create In.states

  (* Remove states unreachable from initial state *)
  let () =
    Partition.mark blocks In.initial;
    let transitions_source =
      index_transitions In.states In.transitions In.source
    in
    discard_unreachable blocks transitions_source In.target

  (* Index the set of transitions targeting a state *)
  let transitions_targeting =
    index_transitions In.states In.transitions In.target

  (* Remove states unreachable from final states *)
  let () =
    Array.iter (Partition.mark blocks) In.finals;
    discard_unreachable blocks transitions_targeting In.source

  (* Split final states *)
  let () =
    Array.iter (Partition.mark blocks) In.finals;
    Partition.split blocks

  (* Transition partition *)
  let cords =
    let partition t1 t2 = Label.compare (In.label t1) (In.label t2) in
    Partition.create In.transitions ~partition

  let () =
    Partition.discard cords (fun t ->
        Partition.set_of blocks (In.source t) = -1 ||
        Partition.set_of blocks (In.target t) = -1
      )

  (* Main loop, split the sets *)
  let () =
    let block_set = ref 1 in
    let cord_set = ref 0 in
    while !cord_set < Partition.set_count cords do
      Partition.iter_elements cords !cord_set
        (fun transition -> Partition.mark blocks (In.source transition));
      Partition.split blocks;
      while !block_set < Partition.set_count blocks do
        Partition.iter_elements blocks !block_set (fun state ->
            transitions_targeting state (Partition.mark cords)
          );
        Partition.split cords;
        incr block_set;
      done;
      incr cord_set;
    done

  module States =
    Strong.Natural.Nth(struct let n = Partition.set_count blocks end)
  type states = States.n
  let states = States.n

  module Transitions = Fin.Array.Of_array(struct
      type a = In.transitions Fin.elt
      let table =
        match Partition.set_count cords with
        | 0 -> [||]
        | count ->
          let count' = ref 0 in
          for i = 0 to count - 1 do
            let elt = Partition.choose cords i in
            if Partition.set_of blocks (In.target elt) > -1 then
              incr count';
          done;
          let table = Array.make !count' (Partition.choose cords 0) in
          let count' = ref 0 in
          for i = 0 to count - 1 do
            let elt = Partition.choose cords i in
            if Partition.set_of blocks (In.target elt) > -1 then (
              table.(!count') <- elt;
              incr count';
            )
          done;
          table
    end)
  type transitions = Transitions.n
  let transitions = Transitions.n

  type label = Label.t

  let transport_state_unsafe state =
    Fin.Elt.of_int states (Partition.set_of blocks state)

  let represent_state state =
    Partition.choose blocks (state : states Fin.elt :> int)

  let represent_transition transition =
    Fin.(Transitions.table.(transition))

  let label transition : Label.t =
    In.label (represent_transition transition)

  let source transition =
    transport_state_unsafe (In.source (represent_transition transition))

  let target transition =
    transport_state_unsafe (In.target (represent_transition transition))

  let initial =
    Fin.Elt.of_int states (Partition.set_of blocks In.initial)

  let finals =
    Array.iter (Partition.mark blocks) In.finals;
    let sets = Partition.marked_sets blocks in
    Partition.clear_marks blocks;
    Array.map (Fin.Elt.of_int states) (Array.of_list sets)

  let transport_state state =
    match Partition.set_of blocks state with
    | -1 -> None
    | n -> Some (Fin.Elt.of_int states n)

  let transport_transition transition =
    match Partition.set_of cords transition with
    | -1 -> None
    | n -> Some (Fin.Elt.of_int transitions n)

end
