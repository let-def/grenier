open Strong.Finite

module type Regex = sig
  type t

  val epsilon : t

  (* Concatenation *)
  val (^.) : t -> t -> t

  (* Disjunction *)
  val (|.) : t -> t -> t

  (* Kleene star *)
  val star : t -> t
end

module type NFA = sig
  type states
  val states : states set
  type transitions
  val transitions : transitions set
  type label

  val label  : transitions elt -> label
  val source : transitions elt -> states elt
  val target : transitions elt -> states elt

  type initials
  val initials : (initials, states elt) Array.t
  type finals
  val finals : (finals, states elt) Array.t
end

module Convert
    (Regex : Regex) (NFA: NFA with type label := Regex.t) :
sig
  val result : (NFA.initials, (NFA.finals elt * Regex.t list) list) Array.t
end =
struct

  type temp =
    | Unused
    | Label of Regex.t
    | Final of { index: NFA.finals elt; mutable regexes : Regex.t list }

  type state = {
    mutable preds: (state * Regex.t) list;
    mutable succs: (state * Regex.t) list;
    mutable temp: temp;
  }

  let is_alive = function {preds = []; succs = []; _} -> false | _ -> true

  let state_counter = ref 0
  let make_state () = incr state_counter;
    { preds = []; succs = []; temp = Unused }

  let states : (NFA.states, state) Array.t =
    Array.init NFA.states (fun _ -> make_state ())

  let update_list state label = function
    | (state', label') :: rest when state == state' ->
      (state', Regex.(|.) label label') :: rest
    | otherwise -> (state, label) :: otherwise

  let link source target label = (
    source.succs <- update_list target label source.succs;
    target.preds <- update_list source label target.preds;
  )

  let () = Set.iter NFA.transitions (fun transition ->
      link
        states.(NFA.source transition)
        states.(NFA.target transition)
        (NFA.label transition)
    )

  let initials =
    let prepare_initial nfa_state =
      let state = make_state () in
      link state states.(nfa_state) Regex.epsilon;
      state
    in
    Array.map prepare_initial NFA.initials

  let finals =
    let prepare_final nfa_state =
      let state = make_state () in
      link states.(nfa_state) state Regex.epsilon;
      state
    in
    Array.map prepare_final NFA.finals

  let normalize_transitions transitions =
    let to_temp transitions =
      assert (List.for_all (fun (state, _) -> state.temp = Unused) transitions);
      List.iter (fun (state, label) ->
          if is_alive state then (
            match state.temp with
            | Unused -> state.temp <- Label label
            | Label label' -> state.temp <- Label (Regex.(|.) label label')
            | Final _ -> assert false
          )
        ) transitions
    in
    let extract_temp (state, _) =
      match state.temp with
      | Unused -> None
      | Label label -> state.temp <- Unused; Some (state, label)
      | Final _ -> assert false
    in
    to_temp transitions;
    List.filter_map extract_temp transitions

  let eliminate state =
    decr state_counter;
    let preds = state.preds and succs = state.succs in
    state.succs <- [];
    state.preds <- [];
    let stars =
      List.fold_left
        (fun acc (succ, label) ->
           if succ == state then Regex.(|.) label acc else acc)
        Regex.epsilon
        succs
    in
    let preds = normalize_transitions preds in
    let succs = normalize_transitions succs in
    Printf.eprintf "state %d, %d predecessors, %d successors\n%!"
      !state_counter (List.length preds) (List.length succs);
    let stars =
      if stars == Regex.epsilon
      then Regex.epsilon
      else Regex.star stars
    in
    List.iter (fun (succ, label_succ) ->
        List.iter (fun (pred, label_pred) ->
            let label = Regex.(
                if stars == epsilon
                then label_pred ^. stars ^. label_succ
                else label_pred ^. label_succ
              )
            in
            link pred succ label;
          ) preds
      ) succs

  let () = Array.iter eliminate states

  let result =
    let normalize_initial initial = normalize_transitions initial.succs in
    let normalized = Array.map normalize_initial initials in
    let tag_final index state = state.temp <- Final {index; regexes = []} in
    Array.iteri tag_final finals;
    let non_null = ref [] in
    let prepare_transition (state, regex) =
      match state.temp with
      | Final t ->
        if t.regexes = [] then non_null := state.temp :: !non_null;
        t.regexes <- regex :: t.regexes
      | _ -> assert false
    in
    let flush_final = function
      | Final t ->
        let result = t.index, t.regexes in
        t.regexes <- [];
        result
      | _ -> assert false
    in
    let prepare_initial transitions =
      List.iter prepare_transition transitions;
      let result = List.map flush_final !non_null in
      non_null := [];
      result
    in
    Array.map prepare_initial normalized
end

let convert
    (type regex initials finals)
    (module Regex : Regex with type t = regex)
    (module NFA : NFA with type label = regex
                       and type initials = initials
                       and type finals = finals)
    : (initials, (finals elt * regex list) list) Array.t
    =
    let module Result = Convert(Regex)(NFA) in
    Result.result
