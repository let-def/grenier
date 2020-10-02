open Strong

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
  module States : Finite.Set
  module Transitions : Finite.Set
  type label

  val label  : Transitions.n Finite.elt -> label
  val source : Transitions.n Finite.elt -> States.n Finite.elt
  val target : Transitions.n Finite.elt -> States.n Finite.elt

  module Initial : Finite.Map with type codomain = States.n Finite.elt
  module Final : Finite.Map with type codomain = States.n Finite.elt
end

module Convert
    (Regex : Regex) (NFA: NFA with type label = Regex.t) : sig
  val result : Regex.t
end =
struct

  type state = {
    mutable preds: (state * Regex.t) list;
    mutable succs: (state * Regex.t) list;
  }

  let is_alive = function {preds = []; succs = []} -> false | _ -> true

  let make_state () = { preds = []; succs = [] }

  let states : (NFA.States.n, state) Finite.map' =
    Finite.init_map NFA.States.n (fun _ -> make_state ())
  module States = (val states)

  let update_list state label = function
    | (state', label') :: rest when state == state' ->
      (state', Regex.(|.) label label') :: rest
    | otherwise -> (state, label) :: otherwise

  let link source target label = (
    source.succs <- update_list target label source.succs;
    target.preds <- update_list source label target.preds;
  )

  let () = Finite.iter_set NFA.Transitions.n (fun transition ->
      link
        (States.get (NFA.source transition))
        (States.get (NFA.target transition))
        (NFA.label transition)
    )

  let initial = make_state ()
  let final = make_state ()

  let () = (
    Finite.iter_map (module NFA.Initial)
      (fun state -> link initial (States.get state) Regex.epsilon);
    Finite.iter_map (module NFA.Final)
      (fun state -> link (States.get state) final Regex.epsilon)
  )

  let eliminate state =
    let preds = state.preds and succs = state.succs in
    state.succs <- [];
    state.preds <- [];
    let stars =
      List.fold_left
        (fun acc (_succ, label) -> Regex.(|.) label acc)
        Regex.epsilon
        succs
    in
    let stars =
      if stars == Regex.epsilon
      then Regex.epsilon
      else Regex.star stars
    in
    List.iter (fun (succ, label_succ) ->
        if is_alive succ then (
          List.iter (fun (pred, label_pred) ->
              if is_alive pred then (
                let label = Regex.(
                    if stars == epsilon
                    then label_pred ^. stars ^. label_succ
                    else label_pred ^. label_succ
                  )
                in
                link pred succ label;
              )
            ) preds
        )
      ) succs

  let () = Finite.iter_map (module States) eliminate

  let result =
    List.fold_left (fun acc (_final, label) -> Regex.(|.) acc label)
      Regex.epsilon initial.succs
end
