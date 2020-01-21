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
end
