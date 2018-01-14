module type DFA = sig
  module States : Finite.Set
  module Transitions : Finite.Set
  module Label : Map.OrderedType

  val label  : Transitions.t Finite.element -> Label.t
  val source : Transitions.t Finite.element -> States.t Finite.element
  val target : Transitions.t Finite.element -> States.t Finite.element

  val initial_state : States.t Finite.element
  module Final : Finite.Map with type codomain = States.t Finite.element
end

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
end
