open Strong.Finite

module type DFA = sig
  type states
  val states : states set
  type transitions
  val transitions : transitions set

  type label
  val label  : transitions elt -> label
  val source : transitions elt -> states elt
  val target : transitions elt -> states elt

  val initial : states elt
  val finals : states elt array
end

module Minimize
    (Label : Map.OrderedType)
    (In: DFA with type label := Label.t) :
sig
  include DFA with type label = Label.t

  val transport_state : In.states elt -> states elt option
  val transport_transition : In.transitions elt -> transitions elt option

  val represent_state : states elt -> In.states elt
  val represent_transition : transitions elt -> In.transitions elt
end
