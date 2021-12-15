(**
   A library to compute graph dominators using
      "A Simple, Fast Dominance Algorithm"
   by Keith D. Cooper, Timothy J. Harvey, Ken Kennedy.
*)

(** {1 Graph representation} *)

(** Abstraction of graphs with nodes of type ['a].
    Instance of [graph] must be provided by the user of the library.*)
type 'a graph = {

  memoize: 'b. ('a -> 'b) -> ('a -> 'b);
  (** [memoize f] memoizes a function [f] over nodes of the graph.
      The function returned must evaluate [f x] at most once for each
      node [x].
      If [f] raises an exception, the same exception must be
      propagated to the caller but it is not necessary to memoize
      it. *)

  successors: 'b. ('b -> 'a -> 'b) -> 'b -> 'a -> 'b;
  (** [successors f acc n] fold over the successors of node [n],
      threading and updating the [acc] value. *)
}

(** {1 Dominance information} *)

type 'a t
(** Dominance information for a vertex of type ['a] *)

val node : 'a t -> 'a
(** The node to which this information applies. *)

val dominator : 'a t -> 'a t
(** [dominator (node n)] returns the information associated
    to the dominator of [n].
    If [n] is its own dominator, then
      [dominator (node n) == node n].
*)

val is_reachable : 'a t -> bool
(** [is_reachable (node n)] returns true iff [n] is reachable from
    [entrypoint] following the [successors] relation. *)

val postorder_index : 'a t -> int
(** [postorder_index (node n)] is the index of [n] in the postorder
    traversal of the graph (see [dominance]), starting from 0.

    If [n] was not reachable from the entrypoint,
      [post_order_index (node n) = -1]

    Though in this case, it is better to use [is_reachable] before to
    check the validity of the node. *)

val predecessors : 'a t -> 'a t list
(** Reverse the [successors] relation (on the subset of the graph
    reachable from [entrypoint]). *)

(** {1 Dominance computation} *)

(** [dominance graph entrypoint = (info, map)]
    computes the dominators of [graph] starting from [entrypoint].

    The [info] array is indexed by the postorder index of a vertex,
    see [postorder_index].

    [map n] is the dominance information of node [n].
    If [n] is not reachable from [entrypoint], then
      [is_reachable (map n) = false].
*)
val dominance : 'a graph -> 'a -> 'a t array * ('a -> 'a t)
