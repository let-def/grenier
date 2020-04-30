(* Part 1 : Sequence of elements *)

type 'a t

val empty : 'a t
(* The empty sequence *)

val element : 'a -> 'a t
(* A sequence with a single element *)

val concat : 'a t -> 'a t -> 'a t
(* Concatenation of two sequences *)


(* Part 2 : Transformation of sequences *)

(* A type that stores the information for transforming incrementally a sequence
   of type 'a into a value of type 'b *)
type ('a, 'b) reducer

val reducer : map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> ('a, 'b) reducer
(* Create a new transformation from a map and a reduce functions. *)

val reduce : ('a, 'b) reducer -> 'b option
(* Get the result of the reduction. Returns:
   - [None] for the empty sequence
   - [Some (reduce (map elt1) (...))]
*)

val update : ('a, 'b) reducer -> 'a t -> 'b option array * ('a, 'b) reducer
(* Update a transformation to apply to a new sequence.
   The array contains the elements produced by the previous reduction that
   are no longer used in the new reduction.
   Because the evaluation is lazy, the array may contain None if the function
   [reduce] has not been called.
   Call [reduce] on the updated reducer to get the result of the reduction. *)
