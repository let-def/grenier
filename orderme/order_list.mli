(** {0 Basic ordering operations} *)

(** An element of an ordering. *)
type t

(** Create a new ordering with a single element. O(1) *)
val root : unit -> t

(** [after t] inserts a new element to the ordering, greater than [t] but
    less than all existing elements greater than [t].

    O(1) amortized. *)
val after  : t -> t

(** [before t] inserts a new element to the ordering, less than [t] but
    greater than all existing elements less than [t].

    O(1) amortized. *)
val before : t -> t

(** Check if two elements belong to the same order. O(1) *)
val same_order : t -> t -> bool

(** Compare two elements. O(1) *)
val compare : t -> t -> int

(** How many elements are ordered. O(1) *)
val cardinal : t -> int

(** {1 Memory management} *)

(** Memory of every element is retained. When you know you are not going to use
    an element any longer, [forget] it to release memory. O(1). *)
val forget : t -> unit

(** After calling [forget], an element should not be used.
    You can check if it is the case with [is_valid]. *)
val is_valid : t -> bool

(* Algorithm due to:
   Two Simplified Algorithms for Maintaining Order in a List
   Bender et al., 2002 *)

(* Unsafe functions.  Used internally and for debug purposes. *)
val unsafe_next : t -> t
val unsafe_prev : t -> t
val unsafe_check : t -> string -> unit
