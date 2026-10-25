(* The interfaces compared by [compare.exe], over int keys. *)

module type SET = Set.S with type elt = int

(* The part of Map.S that Baby's maps provide too. *)
module type MAP = sig
  type 'a t
  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val remove : int -> 'a t -> 'a t
  val find_opt : int -> 'a t -> 'a option
  val union : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val merge : (int -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val split : int -> 'a t -> 'a t * 'a option * 'a t
  val filter : (int -> 'a -> bool) -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val min_binding : 'a t -> int * 'a
  val max_binding : 'a t -> int * 'a
  val of_list : (int * 'a) list -> 'a t
  val of_seq : (int * 'a) Seq.t -> 'a t
end
