type t
val new_lossy_counter : support:float -> error_tolerance:float -> t

val prune : t -> float -> unit
val items_above_threshold : t -> threshold:float -> (string * float) list
val observe : t -> string -> unit
