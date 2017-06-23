type sticky_sampler

val new_sampler : support:float -> error_tolerance:float -> failure_prob:float -> sticky_sampler
val prune : sticky_sampler -> unit
val items_above_threshold : sticky_sampler -> float -> (string * float) list -> (string * float) list
val observe : sticky_sampler -> string -> unit
