(** An implementation of HyperLogLog probabilistic cardinality estimator. *)

(** Type of HyperLogLog counters *)
type t

(** Create a new counter with [error] error rate.
    [error] should verify [0.0 < error && error < 1.0].
    [0.05] is a reasonable default.

    Use [estimate_memory] to measure memory consumption and runtime of this
    function.
*)
val make : error:float -> t

(** [add t k] counts item [k] in [t].

    [k] should be "random": it should be the output of some cryptographic
    hashing algorithm like SHA.  It is not treated as an integer.
    This is key to getting proper results.
    No patterns should appear in the bits of the different items added.

    Runtime is O(1).
*)
val add   : t -> int64 -> unit

(** Estimate the memory consumed in bytes by a counter with the specified error
    rate.

    This ignores the constant overhead of the OCaml representation, around two
    words.  It is a [bytes] of [estimate_memory ~error + 1] length.
*)
val estimate_memory : error:float -> int

(* All remaining functions are O(estimate_memory ~error) *)

(** Get the cardinality estimation. Defaults to HyperLogLog++. *)
val card : t -> float

(* For benchmarking purpose, estimate cardinality with original HyperLogLog. *)
val card_hll : t -> float

(* Estimate cardinality with HyperLogLog++ (less biased). *)
val card_hllpp : t -> float

(** Get a copy of a counter. *)
val copy : t -> t

(** [merge ~into:t0 t'] has the same effect as adding all items added to
    [t'] to [t0].

    [t0] and [t'] must have been constructed with the same error rate!
*)
val merge : into:t -> t -> unit

(** Reset counter to 0. *)
val clear : t -> unit

(** The following algorithm provide a reasonable hashing function for integers,
    if you want to feed the HLL with "normal" integers.  *)
val hash_int64 : int64 -> int64

(** {0 Serialization} *)

(** Returns a string with the current state stored. *)
val to_string : t -> string

(** Restore a HLL saved with [to_string].

    [of_string (to_string t)] is functionnally equivalent to [copy t],
    except a bit more expensive.

    It can raise [Invalid_argument] if the string provided was not saved by
    [to_string].
*)
val of_string : string -> t
