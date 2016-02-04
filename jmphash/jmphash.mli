(** An implementation of "A Fast, Minimal Memory, Consistent Hash Algorithm" *)

(** [host ~hosts key] tells you on which host, between 0 and hosts-1, you should
    store value indexed by [key].
*)
val host : hosts:int -> int64 -> int

(*  Runtime is O(log n).
    The key property of the result for a given key is that, when [hosts] goes
    from [n] to [m], n < m, the probability of the result changing is (m-n)/m.

    If you are storing data on different servers:
    - this formula allows you to distribute data uniformly,
    - when adding or removing boxes, a minimum amoun of data has to be
      moved.

    This is completely stateless, you will have to keep track separately of the
    meaning associated to each host.

    The algorithm is described in this paper:
      A Fast, Minimal Memory, Consistent Hash Algorithm
      John Lamping and Eric Veach
      http://arxiv.org/abs/1406.2294
*)
