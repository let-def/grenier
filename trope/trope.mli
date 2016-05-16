(** {1 Buffer management} *)

(** Type of semi-persistent buffers *)
type 'a t

(** Create a new lineage of buffers *)
val create   : unit -> 'a t

(** Clear contents of a buffer *)
val clear    : 'a t -> 'a t

(** true iff is buffer empty ? *)
val is_empty : 'a t -> bool

(** Shift contents of the buffer by removing [len] units starting at [at].
    Valid iff [at >= 0 && len >= 0].
    Cursors exactly at position [at] are removed iff called with [~left_of:()].
*)
val remove   : ?left_of:unit -> 'a t -> at:int -> len:int -> 'a t

(** Shift contents of the buffer by inserting [len] units starting at [at].
    Valid iff [at >= 0 && len >= 0].
    Cursors exactly at position [at] are shifted iff called with [~left_of:()].
*)
val insert   : ?left_of:unit -> 'a t -> at:int -> len:int -> 'a t


(** {1 Cursor management} *)

(** Type of cursors *)
type 'a cursor

(** Is a cursor member of a buffer ? *)
val member  : 'a t -> 'a cursor -> bool

(** Compare the position of two cursors *)
val compare : 'a cursor -> 'a cursor -> int

(** Get user data associated with a cursor *)
val content : 'a cursor -> 'a

(** Get the physical position of a cursor in a revision of a buffer *)
val position : 'a t -> 'a cursor -> int

(** {2 Creation and removal of cursors} *)

(** Create a new cursors at position [at]
    Valid iff [at >= 0].
*)
val put_cursor : 'a t -> at:int -> 'a -> 'a t * 'a cursor

(** Create a new cursor immediately before an existing one, at the same
    physical position but logically preceding it by an infinitesimal amount.
    If you insert after the new cursor or before the original one, they will be
    separated.
    [put_before t c x] is valid iff [member t c].
*)
val put_before : 'a t -> 'a cursor -> 'a -> 'a t * 'a cursor

(** Create a new cursor immediately after an existing one, at the same
    physical position but logically following it by an infinitesimal amount.
    If you insert before the new cursor or after the original one, they will be
    separated.
    [put_after t c x] is valid iff [member t c].
*)
val put_after : 'a t -> 'a cursor -> 'a -> 'a t * 'a cursor

(** [rem_cursor t c] removes a cursor from the buffer.
    Valid iff [member t c]. *)
val rem_cursor : 'a t -> 'a cursor -> 'a t


(** {2 Modification of buffers} *)

(** Remove anything between two cursors.
    [remove_between t a b] is valid iff
      [member t a && member t b && compare a b <= 0]
*)
val remove_between : 'a t -> 'a cursor -> 'a cursor -> 'a t

(** [remove_before t c len] removes [len] units before [c].
    Valid iff [member t c && len >= 0].
*)
val remove_before  : 'a t -> 'a cursor -> int -> 'a t

(** [remove_after t c len] removes [len] units after [c].
    Valid iff [member t c && len >= 0].
*)
val remove_after   : 'a t -> 'a cursor -> int -> 'a t

(** [insert_before t c len] inserts [len] units before [c].
    Valid iff [member t c && len >= 0].
*)
val insert_before  : 'a t -> 'a cursor -> int -> 'a t

(** [insert_before t c len] inserts [len] units after [c].
    Valid iff [member t c && len >= 0].
*)
val insert_after   : 'a t -> 'a cursor -> int -> 'a t


(** {2 Looking up cursors in the buffer} *)

(** [find_before t at]
    finds the last cursor [c] in [t] satisfying [position t c <= at] *)
val find_before    : 'a t -> int -> 'a cursor option

(** [find_after t at]
    finds the first cursor [c] in [t] satisfying [position t c >= at] *)
val find_after     : 'a t -> int -> 'a cursor option

(** [cursor_before t c]
    finds the last cursor [c'] in [t] satisfying [compare c' c < 0] *)
val cursor_before  : 'a t -> 'a cursor -> 'a cursor option

(** [cursor_after t c]
    finds the first cursor [c'] in [t] satisfying [compare c' c > 0] *)
val cursor_after   : 'a t -> 'a cursor -> 'a cursor option

val to_list : 'a t -> (int * 'a cursor) list
