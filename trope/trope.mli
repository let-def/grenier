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
type cursor

(** Is a cursor member of a buffer ? *)
val member  : 'a t -> cursor -> bool

(** Find value associated to a cursor, or raise [Not_found] *)
val find  : 'a t -> cursor -> 'a

(** Compare the position of two cursors *)
val compare : cursor -> cursor -> int

(** Get the physical position of a cursor in a revision of a buffer *)
val position : 'a t -> cursor -> int

(** {2 Creation and removal of cursors} *)

(** Create a new cursors at position [at]
    Valid iff [at >= 0].
*)
val put_cursor : 'a t -> at:int -> 'a -> 'a t * cursor

(** Insert or update a cursor.
    Cursor is inserted at the left-most valid position.
*)
val put_left : 'a t -> cursor -> 'a -> 'a t

(** Insert or update a cursor.
    Cursor is inserted at the right-most valid position before buffer end.
*)
val put_right : 'a t -> cursor -> 'a -> 'a t

(** [rem_cursor t c] removes a cursor from the buffer.
    Valid iff [member t c]. *)
val rem_cursor : 'a t -> cursor -> 'a t

(** [cursor_after c] creates a cursor that is immediately after [c].  *)
val cursor_after : cursor -> cursor

(** [cursor_before c] creates a cursor that is immediately before [c]. *)
val cursor_before : cursor -> cursor

(** [cursor_at_origin t] creates a cursor that is minimal for [t] (before all
    other cursors in [t]) *)
val cursor_at_origin : 'a t -> cursor


(** {2 Modification of buffers} *)

(** Remove anything between two cursors.
    [remove_between t a b] is valid iff
      [member t a && member t b && compare a b <= 0]
*)
val remove_between : 'a t -> cursor -> cursor -> 'a t

(** [remove_before t c len] removes [len] units before [c].
    Valid iff [member t c && len >= 0].
*)
val remove_before  : 'a t -> cursor -> int -> 'a t

(** [remove_after t c len] removes [len] units after [c].
    Valid iff [member t c && len >= 0].
*)
val remove_after   : 'a t -> cursor -> int -> 'a t

(** [insert_before t c len] inserts [len] units before [c].
    Valid iff [member t c && len >= 0].
*)
val insert_before  : 'a t -> cursor -> int -> 'a t

(** [insert_before t c len] inserts [len] units after [c].
    Valid iff [member t c && len >= 0].
*)
val insert_after   : 'a t -> cursor -> int -> 'a t


(** {2 Looking up cursors in the buffer} *)

(** [find_before t at]
    finds the last cursor [c] in [t] satisfying [position t c <= at] *)
val find_before    : 'a t -> int -> (cursor * 'a) option

(** [find_after t at]
    finds the first cursor [c] in [t] satisfying [position t c >= at] *)
val find_after     : 'a t -> int -> (cursor * 'a) option

(** [seek_before t c]
    finds the last cursor [c'] in [t] satisfying [compare c' c < 0] *)
val seek_before  : 'a t -> cursor -> (cursor * 'a) option

(** [seek_after t c]
    finds the first cursor [c'] in [t] satisfying [compare c' c > 0] *)
val seek_after   : 'a t -> cursor -> (cursor * 'a) option

val to_list : 'a t -> (int * cursor * 'a) list
