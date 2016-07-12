type 'bin t

val empty : 'bin t

(** [make ~width ~height] return a packer ready to place stuff in a
    [width * height] area *)
val add_bin : 'bin -> int -> int -> 'bin t -> 'bin t

(** Input to packing is a box *)
type 'tag box =
  { tag    : 'tag
  ; width  : int
  ; height : int
  ; allow_rotation : bool (** can the box be rotated to optimize packing *)
  }
val box : ?allow_rotation:bool -> 'tag -> int -> int -> 'tag box

(** Output of packing is an optional rectangle *)
type ('bin, 'tag) rect =
  { x : int ; y : int ; w : int ; h : int
  ; rotated: bool (** True iff the input box was rotated.
                      If true, w = box.height && h = box.width
                      Otherwise, w = box.width && h = box.height
                  *)
  ; bin: 'bin
  ; box: 'tag box
  }

type heuristic =
  [ (** BSSF. Positions the rectangle against the short side of a free
        rectangle into which it fits the best. *)
    `Short_side_fit
  | (** BLSF: Positions the rectangle against the long side of a free rectangle
        into which it fits the best. *)
    `Long_side_fit
  | (** BAF: Positions the rectangle into the smallest free rect into which it
        fits. *)
    `Area_fit
  | (** BL: Does the Tetris placement. *)
    `Bottom_left
  ]

(** Online insertion of one item.
    Efficient but the packing is not very good.

    Worst-case: O(n^3) (n is total number of items inserted).  *)
val insert :  'bin t -> ?heuristic:heuristic -> 'tag box
           -> 'bin t * ('bin, 'tag) rect option

(** Online insertion of a batch of items.
    Runtime is roughly the cost of inserting each item independently, but order
    of insertion is chosen to give a better packing.

    Worst-case: O(n^4) (n is total number of items inserted). *)
val insert_batch :  'bin t -> ?heuristic:heuristic -> 'tag box list
                 -> 'bin t * ('bin, 'tag) rect option list

(** Offline insertion.
    Better packing but way too slow for real-time usage.

    Worst-case: O(n^5) (n is total number of items inserted). *)
val insert_global :  'bin t -> ?heuristic:heuristic -> 'tag box list
                  -> 'bin t * ('bin, 'tag) rect option list
