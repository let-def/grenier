(* Bit-twiddling primitives, implemented in C (see bit_stubs.c). *)

external pop_count : (int [@untagged]) -> (int [@untagged]) =
  "maxtrie_pop_count_tagged" "maxtrie_pop_count" [@@noalloc]

external msb_index : (int [@untagged]) -> (int [@untagged]) =
  "maxtrie_msb_index_tagged" "maxtrie_msb_index" [@@noalloc]

external lsb_index : (int [@untagged]) -> (int [@untagged]) =
  "maxtrie_lsb_index_tagged" "maxtrie_lsb_index" [@@noalloc]

external extract_msb : (int [@untagged]) -> (int [@untagged]) =
  "maxtrie_extract_msb_tagged" "maxtrie_extract_msb" [@@noalloc]

let[@inline] extract_lsb x = x land (-x)

(* [lt_msb x y] holds iff the most significant bit of [x] is strictly lower
   than the most significant bit of [y]. Both must be non-negative. *)
let[@inline] lt_msb x y = x < y && x < x lxor y

(* [share_field a b i] unifies the fields [i] of the blocks [a] and [b],
   which must be structurally equal but physically distinct: the field of one
   block is replaced with the field of the other, which is returned. A value
   outside the minor heap is kept if there is one, otherwise the one at the
   higher address. *)
external share_field : Obj.t -> Obj.t -> (int [@untagged]) -> Obj.t =
  "maxtrie_share_field_tagged" "maxtrie_share_field" [@@noalloc]

(* [share_fields a i b j] is [share_field], unifying the field [i] of [a]
   with the field [j] of [b] (for PatSetCompact, where the index of a child
   depends on the constructor). *)
external share_fields :
  Obj.t -> (int [@untagged]) -> Obj.t -> (int [@untagged]) -> Obj.t =
  "maxtrie_share_fields_tagged" "maxtrie_share_fields" [@@noalloc]
