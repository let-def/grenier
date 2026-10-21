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

(* [share_choose a b] chooses which of two equal values to keep when unifying
   them: a value outside the minor heap if there is one, otherwise the one at
   the higher address. *)
external share_choose : 'a -> 'a -> 'a = "maxtrie_share_choose" [@@noalloc]
