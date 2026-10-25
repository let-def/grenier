// Bit-twiddling primitives for js_of_ocaml (see bit_stubs.c), where OCaml
// integers are 32-bit.

//Provides: maxtrie_pop_count_tagged const
function maxtrie_pop_count_tagged(x) {
  x = x - ((x >>> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >>> 2) & 0x33333333);
  x = (x + (x >>> 4)) & 0x0f0f0f0f;
  return Math.imul(x, 0x01010101) >>> 24;
}

//Provides: maxtrie_msb_index_tagged const
function maxtrie_msb_index_tagged(x) {
  return 31 - Math.clz32(x);
}

//Provides: maxtrie_lsb_index_tagged const
function maxtrie_lsb_index_tagged(x) {
  return 31 - Math.clz32(x & -x);
}

//Provides: maxtrie_extract_msb_tagged const
function maxtrie_extract_msb_tagged(x) {
  return x === 0 ? 0 : (1 << (31 - Math.clz32(x))) | 0;
}

// There is no minor heap and no address order: keep the field of the first
// block, which is a consistent choice. Field [i] of a block is at index
// [i + 1] (index 0 holds the tag).
//Provides: maxtrie_share_field_tagged
function maxtrie_share_field_tagged(a, b, i) {
  return b[i + 1] = a[i + 1];
}
