/* Bit-twiddling primitives on OCaml integers.

   Each primitive has an unboxed/untagged native version (used by ocamlopt,
   declared [@@noalloc]) and a tagged version (used by the bytecode
   interpreter). Untagged arguments are the 63-bit (or 31-bit) OCaml integer
   sign-extended to a machine word. */

#include <stdint.h>
#include <caml/mlvalues.h>

#if UINTPTR_MAX == 0xFFFFFFFFFFFFFFFFu
#define CLZ(x) __builtin_clzll(x)
#define CTZ(x) __builtin_ctzll(x)
#define POPCOUNT(x) __builtin_popcountll(x)
#define WORD_BITS 64
#else
#define CLZ(x) __builtin_clz(x)
#define CTZ(x) __builtin_ctz(x)
#define POPCOUNT(x) __builtin_popcount(x)
#define WORD_BITS 32
#endif

/* Number of bits set among the [WORD_BITS - 1] bits of an OCaml integer. */
intnat maxtrie_pop_count(intnat x)
{
  return POPCOUNT((uintptr_t)x << 1);
}

value maxtrie_pop_count_tagged(value x)
{
  return Val_long(maxtrie_pop_count(Long_val(x)));
}

/* Index of the most significant set bit; [x] must be non-zero. */
intnat maxtrie_msb_index(intnat x)
{
  return (WORD_BITS - 2) - CLZ((uintptr_t)x << 1);
}

value maxtrie_msb_index_tagged(value x)
{
  return Val_long(maxtrie_msb_index(Long_val(x)));
}

/* Index of the least significant set bit; [x] must be non-zero. */
intnat maxtrie_lsb_index(intnat x)
{
  return CTZ((uintptr_t)x);
}

value maxtrie_lsb_index_tagged(value x)
{
  return Val_long(maxtrie_lsb_index(Long_val(x)));
}

/* Isolate the most significant set bit of a non-negative integer
   (0 is mapped to 0). */
intnat maxtrie_extract_msb(intnat x)
{
  uintptr_t u = ((uintptr_t)x << 1) >> 1;
  return u == 0 ? 0 : (intnat)((uintptr_t)1 << (WORD_BITS - 1 - CLZ(u)));
}

value maxtrie_extract_msb_tagged(value x)
{
  return Val_long(maxtrie_extract_msb(Long_val(x)));
}

/* Sharing: given two parents whose fields [i] are structurally equal,
   physically distinct values, make both fields point to the same one, and
   return it.
   A value outside the minor heap (major heap, static data, or an immediate)
   is preferred, so that unification does not create pointers from the major
   heap to the minor heap and the minor copy can die young. Between two values
   of the same kind, the one at the higher address is kept, so that the choice
   is consistent. The minor heap is filled downwards, so this keeps the older
   copy, which other values are more likely to reference already; in the major
   heap, older blocks also tend to be at higher addresses. Each value is
   mapped to the key (young, -address), compared lexicographically, by putting
   the young flag in the top bit of the complemented address shifted right by
   one; the lowest key is kept.

   The write goes through [caml_modify], which neither allocates nor runs the
   GC (the native compiler itself calls it as a noalloc external), so this
   primitive can be [@@noalloc]: unifying costs a single call into C. */
#include <caml/memory.h>
#include <caml/address_class.h>
#include <caml/version.h>

#if OCAML_VERSION_MAJOR >= 5
#define MAXTRIE_YOUNG_START caml_minor_heaps_start
#define MAXTRIE_YOUNG_END caml_minor_heaps_end
#elif OCAML_VERSION_MINOR >= 10
#define MAXTRIE_YOUNG_START Caml_state_field(young_start)
#define MAXTRIE_YOUNG_END Caml_state_field(young_end)
#else
CAMLextern value *caml_young_start, *caml_young_end;
#define MAXTRIE_YOUNG_START caml_young_start
#define MAXTRIE_YOUNG_END caml_young_end
#endif

/* 1 if [v] is a block in the minor heaps, 0 otherwise. The block test is
   needed because an immediate integer may fall in the address range. */
static inline uintnat maxtrie_young_bit(value v)
{
  uintnat start = (uintnat)MAXTRIE_YOUNG_START;
  uintnat end = (uintnat)MAXTRIE_YOUNG_END;
  uintnat in_range = ((uintnat)v - start - 1) < (end - start - 1);
  return in_range & ~(uintnat)v & 1;
}

static inline uintnat maxtrie_share_key(value v)
{
  return (maxtrie_young_bit(v) << (WORD_BITS - 1)) | (~(uintnat)v >> 1);
}

value maxtrie_share_field(value a, value b, intnat i)
{
  value x = Field(a, i), y = Field(b, i);
  if (maxtrie_share_key(x) <= maxtrie_share_key(y)) {
    caml_modify(&Field(b, i), x);
    return x;
  } else {
    caml_modify(&Field(a, i), y);
    return y;
  }
}

value maxtrie_share_field_tagged(value a, value b, value i)
{
  return maxtrie_share_field(a, b, Long_val(i));
}

value maxtrie_share_fields(value a, intnat i, value b, intnat j)
{
  value x = Field(a, i), y = Field(b, j);
  if (maxtrie_share_key(x) <= maxtrie_share_key(y)) {
    caml_modify(&Field(b, j), x);
    return x;
  } else {
    caml_modify(&Field(a, i), y);
    return y;
  }
}

value maxtrie_share_fields_tagged(value a, value i, value b, value j)
{
  return maxtrie_share_fields(a, Long_val(i), b, Long_val(j));
}
