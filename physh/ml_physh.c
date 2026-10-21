#include <assert.h>
#include <stdint.h>

#define CAML_INTERNALS
#include <caml/version.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/gc.h>
#include <caml/gc_ctrl.h>
#if OCAML_VERSION >= 50200
#include <caml/shared_heap.h>
#endif

#if OCAML_VERSION < 50000
#define get_compactions() caml_stat_compactions
#elif OCAML_VERSION < 50200
// No compaction in OCaml 5.0 and 5.1
#define get_compactions() 0
#else
// caml_stat_compactions is defined to 0 in OCaml 5, but compaction is back
// since 5.2
// Relaxed: compactions are stop-the-world, the domain synchronizes with them.
#define get_compactions() \
  atomic_load_explicit(&caml_compactions_count, memory_order_relaxed)
#endif

/* Physical hashtables and hashsets.
 *
 * Objects are hashed by address. A table is split in two parts:
 * - a minor table for young keys, invalidated by minor collections,
 * - a major table for other keys, invalidated by compactions.
 * After a minor collection, the minor table is flushed into the major table.
 * After a compaction, the major table is rehashed.
 *
 * Sentinels
 *
 * EMPTY marks empty slots (and is returned by [find] for missing keys).
 * STALE is returned by fast paths when the tables need a sync.
 * They are static blocks outside of the OCaml heap: they are never young,
 * never move and are ignored by the GC. Writing them requires no barrier.
 *
 * GC-safety
 *
 * Any allocation can trigger a minor collection: even when the minor heap
 * has room, OCaml 5 runs pending GC requests (from another domain, or from
 * a ref table overflow) when allocating from C. Another domain can even
 * trigger a compaction.
 *
 * Fast paths ([@@noalloc]) never allocate: they report STALE (or failure)
 * when a sync or a resize is needed and the OCaml side then calls a slow
 * path. Slow paths re-check the tables after each allocation, and retry
 * until they can complete without allocating.
 *
 * The minor table is stale when its keys are no longer young (a minor
 * collection promotes all of them at once). One of its keys is kept as a
 * witness. The minor collection counter cannot be used: with multiple
 * domains, it is incremented before the other domains are stopped.
 *
 * The major table is stale when a compaction happened since it was last
 * hashed. An empty table is never stale.
 */

#ifdef Caml_out_of_heap_header
#define STATIC_HEADER(wosize) Caml_out_of_heap_header(wosize, 0)
#else
#define STATIC_HEADER(wosize) Make_header(wosize, 0, Caml_black)
#endif

static value sentinels[4] = {
  (value)STATIC_HEADER(1), Val_unit,
  (value)STATIC_HEADER(1), Val_unit,
};

#define EMPTY ((value)&sentinels[1])
#define STALE ((value)&sentinels[3])

// Layout of a table, must match the record defined in physh.ml.
// Young keys are stored in the minor table, other keys in the major table.
// The fill count of each table is stored in the field just before it.

#define T_WITNESS     0
#define T_MINOR_FILL  1
#define T_MINOR       2
#define T_COMPACTIONS 3
#define T_MAJOR_FILL  4
#define T_MAJOR       5

#define T_FILL(part) ((part) - 1)

// Tables have [width] words per entry: the key, then the value for maps.

#define SET_WIDTH 1
#define MAP_WIDTH 2

// Prior state of a key, must match [state] in physh.ml
enum { ABSENT = 0, PRESENT = 1, NEED_SYNC = 2 };

static int value_is_young(value obj)
{
  return (Is_block(obj) && Is_young(obj));
}

// C implementation of Hashtable

static unsigned log2_pow2 (size_t n)
{
#if defined(__GNUC__)
  return __builtin_ctzll(n);
#else
  unsigned r = 0;
  while (n >>= 1) ++r;
  return r;
#endif
}

static inline size_t table_slots (value table, const int width)
{
  size_t slots = Wosize_val(table) / width;
  /* number of slots must be a power of 2 */
  assert ((slots & (slots-1)) == 0);
  return slots;
}

// Fibonacci hashing: the low bits of an address carry little entropy
// (alignment, sequential allocation), so keep the high bits of the product.
static size_t table_hash (value v, size_t slots)
{
  unsigned bits = log2_pow2(slots);
  uint64_t h = (uint64_t)v * UINT64_C(0x9E3779B97F4A7C15);
  return (bits == 0 ? 0 : (size_t)(h >> (64 - bits)));
}

// Index of the slot holding [obj], or of the empty slot where it belongs.
// Table must not be full.
static inline size_t table_find (value table, value obj, const int width)
{
  size_t slots = table_slots (table, width);
  size_t mask = slots - 1;
  size_t index = table_hash (obj, slots);

  while (1)
  {
    value v = Field(table, index * width);
    if (v == EMPTY || v == obj)
      return index;
    index = (index + 1) & mask;
  }
}

// Max fill-factor is 3/4

static int has_capacity(size_t slots, size_t n)
{
  return (slots * 3 >= n * 4);
}

// Fill an empty slot.
//
// Empty slots hold EMPTY, which is not a heap value: overwriting it requires
// no darkening (deletion barrier). caml_initialize records young values in
// the ref table when the table is not young: this is what caml_modify would
// do, without the barrier and the fence.
static void initialize_word (value table, size_t i, value v)
{
  assert (Field(table, i) == EMPTY);
  // caml_initialize expects a field without pointer
  Field(table, i) = Val_unit;
  caml_initialize(&Field(table, i), v);
}

static inline void fill_slot (value table, size_t index, value key, value val,
                              const int width)
{
  initialize_word(table, index * width, key);
  if (width == MAP_WIDTH)
    initialize_word(table, index * width + 1, val);
}

// Insert all entries of [src] in [dst], which must have room for them.
// Returns the number of entries. Does not allocate.
static inline size_t move_entries (value src, value dst, const int width)
{
  size_t sz = table_slots(src, width), n = 0, i;

  for (i = 0; i < sz; ++i)
  {
    value k = Field(src, i * width);
    if (k == EMPTY) continue;

    value v = (width == MAP_WIDTH ? Field(src, i * width + 1) : Val_unit);
    fill_slot(dst, table_find(dst, k, width), k, v, width);
    ++n;
  }

  return n;
}

// Allocate a table of [wosize] words filled with EMPTY.
// Can trigger a GC.
static value alloc_table (mlsize_t wosize)
{
  value v;
  mlsize_t i;

  if (wosize <= Max_young_wosize)
    v = caml_alloc_small(wosize, 0);
  else
    v = caml_alloc_shr(wosize, 0);

  for (i = 0; i < wosize; ++i)
    Field(v, i) = EMPTY;

  return v;
}

// Accessors

// The table where [key] belongs
static int key_part(value key)
{
  return (value_is_young(key) ? T_MINOR : T_MAJOR);
}

static size_t t_fill(value t, int part)
{
  return Long_val(Field(t, T_FILL(part)));
}

static void t_set_fill(value t, int part, size_t n)
{
  Field(t, T_FILL(part)) = Val_long(n);
}

static int t_minor_is_valid(value t)
{
  return (t_fill(t, T_MINOR) == 0 || value_is_young(Field(t, T_WITNESS)));
}

static int t_major_is_valid(value t)
{
  return (t_fill(t, T_MAJOR) == 0 ||
          Long_val(Field(t, T_COMPACTIONS)) == (intnat)get_compactions());
}

static void t_major_set_valid(value t)
{
  Field(t, T_COMPACTIONS) = Val_long(get_compactions());
}

static int tables_valid (value t)
{
  return (t_minor_is_valid(t) && t_major_is_valid(t));
}

// Low-level maintenance functions

// Move the (promoted) content of the minor table to the major table.
// Does not allocate.
static inline void flush_minor (value t, const int width)
{
  value major = Field(t, T_MAJOR);

  /* preconditions */
  assert (!t_minor_is_valid(t));
  assert (t_major_is_valid(t));
  assert (has_capacity(table_slots(major, width),
                       t_fill(t, T_MAJOR) + t_fill(t, T_MINOR) + 1));

  size_t n = move_entries(Field(t, T_MINOR), major, width);

  assert (n == t_fill(t, T_MINOR));
  t_set_fill(t, T_MAJOR, t_fill(t, T_MAJOR) + n);
  t_major_set_valid(t);

  // Drop the minor table rather than clearing it: it has been promoted, so
  // clearing would require write barriers. Tables are cheap to regrow.
  t_set_fill(t, T_MINOR, 0);
  Store_field(t, T_MINOR, Atom(0));
  Store_field(t, T_WITNESS, Val_unit);

  /* post conditions */
  assert (t_minor_is_valid(t));
}

// Double the size of the minor table.
// The minor table can outgrow the minor heap: it is then allocated in the
// major heap, and stores of young keys go through the ref table.
// Might trigger a GC; if it does, the minor table is left unchanged (and
// stale).
static inline void grow_minor (value t, const int width)
{
  CAMLparam1(t);
  CAMLlocal1(minor_new);

  size_t sz = table_slots(Field(t, T_MINOR), width);

  minor_new = alloc_table((sz < 2 ? 2 : sz * 2) * width);

  // A GC happened during allocation, the keys of the minor table are no
  // longer young: drop the new table.
  if (!t_minor_is_valid(t))
    CAMLreturn0;

  size_t n = move_entries(Field(t, T_MINOR), minor_new, width);
  assert (n == t_fill(t, T_MINOR));
  (void)n;
  Store_field(t, T_MINOR, minor_new);

  CAMLreturn0;
}

// Rehash the major table into a new table with [new_sz] slots.
// Might trigger a GC. Addresses are read after allocating, so the major
// table is valid when this returns, but the minor table might not be.
static inline void rehash_major (value t, size_t new_sz, const int width)
{
  CAMLparam1(t);
  CAMLlocal1(major_new);

  major_new = alloc_table(new_sz * width);

  size_t n = move_entries(Field(t, T_MAJOR), major_new, width);
  assert (n == t_fill(t, T_MAJOR));
  (void)n;
  Store_field(t, T_MAJOR, major_new);
  t_major_set_valid(t);

  CAMLreturn0;
}

// Grow the major table to hold [target] entries. Might trigger a GC.
static inline void realloc_major (value t, size_t target, const int width)
{
  size_t size = table_slots(Field(t, T_MAJOR), width);

  if (size < 2)
    size = 2;

  while (!has_capacity(size, target))
    size = size * 2;

  rehash_major (t, size, width);
}

// Bring both tables up-to-date with the GC.
// Allocation (and hence GC) can only happen before the final check.
static inline void table_sync (value t, const int width)
{
  CAMLparam1(t);

  while (!tables_valid(t))
  {
    // Entries of a stale minor table move to the major table
    size_t pending = t_minor_is_valid(t) ? 0 : t_fill(t, T_MINOR);
    size_t needed = t_fill(t, T_MAJOR) + pending + 1;
    size_t major_slots = table_slots(Field(t, T_MAJOR), width);

    if (!has_capacity(major_slots, needed))
      realloc_major(t, needed, width);
    else if (!t_major_is_valid(t))
      rehash_major(t, major_slots, width);
    else
      flush_minor(t, width);
  }

  CAMLreturn0;
}

// Generic operations, on valid tables

// Index of [key] in the table [part], or -1 if absent
static inline intnat lookup (value t, int part, value key, const int width)
{
  // An empty table might have no slot at all
  if (t_fill(t, part) == 0)
    return -1;

  value table = Field(t, part);
  size_t index = table_find(table, key, width);
  return (Field(table, index * width) == key ? (intnat)index : -1);
}

// Add a binding without allocating.
// Returns:
// - EMPTY if [key] was absent and has been added,
// - STALE if the table has to grow first (nothing changed),
// - otherwise [key] was present: for a map, its previous value (replaced by
//   [val] if [overwrite]), for a set, [key].
static inline value try_add (value t, value key, value val, int overwrite,
                             const int width)
{
  int part = key_part(key);
  value table = Field(t, part);
  size_t fill = t_fill(t, part);
  intnat found = lookup(t, part, key, width);

  if (found >= 0)
  {
    if (width == SET_WIDTH)
      return key;
    value prev = Field(table, found * width + 1);
    if (overwrite)
      Store_field(table, found * width + 1, val);
    return prev;
  }

  if (!has_capacity(table_slots(table, width), fill + 1))
    return STALE;

  fill_slot(table, table_find(table, key, width), key, val, width);
  t_set_fill(t, part, fill + 1);

  // An empty table is always valid, record what it is now valid for
  if (fill == 0)
  {
    if (part == T_MINOR)
      Store_field(t, T_WITNESS, key);
    else
      t_major_set_valid(t);
  }

  return EMPTY;
}

// Empty the slot at [i] by backward-shift deletion: entries that follow in
// the same cluster move back when their probe sequence goes through [i].
// No tombstones, so lookups stop at the first EMPTY slot as usual.
// Overwritten slots hold heap values: use Store_field for the deletion
// barrier. Does not allocate.
static inline void remove_at (value table, size_t i, const int width)
{
  size_t slots = table_slots(table, width), mask = slots - 1, j = i;

  while (1)
  {
    j = (j + 1) & mask;
    value k = Field(table, j * width);
    if (k == EMPTY)
      break;

    // [k] can move to [i] if [i] lies on its probe sequence, in [home, j)
    size_t home = table_hash(k, slots);
    if (((i - home) & mask) < ((j - home) & mask))
    {
      Store_field(table, i * width, k);
      if (width == MAP_WIDTH)
        Store_field(table, i * width + 1, Field(table, j * width + 1));
      i = j;
    }
  }

  Store_field(table, i * width, EMPTY);
  if (width == MAP_WIDTH)
    Store_field(table, i * width + 1, EMPTY);
}

// Remove [key] from a valid table. Does not allocate.
// Returns EMPTY if [key] was absent, otherwise its value for a map, [key]
// for a set.
static inline value try_remove (value t, value key, const int width)
{
  int part = key_part(key);
  intnat found = lookup(t, part, key, width);

  if (found < 0)
    return EMPTY;

  value table = Field(t, part);
  value prev = (width == MAP_WIDTH ? Field(table, found * width + 1) : key);
  size_t fill = t_fill(t, part) - 1;

  remove_at(table, found, width);
  t_set_fill(t, part, fill);

  // The witness can outlive its entry: it stays alive (and young) until the
  // next minor collection, like the other keys. Release it with the last key.
  if (part == T_MINOR && fill == 0)
    Store_field(t, T_WITNESS, Val_unit);

  return prev;
}

// Same as [try_add], but never returns STALE: syncs and grows the tables as
// needed. Can trigger a GC.
static inline value add_slow (value t, value key, value val, int overwrite,
                              const int width)
{
  CAMLparam3(t, key, val);
  value result;

  while (1)
  {
    table_sync(t, width);
    result = try_add(t, key, val, overwrite, width);
    if (result != STALE)
      break;

    // Make room, this might trigger a GC so start over
    if (key_part(key) == T_MINOR)
      grow_minor(t, width);
    else
      realloc_major(t, t_fill(t, T_MAJOR) + 1, width);
  }

  // No allocation since [try_add]: [result] is still valid
  CAMLreturn(result);
}

// Sentinels

value ml_physh_empty (value unit)
{
  (void)unit;
  return EMPTY;
}

value ml_physh_stale (value unit)
{
  (void)unit;
  return STALE;
}

// Set stubs

value ml_physh_set_sync (value t)
{
  table_sync(t, SET_WIDTH);
  return Val_unit;
}

value ml_physh_set_mem_fast (value t, value obj)
{
  if (!tables_valid(t))
    return Val_int(NEED_SYNC);
  return Val_int(lookup(t, key_part(obj), obj, SET_WIDTH) >= 0 ? PRESENT : ABSENT);
}

// Add [obj], returns its prior state (NEED_SYNC: nothing done)
value ml_physh_set_add_fast (value t, value obj)
{
  if (!tables_valid(t))
    return Val_int(NEED_SYNC);

  value result = try_add(t, obj, Val_unit, 0, SET_WIDTH);
  return Val_int(result == STALE ? NEED_SYNC : result == EMPTY ? ABSENT : PRESENT);
}

// Add [obj], returns true if it was already present
value ml_physh_set_add_slow (value t, value obj)
{
  return Val_bool(add_slow(t, obj, Val_unit, 0, SET_WIDTH) != EMPTY);
}

// Remove [obj], returns its prior state (NEED_SYNC: nothing done)
value ml_physh_set_remove_fast (value t, value obj)
{
  if (!tables_valid(t))
    return Val_int(NEED_SYNC);
  return Val_int(try_remove(t, obj, SET_WIDTH) == EMPTY ? ABSENT : PRESENT);
}

// Map stubs

value ml_physh_map_sync (value t)
{
  table_sync(t, MAP_WIDTH);
  return Val_unit;
}

// Returns the value bound to [key], EMPTY if absent, STALE if a sync is
// needed.
value ml_physh_map_find_fast (value t, value key)
{
  if (!tables_valid(t))
    return STALE;

  int part = key_part(key);
  intnat index = lookup(t, part, key, MAP_WIDTH);
  return (index < 0 ? EMPTY : Field(Field(t, part), index * MAP_WIDTH + 1));
}

// Bind [key] to [val] if absent, or if [overwrite].
// Returns the previous value, EMPTY if [key] was absent, STALE if nothing was
// done (use add_slow).
value ml_physh_map_add_fast (value t, value key, value val, value overwrite)
{
  if (!tables_valid(t))
    return STALE;
  return try_add(t, key, val, Bool_val(overwrite), MAP_WIDTH);
}

// Same as add_fast, never returns STALE
value ml_physh_map_add_slow (value t, value key, value val, value overwrite)
{
  return add_slow(t, key, val, Bool_val(overwrite), MAP_WIDTH);
}

// Unbind [key]. Returns the previous value, EMPTY if [key] was absent, STALE
// if a sync is needed (nothing done).
value ml_physh_map_remove_fast (value t, value key)
{
  if (!tables_valid(t))
    return STALE;
  return try_remove(t, key, MAP_WIDTH);
}
