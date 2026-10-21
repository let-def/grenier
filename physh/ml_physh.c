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
// The tables have [width] words per entry: the key, then the value for maps.

#define T_WITNESS     0
#define T_MINOR_FILL  1
#define T_MINOR       2
#define T_COMPACTIONS 3
#define T_MAJOR_FILL  4
#define T_MAJOR       5

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

// Table must not be full
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

// Accessors

static size_t t_minor_fill(value t)
{
  return Long_val(Field(t, T_MINOR_FILL));
}

static size_t t_major_fill(value t)
{
  return Long_val(Field(t, T_MAJOR_FILL));
}

static int t_minor_is_valid(value t)
{
  return (t_minor_fill(t) == 0 || value_is_young(Field(t, T_WITNESS)));
}

static int t_major_is_valid(value t)
{
  return (t_major_fill(t) == 0 ||
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

// Stores in fresh tables
//
// Empty slots hold EMPTY, which is not a heap value: overwriting it requires
// no darkening (deletion barrier). Old values need no ref table entry
// either, so they can be written directly. Possibly young values go through
// caml_initialize.

static void store_old_over_empty (value table, size_t i, value v)
{
  assert (Field(table, i) == EMPTY);
  assert (!value_is_young(v));
  Field(table, i) = v;
}

static void initialize_over_empty (value table, size_t i, value v)
{
  assert (Field(table, i) == EMPTY);
  // caml_initialize expects a field without pointer
  Field(table, i) = Val_unit;
  caml_initialize(&Field(table, i), v);
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

// Low-level maintenance functions

// Move the (promoted) content of the minor table to the major table.
// Does not allocate.
static inline void flush_minor (value t, const int width)
{
  value minor = Field(t, T_MINOR);
  value major = Field(t, T_MAJOR);
  size_t sz = table_slots(minor, width), fill = 0, i;

  /* preconditions */
  assert (!t_minor_is_valid(t));
  assert (t_major_is_valid(t));
  assert (has_capacity(table_slots(major, width),
                       t_major_fill(t) + t_minor_fill(t) + 1));

  for (i = 0; i < sz; ++i)
  {
    value k = Field(minor, i * width);
    if (k == EMPTY) continue;

    // A minor collection happened: keys and values are no longer young
    size_t index = table_find(major, k, width);
    store_old_over_empty(major, index * width, k);
    if (width == 2)
      store_old_over_empty(major, index * width + 1, Field(minor, i * width + 1));
    ++fill;
  }

  assert (fill == t_minor_fill(t));
  Field(t, T_MAJOR_FILL) = Val_long(t_major_fill(t) + fill);
  t_major_set_valid(t);

  // Drop the minor table rather than clearing it: it has been promoted, so
  // clearing would require write barriers. Tables are cheap to regrow.
  Field(t, T_MINOR_FILL) = Val_long(0);
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
  CAMLlocal2(minor_old, minor_new);

  size_t sz = table_slots(Field(t, T_MINOR), width);
  size_t new_sz = (sz < 2 ? 2 : sz * 2), fill = 0, i;

  minor_new = alloc_table(new_sz * width);

  // A GC happened during allocation, the keys of the minor table are no
  // longer young: drop the new table.
  if (!t_minor_is_valid(t))
    CAMLreturn0;

  minor_old = Field(t, T_MINOR);

  for (i = 0; i < sz; ++i)
  {
    value k = Field(minor_old, i * width);
    if (k == EMPTY) continue;

    size_t index = table_find(minor_new, k, width);
    initialize_over_empty(minor_new, index * width, k);
    if (width == 2)
      initialize_over_empty(minor_new, index * width + 1,
                            Field(minor_old, i * width + 1));
    ++fill;
  }

  assert (fill == t_minor_fill(t));
  Store_field(t, T_MINOR, minor_new);

  CAMLreturn0;
}

// Rehash the major table into a new table with [new_sz] slots.
// Might trigger a GC. Addresses are read after allocating, so the major
// table is valid when this returns, but the minor table might not be.
static inline void rehash_major (value t, size_t new_sz, const int width)
{
  CAMLparam1(t);
  CAMLlocal2(major_old, major_new);

  major_new = alloc_table(new_sz * width);
  major_old = Field(t, T_MAJOR);

  size_t sz = table_slots(major_old, width), fill = 0, i;

  for (i = 0; i < sz; ++i)
  {
    value k = Field(major_old, i * width);
    if (k == EMPTY) continue;

    size_t index = table_find(major_new, k, width);
    store_old_over_empty(major_new, index * width, k);
    // Values can be young
    if (width == 2)
      initialize_over_empty(major_new, index * width + 1,
                            Field(major_old, i * width + 1));
    ++fill;
  }

  assert (fill == t_major_fill(t));
  Store_field(t, T_MAJOR, major_new);
  t_major_set_valid(t);

  CAMLreturn0;
}

// Grow the major table to hold its content and the content of the minor
// table. Might trigger a GC.
static inline void realloc_major (value t, const int width)
{
  size_t size = table_slots(Field(t, T_MAJOR), width);
  size_t target = t_major_fill(t) + t_minor_fill(t) + 1;

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
    size_t pending = t_minor_is_valid(t) ? 0 : t_minor_fill(t);
    size_t major_slots = table_slots(Field(t, T_MAJOR), width);

    if (!has_capacity(major_slots, t_major_fill(t) + pending + 1))
      realloc_major(t, width);
    else if (!t_major_is_valid(t))
      rehash_major(t, major_slots, width);
    else
      flush_minor(t, width);
  }

  CAMLreturn0;
}

// Generic operations

// Lookup, returns the slot index of [key] or -1
static inline intnat lookup (value t, value key, const int width)
{
  value table;
  size_t fill;

  if (value_is_young(key))
  {
    table = Field(t, T_MINOR);
    fill = t_minor_fill(t);
  }
  else
  {
    table = Field(t, T_MAJOR);
    fill = t_major_fill(t);
  }

  if (fill == 0)
    return -1;

  size_t index = table_find(table, key, width);
  if (Field(table, index * width) != key)
    return -1;

  return index;
}

// Add or update a binding without allocating.
// Requires valid tables, returns 0 if the table has to grow first.
static inline int try_add (value t, value key, value val, const int width)
{
  int young = value_is_young(key);
  value table = Field(t, young ? T_MINOR : T_MAJOR);
  size_t fill = young ? t_minor_fill(t) : t_major_fill(t);
  size_t index;

  // Key already present: just update the value
  if (fill > 0)
  {
    index = table_find(table, key, width);
    if (Field(table, index * width) == key)
    {
      if (width == 2)
        Store_field(table, index * width + 1, val);
      return 1;
    }
  }

  if (!has_capacity(table_slots(table, width), fill + 1))
    return 0;

  index = table_find(table, key, width);

  if (young)
  {
    Store_field(table, index * width, key);
    if (fill == 0)
      Store_field(t, T_WITNESS, key);
    Field(t, T_MINOR_FILL) = Val_long(fill + 1);
  }
  else
  {
    store_old_over_empty(table, index * width, key);
    // The table is valid: either already hashed for the current heap
    // layout, or empty.
    if (fill == 0)
      t_major_set_valid(t);
    Field(t, T_MAJOR_FILL) = Val_long(fill + 1);
  }

  if (width == 2)
    Store_field(table, index * width + 1, val);

  return 1;
}

static inline void add_slow (value t, value key, value val, const int width)
{
  CAMLparam3(t, key, val);

  while (1)
  {
    table_sync(t, width);
    if (try_add(t, key, val, width))
      break;

    // Make room, this might trigger a GC so start over
    if (value_is_young(key))
      grow_minor(t, width);
    else
      realloc_major(t, width);
  }

  CAMLreturn0;
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
  table_sync(t, 1);
  return Val_unit;
}

// 0: absent, 1: present, 2: stale
value ml_physh_set_mem_fast (value t, value obj)
{
  if (!tables_valid(t))
    return Val_int(2);
  return Val_bool(lookup(t, obj, 1) >= 0);
}

value ml_physh_set_add_fast (value t, value obj)
{
  return Val_bool(tables_valid(t) && try_add(t, obj, Val_unit, 1));
}

value ml_physh_set_add_slow (value t, value obj)
{
  add_slow(t, obj, Val_unit, 1);
  return Val_unit;
}

// Map stubs

value ml_physh_map_sync (value t)
{
  table_sync(t, 2);
  return Val_unit;
}

// Returns the value bound to [key], EMPTY if absent, STALE if a sync is
// needed.
value ml_physh_map_find_fast (value t, value key)
{
  if (!tables_valid(t))
    return STALE;

  intnat index = lookup(t, key, 2);
  if (index < 0)
    return EMPTY;

  value table = Field(t, value_is_young(key) ? T_MINOR : T_MAJOR);
  return Field(table, index * 2 + 1);
}

value ml_physh_map_add_fast (value t, value key, value val)
{
  return Val_bool(tables_valid(t) && try_add(t, key, val, 2));
}

value ml_physh_map_add_slow (value t, value key, value val)
{
  add_slow(t, key, val, 2);
  return Val_unit;
}
