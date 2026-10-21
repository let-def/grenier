#include <assert.h>
#include <stdint.h>

#define CAML_INTERNALS
#include <caml/version.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/gc_ctrl.h>
#if OCAML_VERSION >= 50200
#include <caml/shared_heap.h>
#endif

#if OCAML_VERSION < 41000
CAMLextern void caml_minor_collection (void);
#endif

#if OCAML_VERSION < 50000
#define get_compactions() caml_stat_compactions
#elif OCAML_VERSION < 50200
// No compaction in OCaml 5.0 and 5.1
#define get_compactions() 0
#else
// caml_stat_compactions is defined to 0 in OCaml 5, but compaction is back
// since 5.2
#define get_compactions() atomic_load(&caml_compactions_count)
#endif

/* GC-safety
 *
 * Any allocation can trigger a minor collection: even when the minor heap
 * has room, OCaml 5 runs pending GC requests (from another domain, or from
 * a ref table overflow) when allocating from C. Another domain can even
 * trigger a compaction.
 *
 * Therefore, the validity of the tables is re-checked after each allocation,
 * and operations retry from [table_sync] until they can complete without
 * allocating.
 *
 * The minor table is stale when its keys are no longer young (a minor
 * collection promotes all of them at once). One of its keys is kept as a
 * witness. The minor collection counter cannot be used: with multiple
 * domains, it is incremented before the other domains are stopped.
 */

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

static size_t table_slots (value table)
{
  size_t sz = Wosize_val(table);
  /* size must be a power of 2 */
  assert ((sz & (sz-1)) == 0);
  return sz;
}

// Fibonacci hashing: the low bits of an address carry little entropy
// (alignment, sequential allocation), so keep the high bits of the product.
static size_t table_hash (value v, size_t slots)
{
  unsigned bits = log2_pow2(slots);
  uint64_t h = (uint64_t)v * UINT64_C(0x9E3779B97F4A7C15);
  return (bits == 0 ? 0 : (size_t)(h >> (64 - bits)));
}

static size_t table_find (value table, value null, value obj)
{
  size_t slots = table_slots (table);
  size_t mask = slots - 1;
  size_t index = table_hash (obj, slots);

  while (1)
  {
    value v = Field(table, index);
    if (v == null || v == obj)
      return index;
    index = (index + 1) & mask;
  }
}

// Basic set data structure

value ml_physh_set_alloc(value empty, value null)
{
  CAMLparam2(empty, null);
  CAMLlocal2(v,vmin);

  vmin = caml_alloc_small(2, 0);
  Field(vmin, 0) = null;
  Field(vmin, 1) = null;

  v = caml_alloc_small(6, 0);
  Field(v, 0) = null;
  Field(v, 1) = Val_int(0);
  Field(v, 2) = vmin;
  Field(v, 3) = Val_int(get_compactions());
  Field(v, 4) = Val_int(0);
  Field(v, 5) = empty;

  CAMLreturn(v);
}

// Max fill-factor is 3/4

static int has_capacity(size_t sz, size_t n)
{
  return (sz * 3 >= n * 4);
}

// Access minor table

static size_t t_minor_fill(value t)
{
  return Int_val(Field(t, 1));
}

static int t_minor_is_valid(value t)
{
  value witness = Field(t, 0);
  return (t_minor_fill(t) == 0 || value_is_young(witness));
}

static void t_minor_set_witness(value t, value v)
{
  Store_field(t, 0, v);
}

static void t_minor_set_fill(value t, size_t v)
{
  Field(t, 1) = Val_int(v);
}

static value t_minor_get(value t)
{
  return Field(t, 2);
}

static void t_minor_set(value t, value v)
{
  Store_field(t, 2, v);
}

static size_t t_minor_size(value t)
{
  return Wosize_val(Field(t, 2));
}

static int t_minor_has_capacity(value t, size_t n)
{
  return has_capacity(t_minor_size(t), n);
}

// Access major table

static int t_major_is_valid(value t)
{
  return (Int_val(Field(t, 3)) == get_compactions());
}

static void t_major_set_valid(value t)
{
  Field(t, 3) = Val_int(get_compactions());
}

static size_t t_major_fill(value t)
{
  return Int_val(Field(t, 4));
}

static void t_major_set_fill(value t, size_t v)
{
  Field(t, 4) = Val_int(v);
}

static value t_major_get(value t)
{
  return Field(t, 5);
}

static void t_major_set(value t, value v)
{
  Store_field(t, 5, v);
}

static size_t t_major_size(value t)
{
  return Wosize_val(Field(t, 5));
}

static int t_major_has_capacity(value t, size_t n)
{
  return has_capacity(t_major_size(t), n);
}

// Low-level maintenance functions

// Does not allocate
static void flush_minor (value t, value null)
{
  value minor = t_minor_get(t);
  value major = t_major_get(t);
  size_t sz = t_minor_size(t);

  /* preconditions */

  // Major has enough room for storing minor
  assert (t_major_has_capacity(t, t_major_fill(t) + t_minor_fill(t) + 1));
  assert (t_major_is_valid(t));

  // No young values
  assert (!t_minor_is_valid(t));

  /* promote minor to major */
  if (t_minor_fill(t) > 0)
  {
    size_t fill = 0, i;

    for (i = 0; i < sz; ++i)
    {
      value k = Field(minor, i);
      if (k == null) continue;

      assert (!value_is_young(k));

      size_t index = table_find(major, null, k);
      assert (Field(major, index) == null);

      Store_field(major, index, k);
      Store_field(minor, i, null);
      ++fill;
    }

    assert (fill == t_minor_fill(t));
    t_major_set_fill(t, t_major_fill(t) + fill);
    t_minor_set_fill(t, 0);
  }

  t_minor_set_witness(t, null);

  /* post conditions */

  assert (t_minor_is_valid(t));
  assert (t_minor_fill(t) == 0);
}

// Might trigger a GC; if it does, the minor table is left unchanged (and
// stale).
static void grow_minor (value t, value null)
{
  CAMLparam2(t, null);
  CAMLlocal2(minor_old, minor_new);

  size_t sz = t_minor_size(t), new_sz = sz * 2, i;

  if ((new_sz > Max_young_wosize) ||
      (caml_young_ptr - Whsize_wosize (new_sz) < caml_young_start))
  {
    // Not enough room, no need to allocate:
    // collect minor heap, the caller will flush minor table
    caml_minor_collection();
    CAMLreturn0;
  }

  minor_new = caml_alloc_small(new_sz, 0);

  for (i = 0; i < new_sz; ++i)
    Field(minor_new, i) = null;

  // A GC happened during allocation, the keys of the minor table are no
  // longer young: drop the new table.
  if (!t_minor_is_valid(t))
    CAMLreturn0;

  minor_old = t_minor_get(t);
  size_t fill = 0;

  for (i = 0; i < sz; ++i)
  {
    value k = Field(minor_old, i);
    if (k == null) continue;

    size_t index = table_find(minor_new, null, k);
    assert (Field(minor_new, index) == null);

    Field(minor_new, index) = k;
    ++fill;
  }

  assert (fill == t_minor_fill(t));
  t_minor_set(t, minor_new);

  CAMLreturn0;
}

// Might trigger a GC. Addresses are read after allocating, so the major
// table is valid when this returns, but the minor table might not be.
static void rehash_major (value t, value null, size_t new_sz)
{
  CAMLparam2(t, null);

  CAMLlocal2(major_old, major_new);
  size_t i;

  /* rehash major */

  major_new = caml_alloc(new_sz, 0);

  for (i = 0; i < new_sz; ++i)
    Store_field(major_new, i, null);

  major_old = t_major_get(t);
  size_t sz = t_major_size(t), fill = 0;

  for (i = 0; i < sz; ++i)
  {
    value k = Field(major_old, i);
    if (k == null) continue;

    size_t index = table_find(major_new, null, k);
    assert (Field(major_new, index) == null);

    Store_field(major_new, index, k);
    ++fill;
  }

  assert (fill == t_major_fill(t));
  t_major_set(t, major_new);
  t_major_set_valid(t);

  /* post conditions */

  assert (t_major_is_valid(t));

  CAMLreturn0;
}

static void realloc_major (value t, value null)
{
  size_t size = t_major_size(t);
  size_t target  = t_major_fill(t) + t_minor_fill(t) + 1;

  if (size == 0)
    size = 1;

  while (!has_capacity(size, target))
    size = size * 2;

  rehash_major (t, null, size);
}

// Bring both tables up-to-date with the GC.
// Allocation (and hence GC) can only happen before the final check.
static void table_sync (value t, value null)
{
  CAMLparam2(t, null);

  while (!t_minor_is_valid(t) || !t_major_is_valid(t))
  {
    if (!t_major_has_capacity(t, t_major_fill(t) + t_minor_fill(t) + 1))
      realloc_major(t, null);
    else if (!t_major_is_valid(t))
      rehash_major(t, null, t_major_size(t));
    else
      flush_minor(t, null);
  }

  CAMLreturn0;
}

// High-level functions

value ml_physh_set_length (value t)
{
  return Val_int(t_major_fill(t) + t_minor_fill(t));
}

value ml_physh_set_mem (value t, value obj, value null)
{
  CAMLparam3(t, null, obj);

  table_sync(t, null);

  value table;
  size_t fill;

  if (value_is_young(obj))
  {
    table = t_minor_get(t);
    fill = t_minor_fill(t);
  }
  else
  {
    table = t_major_get(t);
    fill = t_major_fill(t);
  }

  int found = fill > 0 && Field(table, table_find(table, null, obj)) == obj;

  CAMLreturn(Val_bool(found));
}

value ml_physh_set_add (value t, value obj, value null)
{
  CAMLparam3(t, null, obj);

  while (1)
  {
    table_sync(t, null);

    int young = value_is_young(obj);
    value table = young ? t_minor_get(t) : t_major_get(t);
    size_t fill = young ? t_minor_fill(t) : t_major_fill(t);

    // Already present
    if (fill > 0 && Field(table, table_find(table, null, obj)) == obj)
      break;

    // Make room, this might trigger a GC so start over
    if (young && !t_minor_has_capacity(t, fill + 1))
    {
      grow_minor(t, null);
      continue;
    }

    if (!young && !t_major_has_capacity(t, fill + 1))
    {
      realloc_major(t, null);
      continue;
    }

    size_t index = table_find(table, null, obj);
    assert (Field(table, index) == null);
    Store_field(table, index, obj);

    if (young)
    {
      if (fill == 0)
        t_minor_set_witness(t, obj);
      t_minor_set_fill(t, fill + 1);
    }
    else
      t_major_set_fill(t, fill + 1);

    break;
  }

  CAMLreturn(Val_unit);
}
