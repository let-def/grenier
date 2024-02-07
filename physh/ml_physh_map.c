#include <assert.h>
#include <stdint.h>

#define CAML_INTERNALS
#include <caml/version.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/gc_ctrl.h>

#if OCAML_VERSION < 41000
CAMLextern void caml_minor_collection (void);
#endif

#if OCAML_VERSION < 50100
#define get_minor_collections() caml_stat_minor_collections
#else
#define get_minor_collections() atomic_load(&caml_minor_collections_count)
#endif

static int value_is_young(value obj)
{
  return (Is_block(obj) && Is_young(obj));
}
// C implementation of Hashtable

static size_t table_mask (value table)
{
  size_t sz = Wosize_val(table);
  /* size must be a power of 2 */
  assert ((sz & (sz-1)) == 0);
  return ((sz / 2) - 1);
}

static uintptr_t table_hash (value v)
{
  uintptr_t n = (uintptr_t) v;
  return ((n + 15485863) * 2654435761);
}

static size_t table_find (value table, value null, value obj)
{
  size_t mask = table_mask (table);
  size_t index = table_hash (obj) & mask;

  while (1)
  {
    value v = Field(table, index * 2);
    if (v == null || v == obj)
      return index;
    index = (index + 1) & mask;
  }
}

// Basic set data structure

value ml_physh_map_alloc(value empty, value null)
{
  CAMLparam2(empty, null);
  CAMLlocal2(v,vmin);

  vmin = caml_alloc_small(4, 0);
  Field(vmin, 0) = null;
  Field(vmin, 1) = null;
  Field(vmin, 2) = null;
  Field(vmin, 3) = null;

  v = caml_alloc_small(6, 0);
  Field(v, 0) = Val_int(get_minor_collections());
  Field(v, 1) = Val_int(0);
  Field(v, 2) = vmin;
  Field(v, 3) = Val_int(caml_stat_compactions);
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

static int t_minor_is_valid(value t)
{
  return (Int_val(Field(t, 0)) == get_minor_collections());
}

static void t_minor_set_valid(value t)
{
  Field(t, 0) = Val_int(get_minor_collections());
}

static size_t t_minor_fill(value t)
{
  return Int_val(Field(t, 1));
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

static value t_minor_size(value t)
{
  return Wosize_val(Field(t, 2)) / 2;
}

static int t_minor_has_capacity(value t, size_t n)
{
  return has_capacity(t_minor_size(t), n);
}

// Access major table

static int t_major_is_valid(value t)
{
  return (Int_val(Field(t, 3)) == caml_stat_compactions);
}

static void t_major_set_valid(value t)
{
  Field(t, 3) = Val_int(caml_stat_compactions);
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

static value t_major_size(value t)
{
  return Wosize_val(Field(t, 5)) / 2;
}

static int t_major_has_capacity(value t, size_t n)
{
  return has_capacity(t_major_size(t), n);
}

// Low-level maintenance functions

static void flush_minor (value t, value null)
{
  value minor = t_minor_get(t);
  value major = t_major_get(t);
  size_t sz = t_minor_size(t);

  /* preconditions */

  // Major has enough room for storing minor
  assert (t_major_has_capacity(t, t_major_fill(t) + t_minor_fill(t) + 1));

  // No young values
  assert (!t_minor_is_valid(t));
  // for (size_t i = 0; i < sz; ++i)
  // {
  //   value v = Field(minor, i);
  //   assert (v == null || !value_is_young(v));
  // }

  /* promote minor to major */
  if (t_minor_fill(t) > 0)
  {
    size_t fill = 0, i;

    for (i = 0; i < sz; ++i)
    {
      value k = Field(minor, i * 2);
      if (k == null) continue;

      assert (!value_is_young(k));

      size_t index = table_find(major, null, k);
      assert (Field(major, index * 2) == null);

      Store_field(major, index * 2 + 0, k);
      Store_field(major, index * 2 + 1, Field(minor, i * 2 + 1));
      Store_field(minor, i * 2, null);
      Store_field(minor, i * 2 + 1, null);
      ++fill;
    }

    assert (fill == t_minor_fill(t));
    t_major_set_fill(t, t_major_fill(t) + fill);
    t_minor_set_fill(t, 0);
  }

  t_minor_set_valid(t);

  /* post conditions */

  assert (t_minor_is_valid(t));
  assert (t_minor_fill(t) == 0);
}

static void rehash_minor (value t, value null, size_t new_sz)
{
  CAMLparam2(t, null);

  CAMLlocal2(minor_old, minor_new);
  minor_old = t_minor_get(t);
  size_t sz = t_minor_size(t), i;

  /* rehash minor */

  minor_new = caml_alloc_small(new_sz * 2, 0);

  for (i = 0; i < new_sz * 2; ++i)
    Store_field(minor_new, i, null);

  size_t fill = 0;

  for (i = 0; i < sz; ++i)
  {
    value k = Field(minor_old, i * 2);
    if (k == null) continue;

    size_t index = table_find(minor_new, null, k);
    assert (Field(minor_new, index * 2) == null);

    Store_field(minor_new, index * 2 + 0, k);
    Store_field(minor_new, index * 2 + 1, Field(minor_old, i * 2 + 1));
    ++fill;
  }

  assert (fill == t_minor_fill(t));
  t_minor_set(t, minor_new);

  /* post conditions */

  assert (t_minor_is_valid(t));

  CAMLreturn0;
}

static void rehash_major (value t, value null, size_t new_sz)
{
  CAMLparam2(t, null);

  CAMLlocal2(major_old, major_new);
  major_old = t_major_get(t);
  size_t sz = t_major_size(t), i;

  /* rehash major */

  major_new = caml_alloc(new_sz * 2, 0);

  for (i = 0; i < new_sz * 2; ++i)
    Store_field(major_new, i, null);

  size_t fill = 0;

  for (i = 0; i < sz; ++i)
  {
    value k = Field(major_old, i * 2);
    if (k == null) continue;

    size_t index = table_find(major_new, null, k);
    assert (Field(major_new, index) == null);

    Store_field(major_new, index * 2 + 0, k);
    Store_field(major_new, index * 2 + 1, Field(major_old, i * 2 + 1));
    ++fill;
  }

  assert (fill == t_major_fill(t));
  t_major_set(t, major_new);
  t_major_set_valid(t);

  /* post conditions */

  assert (t_major_is_valid(t));

  CAMLreturn0;
}

static void flush_major (value t, value null)
{
  assert (!t_major_is_valid(t));
  rehash_major (t, null, t_major_size(t));
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

static void realloc_minor (value t, value null)
{
  CAMLparam2(t, null);

  size_t size = t_minor_size(t);

  if (size == 0)
    size = 1;

  size = size * 2;

  if ((size * 2 > Max_young_wosize) ||
      (caml_young_ptr - Whsize_wosize (size * 2) < caml_young_start))
  {
    // Not enough room, no need to allocate:
    // collect minor heap, flush minor table
    if (!t_major_has_capacity(t, t_major_fill(t) + t_minor_fill(t) + 1))
      realloc_major(t, null);

    caml_minor_collection();

    flush_minor(t, null);
  }
  else
  {
    rehash_minor(t, null, size);
  }

  CAMLreturn0;
}

static void minor_sync (value t, value null)
{
  CAMLparam2(t, null);

  if (!t_minor_is_valid(t))
  {
    if (!t_major_has_capacity(t, t_major_fill(t) + t_minor_fill(t) + 1))
      realloc_major(t, null);
    else if (!t_major_is_valid(t))
      flush_major(t, null);

    assert (t_major_is_valid(t));

    flush_minor(t, null);
  }

  assert (t_minor_is_valid(t));

  CAMLreturn0;
}

// High-level functions

value ml_physh_map_length (value t)
{
  return Val_int(t_major_fill(t) + t_minor_fill(t));
}


value ml_physh_map_find (value t, value key, value null)
{
  CAMLparam3(t, null, key);
  CAMLlocal1(v);

  minor_sync(t, null);
  v = null;
  int found = 0;

  if (value_is_young(key))
  {
    if (t_minor_fill(t) > 0)
    {
      value table = t_minor_get(t);
      size_t index = table_find(table, null, key);
      if (Field(table, index * 2) == key)
      {
        v = Field(table, index * 2 + 1);
        found = 1;
      }
    }
  }
  else
  {
    if (t_major_fill(t) > 0)
    {
      value table = t_major_get(t);
      size_t index = table_find(table, null, key);
      if (Field(table, index * 2) == key)
      {
        v = Field(table, index * 2 + 1);
        found = 1;
      }
    }
  }

  if (found == 0)
    caml_raise_not_found();

  CAMLreturn(v);
}

value ml_physh_map_add (value t, value key, value val, value null)
{
  CAMLparam4(t, null, key, val);

  minor_sync(t, null);

  if (value_is_young(key) && !t_minor_has_capacity(t, t_minor_fill(t) + 1))
    realloc_minor(t, null);

  if (value_is_young(key))
  {
    value table = t_minor_get(t);
    size_t index = table_find(table, null, key);
    if (Field(table, index * 2) != key)
    {
      assert (Field(table, index * 2) == null);
      Store_field(table, index * 2, key);

      t_minor_set_fill(t, t_minor_fill(t) + 1);
    }

    Store_field(table, index * 2 + 1, val);
  }
  else
  {
    if (!t_major_has_capacity(t, t_major_fill(t) + 1))
      realloc_major(t, null);

    value table = t_major_get(t);
    size_t index = table_find(table, null, key);
    if (Field(table, index * 2) != key)
    {
      assert (Field(table, index * 2) == null);
      Store_field(table, index * 2, key);

      t_major_set_fill(t, t_major_fill(t) + 1);
    }

    Store_field(table, index * 2 + 1, val);
  }

  CAMLreturn(Val_unit);
}
