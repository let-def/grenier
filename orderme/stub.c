#include <caml/mlvalues.h>

CAMLextern value ml_orderme_i_am_live(value v)
{
  (void)v;
  return Val_unit;
}
