open Congre

let gr = make ()

let f   = fresh gr "f"
let g   = fresh gr "g"
let x   = fresh gr "x"
let y   = fresh gr "y"
let f_x = fresh gr "f x"
let g_y = fresh gr "g y"

let () =
  let sn = snapshot gr in
  assert (not (same f_x g_y));
  assume_application f x ~equal:f_x;
  assert (not (same f_x g_y));
  assume_application g y ~equal:g_y;
  assert (not (same f_x g_y));
  assume_equal f g;
  assert (not (same f_x g_y));
  assume_equal x y;
  assert (same f_x g_y);
  restore sn;
  assert (not (same f_x g_y))
