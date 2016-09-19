#load "binpacking.cma";;
let m = Maxrects.add_bin () 256 256 Maxrects.empty ;;
let rects = ref [];;
let push () = rects := Maxrects.box ~allow_rotation:true () (Random.int 32) (Random.int 32) :: !rects;;
let occupancy rects =
  float_of_int (List.fold_left (fun acc -> function
      | None -> acc
      | Some { Maxrects. w; h } -> acc + w * h) 0 rects);;

Random.self_init ();
for i = 1 to 16 do
  let i = i * 16 in
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  push ();
  Printf.printf "packing %d rects\n%!" i;
  let _, result = Maxrects.insert_batch ?? ~heuristic:??

  in
;;

 (*m !rects in
  Printf.printf "occupancy (batch): %.02f\n%!" (occupancy result *. 100. /. 65536.);
  let _, result = Maxrects.insert_global m !rects in
  Printf.printf "occupancy (global): %.02f\n%!" (occupancy result *. 100. /. 65536.);
  ()
d*)
