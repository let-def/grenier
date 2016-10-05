#load "binpacking.cma";;

let assertf b fmt =
  if b
  then Printf.ifprintf () fmt
  else Printf.ksprintf failwith fmt
;;

let genbox ?allow_rotation () =
  let input = (Random.int 32, Random.int 32) in
  Maxrects.box ?allow_rotation input (fst input) (snd input)
;;

let rec genboxes ?allow_rotation n =
  if n > 0 then
    genbox ?allow_rotation () :: genboxes ?allow_rotation (n - 1)
  else []
;;

let validate_rect t =
  let open Maxrects in
  assertf (t.x >= 0) "x:%d >= 0 failed" t.x;
  assertf (t.y >= 0) "y:%d >= 0 failed" t.y;
  assertf (t.w >= 0) "w:%d >= 0 failed" t.w;
  assertf (t.h >= 0) "h:%d >= 0 failed" t.h;
  if not t.rotated then (
    assertf (t.w = t.box.width && t.w = fst t.box.tag)
      "(unrotated) w:%d = box.width:%d = input.width:%d failed"
      t.w t.box.width (fst t.box.tag);
    assertf (t.h = t.box.height && t.h = snd t.box.tag)
      "(unrotated) h:%d = box.height:%d = input.height:%d failed"
      t.h t.box.height (snd t.box.tag);
  ) else (
    assertf t.box.allow_rotation "box rotated but allow_rotation = false";
    assertf (t.w = t.box.height && t.w = snd t.box.tag)
      "(rotated) w:%d = box.height:%d = input.height:%d failed"
      t.w t.box.height (snd t.box.tag);
    assertf (t.h = t.box.width && t.h = fst t.box.tag)
      "(rotated) h:%d = box.width:%d = input.width:%d failed"
      t.h t.box.width (fst t.box.tag);
  );
  assertf (t.x + t.w <= fst t.bin)
    "x:%d + w:%d < %d failed, box doesn't fit in bin"
    t.x t.w (fst t.bin);
  assertf (t.y + t.h <= snd t.bin)
    "y:%d + h:%d < %d failed, box doesn't fit in bin"
    t.y t.h (snd t.bin)
;;

let overlapping r1 r2 =
  let open Maxrects in
  not (r1.x >= r2.x + r2.w || r2.x >= r1.x + r1.w) &&
  not (r1.y <= r2.y + r2.w || r2.y <= r1.y + r1.h)
;;

let rec validate_rects = function
  | [] -> ()
  | r :: rs ->
    validate_rect r;
    if List.exists (overlapping r) rs then
      failwith "two boxes overlap";
    validate_rects rs
;;

let occupancy rects =
  float_of_int
    (List.fold_left (fun acc { Maxrects. w; h } -> acc + w * h) 0 rects)
;;

let rec filter_none = function
  | [] -> []
  | None :: xs -> filter_none xs
  | Some x :: xs -> x :: filter_none xs
;;

let insert_simple t boxes =
  let t = ref t in
  let rects = List.map (fun b ->
      let t', r = Maxrects.insert !t b in
      t := t'; r) boxes in
  !t, rects
;;

let run_test count rotate =
  Printf.printf "packing %d rects, allow rotation = %b\n%!" count rotate;
  let empty = Maxrects.add_bin (256,256) 256 256 Maxrects.empty in
  let boxes = genboxes ~allow_rotation:rotate count in
  let check name rects =
    let rects = filter_none rects in
    validate_rects rects;
    Printf.printf "%s: packed = %d/%d, occupancy = %.02f\n%!"
      name (List.length rects) count (occupancy rects *. 100. /. 65536.)
  in
  check "batch" (snd (Maxrects.insert_batch empty boxes));
  check "global" (snd (Maxrects.insert_global empty boxes));
  check "simple" (snd (insert_simple empty boxes))
;;

let () =
  Random.self_init ();
  for i = 1 to 16 do
    run_test (i * 16) false;
    run_test (i * 16) true;
  done
;;
