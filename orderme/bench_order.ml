module type S = module type of Order_indir

let t () = (Unix.times()).Unix.tms_utime

let once name (module M : S) count =
  let bench () =
    let r = ref (M.root()) in
    for _ = 1 to count do
      r := M.after !r;
    done
  in
  Gc.major (); Gc.major ();
  let t0 = t () in
  bench ();
  let dt = t () -. t0 in
  Printf.printf "%s bench time: %.02f (%d items)\n%!" name dt count

let () =
  let () = Random.self_init () in
  let count = try int_of_string (Sys.argv.(1)) with _ -> 1_000_000 in
  once "Order_indir"   (module Order_indir)   count;
  once "Order_list"    (module Order_list)    count;
  once "Order_managed" (module Order_managed) count;
