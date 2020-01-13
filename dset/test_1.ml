open Dset

let check ?(right=[]) ?(left=[]) diff =
  List.for_all (fun x -> List.mem x left) diff.left_only &&
  List.for_all (fun x -> List.mem x right) diff.right_only

let e1 = element 1 and e2 = element 2

let () =
  assert (check (diff empty empty));
  assert (check (diff (element 1) (element 2)) ~left:[1] ~right:[2]);
  assert (check (diff (element 1) (element 1)) ~left:[1] ~right:[1]);
  assert (check (diff e1 e1));
  assert (check (diff e1 e2) ~left:[1] ~right:[2]);
  assert (check (diff e2 e1) ~left:[2] ~right:[1]);
  assert (check (diff e2 e2));
  assert (check (diff (union e1 e1) (union e1 e1)));
  assert (check (diff (union e1 e2) (union e1 e1)) ~left:[2]);
  assert (check (diff (union e1 e1) (union e1 e2)) ~right:[2]);
  assert (check (diff (union e1 e2) (union e1 e2)));
  assert (check (diff (union e1 e2) empty) ~left:[2; 1]);
  assert (check (diff empty (union e1 e2)) ~right:[1; 2]);
