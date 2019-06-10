let smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)
let disbalanced smin smax = smaller_ell smin (smax lsr 1)

let rec validate = function
  | Bt1.Node (_, l, _, r) ->
    let sl = Bt1.size l in
    let sr = Bt1.size r in
    if sl < sr then
      assert (not (disbalanced sl sr))
    else
      assert (not (disbalanced sr sl));
    validate l;
    validate r
  | Bt1.Leaf -> ()

let enum i =
  let rec aux acc i =
    if i >= 0 then aux (i :: acc) (i - 1) else acc
  in
  aux [] (i - 1)

let () =
  let add tree x = Bt1.node tree x Bt1.leaf in
  validate (List.fold_left add Bt1.leaf (enum 100))
