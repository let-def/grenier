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

(* [node] and [join] on balanced trees of all pairs of sizes up to [n]: the
   result is balanced, has the right size and keeps the order of elements. *)

let rec elements1 acc = function
  | Bt1.Leaf -> acc
  | Bt1.Node (_, l, x, r) -> elements1 (x :: elements1 acc r) l

let rec of_range1 lo hi =
  if lo >= hi then Bt1.leaf
  else let m = (lo + hi) / 2 in Bt1.node (of_range1 lo m) m (of_range1 (m + 1) hi)

let rec validate2 = function
  | Bt2.Node (_, l, _, _, r) ->
    let sl = Bt2.size l and sr = Bt2.size r in
    assert (not (disbalanced sl sr) && not (disbalanced sr sl));
    validate2 l;
    validate2 r
  | Bt2.Leaf -> ()

let rec of_range2 lo hi =
  if lo >= hi then Bt2.leaf
  else let m = (lo + hi) / 2 in Bt2.node (of_range2 lo m) m (-m) (of_range2 (m + 1) hi)

(* Mbt, measuring the number of elements. *)
module M = Mbt.Make (struct
    type 'a measurable = 'a
    type measure = int
    let empty = 0
    let cat a _ b = a + 1 + b
  end)

let rec validate_m = function
  | M.Node (_, l, _, r, m) ->
    let sl = M.size l and sr = M.size r in
    assert (not (disbalanced sl sr) && not (disbalanced sr sl));
    assert (m = sl + 1 + sr);
    validate_m l;
    validate_m r
  | M.Leaf -> ()

let rec of_range_m lo hi =
  if lo >= hi then M.leaf
  else let m = (lo + hi) / 2 in M.node (of_range_m lo m) m (of_range_m (m + 1) hi)

(* Lbt, with a measure counting elements, computed on demand. Nodes built by
   rebalancing must have no measure, and the cached measures of the others
   must stay valid. *)
let rec measure_l t = match Lbt.measure t, t with
  | Some m, _ -> m
  | None, Lbt.Leaf -> 0
  | None, (Lbt.Node {l; r; _}) ->
    let m = measure_l l + 1 + measure_l r in
    Lbt.set_measure t m;
    m

let rec validate_l = function
  | Lbt.Node {size; l; r; m; _} ->
    let sl = Lbt.size l and sr = Lbt.size r in
    assert (not (disbalanced sl sr) && not (disbalanced sr sl));
    assert (size = sl + 1 + sr);
    assert (match m with None -> true | Some m -> m = size);
    validate_l l;
    validate_l r
  | Lbt.Leaf -> ()

let rec elements_l acc = function
  | Lbt.Leaf -> acc
  | Lbt.Node {l; x; r; _} -> elements_l (x :: elements_l acc r) l

let rec of_range_l lo hi =
  if lo >= hi then Lbt.leaf
  else let m = (lo + hi) / 2 in Lbt.node (of_range_l lo m) m (of_range_l (m + 1) hi)

let check_l ~fresh n t =
  validate_l t;
  assert ((not fresh || Lbt.measure t = None) && elements_l [] t = enum n);
  assert (measure_l t = n);
  validate_l t

let () =
  let n = 150 in
  for a = 0 to n do
    for b = 0 to n do
      let l = of_range1 0 a and r = of_range1 a (a + b) in
      let t = Bt1.join l r in
      validate t;
      assert (Bt1.size t = a + b && elements1 [] t = enum (a + b));
      let r = of_range1 (a + 1) (a + 1 + b) in
      let t = Bt1.node l a r in
      validate t;
      assert (Bt1.size t = a + b + 1 && elements1 [] t = enum (a + b + 1));
      validate2 (Bt2.join (of_range2 0 a) (of_range2 a (a + b)));
      validate2 (Bt2.node (of_range2 0 a) a (-a) (of_range2 (a + 1) (a + 1 + b)));
      validate_m (M.join (of_range_m 0 a) (of_range_m a (a + b)));
      validate_m (M.node (of_range_m 0 a) a (of_range_m (a + 1) (a + 1 + b)));
      let l = of_range_l 0 a and r = of_range_l a (a + b) in
      ignore (measure_l l, measure_l r);
      check_l ~fresh:(a > 0 && b > 0) (a + b) (Lbt.join l r);
      let r = of_range_l (a + 1) (a + 1 + b) in
      ignore (measure_l r);
      check_l ~fresh:true (a + b + 1) (Lbt.node l a r)
    done
  done
