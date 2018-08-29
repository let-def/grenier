module O = Order_indir

type t = {
  a : O.t;
  b : O.t;
}

let forget {a; b} =
  O.forget a;
  O.forget b

let is_valid t = O.is_valid t.a

let root () =
  let a = O.root () in
  let b = O.after a in
  {a; b}

let after t =
  let b = O.after t.b in
  let a = O.before b in
  {a; b}

let before t =
  let a = O.before t.a in
  let b = O.after a in
  {a; b}

let inside t =
  let a = O.after t.a in
  let b = O.before t.b in
  {a; b}

let outside t =
  let a = O.before t.a in
  let b = O.after t.b in
  {a; b}

let same_order t1 t2 =
  O.same_order t1.a t2.a

type rel =
  | Before
  | Inside
  | Equal
  | Outside
  | After

let compare t1 t2 =
  if t1 == t2 then Equal else
    let ca = O.compare t1.a t2.a <= 0 in
    let cb = O.compare t1.b t2.b <= 0 in
    match ca, cb with
    | true, true   -> Before
    | true, false  -> Outside
    | false, true  -> Inside
    | false, false -> After

let cardinal t =
  O.cardinal t.a / 2

let unsafe_check t msg =
  O.unsafe_check t.a ("(Order_interval a) " ^ msg);
  O.unsafe_check t.b ("(Order_interval b) " ^ msg);
