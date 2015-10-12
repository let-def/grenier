let strong_assert x =
  if not x then assert false

let check = ref false

module Simple : sig
  type 'a t
  type 'a label

  val create : unit -> 'a t
  val count : 'a t -> int

  val label : 'a -> 'a label
  val get : 'a label -> 'a
  val set : 'a label -> 'a -> unit

  val insert : 'a t -> 'a label -> unit
  val order : 'a t -> set:'a label -> before:'a label -> unit
  val forget : 'a t -> 'a label -> unit
  exception Wrong_tree

  val path : 'a label -> int
  val compare : 'a label -> 'a label -> int

  val from_array : ?range:(int * int) -> 'a label array -> 'a t
  val fold : ('b -> 'a label -> 'b) -> 'b -> 'a t -> 'b

  val choose : 'a t -> 'a label option
end = struct
  type 'a label = {
    mutable path: int;
    mutable data: 'a;
  }
  let label a = { path = -1; data = a }
  let path l = l.path
  let get lbl = lbl.data
  let set lbl a = lbl.data <- a
  let compare l1 l2 = compare l1.path l2.path

  type 'a node =
    | Leaf
    | Inner of 'a node * 'a label * 'a node

  type 'a t = {
    mutable n: int;
    mutable max_n: int;
    mutable root: 'a node;
  }
  let create () = {n = 0; max_n = 0; root = Leaf}

  let count t = t.n

  let hibit = (max_int lsr 1) + 1
  let seal path level =
    let mark = hibit lsr level in
    assert (mark > 0);
    path lor mark

  let rec size = function
    | Leaf -> 0
    | Inner (l,_,r) -> size l + 1 + size r

  let rec wellformed_node path level = function
    | Leaf -> true
    | Inner (l,lbl,r) ->
      let path' = seal path level in
      lbl.path = path' &&
      wellformed_node path  (level + 1) l &&
      wellformed_node path' (level + 1) r

  let wellformed {root; n} =
    size root = n && wellformed_node 0 hibit root

  let check_wellformed path level node =
    if !check then
      strong_assert (wellformed_node path level node)

  let rec fold_node f acc = function
    | Leaf -> acc
    | Inner (l,lbl,r) ->
      let acc = fold_node f acc l in
      let acc = f acc lbl in
      fold_node f acc r

  let fold f acc t = fold_node f acc t.root

  let rec node_from_array a path level l r =
    if r < l then Leaf else
      let midpoint = l + (r - l) / 2 in
      let label = a.(midpoint) in
      let path' = seal path level in
      label.path <- path';
      Inner (
        node_from_array a path (level + 1) l (midpoint - 1),
        label,
        node_from_array a path' (level + 1) (midpoint + 1) r
      )

  let rebuild path level node =
    let placeholder = match node with
      | Leaf -> assert false
      | Inner (_,lbl,_) -> lbl
    in
    let size = size node in
    let a = Array.make size placeholder in
    strong_assert (fold_node (fun i lbl -> a.(i) <- lbl; i + 1) 0 node = size);
    let result = node_from_array a path level 0 (size - 1) in
    check_wellformed path level result;
    result

  let from_array ?range a =
    let len = Array.length a in
    let l, r = match range with
      | None -> 0, len - 1
      | Some (l,r) ->
        if l < 0 || l >= len ||
           r < 0 || r >= len
        then invalid_arg "from_array: invalid range";
        l, r
    in
    let len = max (r - l + 1) 0 in
    { n = len; max_n = len; root = node_from_array a 0 0 l r }

  let rec insert_before_node scapesize max_level lbl path level = function
    | Leaf ->
      lbl.path <- seal path level;
      if level > max_level then scapesize := 1;
      Inner (Leaf, lbl, Leaf)

    | Inner (l, lbl', r) when lbl.path <= lbl'.path ->
      let result = Inner (insert_before_node scapesize max_level lbl path (level + 1) l, lbl', r) in
      begin match !scapesize with
        | 0 -> result
        | lsize ->
          let size = lsize + 1 + size r in
          if 3 * lsize > 2 * size then
            (scapesize := 0; rebuild path level result)
          else
            (scapesize := size; result)
      end
    | Inner (l, lbl', r) ->
      let result = Inner (l, lbl', insert_before_node scapesize max_level lbl (seal path level) (level + 1) r) in
      begin match !scapesize with
        | 0 -> result
        | rsize ->
          let size = size l + 1 + rsize in
          if 3 * rsize > 2 * size then
            (scapesize := 0; rebuild path level result)
          else
            (scapesize := size; result)
      end

  exception Wrong_tree

  let rec remove_rightmost ref = function
    | Leaf -> assert false
    | Inner (l,lbl,(Inner (l',lbl',Leaf))) ->
      ref := lbl';
      Inner (l,lbl,l')
    | Inner (l,lbl,r) -> Inner (l, lbl, remove_rightmost ref r)

  let rec delete_node lbl = function
    | Inner (l, lbl', r) when lbl' == lbl ->
      if l = Leaf then r
      else if r = Leaf then l
      else
        let ref = ref lbl' in
        let l' = remove_rightmost ref l in
        Inner (l', !ref, r)
    | Inner (l, lbl', r) when lbl'.path > lbl.path ->
      Inner (delete_node lbl l, lbl', r)
    | Inner (l, lbl', r) when lbl'.path < lbl.path ->
      Inner (l, lbl', delete_node lbl r)
    | Leaf | Inner _ -> raise Wrong_tree

  let forget t lbl =
    if lbl.path > -1 then begin
      t.root <- delete_node lbl t.root;
      t.n <- t.n - 1;
      if t.n > 0 && (3 * t.n < 2 * t.max_n) then
        (t.root <- rebuild 0 0 t.root; t.max_n <- t.n);
      lbl.path <- -1
    end

  let log23 = 2.4663034623764317

  let insert_before t lbl =
    let scapesize = ref 0 in
    let max_level = int_of_float (ceil (log (float t.n) *. log23)) in
    let result = insert_before_node scapesize max_level lbl 0 0 t.root in
    let result = if !scapesize = 0 then result else rebuild 0 0 result in
    t.root <- result;
    t.n <- t.n + 1;
    t.max_n <- max t.max_n t.n

  let insert t node =
    if node.path < 0 then
      (node.path <- hibit; insert_before t node)

  let order t ~set ~before =
    if before != set && (set.path < 0 || before.path <= set.path) then
      begin
        forget t set;
        insert t before;
        set.path <- before.path;
        insert_before t set;
        assert (set.path < before.path);
        check_wellformed 0 0 t.root;
      end

  let choose t = match t.root with
    | Leaf -> None
    | Inner (_,lbl,_) -> Some lbl
end

module Make_simple () : sig
  type label
  val count : unit -> int
  val label : unit -> label
  val order : set:label -> before:label -> unit
  val forget : label -> unit
  val compare : label -> label -> int
end = struct
  type label = unit Simple.label
  let t = Simple.create ()

  let count () = Simple.count t
  let label = Simple.label
  let order ~set ~before = Simple.order t ~set ~before
  let forget lbl = Simple.forget t lbl
  let compare = Simple.compare
end

module Indirect : sig
  type t
  type label

  val create : unit -> t
  val count : t -> int

  val label : unit -> label

  val insert : t -> label -> unit
  val order : t -> set:label -> before:label -> unit
  val forget : t -> label -> unit

  val compare : label -> label -> int

  val from_array : label array -> t
end = struct

  module Label : sig
    type t

    val create : unit -> t

    val compare : t -> t -> int

    val first : t -> t
    val count : t -> int
    val nth : int -> t -> t

    val fold : ('a -> t -> 'a) -> 'a -> t -> 'a
    val renumber : t Simple.label option -> t -> unit
    val insert : t -> before:t -> [`Need_split | `Ok]
    val split : t Simple.t -> t -> unit

    val unlink : t -> t Simple.label option
    val linked : t -> t Simple.label option

    val link : t -> t Simple.label
    val link_array : first:int -> last:int -> t array -> t Simple.label
  end = struct
    type t = {
      mutable outer : t Simple.label option;
      mutable next  : t;
      mutable prev  : t;
      mutable index : int;
    }

    let create () =
      let rec l = { outer = None; next = l; prev = l; index = 0 } in
      l

    let compare a b =
      match a.outer, b.outer with
      | Some ao, Some bo when ao == bo ->
        compare a.index b.index
      | Some ao, Some bo ->
        Simple.compare ao bo
      | _ -> invalid_arg "Indirect.Label.compare"

    let first item =
      if item == item.prev then item else
        let rec aux item =
          if item.index >= item.prev.index
          then aux item.prev
          else item
        in
        aux item

    let count first =
      let rec aux n item =
        if item == first
        then n
        else aux (n + 1) item.next
      in
      aux 1 first.next

    let rec nth n item =
      if n = 0
      then item
      else nth (n - 1) item.next

    let fold f acc first =
      let rec aux acc item =
        let acc = f acc item in
        if item.next == first
        then acc
        else aux acc item.next
      in
      aux acc first

    let renumber outer first =
      assert (outer <> None);
      let count = count first in
      let step = (max_int / (count + 1)) * 2 in
      ignore (fold (fun i item ->
          assert (i <> min_int);
          item.index <- i;
          item.outer <- outer;
          i + step
        ) (min_int + step) first : int)

    let average x y =
      (x land y) + (x lxor y) asr 1

    let prev_index t =
      if t.index <= t.prev.index
      then (assert (t.index > min_int); min_int)
      else t.prev.index

    let insert x ~before =
      assert (x.next == x && x.prev == x && x.outer = None);
      assert (before.outer <> None);
      let index1 = prev_index before and index2 = before.index in
      x.index <- average index1 index2;
      let prev = before.prev in
      x.outer <- before.outer;
      prev.next <- x;
      before.prev <- x;
      x.prev <- prev;
      x.next <- before;
      if x.index = index1
      then `Need_split
      else `Ok

    let get_outer item =
      match item.outer with
      | None -> assert false
      | Some l -> l

    let split root item =
      (* print_endline "split"; *)
      let first = first item in
      let count0 = count first in
      let first' = nth (count0 / 2) first in
      let last = first'.prev and last' = first.prev in
      first.prev <- last;
      last.next <- first;
      first'.prev <- last';
      last'.next <- first';
      let label = Simple.label first in
      let label' = get_outer first' in
      Simple.set label' first';
      renumber (Some label) first;
      renumber (Some label') first';
      (*Printf.printf "split size %d = %d + %d\n"
        count0 (count first) (count first');*)
      Simple.order root ~set:label ~before:label'

    let unlink item =
      assert (item.outer != None);
      let {prev; next} = item in
      next.prev <- prev;
      prev.next <- next;
      let outer = get_outer item in
      item.outer <- None;
      item.prev <- item;
      item.next <- item;
      if Simple.get outer == item then begin
        if prev == item then
          Some outer
        else
          (Simple.set outer prev; None)
      end else
        None

    let linked t = t.outer

    let link t =
      assert (t.outer = None && t.prev == t && t.next == t);
      let label = Simple.label t in
      renumber (Some label) t;
      label

    let link_array ~first ~last arr =
      assert (first <= last);
      let afirst = arr.(first) and alast = arr.(last) in
      let label = Simple.label afirst in
      afirst.prev <- alast;
      alast.next <- afirst;
      for j = first + 1 to last do
        let a = arr.(j - 1) and b = arr.(j) in
        a.next <- b;
        b.prev <- a;
      done;
      renumber (Some label) afirst;
      label
  end

  type label = Label.t
  let label = Label.create
  let compare = Label.compare

  let dummy = label ()

  let to_array n root =
    let array = Array.make n dummy in
    strong_assert (
      Simple.fold (fun i outer ->
          let first = Label.first (Simple.get outer) in
          Label.fold (fun i label -> array.(i) <- label; i + 1) i first
        ) 0 root
      = n
    );
    array

  let rebuild array =
    (* print_endline "rebuild"; *)
    let len = Array.length array in
    let clen = int_of_float (float len /. (log (float len)) +. 1.) in
    let inners = Array.init ((len + clen - 1) / clen) @@ fun i ->
      let first = i * clen in
      let last = min len ((i + 1) * clen) - 1 in
      Label.link_array ~first ~last array
    in
    Simple.from_array inners

  type t = {
    mutable n : int;
    mutable n0 : int;
    mutable root : Label.t Simple.t;
  }

  let update_count t delta =
    let n = t.n + delta in
    t.n <- n;
    if (n > 2) && (3 * n < t.n0 || 2 * t.n0 < n) then
      begin
        t.n0 <- n;
        t.root <- rebuild (to_array n t.root);
      end

  let create () = {
    n = 0;
    n0 = 3;
    root = Simple.create ();
  }

  let forget t lbl =
    (* print_endline "forget"; *)
    match Label.linked lbl with
    | None -> ()
    | Some _ ->
      begin match Label.unlink lbl with
        | None -> ()
        | Some node ->
          (* prerr_endline "root forget"; *)
          Simple.forget t.root node
      end;
      update_count t (-1)

  let insert t lbl =
    (* print_endline "insert"; *)
    match Label.linked lbl with
    | Some _ -> ()
    | None ->
      match Simple.choose t.root with
      | None ->
        Simple.insert t.root (Label.link lbl);
        update_count t (+1)
      | Some node ->
        match Label.insert lbl ~before:(Simple.get node) with
        | `Ok ->
          update_count t (+1)
        | `Need_split ->
          Label.split t.root lbl;
          update_count t (+1)

  let order t ~set ~before =
    if before != set then begin
      insert t before;
      forget t set;
      match Label.insert set ~before with
      | `Ok -> update_count t (+1)
      | `Need_split ->
        Label.split t.root set;
        update_count t (+1)
    end

  let count t = t.n

  let from_array = function
    | [||] -> create ()
    | a ->
      let len = Array.length a in
      {n = len; n0 = len; root = rebuild a}
end

module Make () : sig
  type label
  val count : unit -> int
  val label : unit -> label
  val order : set:label -> before:label -> unit
  val forget : label -> unit
  val compare : label -> label -> int
end = struct
  type label = Indirect.label
  let t = Indirect.create ()

  let count () = Indirect.count t
  let label = Indirect.label
  let order ~set ~before = Indirect.order t ~set ~before
  let forget lbl = Indirect.forget t lbl
  let compare = Indirect.compare
end
