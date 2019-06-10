type 'bin bin = {
  bin_x : int;
  bin_y : int;
  bin_w : int;
  bin_h : int;
  bin_root : 'bin bin;
  bin_tag : 'bin;
}

type 'bin t = {
  free: 'bin bin list;
}

type 'tag box =
  { tag    : 'tag
  ; width  : int
  ; height : int
  ; allow_rotation : bool
  }

let box ?(allow_rotation=false) tag width height =
  { allow_rotation; width; height; tag }

type ('bin, 'tag) rect =
  { x : int ; y : int ; w : int ; h : int
  ; rotated: bool
  ; bin: 'bin
  ; box: 'tag box
  }

type heuristic =
  [ `Short_side_fit
  | `Long_side_fit
  | `Area_fit
  | `Bottom_left
  ]

let empty = { free = [] }

let add_bin bin_tag bin_w bin_h t =
  let rec bin = { bin_x = 0 ; bin_y = 0 ; bin_w; bin_h; bin_tag; bin_root = bin } in
  { free = bin :: t.free }

type score = { hi : int ; lo : int }

let score_heuristic = function
  | `Short_side_fit ->
    fun rect w h ->
      let dw = rect.bin_w - w and dh = rect.bin_h - h in
      { hi = min dw dh; lo = max dw dh }
  | `Long_side_fit ->
    fun rect w h ->
      let dw = rect.bin_w - w and dh = rect.bin_h - h in
      { hi = max dw dh; lo = min dw dh }
  | `Bottom_left ->
    fun rect _w h ->
      { hi = rect.bin_y + h; lo = rect.bin_x }
  | `Area_fit ->
    fun rect w h ->
      let area_fit = rect.bin_w * rect.bin_h - w * h in
      let dw = rect.bin_w - w and dh = rect.bin_h - h in
      { hi = area_fit; lo = min dw dh }

let null_score = { hi = max_int; lo = max_int }
let null_acc tag bin = (tag, bin, { hi = max_int; lo = max_int })

let score_is_better a b =
  a.hi < b.hi || (a.hi = b.hi && a.lo < b.lo)

let select_best score_fun tag w h (_, _, score0 as acc) rect =
  if rect.bin_w >= w && rect.bin_h >= h then
    let score = score_fun rect w h in
    if score_is_better score score0 then
      (tag, rect, score)
    else
      acc
  else
    acc

let split_free_node bin used free =
  let aux rects free =
    if free.bin_root != bin.bin_root ||
      (used.x >= free.bin_x + free.bin_w || used.x + used.w <= free.bin_x ||
       used.y >= free.bin_y + free.bin_h || used.y + used.h <= free.bin_y)
    then free :: rects
    else
      let rects = ref rects in
      if (used.x < free.bin_x + free.bin_w && used.x + used.w > free.bin_x) then (
        if (used.y > free.bin_y && used.y < free.bin_y + free.bin_h) then
          (* New node at the top side of the used node. *)
          rects := { free with bin_h = used.y - free.bin_y } :: !rects;
        if (used.y + used.h < free.bin_y + free.bin_h) then
          (* New node at the bottom side of the used node. *)
          rects := { free with bin_y = used.y + used.h;
                               bin_h = (free.bin_y + free.bin_h) - (used.y + used.h) } :: !rects
      );
      if (used.y < free.bin_y + free.bin_h && used.y + used.h > free.bin_y) then (
        if (used.x > free.bin_x && used.x < free.bin_x + free.bin_w) then
          (* New node at the left side of the used node. *)
          rects := {free with bin_w = used.x - free.bin_x} :: !rects;
        if (used.x + used.w < free.bin_x + free.bin_w) then
          (* New node at the right side of the used node. *)
          rects := {free with bin_x = used.x + used.w;
                              bin_w = (free.bin_x + free.bin_w) - (used.x + used.w) } :: !rects
      );
      !rects
  in
  List.fold_left aux [] free

module Pop_array = struct
  type 'a t = {
    mutable len: int;
    arr: 'a array;
  }

  let of_list l =
    let arr = Array.of_list l in
    { len = Array.length arr; arr }

  let length t = t.len

  let get t n =
    if n < 0 || n >= t.len then
      invalid_arg "Pop_array.get: index out of bounds";
    t.arr.(n)

  let pop t n =
    if n < 0 || n >= t.len then
      invalid_arg "Pop_array.pop: index out of bounds";
    let x = t.arr.(n) in
    let len = t.len - 1 in
    t.arr.(n) <- t.arr.(len);
    t.len <- len;
    x

  let filter t f =
    let i = ref 0 in
    while !i < t.len do
      while !i < t.len && f !i t.arr.(!i) do
        ignore (pop t !i);
      done;
      incr i
    done

  let fold t f acc =
    let len = t.len in
    let acc = ref acc in
    for i = 0 to len - 1 do
      acc := f i t.arr.(i) !acc;
      if t.len <> len then
        invalid_arg "Pop_array.fold: functional argument is mutating the array"
    done;
    !acc

  let maximums pred = function
    | ([] | [_]) as l -> l
    | [a;b] as l ->
      if pred a b then [b]
      else if pred b a then [a]
      else l
    | l ->
      let a = of_list l in
      let result = ref [] in
      while length a > 0 do
        let item' = ref (get a 0) in
        filter a
          (fun _ item ->
             if pred item !item' then
               true
             else if pred !item' item then
               (item' := item; true)
             else
               false);
        result := !item' :: !result
      done;
      !result
end

let is_contained_in a b =
  a.bin_root == b.bin_root
  && a.bin_x >= b.bin_x
  && a.bin_y >= b.bin_y
  && a.bin_x+a.bin_w <= b.bin_x+b.bin_w
  && a.bin_y+a.bin_h <= b.bin_y+b.bin_h

let rec prune_free_list_reference = function
  | [] -> []
  | r :: rects ->
    let rec aux r = function
      | r' :: rects when is_contained_in r r' ->
        aux r' rects
      | r' :: rects when is_contained_in r' r ->
        aux r rects
      | r' :: rects ->
        r' :: aux r rects
      | [] -> [r]
    in
    aux r (prune_free_list_reference rects)

let prune_free_list l = Pop_array.maximums is_contained_in l

let used_rect bin box rotated =
  let w, h = if rotated then box.height, box.width else box.width, box.height in
  { x = bin.bin_x; y = bin.bin_y; w; h; rotated; bin = bin.bin_tag; box }

let update_free bin used {free} =
  { free = prune_free_list (split_free_node bin used free) }

let insert t ?(heuristic=`Short_side_fit) ({ width = w; height = h; _ } as box) =
  match t.free with
  | [] -> t, None
  | default_bin :: _ ->
    let (rotated, bin, score) =
      let score_fun = score_heuristic heuristic in
      let acc = null_acc false default_bin in
      let acc = List.fold_left (select_best score_fun false w h) acc t.free in
      if box.allow_rotation && w <> h then
        List.fold_left (select_best score_fun true h w) acc t.free
      else
        acc
    in
    if score = null_score then
      (t, None)
    else
      let rect = used_rect bin box rotated in
      (update_free bin rect t, Some rect)

let insert_global t ?(heuristic=`Short_side_fit) boxes =
  match t.free with
  | [] -> t, List.map (fun _ -> None) boxes
  | default_bin :: _ ->
    let boxes = boxes |> List.mapi (fun i r -> i,r) |> Pop_array.of_list in
    let result = Array.make (Pop_array.length boxes) None in
    let score_fun = score_heuristic heuristic in
    let t = ref t in
    let select_candidate i (_,{ allow_rotation; width=w; height=h; _ }) acc =
      let free = (!t).free in
      let acc = List.fold_left (select_best score_fun (i,false) w h) acc free in
      if allow_rotation && w <> h then
        List.fold_left (select_best score_fun (i,true) h w) acc free
      else acc
    in
    begin try
        while Pop_array.length boxes > 0 do
          let (pidx, rotated), bin, score =
            Pop_array.fold boxes select_candidate
              (null_acc (-1, false) default_bin) in
          if pidx = -1 then raise Exit;
          let (idx, box) = Pop_array.pop boxes pidx in
          let rect = used_rect bin box rotated in
          t := update_free bin rect !t;
          result.(idx) <- Some rect;
        done;
      with Exit -> ()
    end;
    !t, Array.to_list result

let insert_batch t ?(heuristic=`Bottom_left) boxes =
  let cmp (_, b1) (_, b2) =
    match compare (min b1.width b1.height) (min b2.width b2.height) with
    | 0 -> compare (max b1.width b1.height) (max b2.width b2.height)
    | n -> n
  in
  let boxes = boxes |> List.mapi (fun i r -> i,r) |> List.sort cmp in
  let results = Array.make (List.length boxes) None in
  let t = List.fold_left (fun t (idx, box) ->
      let t, rect = insert t ~heuristic box in
      results.(idx) <- rect;
      t
    ) t boxes
  in
  (t, Array.to_list results)
