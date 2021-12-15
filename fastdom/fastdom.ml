(** Metadata associated to a graph node (of type 'a) *)

type 'a t = {

  node: 'a;
  (** Node the metadata applies to *)

  mutable index: int;
  (** Index of the node in postorder traversal:
      [-1] for invalid or unvisited nodes
      [max_int] for a marked node during traversal
      [n >= 0] for a valid and visited node
  *)

  mutable predecessors: 'a t list;
  (** List of node predecessors (used temporarily) *)

  mutable dom: 'a t;
  (** Dominator of this node *)

}

(** Public accessors *)

let node t = t.node
let dominator t = t.dom
let postorder_index t = t.index
let predecessors t = t.predecessors

(* We use [-1] index for identifying invalid or unvisited nodes *)
let is_valid node = node.index >= 0

(* Intersect set of nodes, using the encoding defined in the paper *)
let rec maximize ~target node =
  (*Printf.eprintf "maximize(%d,%d)\n" node.index target;*)
  if node.index < target
  then maximize ~target node.dom
  else node

let rec intersect b1 b2 =
  if b1 != b2 then
    let b1 = maximize ~target:b2.index b1 in
    let b2 = maximize ~target:b1.index b2 in
    intersect b1 b2
  else b1

(* Intersect immediate dominators *)
let rec update_idom = function
  | [] -> None
  | x :: xs ->
    if is_valid x.dom then
      let isect acc p = if is_valid p.dom then intersect p acc else acc in
      Some (List.fold_left isect x xs)
    else update_idom xs

(* Traverse and update dominators until a fixpoint is reached *)
let dominator_fixpoint nodes count =
  let changed = ref true in
  while !changed do
    changed := false;
    for i = count - 2 downto 0 do
      let node = nodes.(i) in
      match update_idom node.predecessors with
      | None -> ()
      | Some dom -> if dom != node.dom then (node.dom <- dom; changed := true)
    done
  done

(** Representation of a graph with nodes of type 'a *)
type 'a graph = {
  memoize: 'b. ('a -> 'b) -> ('a -> 'b);
  (** Memoize a function on nodes *)

  successors: 'b. ('b -> 'a -> 'b) -> 'b -> 'a -> 'b;
  (** Fold over successors of a node *)
}

(* Compute a postorder traversal:
   - associate tags to each node of a graph
   - number the tags
   - return an array of all tags in postorder *)
let postorder (type a) (graph : a graph) (start : a) =
  (* Sentinel value for undefined nodes *)
  let rec undefined =
    {node = start; index = -1; predecessors = []; dom = undefined}
  in
  (* A function to associate a `'a t` tag to each node of the graph *)
  let tag_of =
    let mk node = {node; index = -1; predecessors = []; dom = undefined} in
    graph.memoize mk
  in
  (* A vector to record all the tags *)
  let buffer = ref [|undefined; undefined|] in
  let mark tag = tag.index <- max_int in
  let record tag index =
    tag.index <- index;
    if index >= Array.length !buffer then (
      let buffer' = Array.make (index * 2) undefined in
      Array.blit !buffer 0 buffer' 0 (Array.length !buffer);
      buffer := buffer';
    );
    assert ((!buffer).(index) == undefined);
    (!buffer).(index) <- tag;
  in
  (* Visit a node in DFS, record post-order index *)
  let rec process_tag idx tag =
    if tag.index = -1 then (
      mark tag;
      let idx = graph.successors (process_successor tag) idx tag.node in
      record tag idx;
      (idx + 1)
    ) else
      idx
  (* Record predecessors when visiting successors *)
  and process_successor self index succ =
    let tag = tag_of succ in
    tag.predecessors <- self :: tag.predecessors;
    process_tag index tag
  in
  (* Begin post-order visit *)
  let start = tag_of start in
  start.dom <- start;
  let count = process_tag 0 start in
  (tag_of, Array.sub !buffer 0 count)

(* dominance = postorder traversal & dominators fixpoint *)
let dominance (type a) (graph : a graph) (start : a) =
  let tag_of, postorder = postorder graph start in
  (*Printf.eprintf "postorder: %d nodes\n" (Array.length postorder);
  Array.iteri (fun i tag ->
      Printf.eprintf "postorder[%d]: node=%d index=%d |predecessors|=%d dominator=%d\n" i (Obj.magic tag.node) tag.index
        (List.length tag.predecessors) tag.dom.index;
    ) postorder;*)
  dominator_fixpoint postorder (Array.length postorder);
  (postorder, tag_of)

let is_reachable = is_valid
