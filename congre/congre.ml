type mark = bool ref
let mark_new () = ref true
let mark_invalidate m = m := false; mark_new ()
let mark_is_valid m = !m

module rec PairTable : Hashtbl.S with type key = Def.epair =
  Hashtbl.Make(struct
    external ( === ) : 'a -> 'b -> bool = "%eq"

    type t = Def.epair

    let equal (Def.EPair (f1, x1)) (Def.EPair (f2, x2)) =
      (f1 === f2) && (x1 === x2)

    let hash (Def.EPair (f, x)) =
      Hashtbl.seeded_hash f.id x.id
  end)

and Def : sig

  type 'a node = {
    id: int;
    mutable value: 'a;
    mutable snapshot: 'a snapshot_;
    mutable repr: 'a node;
    mutable class_list: 'a node list;
    mutable use_list: 'a application list;
  }

  and 'a application = {
    (* func(arg) = result *)
    func: 'a node;
    arg: 'a node;
    result: 'a node;
  }

  and 'a snapshot_ = {
    sn_graph: 'a graph;
    mutable sn_mark: bool ref;
    mutable sn_changes: change list;
    mutable sn_next: 'a snapshot_;
  }

  and snapshot = Snapshot : 'a snapshot_ -> snapshot [@@ocaml.unboxed]

  and 'a kind =
    | Uninterpreted
    | Injective
    | Constructor
    | Construction of 'a node * 'a node

  and 'a var = {
    mutable var_value: 'a;
    mutable var_snapshot: snapshot;
  }

  and change =
    | Backup_class : {
        node: 'a node;
        value: 'a;
        snapshot: 'a snapshot_;
        repr: 'a node;
        class_list: 'a node list;
        use_list: 'a application list;
      } -> change
    | Backup_var : {
        var: 'a var;
        snapshot: snapshot;
        value: 'a;
      } -> change

  and 'a merger = repr:'a node -> 'a node -> unit

  and 'a graph = {
    mutable gr_counter: int;
    mutable gr_pending_equalities: ('a node * 'a node) list;
    mutable gr_pending_applications: 'a application list;
    mutable gr_merge : 'a merger;
    mutable gr_propagating : bool;
    gr_app_table: (mark * 'a application) PairTable.t;
    mutable gr_snapshot: 'a snapshot_;
    gr_root: 'a snapshot_;
  }

  type epair = EPair : 'a node * 'a node -> epair

end = Def

include Def

(* Creation of graph *)

let make ?(on_merge=fun ~repr:_ _ -> ()) () =
  let rec graph = {
    gr_counter = 0;
    gr_pending_equalities = [];
    gr_pending_applications = [];
    gr_merge = on_merge;
    gr_propagating = false;
    gr_app_table = PairTable.create 7;
    gr_snapshot = snapshot;
    gr_root = snapshot;
  } and snapshot = {
    sn_graph = graph;
    sn_changes = [];
    sn_next = snapshot;
    sn_mark = mark_new ();
  } in
  graph

let set_on_merge gr on_merge =
  gr.gr_merge <- on_merge

(* Variables *)

let var gr b = {
  var_value = b;
  var_snapshot = Snapshot gr.gr_snapshot;
}

let do_backup_var var sn =
  sn.sn_changes <-
    Backup_var {
      var;
      value = var.var_value;
      snapshot = var.var_snapshot;
    } :: sn.sn_changes;
  var.var_snapshot <- Snapshot sn

let backup_var var =
  let Snapshot sn = var.var_snapshot in
  if sn != sn.sn_graph.gr_snapshot then
    do_backup_var var sn.sn_graph.gr_snapshot

let get_var var =
  var.var_value

let set_var var b =
  backup_var var;
  var.var_value <- b

(* Backtracking for classes *)

let do_backup ec sn =
  sn.sn_changes <-
    Backup_class {
      node = ec;
      value = ec.value;
      snapshot = ec.snapshot;
      repr = ec.repr;
      class_list = ec.class_list;
      use_list = ec.use_list;
    } :: sn.sn_changes;
  ec.snapshot <- sn

let backup ec =
  assert (ec.repr == ec);
  let snapshot = ec.snapshot.sn_graph.gr_snapshot in
  if ec.snapshot != snapshot then
    do_backup ec snapshot

(* Snapshot management *)

let snapshot eg =
  let rec sn = {
    sn_graph = eg;
    sn_changes = [];
    sn_next = sn;
    sn_mark = mark_new ();
  } in
  eg.gr_snapshot.sn_next <- sn;
  eg.gr_snapshot <- sn;
  sn

let is_valid sn =
  sn.sn_next != sn || sn.sn_graph.gr_snapshot == sn

let restore (Snapshot sn) =
  if not (is_valid sn) then
    invalid_arg "Congre.restore: invalid snapshot";
  let undo_change = function
    | Backup_class { node; value; snapshot; repr; class_list; use_list } ->
      node.value <- value;
      node.snapshot <- snapshot;
      node.repr <- repr;
      node.class_list <- class_list;
      begin match class_list with
        | (x :: _) as xs when x.repr != node ->
          List.iter (fun node' -> node'.repr <- node) xs;
        | _ -> ()
      end;
      node.use_list <- use_list;
    | Backup_var {var; value; snapshot} ->
      var.var_value <- value;
      var.var_snapshot <- snapshot
  in
  let rec backtrack sn =
    if sn.sn_next != sn then backtrack sn.sn_next;
    List.iter undo_change sn.sn_changes;
    sn.sn_mark <- mark_invalidate sn.sn_mark;
    sn.sn_next <- sn;
    sn.sn_changes <- []
  in
  backtrack sn;
  sn.sn_graph.gr_snapshot <- sn

let invalid_snapshot =
  let graph = make () in
  let rec snapshot = {
    sn_graph = graph;
    sn_changes = [];
    sn_next = snapshot;
    sn_mark = ref true;
  } in
  Snapshot snapshot

(* Creations of new classes *)

let fresh eg value =
  eg.gr_counter <- eg.gr_counter + 1;
  let id = eg.gr_counter in
  let rec result = {
    id; value;
    repr = result;
    class_list = [result];
    use_list = [];
    snapshot = eg.gr_root;
  } in
  result

(* Maintain indexes *)

let rec app_lookup eg key =
  match PairTable.find_opt eg.gr_app_table key with
  | None -> None
  | Some (mark, result) when mark_is_valid mark -> Some result
  | Some _ ->
    PairTable.remove eg.gr_app_table key;
    app_lookup eg key

let app_lookup_set eg key result =
  PairTable.add eg.gr_app_table key (eg.gr_snapshot.sn_mark, result)

(* Propagating equations *)

let graph ec =
  ec.snapshot.sn_graph

let order_by_class_list a b =
  let c = List.compare_lengths a.class_list b.class_list in
  if c <= 0 then (a, b) else (b, a)

let add_equality eg s t =
  if s.repr != t.repr then
    eg.gr_pending_equalities <- (s.repr, t.repr) :: eg.gr_pending_equalities

let add_application eg app =
  eg.gr_pending_applications <- app :: eg.gr_pending_applications

let lookup_key func arg =
  EPair (func.repr, arg.repr)

let propagate_equality eg node ~repr =
  let merge_app eg acc app =
    let key = lookup_key app.func app.arg in
    match app_lookup eg key with
    | Some app' ->
      add_equality eg app.result app'.result;
      acc
    | None ->
      app_lookup_set eg key app;
      app :: acc
  in
  let set_repr repr acc node =
    node.repr <- repr;
    node :: acc
  in
  backup node;
  backup repr;
  let {class_list; use_list; _} = node in
  node.class_list <- [];
  node.use_list <- [];
  repr.class_list <- List.fold_left (set_repr repr) repr.class_list class_list;
  repr.use_list <- List.fold_left (merge_app eg) repr.use_list use_list

let propagate_application eg app =
  let {func; arg; result} = app in
  let key = lookup_key func arg in
  match app_lookup eg key with
  | Some app -> add_equality eg app.result result
  | None ->
    let func = func.repr in
    let arg = arg.repr in
    app_lookup_set eg key app;
    (* Printf.eprintf "adding binding for %s\n%!" (app_to_string app); *)
    backup func;
    backup arg;
    func.use_list <- app :: func.use_list;
    if func != arg then
      arg.use_list <- app :: arg.use_list

let rec propagate eg eqs =
  match eqs with
  | (a, b) :: pending ->
    if a.repr != b.repr then (
      let (node, repr) = order_by_class_list a.repr b.repr in
      match eg.gr_merge ~repr node with
      | exception exn ->
        eg.gr_pending_equalities <- eg.gr_pending_equalities @ eqs;
        eg.gr_propagating <- false;
        raise exn
      | () -> propagate_equality eg ~repr node
    );
    propagate eg pending
  | [] ->
    (* No more equations?
       Check that nothing has been added to the pending set during processing.
    *)
    propagate_applications eg

and propagate_applications eg =
  match eg.gr_pending_equalities with
  | (_ :: _) as pending ->
    eg.gr_pending_equalities <- [];
    propagate eg pending
  | [] ->
    match eg.gr_pending_applications with
    | [] -> eg.gr_propagating <- false
    | app :: apps ->
      eg.gr_pending_applications <- apps;
      propagate_application eg app;
      propagate_applications eg

let propagate caller eg =
  if eg.gr_propagating then
    invalid_arg (
      "Congre: this function (" ^ caller ^
      ") is unsafe to call during equality propagation (inside on_merge function)");
  eg.gr_propagating <- true;
  propagate eg []

let assert_graph eg ec msg =
  if eg != graph ec then invalid_arg msg

let assume_equal s t =
  let eg = graph s in
  assert_graph eg t "Congre.merge: classes belong to different graphs";
  add_equality eg s t

let assume_application func arg ~equal:result =
  let eg = graph func in
  let func = func.repr in
  let arg = arg.repr in
  let msg = "Congre.merge_app: classes belong to different graphs" in
  assert_graph eg func msg;
  assert_graph eg arg msg;
  let key = lookup_key func arg in
  match app_lookup eg key with
  | Some app -> add_equality eg app.result result
  | None -> add_application eg {func; arg; result}

let find_app cf cx =
  propagate "Congre.find_app" (graph cf);
  let key = lookup_key cf cx in
  match app_lookup cf.snapshot.sn_graph key with
  | Some eqn -> Some eqn.result.repr
  | None -> None

let is_valid (Snapshot sn) = is_valid sn

let same ec1 ec2 =
  let eg = graph ec1 in
  assert_graph eg ec2 "Congre.same: classes belong to different graphs";
  propagate "Congre.same" eg;
  (ec1.repr == ec2.repr)

let snapshot eg =
  propagate "Congre.snapshot" eg;
  Snapshot (snapshot eg)

let propagate eg =
  propagate "Congre.propagate" eg

let get_tag ec =
  ec.repr.value

let set_tag ec value =
  backup ec.repr;
  ec.repr.value <- value

let set_root_tag ec value =
  if ec.repr.snapshot != ec.snapshot.sn_graph.gr_root then
    invalid_arg "Congre.set_root_tag: node has already been modified";
  ec.repr.value <- value

let get_id node = node.id

let compare a b =
  Int.compare a.id b.id

let get_repr a = a.repr
