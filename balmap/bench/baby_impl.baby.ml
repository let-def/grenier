(* Baby is installed: it is compared too. *)
let set = Some (module Baby.W.Set.Make (Int) : Sigs.SET)
let map = Some (module Baby.W.Map.Make (Int) : Sigs.MAP)
