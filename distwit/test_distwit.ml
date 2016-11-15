(* A very low quality generator, valid for a single process *)
module W = struct
  type t = int

  let fresh =
    let k = ref 0 in
    fun () -> incr k; !k

  let equal (a : int) b = a = b

  let hash = Hashtbl.hash
end

module X = Distwit.Make_unsafe(W)

let roundtrip (x : 'a) : 'a =
  Marshal.from_string (Marshal.to_string x []) 0

let match_after_roundtrip_0 f g =
  match f (roundtrip (g Not_found)) with
  | Exit -> assert false
  | Not_found -> true
  | _ -> false

let match_after_roundtrip_1 f g =
  match f (roundtrip (g Exit)) with
  | Not_found -> assert false
  | Exit -> true
  | _ -> false

exception Foo of string

let match_after_roundtrip_2 f g =
  match f (roundtrip (g (Foo "bar"))) with
  | Foo "bar" -> true
  | _ -> false

let id x = x

let () = (
  (* marshalling roundtrip doesn't preserve matching *)
  assert (match_after_roundtrip_0 id id = false);
  assert (match_after_roundtrip_1 id id = false);
  assert (match_after_roundtrip_2 id id = false);
  (* marshalling roundtrip with distributed witnesses should preserve matching *)
  assert (match_after_roundtrip_0 X.unwrap X.wrap = true);
  assert (match_after_roundtrip_1 X.unwrap X.wrap = true);
  assert (match_after_roundtrip_2 X.unwrap X.wrap = true);
)
