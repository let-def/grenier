type 'a element = int

module type Set = sig
  type t
  type nonrec element = t element
  val n : int
end

module Set (X : sig val n : int end) : Set = struct
  let n = X.n
  type t
  type nonrec element = t element
end

type 't set = (module Set with type t = 't)

let cardinal (type a) ((module O) : a set) = O.n

module Element = struct

  type 'a t = int

  let of_int (type a) ((module O) : a set) n =
    if n >= 0 && n < O.n then n
    else
      Printf.ksprintf invalid_arg
        "Finite.Set(%d).of_int %d: %d is not in [0; %d[" O.n n n O.n

  let to_int x = x

  let iter (type a) ((module O) : a set) f =
    for i = 0 to O.n - 1 do f i done

  let rev_iter (type a) ((module O) : a set) f =
    for i = O.n - 1 downto 0 do f i done

  let all_elements (type a) ((module O) : a set) =
    Array.init O.n (fun x -> x)

end

module type Map = sig
  module Domain : Set
  type codomain
  val element : Domain.t element -> codomain
end

type 'a map = (module Map with type codomain = 'a)

let iter_map (type a) ((module Map) : a map) (f : a -> unit) =
  Element.iter (module Map.Domain) (fun o -> f (Map.element o))

let map_of_array (type a) (a : a array) : a map =
  let module Result = struct
    module Domain = Set(struct let n = Array.length a end)
    type codomain = a
    let element i = a.((i : Domain.t element :> int))
  end in
  (module Result)

module Map_of_array (A : sig type codomain val table : codomain array end) =
struct
  module Domain = Set(struct let n = Array.length A.table end)
  type codomain = A.codomain
  let element i = A.table.((i : Domain.t element :> int))
end
