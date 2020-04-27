type +'a t =
  | T0
  | T1 of 'a * 'a t'
  | T2 of 'a * 'a * 'a t'
  | T3 of 'a * 'a * 'a * 'a t'
  | T4 of 'a * 'a * 'a * 'a * 'a t'

and +'a t' = ('a * 'a * 'a * 'a) t

let empty = T0

let rec add : type a . a -> a t -> a t =
  fun a0 at ->
  match at with
  | T0 -> T1 (a0, T0)
  | T1 (a1, at') -> T2 (a0, a1, at')
  | T2 (a1, a2, at') -> T3 (a0, a1, a2, at')
  | T3 (a1, a2, a3, at') -> T4 (a0, a1, a2, a3, at')
  | T4 (a1, a2, a3, a4, at') -> T1 (a0, add (a1, a2, a3, a4) at')

let rec get : type a . int -> a t -> a =
  fun n at ->
  match n, at with
  | _, T0 -> raise Not_found
  | 0, (T1 (a0,_) | T2 (a0,_,_) | T3 (a0,_,_,_) | T4 (a0,_,_,_,_)) -> a0
  | 1, (T2 (_,a1,_) | T3 (_,a1,_,_) | T4 (_,a1,_,_,_)) -> a1
  | 2, (T3 (_,_,a2,_) | T4 (_,_,a2,_,_)) -> a2
  | 3, (T4 (_,_,_,a3,_)) -> a3
  | n, (T1 (_, at)) -> get' (n - 1) at
  | n, (T2 (_, _, at)) -> get' (n - 2) at
  | n, (T3 (_, _, _, at)) -> get' (n - 3) at
  | n, (T4 (_, _, _, _, at)) -> get' (n - 4) at

and get' : type a . int -> a t' -> a =
  fun n at ->
  let n' = n lsr 2 in
  let (a0, a1, a2, a3) = get n' at in
  match n land 3 with
  | 0 -> a0
  | 1 -> a1
  | 2 -> a2
  | _ -> a3

let rec update : type a . a t -> int -> (a -> a) -> a t =
  fun at n u ->
  match n, at with
  | _, T0 -> raise Not_found
  | 0, T1 (a0, at) -> T1 (u a0, at)
  | 0, T2 (a0, a1, at) -> T2 (u a0, a1, at)
  | 0, T3 (a0, a1, a2, at) -> T3 (u a0, a1, a2, at)
  | 0, T4 (a0, a1, a2, a3, at) -> T4 (u a0, a1, a2, a3, at)
  | 1, T2 (a0, a1, at) -> T2 (a0, u a1, at)
  | 1, T3 (a0, a1, a2, at) -> T3 (a0, u a1, a2, at)
  | 1, T4 (a0, a1, a2, a3, at) -> T4 (a0, u a1, a2, a3, at)
  | 2, T3 (a0, a1, a2, at) -> T3 (a0, a1, u a2, at)
  | 2, T4 (a0, a1, a2, a3, at) -> T4 (a0, a1, u a2, a3, at)
  | 3, T4 (a0, a1, a2, a3, at) -> T4 (a0, a1, a2, u a3, at)
  | n, T1 (a0, at) -> T1 (a0, update' at (n - 1) u)
  | n, T2 (a0, a1, at) -> T2 (a0, a1, update' at (n - 2) u)
  | n, T3 (a0, a1, a2, at) -> T3 (a0, a1, a2, update' at (n - 3) u)
  | n, T4 (a0, a1, a2, a3, at) -> T4 (a0, a1, a2, a3, update' at (n - 4) u)

and update' : type a . a t' -> int -> (a -> a) -> a t' =
  fun at n u ->
  let n' = n lsr 2 in
  let u = match n land 3 with
    | 0 -> (fun (a0,a1,a2,a3) -> (u a0,  a1,  a2,  a3))
    | 1 -> (fun (a0,a1,a2,a3) -> (  a0,u a1,  a2,  a3))
    | 2 -> (fun (a0,a1,a2,a3) -> (  a0,  a1,u a2,  a3))
    | _ -> (fun (a0,a1,a2,a3) -> (  a0,  a1,  a2,u a3))
  in
  update at n' u

let set n x t = update t n (fun _ -> x)

let rec length : type a . a t -> int =
  fun at ->
  match at with
  | T0 -> 0
  | T1 (_, at) -> 1 + 4 * length at
  | T2 (_, _, at) -> 2 + 4 * length at
  | T3 (_, _, _, at) -> 3 + 4 * length at
  | T4 (_, _, _, _, at) -> 4 + 4 * length at

(* minimal bench, adding elements:
   let () =
     let i = int_of_string Sys.argv.(1) in
     let j = int_of_string Sys.argv.(2) in
     let time = Sys.time () in
     for j = 1 to j do
      let v = ref T0 in
      for i = 1 to i do
        v := add i !v
      done
     done;
     let time = Sys.time () -. time in
     Printf.printf "adding %d elements %d times took %.03fs (%.03fs per pass)\n"
      i j time (time /. float j)
*)
