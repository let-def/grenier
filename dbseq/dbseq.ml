type +'a t =
  | T0
  | T1 of 'a * 'a t'
  | T2 of 'a * 'a * 'a t'
  | T3 of 'a * 'a * 'a * 'a t'
  | T4 of 'a * 'a * 'a * 'a * 'a t'

and +'a t' = ('a * 'a * 'a * 'a) t

let empty = T0

let rec cons : type a . a -> a t -> a t =
  fun a0 at ->
  match at with
  | T0 -> T1 (a0, T0)
  | T1 (a1, at') -> T2 (a0, a1, at')
  | T2 (a1, a2, at') -> T3 (a0, a1, a2, at')
  | T3 (a1, a2, a3, at') -> T4 (a0, a1, a2, a3, at')
  | T4 (a1, a2, a3, a4, at') -> T1 (a0, cons (a1, a2, a3, a4) at')

let rec flatten : type a . (a * a * a * a) t -> a t = function
  | T0 -> T0
  | T1 ((a0, a1, a2, a3), at) ->
    T4 (a0, a1, a2, a3, flatten at)
  | T2 ((a0, a1, a2, a3), aa1, at) ->
    T4 (a0, a1, a2, a3, T1 (aa1, at))
  | T3 ((a0, a1, a2, a3), aa1, aa2, at) ->
    T4 (a0, a1, a2, a3, T2 (aa1, aa2, at))
  | T4 ((a0, a1, a2, a3), aa1, aa2, aa3, at) ->
    T4 (a0, a1, a2, a3, T3 (aa1, aa2, aa3, at))

let rec drop : type a . int -> a t -> a t =
  fun n at ->
  if n = 0 then
    at
  else
    match n, at with
    | _, T0 -> T0
    | 1, T2 (_, a1, at) | 2, T3 (_, _, a1, at) | 3, T4 (_, _, _, a1, at) ->
      T1 (a1, at)
    | 1, T3 (_, a1, a2, at) | 2, T4 (_, _, a1, a2, at) ->
      T2 (a1, a2, at)
    | 1, T4 (_, a1, a2, a3, at) ->
      T3 (a1, a2, a3, at)
    | _, T1 (_, at) -> drop_rest (n - 1) at
    | _, T2 (_, _, at) -> drop_rest (n - 2) at
    | _, T3 (_, _, _, at) -> drop_rest (n - 3) at
    | _, T4 (_, _, _, _, at) -> drop_rest (n - 4) at

and drop_rest : type a . int -> (a * a * a * a) t -> a t =
  fun n at ->
  let n' = n / 4 in
  let at' = drop n' at in
  drop (n land 3) (flatten at')

let uncons : type a . a t -> (a * a t) option =
  fun at ->
  match at with
  | T0 -> None
  | T1 (a1, at') -> Some (a1, flatten at')
  | T2 (a1, a2, at') -> Some (a1, T1 (a2, at'))
  | T3 (a1, a2, a3, at') -> Some (a1, T2 (a2, a3, at'))
  | T4 (a1, a2, a3, a4, at') -> Some (a1, T3 (a2, a3, a4, at'))

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

let is_empty = function
  | T0 -> true
  | _ -> false

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

let seq_cons x xs () = Seq.Cons (x, xs)

let rec seq_flatten : type a. (a * a * a * a) Seq.t -> a Seq.t =
  fun seq () ->
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons ((a1, a2, a3, a4), seq') ->
    Seq.Cons (a1, seq_cons a2 (seq_cons a3 (seq_cons a4 (seq_flatten seq'))))

let rec to_seq : type a. a t -> a Seq.t = function
  | T0 -> Seq.empty
  | T1 (a1, at) -> seq_cons a1 (seq_flatten (to_seq at))
  | T2 (a1, a2, at) ->
    seq_cons a1 (seq_cons a2 (seq_flatten (to_seq at)))
  | T3 (a1, a2, a3, at) ->
    seq_cons a1 (seq_cons a2 (seq_cons a3 (seq_flatten (to_seq at))))
  | T4 (a1, a2, a3, a4, at) ->
    seq_cons a1 (seq_cons a2 (seq_cons a3 (seq_cons a4 (seq_flatten (to_seq at)))))

let rec seq_rev_flatten : type a. (a * a * a * a) Seq.t -> a Seq.t -> a Seq.t =
  fun seq k () ->
  match seq () with
  | Seq.Nil -> k ()
  | Seq.Cons ((a1, a2, a3, a4), seq') ->
    Seq.Cons (a4, seq_cons a3 (seq_cons a2 (seq_cons a1 (seq_rev_flatten seq' k))))

let rec to_rev_seq : type a. a t -> a Seq.t =
  fun t ->
  match t with
  | T0 -> Seq.empty
  | T1 (a1, at) ->
    seq_rev_flatten (to_rev_seq at) (seq_cons a1 Seq.empty)
  | T2 (a1, a2, at) ->
    seq_rev_flatten (to_rev_seq at) (seq_cons a2 (seq_cons a1 Seq.empty))
  | T3 (a1, a2, a3, at) ->
    seq_rev_flatten (to_rev_seq at) (seq_cons a3 (seq_cons a2 (seq_cons a1 Seq.empty)))
  | T4 (a1, a2, a3, a4, at) ->
    seq_rev_flatten (to_rev_seq at) (seq_cons a4 (seq_cons a3 (seq_cons a2 (seq_cons a1 Seq.empty))))
