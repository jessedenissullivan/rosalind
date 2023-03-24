
(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val add : int -> int -> int **)

let rec add = (+)

(** val mul : int -> int -> int **)

let rec mul = ( * )

(** val sub : int -> int -> int **)

let rec sub = fun n m -> Stdlib.max 0 (n-m)

(** val eqb : int -> int -> bool **)

let rec eqb n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> true)
      (fun _ -> false)
      m)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> false)
      (fun m' -> eqb n' m')
      m)
    n

(** val mk_fib : int -> int -> int **)

let rec mk_fib n k =
  let store = fun _ -> 0 in
  let add0 = fun k0 v store0 k' -> if eqb k' k0 then v else store0 k' in
  let internal =
    let rec internal k0 n0 store0 =
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> (0, (add0 0 0 store0)))
        (fun n' ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> ((Stdlib.Int.succ 0),
          (add0 (Stdlib.Int.succ 0) (Stdlib.Int.succ 0) store0)))
          (fun n1 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> ((Stdlib.Int.succ 0),
            (add0 (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0) store0)))
            (fun _ ->
            let (fn_1, store') = internal k0 n' store0 in
            let fn_2 = store' (sub n' (Stdlib.Int.succ 0)) in
            let fn = add fn_1 (mul k0 fn_2) in (fn, (add0 n0 fn store')))
            n1)
          n')
        n0
    in internal
  in
  fst (internal k (add n (Stdlib.Int.succ 0)) store)
