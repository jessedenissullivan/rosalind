
(** val add : int -> int -> int **)

let rec add = (+)

(** val fold_left :
    ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | [] -> a0
  | b :: t -> fold_left f t (f a0 b)

type count =
| Count of int * int * int * int

(** val mk_count : char list -> count **)

let mk_count seq =
  let mk_count_internal = fun acc seq0 ->
    let Count (cnt_a, cnt_c, cnt_g, cnt_t) = acc in
    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun b b0 b1 b2 b3 b4 b5 b6 ->
      if b
      then if b0
           then if b1
                then if b2
                     then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                     else if b3
                          then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                          else if b4
                               then Count (cnt_a, cnt_c, cnt_g,
                                      cnt_t)
                               else if b5
                                    then if b6
                                         then Count (cnt_a, cnt_c,
                                                cnt_g, cnt_t)
                                         else Count (cnt_a, cnt_c,
                                                (add cnt_g
                                                  (Stdlib.Int.succ
                                                  0)), cnt_t)
                                    else Count (cnt_a, cnt_c,
                                           cnt_g, cnt_t)
                else if b2
                     then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                     else if b3
                          then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                          else if b4
                               then Count (cnt_a, cnt_c, cnt_g,
                                      cnt_t)
                               else if b5
                                    then if b6
                                         then Count (cnt_a, cnt_c,
                                                cnt_g, cnt_t)
                                         else Count (cnt_a,
                                                (add cnt_c
                                                  (Stdlib.Int.succ
                                                  0)), cnt_g,
                                                cnt_t)
                                    else Count (cnt_a, cnt_c,
                                           cnt_g, cnt_t)
           else if b1
                then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                else if b2
                     then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                     else if b3
                          then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                          else if b4
                               then Count (cnt_a, cnt_c, cnt_g,
                                      cnt_t)
                               else if b5
                                    then if b6
                                         then Count (cnt_a, cnt_c,
                                                cnt_g, cnt_t)
                                         else Count
                                                ((add cnt_a
                                                   (Stdlib.Int.succ
                                                   0)), cnt_c,
                                                cnt_g, cnt_t)
                                    else Count (cnt_a, cnt_c,
                                           cnt_g, cnt_t)
      else if b0
           then Count (cnt_a, cnt_c, cnt_g, cnt_t)
           else if b1
                then if b2
                     then Count (cnt_a, cnt_c, cnt_g, cnt_t)
                     else if b3
                          then if b4
                               then Count (cnt_a, cnt_c, cnt_g,
                                      cnt_t)
                               else if b5
                                    then if b6
                                         then Count (cnt_a, cnt_c,
                                                cnt_g, cnt_t)
                                         else Count (cnt_a, cnt_c,
                                                cnt_g,
                                                (add cnt_t
                                                  (Stdlib.Int.succ
                                                  0)))
                                    else Count (cnt_a, cnt_c,
                                           cnt_g, cnt_t)
                          else Count (cnt_a, cnt_c, cnt_g, cnt_t)
                else Count (cnt_a, cnt_c, cnt_g, cnt_t))
      seq0
  in
  fold_left mk_count_internal seq (Count (0, 0, 0, 0))
