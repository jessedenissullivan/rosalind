
(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val mk_rna : char list -> char list **)

let mk_rna seq =
  let mk_rna_internal = fun seq0 ->
    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun b b0 b1 b2 b3 b4 b5 b6 ->
      if b
      then seq0
      else if b0
           then seq0
           else if b1
                then if b2
                     then seq0
                     else if b3
                          then if b4
                               then seq0
                               else if b5
                                    then if b6 then seq0 else 'U'
                                    else seq0
                          else seq0
                else seq0)
      seq0
  in
  map mk_rna_internal seq
