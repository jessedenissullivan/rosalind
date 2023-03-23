
val add : int -> int -> int

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

type count =
| Count of int * int * int * int

val mk_count : char list -> count
