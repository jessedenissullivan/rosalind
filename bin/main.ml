open Printf
open Rosalind.Fib
(* let explode_string s = List.init (String.length s) (String.get s) *)
(* let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let str = really_input_string ic len in
  close_in ic;
  str *)
let _ =
  (* let input = 
    if (Array.length Sys.argv) > 1 then
      let path = Sys.argv.(1) in 
      printf "\nParsing input file: %s\n" path;
      let txt = read_file path in explode_string txt
    else explode_string "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" in *)
  let i1, i2 = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2) in
  printf "Output:\n%d\n" (mk_fib i1 i2)