open Rosalind.Dna
let explode_string s = List.init (String.length s) (String.get s)
let _ =
  let input = explode_string "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" in
  match mk_count input with 
  | Count (a, c, g, t) -> Printf.printf "\n Count A:%d C:%d G:%d T:%d\n" a c g t
