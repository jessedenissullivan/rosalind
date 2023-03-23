Require Import String Ascii List Extraction.
Require Import ExtrOcamlString.
Require Import ExtrOcamlNatInt.
Local Open Scope char_scope.

Inductive Count : Type :=
| count (a c g t : nat)
.

Definition mk_count seq :=
    let mk_count_internal acc seq :=
        (match acc with
        | count cnt_a cnt_c cnt_g cnt_t =>
            (match seq with
            | "A" => count (cnt_a + 1) cnt_c cnt_g cnt_t
            | "C" => count cnt_a (cnt_c + 1) cnt_g cnt_t
            | "G" => count cnt_a cnt_c (cnt_g + 1) cnt_t
            | "T" => count cnt_a cnt_c cnt_g (cnt_t + 1)
            | _          => count cnt_a cnt_c cnt_g cnt_t
            end)
        end) in
    List.fold_left mk_count_internal seq (count 0 0 0 0).

Example sample_data : mk_count (String.list_ascii_of_string "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") = count 20 12 17 21. Proof. intros. reflexivity. Qed.


Extraction "lib/dna.ml" mk_count.