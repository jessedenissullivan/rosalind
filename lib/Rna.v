Require Import String Ascii Extraction.
Require Import ExtrOcamlString.
Require Import ExtrOcamlNatInt.
Open Scope char_scope.

Definition mk_rna (seq : list ascii) :=
    let mk_rna_internal seq :=
        match seq with
        | "T" => "U"
        | n   => n
        end in
    List.map mk_rna_internal seq.

Example sample_data : mk_rna (String.list_ascii_of_string "GATGGAACTTGACTACGTAAATT") = (String.list_ascii_of_string "GAUGGAACUUGACUACGUAAAUU"). Proof. intros. reflexivity. Qed.


Extraction "lib/rna.ml" mk_rna.