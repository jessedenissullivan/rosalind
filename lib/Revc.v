Require Import String Ascii Extraction.
Require Import ExtrOcamlString.
Require Import ExtrOcamlNatInt.
Open Scope char_scope.

Definition mk_comp (seq : list ascii) :=
    let internal seq :=
        match seq with
        | "A" => "T"
        | "T" => "A"
        | "G" => "C"
        | "C" => "G"
        | n   => n
        end in
    let complement := List.map internal seq in
    List.rev complement.

Example sample_data : mk_comp (String.list_ascii_of_string "AAAACCCGGT") = (String.list_ascii_of_string "ACCGGGTTTT"). Proof. intros. reflexivity. Qed.


Extraction "lib/revc.ml" mk_comp.