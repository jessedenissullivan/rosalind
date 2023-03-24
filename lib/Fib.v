Require Import Nat Extraction.
Require Import ExtrOcamlNatInt.

Fixpoint mk_fib (n k : nat) : nat :=
    let store := (fun k => 0) in
    let add k v store := (fun k' => if k' =? k then v else store k') in
    let fix internal k n store : nat * (nat -> nat):=
        match n with
        | 0 => (0, add 0 0 store)
        | 1 => (1, add 1 1 store)
        | 2 => (1, add 2 1 store)
        | S n' => 
            let (Fn_1, store') := internal k n' store in 
            let Fn_2 := store' (n' - 1) in 
            let Fn := Fn_1 + k * Fn_2 in
            (Fn, add n Fn store')
        end in
    fst (internal k (n + 1) store).

Example sample_data : mk_fib 5 3 = 19. Proof. intros. unfold mk_fib. simpl.  reflexivity. Qed.

(* Extraction "lib/fib.ml" mk_comp. *)