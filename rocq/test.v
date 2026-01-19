Require Import Stdlib.Lists.List.
Import ListNotations.

Definition SomeProp (a: option nat) (n: nat) (d: nat) := a = Some n /\ n <> d.

Program Fixpoint apply_fight
      (stt: nat)
      (d: nat)
      (nxt: forall (md: option nat) (cs: nat) (fd: nat) (correct: SomeProp md cs fd), list (option nat * nat))
      (frr : list nat) 
      (COR: Forall (fun p => SomeProp (Some p) stt d) frr)
      : list (list (option nat * nat))
      :=
      match frr with
      | [] => []
      | x::xs =>
          let PP := _ : In x frr -> SomeProp (Some x) stt d in
            nxt (Some x) stt d (PP _) :: apply_fight stt d nxt xs _
      end.
Next Obligation.
  apply Forall_cons_iff in COR. tauto.
Qed.
Next Obligation.
  constructor. reflexivity.
Qed.
Next Obligation.
  apply Forall_cons_iff in COR. tauto.
Qed.

Definition counter (a: option nat) (n: nat) (d: nat) :=
      n + d + match a with Some x => x | None => 0 end.

Program Fixpoint fight (mode: option nat)
               (stt: nat) (d: nat)
               (correct: SomeProp mode stt d)
               {measure (counter mode stt d)}:
               list (option nat * nat) :=
  match mode with
  | Some 0 => 
      if Nat.eqb d 1
          then [(None, 0)]
          else [(Some 1, 0)]
  | Some lw =>
    let nstt := stt - 1 in
    let ndetails := d - 1 in
    let RR := _ : counter None nstt ndetails < counter (Some lw) stt d in
    let XX := _ : SomeProp None nstt ndetails in
      fight None nstt ndetails XX
  | None => []
  end.