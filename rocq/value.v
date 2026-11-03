(* The value of a state: the best chance of winning, given the value of finishing the book. Each theorem about an
   optimization compares values, so that it says something about the results and not about the way they are computed. *)
Require Import character.
Require Import chapters.
Require Import proba.
Require Import rules.
Require Import Stdlib.Lists.List.
Require Import Stdlib.QArith.Qcanon.

Open Scope Qc_scope.

(* The value of a state, in a book where finishing with a state is worth score. No choice is worth nothing. The fuel is
   the depth of the search: the books have no cycle, so that enough of it gives the true value, and the theorems below
   hold for any amount. *)
Definition qmax (a b: Qc) : Qc := if Qclt_le_dec a b then b else a.

Definition best (l: list Qc) : Qc := fold_right qmax 0 l.

Fixpoint value (chapters: list (cid * decision)) (score: Stt -> Qc) (fuel: nat) (stt: Stt) : Qc :=
  match fuel with
  | O => 0
  | S f => best (map (ev (fun ns =>
      match ns with
      | Lost => 0
      | Won s => score s
      | New s => value chapters score f s
      end)) (step chapters stt))
  end.
