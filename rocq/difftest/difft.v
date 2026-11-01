(* Extracts the model, to compare it with the Rust solver, see run.sh *)
Require Import chapters character rules proba mps combat.
Require book01 book02 book03 book04 book05.
Require Import Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith Stdlib.QArith.Qcanon.
Import ListNotations.
Require Import Stdlib.extraction.Extraction Stdlib.extraction.ExtrOcamlBasic Stdlib.extraction.ExtrOcamlNatInt.
Definition b1 := book01.chapters. Definition b2 := book02.chapters. Definition b3 := book03.chapters.
Definition b4 := book04.chapters. Definition b5 := book05.chapters.
Definition mk_stt (bk maxe cur cs: nat) (discs: list discipline) (ch: nat) (it: list (item * nat)) (fl: list flag) (prev: list (item * nat)) : Stt :=
  {| book := bk; maxendurance := maxe; curendurance := cur; sk := Z.of_nat cs; disciplines := set_from_list discs;
     chapter := ch; items := from_list it; flags := set_from_list fl; previtems := from_list prev |}.
Extract Constant cartwheel => "Cw.dist".
Extract Inductive option => "Cw.opt" ["Cw.OSome" "Cw.ONone"].
Extraction Language OCaml.
Set Extraction Output Directory "difftest/build".
Extraction "model.ml" mk_stt step expand_evasions b1 b2 b3 b4 b5.
