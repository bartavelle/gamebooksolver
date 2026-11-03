(* The five books as they are played, with the rounds of the fights added. *)
Require Import chapters rules.
Require book01 book02 book03 book04 book05.
Require Import Stdlib.Lists.List.

Definition book1 := expand_evasions 1 book01.chapters.
Definition book2 := expand_evasions 2 book02.chapters.
Definition book3 := expand_evasions 3 book03.chapters.
Definition book4 := expand_evasions 4 book04.chapters.
Definition book5 := expand_evasions 5 book05.chapters.
