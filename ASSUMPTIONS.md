# Assumptions

Everything the Rust solver and its results rely on that has not been proved from the rules of the five books. Each
entry says what is assumed, why it can be trusted so far, and where it lives. New assumptions are added here as they are
made; an entry that is settled stays, with its new status, so that the reasoning is not lost.

First written on 2026-11-02, from a review of the session that audited `gamebooksolver-base`.

## Status

| Status | Meaning |
|---|---|
| **Decided** | A choice made by the project owner. It may still need a proof, which the entry then says |
| **Proved** | Has a Rocq proof, on the terms the entry gives |
| **Tested** | Solves agree, on the sample the entry gives. This is evidence, not proof |
| **Unverified** | Nothing has been run against it |
| **Open** | A question that needs a decision, or that a result depends on |

"Win = 1" below means a test scored a win as 1 instead of using the next book's values. Such a test shows a book on its
own, not the chained result. Scores are probabilities: "0.15 points" means 0.0015.

## 1. What is being solved

- **S1. The goal is the best chance of finishing all five books.** Each book is scored with the value of the next book,
  starting from Book 5. **Decided.**
- **S2. One reference character.** All the cases in `helper.py` have 20 Endurance, 10 Combat Skill and Weapon Skill with
  a Sword (`2010SW`). This is the weakest possible roll. Conclusions may not carry over to other characters, and other
  rolls or weapon skills are not solved. **Unverified.**
- **S3. Mind Blast and Weapon Skill are always learned.** `helper.py` only varies the other disciplines. A sweep of the
  discipline sets found one exception, Healing, which was then added (see C5). **Tested**, on a sample, win = 1.
- **S4. The next-book lookup assumes the best legal start.** `load_results` takes the best case of the next book with at
  most one new discipline, so the player is assumed to pick the discipline and the kit that turn out best. **Decided**
  by the design of the chain.
- **S5. Only the Rust matters.** The Haskell in `src/` and `app/` is legacy and is not looked at. **Decided.**
- **S6. The web app and wasm are out of scope for now.** The wasm crate is not compiled, and the crate does not build
  without the `rug` feature. **Decided.**

## 2. What crosses from one book to the next

`get_boundary` in `gamebooksolver-base/src/lonewolf/data.rs` lists what is kept. Anything else is treated as having no
effect on later books.

- **C1. Permanent Combat Skill losses are dropped.** Book 1 c236 and Book 3 c55 flags are not carried, on the idea that
  those paths are not part of the solution. Turning them into a loss changed nothing on 122 characters, but they would
  matter if used (Book 2 falls from 0.838 to 0.780, Book 5 from 0.799 to 0.404). **Decided**; proof wanted in Rocq.
  The proof must hold with next-book values, as the tests were win = 1. The two cases differ. Book 3 c335 offers c55 or
  c182, and c55 is 5 Endurance and the loss, then c182, so a proof is a dominance: more Endurance and more skill are never
  worse. That needs fights to be monotone, and the rest of Books 3 to 5 not to reward less of either, including `OnDamage`
  fights and conditions on Endurance. Book 1 c9 forces c236 when the Vordak Gem is held, so there is no choice to compare at
  that chapter, and a proof has to go back to taking the gem, which R8 makes automatic.
- **C2. These items and flags have no effect across books:** Crystal Star Pendant, Onyx Medallion, Blue Stone Triangle,
  Firesphere, Rope, the Gorn Cove flag. **Tested**, on a few characters each, win = 1. The medallion and the triangle
  were only tested from c50 and c150. Book 5 does read `ReceivedCrystalStarPendant`, at c221, c288 and c382, and nothing sets
  it. The branches can't matter: c336 and c275 have the same outcomes, and c399 and c294 both go to c323. Proving this needs
  a relation between chapters with the same content, which `simulation.v` does not have yet. The generic items are numbered per
  book, so the same `GenSpecial` means something else in another book: Rocq can't check that a later book never mentions an
  item, only that its transcription doesn't. Checking the text of the later books is a separate step.
- **C3. `FLGKnowledge01` has no effect**, and was removed from `B03OPTIONS`. **Tested**, on 5 characters, win = 1.
- **C4. Gold does not cross Book 2 to 3, Book 3 to 4 or Book 4 to 5.** See G4 and G5 for the effect on Book 5. **Proved**
  for Book 3 from 10 crowns (`book3_gold_above_ten` in `rocq/proofs/gold_books.v`) and for all of Book 4 (`book4_gold_same` in
  `rocq/proofs/gold_books.v`), for any score of the end of the book that does not read the gold. Starting Book 3 with less than 10
  crowns is not covered, and Book 3 c152 reads up to 10. In Book 3, gold had no effect at 0, 15 and 30 on one character, which
  is **Tested** only. The Book 4 branch of `filter_final_state`, which maps gold to 0 or 15, is dead code as a result.
- **C5. Healing can be learned as late as Book 4.** Cases were added after a rough product of book scores favoured it
  (0.558 against 0.546). **Tested** with chained values on one comparison, using older data.
- **C6. Book 3 and Book 4 kits with the Sommerswerd or a Sword cover what matters.** Book 2 can't be finished without the
  Sommerswerd, so Book 3 always has it. Having it or not made about no difference in Books 3 and 4. **Tested.**

## 3. Which cases are solved

All from `helper.py`. These restrict which starts exist; a better start that is not listed would be missed.

- **K1. Book 1:** Sword, Helmet, two Meals, Body Armor, Potion4Hp or 12 extra gold, with 0 to 9 gold. Each roll of the
  book's random item is its own case. **Decided.**
- **K2. Book 2 starts with a Sword and a Shield,** plus Helmet and Body Armor when carried. `B02BASE` also lists Body
  Armor, Meals and Laumspur instead of the Shield, but those are not generated. **Unverified**: no test compares them.
- **K3. Book 3 kits:** Sommerswerd with two of Laumspur, Body Armor and Meals. **Decided**, not compared in full.
- **K4. Book 4 kits, with 0 gold:** Sword and Dagger, or Sommerswerd and Sword, with fixed potions, Shield, Body Armor and
  Meals. **Decided**, not compared in full.
- **K5. Book 5 kits:** Sword or Sommerswerd, with two Meals, a Shield and Laumspur. **Decided**, not compared in full.
- **K6. A Silver Helm and a Helmet are not held together.** The book says to discard one. Book 3 c308 follows this.
  **Decided** from the book text.

## 4. Gold

- **G1. The Belt Pouch holds 50 crowns.** From the books. Excess is lost. **Decided.**
- **G2. Book 2 is solved for every starting gold from 10 to 50, for every discipline set.** This replaced a cap of 27,
  which understated 20 characters by up to 0.0006 points and overstated one by 0.0004. **Decided.**
- **G3. More gold is not always better.** This is a fact, not an assumption, and it limits what can be assumed elsewhere.
  Book 2 s168 reads "If you can pay for the room, turn to 314. If you cannot, turn to 25", and near death, having no
  crowns is the better position (0.008 against 0 at 3 Endurance). No proof may assume gold is monotone in Book 2.
  Whether it is monotone in Book 5 is **Unverified**.
- **G4. Book 5 starts with 50 gold**, the default of `buildsol-rust.py` for that book, and gold is not carried from Book 4.
  This assumes a player who collected gold through Book 4, see G6. It is the cheapest solve, and it gives the true score of
  any player with at least the amount of G5. A player with less would be overstated, by about 0.15 points at 10 crowns with
  a Sword kit. **Decided** on 2026-11-02; a proof that the amounts of G5 make no difference is wanted later.
- **G5. In Book 5, gold above 20 changes nothing.** Chapter 154 sells a Strength Potion, Laumspur and a Potion2Hp for 12
  crowns, and the 10-gold score of HU matches the 9-crown option exactly, so most of the effect stops at 12: HU and TR
  match their 40-gold scores to 9 digits at 15. MO has the same exact fraction at 15 and 50. There is a tiny effect above
  that: on the edited book of E1, MS scores 0.583265136440 at 15 and 16 gold, 0.583265136985 at 17 to 19, and
  0.583265137192 at 20 and 50, a total of about 0.00000008 points. A first guess of 12 for all characters was wrong.
  **Tested** on six characters. 6S has only been solved unedited at 10, 40 and 50, and values below 15 only at 10. Proving
  this is an obligation if Book 5 relies on it. A static proof can't reach this: the need table of `rocq/proofs/gold_needs.v` and Rust
  both give 50 at the start of Book 5, adding up what a path can spend. What is left between 20 and 50 depends on whether
  the spending is worth it, so a proof has to use solved values, or a finite run of every case at every amount.
- **G6. Reaching 50 gold at the start of Book 5 is realistic.** A route can pick up 30 crowns in Book 4 (42 for the
  character lacking Camouflage and Tracking), on top of a 10 to 19 pouch. That is an upper bound: disciplines were
  checked, item and flag requirements were not, and survival was ignored. **Tested** by a scratch script only.
- **G7. Starting gold rolls are uniform and independent.** Only Book 1 to 2 averages the 10 to 19 pouch. **Decided.**

## 5. Rules as interpreted

Readings of the books, and limits of what the model lets a player do. Rust offers fewer choices than the book in several
places, so each of these could lower a score if the missing choice were better.

### Games of chance

- **R1. Cartwheel: the free token is played first.** It can't be cashed and could otherwise go unused. **Decided.**
- **R2. Cartwheel: "win 8" and "win 5" are profits,** and the stake is kept. **Decided.**
- **R3. Cartwheel: staking one crown is optimal.** **Proved** in `rocq/proofs/cartwheel.v`, for any exit values, either payout
  and any cap, using classical axioms. The result is about the game alone. The link to the Rust table is not proved, and
  `rocq/model/rules.v` has the table as a `Parameter`.
- **R4. Cartwheel: playing up to a target is enough, with targets from 20 to 30.** Other stopping rules may do better, as
  gold is not monotone. Single-hole stopping rules and targets up to 40 gave identical scores on 11 Book 2 starts. Seed 9
  showed +1.49e-5 against a baseline without evasion, never settled. **Tested**, with one open result.
- **R5. Portholes (Book 2 c308) is not played.** It loses 0.09 crowns per throw on average. An exact model of every
  stopping rule gave the same score in all 36 comparisons (6 characters, 10 to 27 gold), and the same exact fraction on
  the one case checked. **Decided** on 2026-11-02; a static proof is wanted later. Reached only from c53 through c154,
  and the best path did not visit either in the one exact case inspected. Showing that the gold is not read afterwards is
  not available: 21 chapters that can be reached from c197 read or spend gold, including the Cartwheel. A proof has to
  compare values, and G3 rules out assuming that more gold is better.

### Choices the model leaves out

- **R6. Weapons can be discarded at any time, like Backpack Items.** The books state this for Backpack Items, and their
  weapon rule only sets the limit of two. This reading was **Decided** on 2026-11-02. The model does not offer it in
  general: a choice is only added where it is relevant, see R20.
- **R20. Discarding a weapon only matters when dropping the Sommerswerd at Book 5 c289,** so that is the only place the
  choice is offered. Holding it there leads to c311 and c350, which destroy it and cost 3 Endurance, where the others go
  to c219. **Decided**; a proof is wanted that this is the only place it makes a difference. The evidence is narrow: five
  Sommerswerd cases and one Sword case at 50 gold score the same with the choice only at c289, with it at all four
  branches on the sword (c212, c264, c289, c333), and on the five that have an unedited score, without it. Without the
  choice, the MO and MS solutions swapped the sword for a Dagger at c388 and reached c289 without it. With it, MO reaches
  c289 holding the sword in 19 recorded states, for the same exact score. Lower gold, and discarding any other weapon or at
  any other time, were not tested. **Tested** on that sample.
- **R7. Backpack Items can't be discarded freely** either, although the books allow it. **Unverified**: the experiments
  ran out of memory.
- **R8. An item is always taken when there is room, and gold is always taken.** The book makes it a choice. Since more gold
  can be worse (G3), refusing could help. Declining gold changed nothing on 20 of 24 starts, with only 2 of 6 completed in
  Book 4. **Tested** on that sample, before later fixes.
- **R9. One of each item per offer.** Book 5 s27 and others let a player buy several, and `Cansell` sells only one
  (`TODO` in `choices.rs`). An experiment allowing repeated purchases of anything but weapons and Special Items, and
  repeated sales, found no change on 23 of 24 starts, Books 1 to 4. Repeated weapon purchases were not tested. **Tested**
  on that sample, before later fixes.
- **R10. No duplicate weapons.** A weapon is one bit in the state. As a result, a character who already has a Sword can't
  take Captain D'Val's at Book 4 c222. **Decided**: state size.
- **R11. "Has a Torch" is one bit,** in Book 4, and a single spare is treated as enough for c117. **Decided**: state size.
- **R12. Potions are only offered at set times:** before a fight, after it, and only the smallest that doesn't waste
  healing. No change on 23 of 24 starts, Books 1 to 4. **Tested.**
- **R13. Eating in Book 1 follows a fixed order:** Laumspur if at least 3 Endurance is missing, then a Meal, then Laumspur.
  The book gives a choice, and Laumspur "may also be consumed at any other time". **Unverified.**
- **R14. Curing poison is forced when Laumspur is held.** No change in the same experiments as R12. **Tested.**

### Transcription

- **R15. The JSON differs from the XML on purpose, to avoid cycles.** Keep this when sound. **Decided.** Restoring these
  would create a cycle: Book 1 c36, c70, c125, c290, c323; Book 2 c47, c136, c172; Book 3 c181; Book 5 c67, c373. Whether
  each preserves the game is **Unverified**.
- **R16. Other JSON differences from the XML are assumed sound.** Book 2 c160, c186, c200, c238, c265, c302; Book 3 c10,
  c146, c164, c180. They create no cycle, and have not been reviewed. Book 2 c302 gave the same score on
  five starts when set back to the book's 244, with 15% to 22% more states. **Open.**
- **R23. Book 2 c40 and Book 3 c10 offer their item instead of forcing it.** **Decided** on 2026-11-03. The book gives the
  potion of c40 and the graveweed of c10 outright. They used to be added with no check, which could make 9 Backpack Items,
  or a potion without a Backpack at c40, against the limit of 8. They are now `CanTake`, so a full Backpack means dropping
  something or refusing, and no Backpack means no potion at c40. The graveweed goes through the added chapter c422, which
  has no effect of its own. Refusing an item is a choice the book doesn't list, and is assumed never to help. Solutions of
  Book 2 and Book 3 made before this may differ when a Backpack was full at these chapters. **Unverified.**
- **R21. Book 5 c226 is sound.** Its attack option goes to c401, which is c334 with the first 2 rounds without damage, as
  the text of c226 says. c334 itself is also reached from c297 and c360, without that rule. **Decided** after review.
- **R24. `ValidState` in `rocq/model/character.v` says what a character can be while playing a book.** Combat Skill 10 to 19,
  Endurance 20 to 29, `book + 4` disciplines with one Weapon Skill at most, and limits on what is carried, for the stored
  equipment too. That playing keeps it true is **Proved** for the five books, for any depth and any Cartwheel table
  (`play_keeps_valid` and `bookN_keeps_valid` in `rocq/proofs/valid_play.v`), and `rocq/difftest/valid.sh` tests it on the
  extracted model. The proof needs each book to respect a computed check (`rocq/proofs/valid_check.v`): items in the
  Backpack or Weapon slots, Helmets and the Backpack are never given directly, apart from the swap of Book 4 c22, and buying
  never needs two slots. That a valid end of a book gives a valid start of the next is **Unverified**, and so is that the
  solver starts from valid states. `flatten_dec_full`, that the probabilities sum to 1, is not proved.
- **R22. Book 5 c55 follows the book since 2026-11-02.** It goes to c162 or c23, then c114, c90, the baths, c71 and c54. It
  used to go straight to shop choices, skipping the steam damage, the death at c77 and the healing oil. Restoring this
  created no cycle, added about 0.002% of states, and kept the score of all 8 cases tested at 50 gold to 9 digits.
  **Decided.** Results solved before commit `e3cda37e` used the old route.
- **R25. Book 4 was compared with `project-aon/en/xml/04tcod.xml` on 2026-11-03,** which is the authoritative text, and now
  follows it. All 350 sections exist, and their destinations and fight statistics match. The sentences with a game term of
  230 sections were read next to the JSON. The other 120 have no such term, and their prose was not read. **Decided.** Fixed:
  c12 (3 Meals, no Backpack), c77 (1 Endurance per round, with the new `WeakMindblast`), c94, c158 and c342 (the Backpack is
  lost too), c122 (tests Mindshield, not Mind Blast), c137 (restores 6), c167 (1 Meal and a Shovel), c194 and c234 (2 more
  rounds with Mind Over Matter), c202 (the six guards are one combat that can be evaded at any time), c234 and c280 (hold
  their fight and their loot, and an extra section gave a point of Healing), c260 (stops after 3 rounds), c261 (8 crowns and
  a Meal), c269 (a Shovel, read as `BGenCounter 4` from the order of c213), c316 (2 less Combat Skill), c339 (a Meal to eat),
  and c343 (12 Endurance counts as low). The Wildlands are entered at c25 and c171 and left at c120, and Hunting can't be
  used there, as they say: c19, c78, c141, c188, c324 and c339.
  Kept, as they can't change a score: the Whip, Hourglass and Rope are not offered and nothing uses them; the Iron Key is
  not needed at c118, which is only reached from c268 that offers it; the Brass Key of c152 is not offered, and that route
  never reaches the door; c322 allows both a Pick and a Shovel, where only one is ever useful; c22 keeps the
  `BGenCounter 2`, and neither it nor `BGenCounter 1` is read after it; the Pick and Shovel removed at c54, c115, c173, c224
  and c244 are not read later, and could be discarded anyway.
- **R26. Having a discipline can force a branch.** In Book 4, c21, c29, c40, c51, c87, c92, c113, c200, c289, c315 and c335
  send a character with the discipline to its section, where the book also lists options that anyone may take, and c24, c118,
  c224 and c270 leave the choice open. Opening all of them gave the same exact fraction on 8 Book 4 cases (TR.MO, MS.HU, CA.6S
  and MS.CA, each with Sword and Dagger or Sommerswerd and Sword, and no optional item), with Book 5 values. **Tested** on that
  sample. The other books have not been looked at.
- **R17. Book 3's Padded Leather Waistcoat (+2 Endurance) is not modelled,** although it is a starting pick. **Open.**
- **R18. The best weapon is used automatically in a fight.** The book lets the player choose. Book 5 c253 encodes it by
  hand, with +3 for using the Jewelled Mace instead of a Weapon Skill weapon. **Unverified.**
- **R19. Some chapters needed interpretation:** Book 2 c32 (ticket needs the fare), Book 3 c152, Book 4 c40, c54, c258, c173
  and s22, Book 5 c127, c257, c357 and c452. Each was checked against the book text when fixed. **Decided.**

## 6. Optimizations

These must not change a result, and each needs a proof against a rules model on the five books. Rocq models the Rust with
the optimizations left out, and agreed with Rust on 4,391 cases, which checks the model and not the optimizations.

The proofs are about `value` in `rocq/model/value.v`, the expectimax of a book as played by `rocq/model/rules.v`. `value_same` in `rocq/proofs/simulation.v` gives the same
value to states that a `Tracker` can't tell apart, for any depth of search. They rely on functional extensionality and on
the `cartwheel` parameter of `model/rules.v`, and on no `Admitted`. They inherit what `model/rules.v` assumes, which is section 5. They
are about the model: the link to the Rust is the comparison above, and for tables, a mirror of the Rust.

Book 4 check, on 3 cases (20 Endurance, skill 10, no gold; TR.MO and MS.CA with Sword and Dagger, TR.HU with Sommerswerd and
Sword), with Book 5 values: turning off O1, O2, O3, O4 or O5 alone gave the same exact fraction on all 3. Turning all five off
gave the same on the 2 Sword and Dagger cases; TR.HU stopped at the 45 GB limit after 52 minutes and has no result. The switch
for O3 only covers the weapon analysis. Without O1 to O5, the two finished cases took about 5.5 times as long and 14 to 18 times
the memory.

- **O1. Plain weapons are interchangeable.** A weapon with no skill, not magical, and not named in reachable chapters is
  treated as any other (`WeaponUse`, `useful_drops`, `canonical`). No mismatch on 42 + 42 pairs. **Tested.**
- **O2. What can't be read again is cleared** (`Reads`): gold, generic items, and story flags. No mismatch on 15 pairs.
  **Tested.** **Proved** for a flag that a whole book does not read: `flag_unread_same` and the lists in `rocq/proofs/flag_books.v`. A
  flag that is read in some chapters and cleared before them, chapter by chapter as `Reads` does, and the generic items,
  are not proved. The tool for flags is there: `flag_value_same` takes a table per chapter.
- **O3. Character specialisation** removes branches needing a discipline the character lacks. **Tested** on all 24
  characters of Books 1 to 4.
- **O4. Gold above what can still be spent is cleared** (`gold_needs`). Must hold given G3: it only cuts gold that no
  later test or payment can tell apart. **Proved** in `rocq/proofs/gold_needs.v` and `rocq/proofs/gold_books.v` for Books 1 to 5, against a table
  checked on the books, for the gold that is held and the gold that is stored, as Rust clamps both to the same need. c14
  gives outcomes that are not the same when the held gold differs, so it is compared by value (`retrieve_top`). This needs
  the retrieval to be the first thing a chapter does, which is what c14 does; one found deeper in a decision would need all
  the gold. A Python mirror of `GoldNeeds` gives the same table on all 391 chapters of Book 2, all 400 of Book 3 and all 452
  of Book 5, and 0 everywhere in Book 4; the mirror was compared and not the Rust itself. The 326 chapters that the proof
  adds for the evasions of Book 5 were not compared. Book 1 keeps all the gold in Rust.
- **O5. Forced chapters can be elided** (`elide.rs`), replaying what they do. On and off: 44 of 48 cases identical, 0
  different, 4 not compared because they ran out of memory. **Tested.**
- **O6. Round-by-round evasion matches a whole fight** when no evasion is taken. The rounds are assumed acyclic because each
  one changes something. The Rocq and Rust comparison only shows that both implement rounds the same way, not that the
  rounds equal a whole fight. In Book 4, on the 3 cases above, a fight in rounds where evading loses the book and the same fight
  without evasion gave the same exact fraction: 53.98%, 63.75% and 80.62%. **Tested** on that sample, for fights where no
  evasion is taken. The value of an evasion itself is **Unverified**, and it matters: with evasion, the two Sword and Dagger
  cases score 59.86% and 70.11%.
- **O7. Scores only depend on what `get_boundary` lists.** A scorer that reads anything else gives wrong results, since the
  state may already be cleared. **Tested** by `what_scores_the_next_book_is_never_changed`.
- **O8. Endurance can't overflow** on a doubled kill. **Proved** for the books in `rocq/proofs/no_overflow.v`, using the chapter
  data.
- **O9. Reduced retention keeps the score.** It only frees states it can compute again. **Tested** by unchanged scores.

## 7. Numbers and results

- **N1. `f64` is close enough for reports,** and for comparisons printed to 9 digits. A difference in the last digit is
  taken as rounding unless exact fractions are compared. Final solves use exact fractions. **Decided.**
- **N2. Results for Books 1 to 4 and the discipline comparisons are older than the latest fixes,** and many used win = 1.
  `data/` was cleaned on 2026-11-02, and final values need a new run. **Decided.**
- **N3. Six Book 5 characters stand in for 576 cases** in the gold and edit tests: 6S, HU, MO, CA, MS and TR. **Tested**
  on those only.
- **N4. Report rules are accepted at 98% agreement,** as patterns read from decisions in `report/analyse.py`. They describe
  a character's solution, and are not proved. **Decided.**

## 8. Candidate chapter edits

Tried on scratch copies of Book 5, to make low gold cheap. None is applied. See G5 for why 50 gold may make them
unnecessary.

- **E1. Remove later uses of gold in Book 5, keeping the Sword and Dagger at c388.** Not needed with G4, as 50 gold is
  cheaper. It would only help to solve amounts below that. This removes the BroadSword at c388,
  the sash at c169, the potion at c211, and the payments at c248, c265, c276 and c362. At 15 gold, all six scores match the
  unedited ones to 9 digits, with 1 to 2 GB and 6 to 9 minutes against about 7 GB and 40 minutes or more. At 50 gold, MS
  matches to 12 digits. The exact solve of MO at 15 gold (6.4 GB, 17 minutes) gives the same fraction as unedited at 50;
  the difference for MS comes from gold, see G5. These tests predate the discard choice of R20. **Open.**
- **E2. Removing all of c388's weapons was wrong:** MO fell by 3.8 points. This was before the discard choice of R20, which
  may change the result. **Tested.**
- **E3. Keeping only the Dagger at c388 was wrong:** MS fell by 5.4 points, also before R20. **Tested.**
- **E4. How each edit could be proved.** c248 and c276: paying leads to the same place as not paying, so a static proof
  needs only that the lost crowns don't help later. c265 looks the same, but its destinations were not compared. c169, c211,
  c362 and the BroadSword lead to different outcomes that only turned out not to matter, so a proof would need solved
  values. **Unverified.**

## Keeping this document up to date

Add an entry when a change or a result relies on something not proved: a simplified rule, a case left out, a reading of
the book, or a test on a sample standing in for the whole. Give it the next number in its section, a status, and the
evidence. When an assumption is proved, shown false or changed, update its entry instead of deleting it.
