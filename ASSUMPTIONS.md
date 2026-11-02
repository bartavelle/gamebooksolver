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
  The proof must hold with next-book values, as the tests were win = 1.
- **C2. These items and flags have no effect across books:** Crystal Star Pendant, Onyx Medallion, Blue Stone Triangle,
  Firesphere, Rope, the Gorn Cove flag. **Tested**, on a few characters each, win = 1. The medallion and the triangle
  were only tested from c50 and c150.
- **C3. `FLGKnowledge01` has no effect**, and was removed from `B03OPTIONS`. **Tested**, on 5 characters, win = 1.
- **C4. Gold does not cross Book 2 to 3, Book 3 to 4 or Book 4 to 5.** See G4 and G5 for the effect on Book 5. In Book 3,
  gold had no effect at 0, 15 and 30 on one character. By the rules alone, a static analysis groups Book 3 starts of 10
  gold or more as identical, and all Book 4 starts as identical. **Tested** on that sample; the analysis is a scratch
  script, not a proof. The Book 4 branch of `filter_final_state`, which maps gold to 0 or 15, is dead code as a result.
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
- **G4. Book 5 starts with 15 gold**, the default of `buildsol-rust.py`, close to the average pouch of 14.5. A player
  carrying gold from Book 4 could have up to 50. **Open**: 50 gold is proposed, see G5 and E1.
- **G5. In Book 5, gold above 20 changes nothing.** Chapter 154 sells a Strength Potion, Laumspur and a Potion2Hp for 12
  crowns, and the 10-gold score of HU matches the 9-crown option exactly, so most of the effect stops at 12: HU and TR
  match their 40-gold scores to 9 digits at 15. MO has the same exact fraction at 15 and 50. There is a tiny effect above
  that: on the edited book of E1, MS scores 0.583265136440 at 15 and 16 gold, 0.583265136985 at 17 to 19, and
  0.583265137192 at 20 and 50, a total of about 0.00000008 points. A first guess of 12 for all characters was wrong.
  **Tested** on six characters. 6S has only been solved unedited at 10, 40 and 50, and values below 15 only at 10. Proving
  this is an obligation if Book 5 relies on it.
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
- **R3. Cartwheel: staking one crown is optimal.** **Proved** in `rocq/cartwheel.v`, for any exit values, either payout
  and any cap, using classical axioms. The result is about the game alone. The link to the Rust table is not proved, and
  `rules.v` has the table as a `Parameter`.
- **R4. Cartwheel: playing up to a target is enough, with targets from 20 to 30.** Other stopping rules may do better, as
  gold is not monotone. Single-hole stopping rules and targets up to 40 gave identical scores on 11 Book 2 starts. Seed 9
  showed +1.49e-5 against a baseline without evasion, never settled. **Tested**, with one open result.
- **R5. Portholes (Book 2 c308) is not played.** It loses 0.09 crowns per throw on average. An exact model of every
  stopping rule gave the same score in all 36 comparisons (6 characters, 10 to 27 gold), and the same exact fraction on
  the one case checked. **Decided** on 2026-11-02; a static proof is wanted later. Reached only from c53 through c154,
  and the best path did not visit either in the one exact case inspected.

### Choices the model leaves out

- **R6. Weapons can be discarded at any time, like Backpack Items.** The books state this for Backpack Items, and their
  weapon rule only sets the limit of two. This reading was **Decided** on 2026-11-02. The model does not offer it in
  general: a choice is only added where it is relevant, see R20.
- **R20. Discarding a weapon only matters when dropping the Sommerswerd at Book 5 c289,** so that is the only place the
  choice is offered. Holding it there leads to c311 and c350, which destroy it and cost 3 Endurance, where the others go
  to c219. **Decided**; a proof is wanted that this is the only place it makes a difference. The evidence is narrow: five
  Sommerswerd cases and one Sword case at 50 gold score the same with the choice only at c289, with it at all four
  branches on the sword (c212, c264, c289, c333), and on the five that have an unedited score, without it. The MO and MS
  solutions already swap the sword for a Dagger at c388 and reach c289 without it. Lower gold, and discarding any other
  weapon or at any other time, were not tested. **Tested** on that sample.
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
  c146, c164, c180; Book 4 c234, c280; Book 5 c55, c226. They create no cycle, and have not been reviewed. Book 2 c302
  gave the same score on five starts when set back to the book's 244, with 15% to 22% more states. **Open.**
- **R17. Book 3's Padded Leather Waistcoat (+2 Endurance) is not modelled,** although it is a starting pick. **Open.**
- **R18. The best weapon is used automatically in a fight.** The book lets the player choose. Book 5 c253 encodes it by
  hand, with +3 for using the Jewelled Mace instead of a Weapon Skill weapon. **Unverified.**
- **R19. Some chapters needed interpretation:** Book 2 c32 (ticket needs the fare), Book 3 c152, Book 4 c40, c54, c258, c173
  and s22, Book 5 c127, c257, c357 and c452. Each was checked against the book text when fixed. **Decided.**

## 6. Optimizations

These must not change a result, and each needs a proof against a rules model on the five books. None has one yet.
Rocq models the Rust with the optimizations left out, and agreed with Rust on 4,391 cases, which checks the model and
not the optimizations.

- **O1. Plain weapons are interchangeable.** A weapon with no skill, not magical, and not named in reachable chapters is
  treated as any other (`WeaponUse`, `useful_drops`, `canonical`). No mismatch on 42 + 42 pairs. **Tested.**
- **O2. What can't be read again is cleared** (`Reads`): gold, generic items, and story flags. No mismatch on 15 pairs.
  **Tested.**
- **O3. Character specialisation** removes branches needing a discipline the character lacks. **Tested** on all 24
  characters of Books 1 to 4.
- **O4. Gold above what can still be spent is cleared** (`gold_needs`). Must hold given G3: it only cuts gold that no
  later test or payment can tell apart. **Tested** on the same 24.
- **O5. Forced chapters can be elided** (`elide.rs`), replaying what they do. On and off: 44 of 48 cases identical, 0
  different, 4 not compared because they ran out of memory. **Tested.**
- **O6. Round-by-round evasion matches a whole fight** when no evasion is taken. The rounds are assumed acyclic because each
  one changes something. The Rocq and Rust comparison only shows that both implement rounds the same way, not that the
  rounds equal a whole fight. **Unverified.**
- **O7. Scores only depend on what `get_boundary` lists.** A scorer that reads anything else gives wrong results, since the
  state may already be cleared. **Tested** by `what_scores_the_next_book_is_never_changed`.
- **O8. Endurance can't overflow** on a doubled kill. **Proved** for the books in `rocq/no_overflow.v`, using the chapter
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

- **E1. Remove later uses of gold in Book 5, keeping the Sword and Dagger at c388.** This removes the BroadSword at c388,
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
