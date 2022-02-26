use crate::lonewolf::choices::flatten_decision;
use crate::lonewolf::combat::Memoz;
use crate::lonewolf::data::order_chapters;
use crate::lonewolf::mini::{max_hp, Flags};
use crate::lonewolf::rules::update;
use crate::solver::base::{Choice, Outcome, Proba, Solution};
use crate::solver::solve;
use crate::{Chapter, ChapterId, CharacterConstant, CharacterVariable, Equipment, Flag, NextStep};
use rug::Rational;
use std::collections::HashMap;

pub fn step(
  memo: &mut Memoz,
  order: &HashMap<ChapterId, u32>,
  chapters: &HashMap<ChapterId, Chapter>,
  ccst: &CharacterConstant,
  ns: &NextStep,
) -> Vec<Choice<String, NextStep>> {
  match ns {
    NextStep::NewChapter(cid, cvar) => {
      if cvar.curendurance > max_hp(ccst, cvar) {
        panic!("Too many HPs!!");
      }
      let chapter = chapters
        .get(&ChapterId(*cid))
        .expect("could not find chapter");
      let mut nv = cvar.clone();
      nv.flags.unset(Flag::HadCombat);
      let mut out: Vec<Choice<String, NextStep>> = flatten_decision(ccst, cvar, &chapter.pchoice)
        .into_iter()
        .map(|(d, outcome)| Choice {
          desc: d.join(" "),
          res: update(memo, ccst, &nv, ChapterId(*cid), &outcome),
        })
        .collect();
      let scorei = |ns: &NextStep| -> u32 {
        match ns {
          NextStep::HasLost(_) => 0,
          NextStep::HasWon(_) => 0,
          NextStep::NewChapter(cid, _) => *order.get(&ChapterId(*cid)).unwrap_or(&0),
        }
      };
      let scorer =
        |outcome: &Outcome<NextStep>| outcome.iter().map(|r| scorei(&r.v)).max().unwrap_or(0);
      out.sort_by(|a, b| scorer(&a.res).cmp(&scorer(&b.res)));
      if out.is_empty() {
        panic!("Empty! {:?}", ns);
      } else {
        out
      }
    }
    NextStep::HasWon(c) => vec![Choice {
      desc: "won!".into(),
      res: vec![Proba::certain(NextStep::HasWon(c.clone()))],
    }],
    NextStep::HasLost(c) => vec![Choice {
      desc: "lost!".into(),
      res: vec![Proba::certain(NextStep::HasLost(*c))],
    }],
  }
}

fn get_score<F>(scorer: F, tgt: &[ChapterId], ns: &NextStep) -> Option<Rational>
where
  F: Fn(Equipment, Flags) -> Rational,
{
  let score_for = |cvar: &CharacterVariable| scorer(cvar.cequipment, cvar.flags);
  match ns {
    NextStep::HasWon(cvar) => Some(score_for(cvar)),
    NextStep::HasLost(_) => Some(Rational::from(0)),
    NextStep::NewChapter(x, cvar) => {
      if tgt.contains(&ChapterId(*x)) {
        Some(score_for(cvar))
      } else {
        None
      }
    }
  }
}

pub fn solve_lws<F>(
  scorer: F,
  tgt: &[ChapterId],
  book: &[(ChapterId, Chapter)],
  ccst: &CharacterConstant,
  cvar: &CharacterVariable,
) -> Solution<NextStep>
where
  F: Fn(Equipment, Flags) -> Rational,
{
  let mbook: HashMap<ChapterId, Chapter> = book.iter().cloned().collect();
  let order: HashMap<ChapterId, u32> = order_chapters(&mbook);
  let mut memo = Memoz::default();
  let o = solve::<NextStep, _, _, _, _>(
    &mut |ns| step(&mut memo, &order, &mbook, ccst, ns),
    &|ns| get_score(&scorer, tgt, ns),
    &NextStep::NewChapter(1, cvar.clone()),
    &|c| eprintln!("State cache efficiency {:.2}%", c.efficiency() * 100.0),
  );
  eprintln!(
    "Cache efficiency: wholefight[{:.2}%] vanilla[{:.2}%] mindblasted[{:.2}%]",
    memo.whole_fight.efficiency() * 100.0,
    memo.fight_vanilla.efficiency() * 100.0,
    memo.fight_mindblasted.efficiency() * 100.0
  );
  o
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{Book, BoolCond, ChapterOutcome, Decision, Item, SolNode};

  #[test]
  fn dummy_book() {
    let book = &[
      (
        ChapterId(1),
        Chapter {
          title: "1".to_string(),
          desc: "1".to_string(),
          pchoice: Decision::CanTake(
            Item::GenBackpack(1),
            1,
            Box::new(Decision::None(ChapterOutcome::Goto(ChapterId(2)))),
          ),
        },
      ),
      (
        ChapterId(2),
        Chapter {
          title: "1".to_string(),
          desc: "1".to_string(),
          pchoice: Decision::None(ChapterOutcome::Conditionally(vec![
            (
              BoolCond::HasItem(Item::GenBackpack(1), 1),
              ChapterOutcome::GameWon,
            ),
            (BoolCond::Always(true), ChapterOutcome::GameLost),
          ])),
        },
      ),
    ];
    let cvar = CharacterVariable::new(20);
    let sol = solve_lws(
      |_, _| Rational::from(1),
      &[ChapterId(3)],
      book,
      &CharacterConstant {
        bookid: Book::Book01,
        combat_skill: 10,
        maxendurance: 20,
        discipline: Vec::new(),
      },
      &cvar,
    );

    let mut nvar = cvar.clone();
    nvar.cequipment.add_item(&Item::GenBackpack(1), 1);

    let c1 = sol.get(&NextStep::NewChapter(1, cvar)).unwrap();

    assert_eq!(
      c1,
      &SolNode::Chosen(
        Rational::from(1),
        vec![Proba::certain(NextStep::NewChapter(2, nvar))]
      )
    )
  }
}
