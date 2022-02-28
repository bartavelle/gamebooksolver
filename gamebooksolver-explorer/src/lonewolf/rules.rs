use crate::lonewolf::combat::{CombatInfo, Escaped, Memoz};
use crate::lonewolf::mini::{max_hp, Discipline, Flag};
use crate::solver::base::{optimize_outcome, Outcome, Proba};
use crate::{
  Book, BoolCond, CanHunt, ChapterId, ChapterOutcome, CharacterConstant, CharacterVariable,
  Endurance, Equipment, FightModifier, Item, NextStep, SimpleOutcome,
};

pub fn check(ccst: &CharacterConstant, cvar: &CharacterVariable, cond: &BoolCond) -> bool {
  use BoolCond::*;
  match cond {
    Always(b) => *b,
    HasDiscipline(d) => ccst.discipline.contains(d),
    Not(s) => !check(ccst, cvar, s),
    COr(a, b) => check(ccst, cvar, a) || check(ccst, cvar, b),
    CAnd(a, b) => check(ccst, cvar, a) && check(ccst, cvar, b),
    HasItem(i, q) => cvar.cequipment.has_item(i, *q),
    HasEndurance(e) => cvar.curendurance >= e.0,
    HasFlag(f) => cvar.flags.has(*f),
    HasLevel(lvl) => ccst.discipline.len() >= *lvl as usize,
  }
}

pub fn update_simple(
  cvar: &mut CharacterVariable,
  ccst: &CharacterConstant,
  soutcome: &SimpleOutcome,
) {
  use SimpleOutcome::*;

  match soutcome {
    DamagePlayer(dmg) => cvar.damage(dmg.0),
    HealPlayer(hp) => cvar.heal(ccst, hp.0),
    FullHeal => cvar.curendurance = max_hp(ccst, cvar),
    HalfHeal => cvar.curendurance = (max_hp(ccst, cvar) + cvar.curendurance) / 2,
    GainItem(i, q) => cvar.cequipment.add_item(i, *q as i64),
    LoseItem(i, q) => cvar.cequipment.del_item(i, *q as i64),
    LoseItemKind(slots) => {
      for s in slots {
        cvar.cequipment.del_slot(*s)
      }
    }
    SetFlag(f) => cvar.flags.set(*f),
    ClearFlag(f) => cvar.flags.unset(*f),
    StoreEquipment => {
      cvar.cprevequipment = cvar.cequipment;
      cvar.cequipment = Equipment(0);
    }
    MustEat(canhunt) => {
      let b01ls = ccst.bookid == Book::Book01 && cvar.cequipment.has_itemb(&Item::Laumspur);
      if *canhunt == CanHunt::Hunt && ccst.discipline.contains(&Discipline::Hunting) {
        return;
      }
      if b01ls && (max_hp(ccst, cvar) - cvar.curendurance >= 3) {
        cvar.heal(ccst, 3);
        cvar.cequipment.del_item(&Item::Laumspur, 1);
        return;
      }
      if cvar.cequipment.has_itemb(&Item::Meal) {
        cvar.cequipment.del_item(&Item::Meal, 1);
        return;
      }
      if b01ls {
        cvar.heal(ccst, 3);
        cvar.cequipment.del_item(&Item::Laumspur, 1);
        return;
      }
      cvar.damage(3)
    }
  }
}

pub fn update(
  memo: &mut Memoz,
  ccst: &CharacterConstant,
  cvar: &CharacterVariable,
  cid: ChapterId,
  outcome: &ChapterOutcome,
) -> Outcome<NextStep> {
  if cvar.curendurance <= 0 {
    return vec![Proba::certain(NextStep::HasLost(cid.0))];
  }
  match outcome {
    ChapterOutcome::Goto(cid2) => {
      // must be checked before healing
      if cvar.curendurance <= 0 {
        return vec![Proba::certain(NextStep::HasLost(cid.0))];
      }
      let max_chapter = ChapterId(if ccst.bookid == Book::Book05 {
        400
      } else {
        350
      });
      let mut nv = cvar.clone();
      if cid < max_chapter && cvar.flags.has(Flag::Poisonned2) {
        update_simple(&mut nv, ccst, &SimpleOutcome::DamagePlayer(Endurance(2)));
        if nv.curendurance > 0 {
          vec![Proba::certain(NextStep::NewChapter(cid2.0, nv))]
        } else {
          vec![Proba::certain(NextStep::HasLost(cid.0))]
        }
      } else if cid < max_chapter
        && ccst.discipline.contains(&Discipline::Healing)
        && !cvar.flags.has(Flag::HadCombat)
      {
        nv.heal(ccst, 1);
        vec![Proba::certain(NextStep::NewChapter(cid2.0, nv))]
      } else {
        vec![Proba::certain(NextStep::NewChapter(cid2.0, nv))]
      }
    }
    ChapterOutcome::GameLost => vec![Proba::certain(NextStep::HasLost(cid.0))],
    ChapterOutcome::GameWon => vec![Proba::certain(NextStep::HasWon(cvar.clone()))],
    ChapterOutcome::Simple(effects, nxt) => {
      let mut nv = cvar.clone();
      for e in effects {
        update_simple(&mut nv, ccst, e);
      }
      update(memo, ccst, &nv, cid, nxt)
    }
    ChapterOutcome::Conditionally(conditions) => {
      for (bc, no) in conditions {
        if check(ccst, cvar, bc) {
          return update(memo, ccst, cvar, cid, no);
        }
      }
      panic!("No conditions found :(");
    }
    ChapterOutcome::Randomly(rands) => {
      let out: Outcome<NextStep> = rands
        .iter()
        .flat_map(|(pb, o)| {
          update(memo, ccst, cvar, cid, o)
            .into_iter()
            .map(move |r| Proba {
              p: r.p * pb,
              v: r.v,
            })
        })
        .collect();
      optimize_outcome(out)
    }
    ChapterOutcome::OneRound(fd, lose, eq, win) => {
      let cinfo = CombatInfo::make(ccst, cvar, fd);
      let out: Outcome<NextStep> = cinfo
        .fight_round()
        .into_iter()
        .flat_map(|r| {
          let lwloss = cvar.curendurance - r.v.0 .0;
          let oploss = fd.endurance.0 - r.v.1 .0;
          let mut nv: CharacterVariable = cvar.clone();
          nv.curendurance = r.v.0 .0;
          nv.flags.unset(Flag::StrengthPotionActive);
          nv.flags.unset(Flag::PotentStrengthPotionActive);
          nv.flags.set(Flag::HadCombat);
          let nxt = match lwloss.cmp(&oploss) {
            std::cmp::Ordering::Greater => lose,
            std::cmp::Ordering::Equal => eq,
            std::cmp::Ordering::Less => win,
          };
          update(memo, ccst, &nv, cid, nxt)
            .into_iter()
            .map(move |r2| Proba {
              p: r2.p * &r.p,
              v: r2.v,
            })
        })
        .collect();
      optimize_outcome(out)
    }
    ChapterOutcome::Fight(fd, nxt) => {
      let cinfo = CombatInfo::make(ccst, cvar, fd);
      let out: Outcome<NextStep> = cinfo
        .fight(memo)
        .into_iter()
        .flat_map(|r| {
          let (noutcome, charendurance): (ChapterOutcome, Endurance) = match r.v {
            Escaped::Std(n) => {
              let x: &ChapterOutcome = nxt;
              (x.clone(), n)
            }
            Escaped::Escaped(ecid, n) => (ChapterOutcome::Goto(ecid), n),
            Escaped::LateWin(ecid, n) => (ChapterOutcome::Goto(ecid), n),
            Escaped::Lost(ecid) => (ChapterOutcome::Goto(ecid), Endurance(1)),
            Escaped::Stopped(ecid, n) => (ChapterOutcome::Goto(ecid), n),
          };
          let mut nv: CharacterVariable = cvar.clone();
          nv.flags.set(Flag::HadCombat);
          if !fd.fight_mod.contains(&FightModifier::MultiFight) {
            nv.flags.unset(Flag::StrengthPotionActive);
            nv.flags.unset(Flag::PotentStrengthPotionActive);
          }
          if let Some(cid2) = fd
            .fight_mod
            .iter()
            .filter_map(|m| match m {
              FightModifier::FakeFight(cid) => Some(cid),
              _ => None,
            })
            .next()
          {
            if charendurance.dead() {
              vec![Proba {
                v: NextStep::NewChapter(cid2.0, nv),
                p: r.p,
              }]
            } else {
              update(memo, ccst, &nv, cid, &noutcome)
                .into_iter()
                .map(|r2| Proba {
                  p: r2.p * &r.p,
                  v: r2.v,
                })
                .collect()
            }
          } else if charendurance.dead() {
            vec![Proba {
              v: NextStep::HasLost(cid.0),
              p: r.p,
            }]
          } else {
            nv.curendurance = charendurance.0;
            update(memo, ccst, &nv, cid, &noutcome)
              .into_iter()
              .map(|r2| Proba {
                p: r2.p * &r.p,
                v: r2.v,
              })
              .collect()
          }
        })
        .collect();
      optimize_outcome(out)
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{CombatSkill, FightDetails, Weapon};
  use rug::Rational;

  #[test]
  fn combat1() {
    let mut memo = Memoz::default();
    let ccst = CharacterConstant {
      bookid: Book::Book04,
      combat_skill: 10,
      maxendurance: 20,
      discipline: Vec::new(),
    };
    let cvar = CharacterVariable::new(20);
    let fight = ChapterOutcome::Fight(
      FightDetails {
        combat_skill: CombatSkill(17),
        endurance: Endurance(26),
        fight_mod: Vec::new(),
        opponent: "DQSD".to_string(),
      },
      Box::new(ChapterOutcome::Goto(ChapterId(16))),
    );
    let mut actual = update(&mut memo, &ccst, &cvar, ChapterId(50), &fight);
    actual.sort();
    let expected_raw = [
      (0, (992003, 1000000)),
      (1, (709, 1000000)),
      (2, (403, 250000)),
      (3, (1013, 1000000)),
      (4, (147, 125000)),
      (5, (817, 1000000)),
      (6, (117, 200000)),
      (7, (67, 200000)),
      (8, (11, 40000)),
      (9, (31, 200000)),
      (10, (71, 200000)),
      (11, (7, 50000)),
      (12, (31, 100000)),
      (13, (41, 200000)),
      (14, (3, 20000)),
      (15, (1, 20000)),
      (16, (1, 20000)),
      (17, (1, 20000)),
      (20, (1, 100000)),
    ];
    let cc = |hp| {
      let mut c = CharacterVariable::new(hp);
      c.flags.set(Flag::HadCombat);
      c
    };
    let mut expected: Outcome<NextStep> = expected_raw
      .into_iter()
      .map(|(hp, p)| Proba {
        p: Rational::from(p),
        v: if hp > 0 {
          NextStep::NewChapter(16, cc(hp))
        } else {
          NextStep::HasLost(50)
        },
      })
      .collect();
    expected.sort();
    assert_eq!(actual, expected);
  }

  #[test]
  fn oneround() {
    let mut memo = Memoz::default();
    let ccst = CharacterConstant {
      bookid: Book::Book04,
      combat_skill: 10,
      maxendurance: 20,
      discipline: vec![
        Discipline::WeaponSkill(Weapon::Sword),
        Discipline::MindBlast,
      ],
    };
    let mut cvar = CharacterVariable::new(20);
    cvar.add_item(&Item::Weapon(Weapon::Sword), 1);
    cvar.add_item(&Item::Shield, 1);
    let duel = ChapterOutcome::OneRound(
      FightDetails {
        combat_skill: CombatSkill(20),
        endurance: Endurance(26),
        fight_mod: Vec::new(),
        opponent: String::new(),
      },
      Box::new(ChapterOutcome::Goto(ChapterId(1))),
      Box::new(ChapterOutcome::Goto(ChapterId(2))),
      Box::new(ChapterOutcome::Goto(ChapterId(3))),
    );
    let mut actual = optimize_outcome(
      update(&mut memo, &ccst, &cvar, ChapterId(50), &duel)
        .into_iter()
        .map(|r| Proba {
          p: r.p,
          v: r.v.chapter(),
        })
        .collect(),
    );
    actual.sort();
    let mut expected = vec![
      Proba {
        v: Some(1),
        p: Rational::from((3, 10)),
      },
      Proba {
        v: Some(2),
        p: Rational::from((1, 10)),
      },
      Proba {
        v: Some(3),
        p: Rational::from((6, 10)),
      },
    ];
    expected.sort();
    assert_eq!(actual, expected);
  }
}
