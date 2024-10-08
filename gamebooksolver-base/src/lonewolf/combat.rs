mod hits;

use crate::lonewolf::chapter::{ChapterId, CombatSkill, Endurance, FightDetails, FightModifier};
use crate::lonewolf::mini::{CharacterConstant, CharacterVariable, Discipline, Flag, Flags, Item, Weapon};
use crate::solver::base::{optimize_outcome, Cache, Outcome, Proba};
use crate::solver::rational::Rational;
use hits::hits;
use std::collections::HashSet;
use std::hash::Hash;

const RFLAGS: Flags = Flags(0)
    .setn(Flag::LimbDeath)
    .setn(Flag::PermanentSkillReduction)
    .setn(Flag::PermanentSkillReduction2)
    .setn(Flag::StrengthPotionActive)
    .setn(Flag::PotentStrengthPotionActive);

const SWORDLIKE: [Weapon; 3] = [Weapon::ShortSword, Weapon::Sword, Weapon::BroadSword];

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CombatInfo<P> {
    skilldiff: i8,
    specialization: Option<Weapon>,
    weapons: Vec<Weapon>,
    modifiers: Vec<FightModifier<P>>,
    mindblast: bool,
    mindshield: bool,
    cflags: Flags,
    shield: bool,
    silverhelm: bool,
    lwendurance: Endurance,
    opendurance: Endurance,
}

type MemoSimpleFight<P> = Cache<(CombatSkill, Endurance, Endurance), Outcome<P, (Endurance, Endurance)>>;

pub struct Memoz<P> {
    pub whole_fight: Cache<CombatInfo<P>, Outcome<P, Escaped<Endurance>>>,
    pub fight_mindblasted: MemoSimpleFight<P>,
    pub fight_vanilla: MemoSimpleFight<P>,
}

impl<P> Default for Memoz<P> {
    fn default() -> Self {
        Memoz {
            whole_fight: Cache::default(),
            fight_mindblasted: Cache::default(),
            fight_vanilla: Cache::default(),
        }
    }
}

fn fight_mindblasted<P: Rational>(
    memo: &mut MemoSimpleFight<P>,
    ratio: CombatSkill,
    php: Endurance,
    ohp: Endurance,
) -> Outcome<P, (Endurance, Endurance)> {
    if let Some(res) = memo.get(&(ratio, php, ohp)) {
        return res.clone();
    }
    if php.dead() || ohp.dead() {
        vec![Proba::certain((php, ohp))]
    } else {
        let o = hits(ratio.0).iter().flat_map(|r| {
            fight_mindblasted(memo, ratio, php.damage(2).damage(r.lw), ohp.damage(r.op))
                .into_iter()
                .map(|r2| Proba {
                    p: r2.p.mul(&P::from_i64(1, 10)),
                    v: r2.v,
                })
        });
        let outcome = optimize_outcome(o.collect());
        memo.insert((ratio, php, ohp), outcome.clone());
        outcome
    }
}

fn fight_vanilla<P: Rational>(
    memo: &mut MemoSimpleFight<P>,
    ratio: CombatSkill,
    php: Endurance,
    ohp: Endurance,
) -> Outcome<P, (Endurance, Endurance)> {
    if let Some(res) = memo.get(&(ratio, php, ohp)) {
        return res.clone();
    }
    if php.dead() || ohp.dead() {
        vec![Proba::certain((php, ohp))]
    } else {
        let o = hits(ratio.0).iter().flat_map(|r| {
            fight_vanilla(memo, ratio, php.damage(r.lw), ohp.damage(r.op))
                .into_iter()
                .map(|r2| Proba {
                    p: r2.p.mul(&P::from_i64(1, 10)),
                    v: r2.v,
                })
        });
        let outcome = optimize_outcome(o.collect());
        memo.insert((ratio, php, ohp), outcome.clone());
        outcome
    }
}

impl<P: Rational + Ord + std::hash::Hash + std::iter::Sum> CombatInfo<P> {
    pub fn curmodifiers(&self) -> HashSet<&FightModifier<P>> {
        self.modifiers.iter().filter_map(get_timed).collect()
    }

    pub fn make(cconstant: &CharacterConstant, cvariable: &CharacterVariable, fdetails: &FightDetails<P>) -> Self {
        let skilldiff = cconstant.combat_skill as i8 - fdetails.combat_skill.0;
        let specialization: Option<Weapon> = cconstant
            .discipline
            .iter()
            .filter_map(|d| match d {
                Discipline::WeaponSkill(w) => Some(*w),
                _ => None,
            })
            .next();
        let weapons = cvariable.cequipment.weapons();
        let mut modifiers = fdetails.fight_mod.clone();
        modifiers.sort();
        let mindblast = cconstant.discipline.contains(&Discipline::MindBlast);
        let mindshield = cconstant.discipline.contains(&Discipline::MindShield);
        let cflags = Flags(cvariable.flags.0 & RFLAGS.0);
        let shield = cvariable.cequipment.has_item(&Item::Shield, 1);
        let silverhelm = cvariable.cequipment.has_item(&Item::Helmet, 1) & cvariable.flags.has(Flag::HelmetIsSilver);
        CombatInfo {
            skilldiff,
            specialization,
            weapons,
            modifiers,
            mindblast,
            mindshield,
            cflags,
            shield,
            silverhelm,
            lwendurance: Endurance(cvariable.curendurance),
            opendurance: fdetails.endurance,
        }
    }

    pub fn get_ratio(&self) -> CombatSkill {
        let mut o = self.skilldiff;
        let modifiers = self.curmodifiers();
        let combat_bonus: i8 = modifiers
            .iter()
            .filter_map(|m| match m {
                FightModifier::CombatBonus(d) => Some(d.0),
                _ => None,
            })
            .sum();
        o += combat_bonus;
        if modifiers.contains(&FightModifier::BareHanded) || self.weapons.is_empty() {
            o -= 4;
        } else if self.weapons.contains(&Weapon::Sommerswerd) {
            if let Some(w) = self.specialization {
                if SWORDLIKE.contains(&w) {
                    o += 10;
                } else {
                    o += 8;
                }
            } else {
                o += 8;
            }
        } else if let Some(wspec) = self.specialization {
            if self.weapons.contains(&wspec) || (wspec == Weapon::Spear && self.weapons.contains(&Weapon::MagicSpear)) {
                o += 2
            }
        }
        if self.mindblast && !modifiers.contains(&FightModifier::MindblastImmune) {
            o += 2;
        }
        if self.shield && !self.cflags.has(Flag::LimbDeath) {
            o += 2;
        }
        if self.silverhelm {
            o += 2;
        }
        if self.cflags.has(Flag::PermanentSkillReduction) {
            o -= 1;
        }
        if self.cflags.has(Flag::PermanentSkillReduction2) {
            o -= 2;
        }
        if self.cflags.has(Flag::LimbDeath) {
            o -= 3;
        }
        if self.cflags.has(Flag::StrengthPotionActive) {
            o += 2;
        }
        if self.cflags.has(Flag::PotentStrengthPotionActive) {
            o += 4;
        }

        CombatSkill(o)
    }

    pub fn fight_round(&self) -> Outcome<P, (Endurance, Endurance)> {
        let ratio = self.get_ratio();
        let modifiers = self.curmodifiers();
        let mut o: Outcome<P, (Endurance, Endurance)> = Vec::new();
        for dmg in hits(ratio.0) {
            let odmg_opponent: u8 = dmg.op
                + modifiers
                    .iter()
                    .filter_map(|m| match m {
                        FightModifier::Dpr(d) => Some(d.0 as u8),
                        _ => None,
                    })
                    .sum::<u8>();
            let dmg_lonewolf = if modifiers.contains(&FightModifier::PlayerInvulnerable) {
                0
            } else if modifiers.contains(&FightModifier::ForceEMindblast)
                || (!self.mindshield && modifiers.contains(&FightModifier::EnemyMindblast))
            {
                dmg.lw + 2
            } else {
                dmg.lw
            };
            let dmg_opponent = if modifiers.contains(&FightModifier::EnemyInvulnerable) {
                0
            } else if modifiers.contains(&FightModifier::DoubleDamage)
                || (self.weapons.contains(&Weapon::Sommerswerd) && modifiers.contains(&FightModifier::Undead))
            {
                odmg_opponent * 2
            } else {
                odmg_opponent
            };
            o.push(Proba {
                p: P::from_i64(1, 10),
                v: (
                    self.lwendurance.damage(dmg_lonewolf),
                    self.opendurance.damage(dmg_opponent),
                ),
            })
        }

        optimize_outcome(o)
    }

    pub fn fight(&self, memo: &mut Memoz<P>) -> Outcome<P, Escaped<Endurance>> {
        if let Some(o) = memo.whole_fight.get(self) {
            return o.clone();
        }

        struct FightModifiers<P> {
            stop_fight: Option<ChapterId>,
            evaded: Option<ChapterId>,
            poisonous: Option<P>,
            onlose: Option<ChapterId>,
            double_damage: bool,
            undead: bool,
            force_emindblast: bool,
            emindblast: bool,
            notyetwon: Option<ChapterId>,
        }

        let mut fightmodifiers = FightModifiers {
            stop_fight: None,
            evaded: None,
            poisonous: None,
            onlose: None,
            double_damage: false,
            undead: false,
            force_emindblast: false,
            emindblast: false,
            notyetwon: None,
        };

        for m in &self.curmodifiers() {
            match m {
                FightModifier::OnLose(cid) => fightmodifiers.onlose = Some(*cid),
                FightModifier::StopFight(cid) => fightmodifiers.stop_fight = Some(*cid),
                FightModifier::Evaded(cid) => fightmodifiers.evaded = Some(*cid),
                FightModifier::Poisonous(p) => fightmodifiers.poisonous = Some(p.clone()),
                FightModifier::ForceEMindblast => fightmodifiers.force_emindblast = true,
                FightModifier::EnemyMindblast => fightmodifiers.emindblast = true,
                FightModifier::DoubleDamage => fightmodifiers.double_damage = true,
                FightModifier::Undead => fightmodifiers.undead = true,
                FightModifier::OnNotYetWon(cid) => fightmodifiers.notyetwon = Some(*cid),
                _ => (),
            }
        }

        let lost = || {
            if let Some(cid) = fightmodifiers.onlose {
                Escaped::Lost(cid)
            } else {
                Escaped::Std(Endurance(0))
            }
        };

        // stop fight modifier
        if let Some(cid) = fightmodifiers.stop_fight {
            let outcome = vec![Proba {
                p: P::from_i64(1, 1),
                v: Escaped::Stopped(cid, self.lwendurance),
            }];
            memo.whole_fight.insert(self.clone(), outcome.clone());
            return outcome;
        }

        // evasion
        if let Some(cid) = fightmodifiers.evaded {
            let outcome = optimize_outcome(
                self.fight_round()
                    .into_iter()
                    .map(|r| {
                        if r.v.0.dead() {
                            Proba { p: r.p, v: lost() }
                        } else {
                            Proba {
                                p: r.p,
                                v: Escaped::Escaped(cid, r.v.0),
                            }
                        }
                    })
                    .collect(),
            );
            memo.whole_fight.insert(self.clone(), outcome.clone());
            return outcome;
        }

        // poisonous opponent
        if let Some(instakill) = fightmodifiers.poisonous {
            let useful = optimize_outcome(
                self.fight_round()
                    .into_iter()
                    .flat_map(|r| {
                        if r.v.0 < self.lwendurance {
                            vec![
                                Proba {
                                    p: r.p.clone().mul(&instakill),
                                    v: (true, Endurance(1)),
                                },
                                Proba {
                                    p: r.p.mul(&P::from_i64(1, 1).sub(&instakill)),
                                    v: (false, r.v.1),
                                },
                            ]
                            .into_iter()
                        } else {
                            vec![Proba {
                                p: r.p,
                                v: (false, r.v.1),
                            }]
                            .into_iter()
                        }
                    })
                    .filter(|r| r.v != (false, self.opendurance))
                    .collect(),
            );
            let usefulproba: P = useful.iter().map(|r| r.p.clone()).sum();
            let outcome = useful.into_iter().flat_map(|r| {
                if r.v.0 {
                    vec![Proba {
                        p: r.p.div(&usefulproba),
                        v: lost(),
                    }]
                } else if r.v.1.dead() {
                    vec![Proba {
                        p: r.p.div(&usefulproba),
                        v: Escaped::Std(self.lwendurance),
                    }]
                } else {
                    let mut ncinfo = self.clone();
                    ncinfo.opendurance = r.v.1;
                    ncinfo.modifiers = ncinfo.modifiers.into_iter().filter_map(decrement_timed).collect();
                    ncinfo
                        .fight(memo)
                        .into_iter()
                        .map(|r2| Proba {
                            p: r2.p.mul(&r.p).div(&usefulproba),
                            v: r2.v,
                        })
                        .collect()
                }
            });
            let o = optimize_outcome(outcome.collect());
            memo.whole_fight.insert(self.clone(), o.clone());
            return o;
        }

        // round with timed effects and/or Dpr
        if self
            .modifiers
            .iter()
            .any(|x| matches!(x, FightModifier::Dpr(_) | FightModifier::Timed(_, _)))
        {
            let outcome = self.fight_round().into_iter().flat_map(|r| {
                if r.v.0.dead() {
                    vec![Proba { p: r.p, v: lost() }]
                } else if r.v.1.dead() {
                    vec![Proba {
                        p: r.p,
                        v: Escaped::Std(r.v.0),
                    }]
                } else {
                    let mut ncinfo = self.clone();
                    ncinfo.lwendurance = r.v.0;
                    ncinfo.opendurance = r.v.1;
                    ncinfo.modifiers = ncinfo.modifiers.into_iter().filter_map(decrement_timed).collect();
                    ncinfo
                        .fight(memo)
                        .into_iter()
                        .map(|r2| Proba {
                            p: r2.p.mul(&r.p),
                            v: r2.v,
                        })
                        .collect()
                }
            });
            let o = optimize_outcome(outcome.collect());
            memo.whole_fight.insert(self.clone(), o.clone());
            return o;
        }

        // general case, prepare for the fast fight track
        let ratio = self.get_ratio();
        let ohp =
            if fightmodifiers.double_damage || (self.weapons.contains(&Weapon::Sommerswerd) && fightmodifiers.undead) {
                Endurance((self.opendurance.0 + 1) / 2)
            } else {
                self.opendurance
            };

        let eoutcome = if fightmodifiers.force_emindblast || (!self.mindshield && fightmodifiers.emindblast) {
            fight_mindblasted(&mut memo.fight_mindblasted, ratio, self.lwendurance, ohp)
        } else {
            fight_vanilla(&mut memo.fight_vanilla, ratio, self.lwendurance, ohp)
        };
        let outcome = optimize_outcome(
            eoutcome
                .into_iter()
                .map(|r| {
                    if r.v.0.dead() {
                        Proba { p: r.p, v: lost() }
                    } else {
                        Proba {
                            p: r.p,
                            v: if let Some(ny) = fightmodifiers.notyetwon {
                                Escaped::LateWin(ny, r.v.0)
                            } else {
                                Escaped::Std(r.v.0)
                            },
                        }
                    }
                })
                .collect(),
        );
        memo.whole_fight.insert(self.clone(), outcome.clone());
        outcome
    }
}

fn decrement_timed<P: Rational>(m: FightModifier<P>) -> Option<FightModifier<P>> {
    match m {
        FightModifier::Timed(n, x) => {
            if n > 1 {
                Some(FightModifier::Timed(n - 1, x))
            } else {
                match *x {
                    FightModifier::Evaded(_) => Some(*x),
                    FightModifier::OnNotYetWon(_) => Some(*x),
                    FightModifier::ForceEMindblast => Some(*x),
                    _ => None,
                }
            }
        }
        _ => Some(m),
    }
}

fn get_timed<P: Rational>(m: &FightModifier<P>) -> Option<&FightModifier<P>> {
    match m {
        FightModifier::Timed(_, x) => match x.as_ref() {
            FightModifier::Evaded(_) => None,
            _ => Some(x),
        },
        _ => Some(m),
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug)]
pub enum Escaped<A> {
    Escaped(ChapterId, A),
    Std(A),
    LateWin(ChapterId, A),
    Lost(ChapterId),
    Stopped(ChapterId, A),
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lonewolf::mini::{Book, Equipment};
    use num_rational::BigRational;

    static DEFCSTT: CharacterConstant = CharacterConstant {
        bookid: Book::Book01,
        combat_skill: 15,
        maxendurance: 25,
        discipline: Vec::new(),
    };

    static DEFCVAR: CharacterVariable = CharacterVariable {
        curendurance: 25,
        cequipment: Equipment(1 << Item::Weapon(Weapon::BroadSword).get_idx()),
        cprevequipment: Equipment(0),
        flags: Flags(0),
    };

    fn mk_def_combat() -> FightDetails<BigRational> {
        FightDetails {
            opponent: "def".to_string(),
            combat_skill: CombatSkill(28),
            endurance: Endurance(30),
            fight_mod: Vec::new(),
        }
    }

    #[test]
    fn ratio_default() {
        let cinfo = CombatInfo::make(&DEFCSTT, &DEFCVAR, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-13));
    }
    #[test]
    fn ratio_no_weapons_normal() {
        let mut ncvar = DEFCVAR.clone();
        ncvar.cequipment = Equipment(0);

        let cinfo = CombatInfo::make(&DEFCSTT, &ncvar, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-17));
    }
    #[test]
    fn ratio_no_weapons_special() {
        let mut combat = mk_def_combat();
        combat.fight_mod.push(FightModifier::BareHanded);

        let cinfo = CombatInfo::make(&DEFCSTT, &DEFCVAR, &combat);
        assert_eq!(cinfo.get_ratio(), CombatSkill(-17));
    }
    #[test]
    fn ratio_timed_no_weapons_special() {
        let mut combat = mk_def_combat();
        combat
            .fight_mod
            .push(FightModifier::Timed(2, Box::new(FightModifier::BareHanded)));

        let cinfo = CombatInfo::make(&DEFCSTT, &DEFCVAR, &combat);
        assert_eq!(cinfo.get_ratio(), CombatSkill(-17));
    }
    #[test]
    fn ratio_good_weapon() {
        let mut cstt = DEFCSTT.clone();
        cstt.discipline.push(Discipline::WeaponSkill(Weapon::BroadSword));

        let cinfo = CombatInfo::make(&cstt, &DEFCVAR, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-11));
    }
    #[test]
    fn ratio_mindblast() {
        let mut cstt = DEFCSTT.clone();
        cstt.discipline.push(Discipline::MindBlast);

        let cinfo = CombatInfo::make(&cstt, &DEFCVAR, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-11));
    }
    #[test]
    fn ratio_mindblast_countered() {
        let mut cstt = DEFCSTT.clone();
        cstt.discipline.push(Discipline::MindBlast);
        let mut combat = mk_def_combat();
        combat.fight_mod.push(FightModifier::MindblastImmune);

        let cinfo = CombatInfo::make(&cstt, &DEFCVAR, &combat);
        assert_eq!(cinfo.get_ratio(), CombatSkill(-13));
    }
    #[test]
    fn ratio_shield() {
        let mut ncvar = DEFCVAR.clone();
        ncvar.add_item(&Item::Shield, 1);
        let cinfo = CombatInfo::make(&DEFCSTT, &ncvar, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-11));
    }
    #[test]
    fn ratio_sommerswerd() {
        let mut ncvar = DEFCVAR.clone();
        ncvar.add_item(&Item::Weapon(Weapon::Sommerswerd), 1);
        let cinfo = CombatInfo::make(&DEFCSTT, &ncvar, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-5));
    }
    #[test]
    fn ratio_sommerswerd_skill() {
        let mut ncvar = DEFCVAR.clone();
        ncvar.add_item(&Item::Weapon(Weapon::Sommerswerd), 1);
        let mut cstt = DEFCSTT.clone();
        cstt.discipline.push(Discipline::WeaponSkill(Weapon::BroadSword));
        let cinfo = CombatInfo::make(&cstt, &ncvar, &mk_def_combat());
        assert_eq!(cinfo.get_ratio(), CombatSkill(-3));
    }

    #[test]
    fn fight_round() {
        let mut defcmb = mk_def_combat();
        let cinfo = CombatInfo::make(&DEFCSTT, &DEFCVAR, &defcmb);
        defcmb.combat_skill.0 += 1;
        let cinfob = CombatInfo::make(&DEFCSTT, &DEFCVAR, &defcmb);
        let expected_raw = &[
            ((0, 30), BigRational::from_i64(1, 5)),
            ((17, 30), BigRational::from_i64(1, 5)),
            ((18, 29), BigRational::from_i64(1, 10)),
            ((19, 28), BigRational::from_i64(1, 10)),
            ((20, 27), BigRational::from_i64(1, 10)),
            ((21, 26), BigRational::from_i64(1, 10)),
            ((22, 25), BigRational::from_i64(1, 10)),
            ((25, 24), BigRational::from_i64(1, 10)),
        ];
        let mut expected: Outcome<BigRational, (Endurance, Endurance)> = expected_raw
            .iter()
            .map(|((e1, e2), p)| Proba {
                v: (Endurance(*e1), Endurance(*e2)),
                p: p.clone(),
            })
            .collect();
        let mut actual = cinfo.fight_round();
        let mut actualb = cinfob.fight_round();
        actual.sort();
        actualb.sort();
        expected.sort();
        assert_eq!(actual, expected);
        assert_eq!(actualb, expected);
    }

    #[test]
    fn fight() {
        let cinfo = CombatInfo::make(&DEFCSTT, &DEFCVAR, &mk_def_combat());
        let mut memo = Memoz::default();
        let mut actual = cinfo.fight(&mut memo);
        actual.sort();
        let expected_raw: &[(i8, i64, i64)] = &[
            (0, 19879281, 20000000),
            (1, 6883, 10000000),
            (2, 8139, 20000000),
            (3, 9043, 10000000),
            (4, 11197, 20000000),
            (5, 649, 800000),
            (6, 2719, 5000000),
            (7, 91, 200000),
            (8, 147, 500000),
            (9, 111, 500000),
            (10, 253, 2000000),
            (11, 87, 400000),
            (12, 99, 1000000),
            (13, 451, 2000000),
            (14, 33, 250000),
            (15, 27, 200000),
            (16, 1, 12500),
            (17, 11, 200000),
            (18, 7, 200000),
            (19, 1, 50000),
            (20, 1, 200000),
            (21, 1, 200000),
            (22, 1, 200000),
            (25, 1, 100000),
        ];
        let mut expected: Outcome<BigRational, Escaped<Endurance>> = expected_raw
            .iter()
            .map(|(e, pn, pd)| Proba {
                p: BigRational::from_i64(*pn, *pd),
                v: Escaped::Std(Endurance(*e)),
            })
            .collect();
        expected.sort();
        assert_eq!(actual, expected);
    }

    #[test]
    fn evasion0() {
        let cinfo = CombatInfo::make(
            &DEFCSTT,
            &DEFCVAR,
            &FightDetails {
                opponent: "F1".to_string(),
                combat_skill: CombatSkill(20),
                endurance: Endurance(20),
                fight_mod: vec![FightModifier::Evaded(ChapterId(88))],
            },
        );
        assert_eq!(cinfo.get_ratio(), CombatSkill(-5));
        let mut memo = Memoz::default();
        let mut actual = cinfo.fight(&mut memo);
        actual.sort();
        let mut expected: Vec<Proba<BigRational, Escaped<Endurance>>> =
            [(19, 5), (20, 5), (21, 5), (22, 10), (23, 10), (25, 5)]
                .iter()
                .map(|(e, d)| Proba {
                    v: Escaped::Escaped(ChapterId(88), Endurance(*e)),
                    p: BigRational::from_i64(1, *d),
                })
                .collect();
        expected.sort();
        assert_eq!(actual, expected);
    }

    #[test]
    fn evasion2() {
        let cinfo = CombatInfo::make(
            &DEFCSTT,
            &DEFCVAR,
            &FightDetails {
                opponent: "F1".to_string(),
                combat_skill: CombatSkill(20),
                endurance: Endurance(20),
                fight_mod: vec![FightModifier::Timed(2, Box::new(FightModifier::Evaded(ChapterId(88))))],
            },
        );
        assert_eq!(cinfo.get_ratio(), CombatSkill(-5));
        let mut memo = Memoz::default();
        let mut actual = cinfo.fight(&mut memo);
        actual.sort();
        let mut expected: Vec<Proba<BigRational, Escaped<Endurance>>> = [
            (7, 1, 125),
            (8, 3, 125),
            (9, 6, 125),
            (10, 17, 250),
            (11, 21, 250),
            (12, 21, 250),
            (13, 49, 500),
            (14, 51, 500),
            (15, 27, 250),
            (16, 91, 1000),
            (17, 81, 1000),
            (18, 51, 1000),
            (19, 11, 200),
            (20, 9, 250),
            (21, 3, 100),
            (22, 3, 250),
            (23, 3, 250),
            (25, 1, 125),
        ]
        .iter()
        .map(|(e, n, d)| Proba {
            v: Escaped::Escaped(ChapterId(88), Endurance(*e)),
            p: BigRational::from_i64(*n, *d),
        })
        .collect();
        expected.sort();
        assert_eq!(actual, expected);
    }

    fn quick_combatinfo(
        lwe: i8,
        lws: u8,
        ope: i8,
        ops: i8,
        lwitems: &[Item],
        lwdisc: &[Discipline],
        mods: &[FightModifier<BigRational>],
    ) -> CombatInfo<BigRational> {
        let ccst = CharacterConstant {
            bookid: Book::Book04,
            maxendurance: 20,
            combat_skill: lws,
            discipline: lwdisc.to_vec(),
        };
        let mut cvar = CharacterVariable::new(lwe);
        for i in lwitems {
            cvar.add_item(i, 1);
        }
        let fd = FightDetails {
            opponent: String::new(),
            combat_skill: CombatSkill(ops),
            endurance: Endurance(ope),
            fight_mod: mods.to_vec(),
        };
        CombatInfo::make(&ccst, &cvar, &fd)
    }

    fn quick_combat(
        lwe: i8,
        lws: u8,
        ope: i8,
        ops: i8,
        lwitems: &[Item],
        lwdisc: &[Discipline],
        mods: &[FightModifier<BigRational>],
    ) -> Outcome<BigRational, Escaped<Endurance>> {
        let cinfo = quick_combatinfo(lwe, lws, ope, ops, lwitems, lwdisc, mods);
        let mut memo = Memoz::default();
        let mut actual = cinfo.fight(&mut memo);
        actual.sort();
        actual
    }

    #[test]
    fn timed_invulnerability() {
        let actual = quick_combat(
            5,
            15,
            20,
            20,
            &[Item::Weapon(Weapon::Sword)],
            &[],
            &[FightModifier::Timed(2, Box::new(FightModifier::PlayerInvulnerable))],
        );
        let mut expected: Outcome<BigRational, Escaped<Endurance>> = vec![
            Proba {
                v: Escaped::Std(Endurance(0)),
                p: BigRational::from_i64(18873, 25000),
            },
            Proba {
                v: Escaped::Std(Endurance(1)),
                p: BigRational::from_i64(3457, 50000),
            },
            Proba {
                v: Escaped::Std(Endurance(2)),
                p: BigRational::from_i64(4009, 100000),
            },
            Proba {
                v: Escaped::Std(Endurance(3)),
                p: BigRational::from_i64(933, 20000),
            },
            Proba {
                v: Escaped::Std(Endurance(5)),
                p: BigRational::from_i64(223, 2500),
            },
        ];
        expected.sort();
        assert_eq!(actual, expected);
    }

    #[test]
    fn easy_fight() {
        let actual1 = quick_combat(5, 15, 20, 5, &[Item::Weapon(Weapon::Sword)], &[], &[]);
        let actual2 = quick_combat(5, 15, 20, 6, &[Item::Weapon(Weapon::Sword)], &[], &[]);
        let mut expected: Outcome<BigRational, Escaped<Endurance>> = vec![
            Proba {
                v: Escaped::Std(Endurance(0)),
                p: BigRational::from_i64(4, 25),
            },
            Proba {
                v: Escaped::Std(Endurance(1)),
                p: BigRational::from_i64(13, 100),
            },
            Proba {
                v: Escaped::Std(Endurance(2)),
                p: BigRational::from_i64(9, 50),
            },
            Proba {
                v: Escaped::Std(Endurance(3)),
                p: BigRational::from_i64(19, 100),
            },
            Proba {
                v: Escaped::Std(Endurance(4)),
                p: BigRational::from_i64(3, 50),
            },
            Proba {
                v: Escaped::Std(Endurance(5)),
                p: BigRational::from_i64(7, 25),
            },
        ];
        expected.sort();
        assert_eq!(actual1, expected);
        assert_eq!(actual2, expected);
    }

    #[test]
    fn timed_barehanded() {
        let actual = quick_combat(
            18,
            10,
            26,
            17,
            &[Item::Weapon(Weapon::Sword), Item::Shield, Item::BodyArmor],
            &[Discipline::MindBlast, Discipline::WeaponSkill(Weapon::Sword)],
            &[FightModifier::Timed(
                2,
                Box::new(FightModifier::CombatBonus(CombatSkill(-4))),
            )],
        );
        let mut expected: Outcome<BigRational, Escaped<Endurance>> = vec![
            Proba {
                v: Escaped::Std(Endurance(0)),
                p: BigRational::from_i64(320017, 1000000),
            },
            Proba {
                v: Escaped::Std(Endurance(1)),
                p: BigRational::from_i64(52671, 1000000),
            },
            Proba {
                v: Escaped::Std(Endurance(2)),
                p: BigRational::from_i64(37463, 500000),
            },
            Proba {
                v: Escaped::Std(Endurance(3)),
                p: BigRational::from_i64(7801, 100000),
            },
            Proba {
                v: Escaped::Std(Endurance(4)),
                p: BigRational::from_i64(16839, 250000),
            },
            Proba {
                v: Escaped::Std(Endurance(5)),
                p: BigRational::from_i64(211, 4000),
            },
            Proba {
                v: Escaped::Std(Endurance(6)),
                p: BigRational::from_i64(383, 10000),
            },
            Proba {
                v: Escaped::Std(Endurance(7)),
                p: BigRational::from_i64(219, 6250),
            },
            Proba {
                v: Escaped::Std(Endurance(8)),
                p: BigRational::from_i64(2389, 50000),
            },
            Proba {
                v: Escaped::Std(Endurance(9)),
                p: BigRational::from_i64(1137, 20000),
            },
            Proba {
                v: Escaped::Std(Endurance(10)),
                p: BigRational::from_i64(667, 12500),
            },
            Proba {
                v: Escaped::Std(Endurance(11)),
                p: BigRational::from_i64(493, 12500),
            },
            Proba {
                v: Escaped::Std(Endurance(12)),
                p: BigRational::from_i64(291, 10000),
            },
            Proba {
                v: Escaped::Std(Endurance(13)),
                p: BigRational::from_i64(7, 400),
            },
            Proba {
                v: Escaped::Std(Endurance(14)),
                p: BigRational::from_i64(109, 10000),
            },
            Proba {
                v: Escaped::Std(Endurance(15)),
                p: BigRational::from_i64(59, 10000),
            },
            Proba {
                v: Escaped::Std(Endurance(16)),
                p: BigRational::from_i64(89, 10000),
            },
            Proba {
                v: Escaped::Std(Endurance(17)),
                p: BigRational::from_i64(2, 625),
            },
            Proba {
                v: Escaped::Std(Endurance(18)),
                p: BigRational::from_i64(1, 125),
            },
        ];
        expected.sort();
        assert_eq!(actual, expected);
    }
}
