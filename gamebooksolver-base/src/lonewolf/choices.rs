use crate::lonewolf::cartwheel::{cartwheel, CARTWHEEL_LABEL, MAX_CARTWHEEL};
use crate::lonewolf::chapter::{
    ChapterId, ChapterOutcome, Decision, Endurance, FightDetails, FightModifier, SimpleOutcome, SpecialChapter,
};
use crate::lonewolf::mini::{max_hp, Book, CharacterConstant, CharacterVariable, Discipline, Flag, Item, Slot, Weapon};
use crate::lonewolf::rules::{check, update_simple};
use crate::solver::rational::Rational;

enum UsedWeapon {
    WithSkill(Weapon),
    WithoutSkill(Weapon),
    NoWeapon,
}

fn used_weapon(ccst: &CharacterConstant, cvar: &CharacterVariable) -> UsedWeapon {
    let wskill = ccst
        .discipline
        .iter()
        .filter_map(|d| match d {
            Discipline::WeaponSkill(w) => Some(*w),
            _ => None,
        })
        .next();
    if cvar.cequipment.has_itemb(&Item::Weapon(Weapon::Sommerswerd)) {
        if wskill == Some(Weapon::Sword) || wskill == Some(Weapon::ShortSword) || wskill == Some(Weapon::BroadSword) {
            UsedWeapon::WithSkill(Weapon::Sommerswerd)
        } else {
            UsedWeapon::WithoutSkill(Weapon::Sommerswerd)
        }
    } else {
        let weapons = cvar.cequipment.weapons();
        if weapons.is_empty() {
            UsedWeapon::NoWeapon
        } else if let Some(w) = weapons.iter().find(|cw| Some(**cw) == wskill) {
            UsedWeapon::WithSkill(*w)
        } else {
            UsedWeapon::WithoutSkill(weapons[0])
        }
    }
}

fn important_item(i: &Item, ccst: &CharacterConstant, cvar: &CharacterVariable) -> bool {
    match i {
        Item::Weapon(Weapon::Sommerswerd) => true,
        Item::Weapon(Weapon::MagicSpear) => true,
        Item::Weapon(w) => {
            if w == &Weapon::Mace && ccst.bookid == Book::Book05 {
                true
            } else {
                match used_weapon(ccst, cvar) {
                    UsedWeapon::WithSkill(_) => false,
                    UsedWeapon::NoWeapon => true,
                    UsedWeapon::WithoutSkill(_) => ccst.discipline.contains(&Discipline::WeaponSkill(*w)),
                }
            }
        }
        _ => true,
    }
}

enum CanTake {
    SpaceAvailable(u8),
    MustDrop(Vec<Item>),
    Nope,
}

fn can_take(item: &Item, ccst: &CharacterConstant, cvar: &CharacterVariable) -> CanTake {
    match item.slot() {
        Slot::Special => {
            if cvar.cequipment.has_itemb(item) {
                CanTake::Nope
            } else {
                CanTake::SpaceAvailable(1)
            }
        }
        Slot::Pouch => CanTake::SpaceAvailable(50),
        Slot::Weapon => {
            let weapons = cvar.cequipment.weapons();
            match weapons.len() {
                0 => CanTake::SpaceAvailable(2),
                1 => CanTake::SpaceAvailable(1),
                _ => CanTake::MustDrop(match used_weapon(ccst, cvar) {
                    UsedWeapon::WithSkill(w) => weapons
                        .into_iter()
                        .filter_map(|x| if x == w { None } else { Some(Item::Weapon(x)) })
                        .collect(),
                    _ => weapons
                        .into_iter()
                        .filter(|x| *x != Weapon::Sommerswerd)
                        .map(Item::Weapon)
                        .collect(),
                }),
            }
        }
        Slot::Backpack => {
            let mut bpitems = cvar.cequipment.in_backpack();
            if bpitems.len() > 8 {
                panic!("too many items in backpack :(")
            }
            if bpitems.len() < 8 {
                CanTake::SpaceAvailable(8 - bpitems.len() as u8)
            } else {
                bpitems.dedup();
                CanTake::MustDrop(bpitems)
            }
        }
    }
}

fn has_combat<P>(o: &ChapterOutcome<P>) -> Option<&FightDetails<P>> {
    use ChapterOutcome::*;
    match o {
        Fight(fd, _) => Some(fd),
        OneRound(fd, _, _, _) => Some(fd),
        Randomly(choices) => choices.iter().map(|x| &x.1).flat_map(has_combat).next(),
        Conditionally(conds) => conds.iter().map(|x| &x.1).flat_map(has_combat).next(),
        Simple(_, o2) => has_combat(o2),
        Goto(_) => None,
        GameLost => None,
        GameWon => None,
    }
}

pub fn flatten_decision<P: Rational>(
    ccst: &CharacterConstant,
    cvar: &CharacterVariable,
    dec: &Decision<P>,
) -> Vec<(Vec<String>, ChapterOutcome<P>)> {
    let with_effect = |sos: &[SimpleOutcome], nxt: &Decision<P>| -> Vec<(Vec<String>, ChapterOutcome<P>)> {
        let mut nv = cvar.clone();
        for so in sos {
            update_simple(&mut nv, ccst, so);
        }
        flatten_decision(ccst, &nv, nxt)
            .into_iter()
            .map(|(mut dsc, out)| {
                dsc.insert(0, format!("{:?}", sos));
                (dsc, ChapterOutcome::Simple(sos.to_vec(), Box::new(out)))
            })
            .collect()
    };
    // can be healed!
    if cvar.flags.has(Flag::Poisonned2) && cvar.cequipment.has_itemb(&Item::Laumspur) {
        return with_effect(
            &[
                SimpleOutcome::ClearFlag(Flag::Poisonned2),
                SimpleOutcome::LoseItem(Item::Laumspur, 1),
            ],
            dec,
        );
    }
    match dec {
        Decision::None(o) => {
            if let Some(fd) = has_combat(o) {
                if fd.fight_mod.contains(&FightModifier::NoPotion) {
                    vec![(Vec::new(), o.clone())]
                } else {
                    let mut out = vec![(vec!["No strength potion".to_string()], o.clone())];
                    if cvar.cequipment.has_itemb(&Item::StrengthPotion4) {
                        out.push((
                            vec!["Use potent strength potion".to_string()],
                            ChapterOutcome::Simple(
                                vec![
                                    SimpleOutcome::SetFlag(Flag::PotentStrengthPotionActive),
                                    SimpleOutcome::LoseItem(Item::StrengthPotion4, 1),
                                ],
                                Box::new(o.clone()),
                            ),
                        ))
                    }
                    if cvar.cequipment.has_itemb(&Item::StrengthPotion) {
                        out.push((
                            vec!["Use strength potion".to_string()],
                            ChapterOutcome::Simple(
                                vec![
                                    SimpleOutcome::SetFlag(Flag::StrengthPotionActive),
                                    SimpleOutcome::LoseItem(Item::StrengthPotion, 1),
                                ],
                                Box::new(o.clone()),
                            ),
                        ))
                    }
                    out
                }
            } else {
                vec![(Vec::new(), o.clone())]
            }
        }
        Decision::RetrieveEquipment(nxt) => {
            let mut nv: CharacterVariable = cvar.clone();
            let cur = nv.cequipment;
            nv.cequipment = nv.cprevequipment;
            let rdec: &Decision<P> = nxt;
            let ndec: Decision<P> = cur
                .items()
                .into_iter()
                .fold(rdec.clone(), |acc, (i, q)| Decision::CanTake(i, q, Box::new(acc)));
            flatten_decision(ccst, &nv, &ndec)
        }
        Decision::AfterCombat(nxt) => {
            let missing_hp = max_hp(ccst, cvar) - cvar.curendurance;
            let mut out = flatten_decision(ccst, cvar, nxt);
            if missing_hp >= 6 && cvar.cequipment.has_itemb(&Item::Potion6Hp) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::HealPlayer(Endurance(6)),
                        SimpleOutcome::LoseItem(Item::Potion6Hp, 1),
                    ],
                    dec,
                ));
                return out;
            }
            if missing_hp >= 5 && cvar.cequipment.has_itemb(&Item::Potion5Hp) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::HealPlayer(Endurance(5)),
                        SimpleOutcome::LoseItem(Item::Potion5Hp, 1),
                    ],
                    dec,
                ));
                return out;
            }
            if missing_hp >= 4 && cvar.cequipment.has_itemb(&Item::Potion4Hp) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::HealPlayer(Endurance(4)),
                        SimpleOutcome::LoseItem(Item::Potion4Hp, 1),
                    ],
                    dec,
                ));
                return out;
            }
            if ccst.bookid != Book::Book01 && missing_hp >= 4 && cvar.cequipment.has_itemb(&Item::Laumspur) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::HealPlayer(Endurance(4)),
                        SimpleOutcome::LoseItem(Item::Laumspur, 1),
                    ],
                    dec,
                ));
                return out;
            }
            if missing_hp >= 2 && cvar.cequipment.has_itemb(&Item::Potion2Hp) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::HealPlayer(Endurance(2)),
                        SimpleOutcome::LoseItem(Item::Potion6Hp, 1),
                    ],
                    dec,
                ));
                return out;
            }
            if ccst.bookid == Book::Book05 && missing_hp >= 10 && cvar.cequipment.has_itemb(&Item::GenBackpack(2)) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::HealPlayer(Endurance(10)),
                        SimpleOutcome::LoseItem(Item::GenBackpack(2), 1),
                    ],
                    dec,
                ));
                return out;
            }
            out
        }
        Decision::Conditional(bc, nxt) => {
            if check(ccst, cvar, bc) {
                flatten_decision(ccst, cvar, nxt)
            } else {
                Vec::new()
            }
        }
        Decision::EvadeFight(nrounds, cid, fd, co) => {
            let mut nfd: FightDetails<P> = fd.clone();
            let evaded_fm = FightModifier::Evaded(*cid);
            nfd.fight_mod.push(if nrounds.0 > 0 {
                FightModifier::Timed(nrounds.0, Box::new(evaded_fm))
            } else {
                evaded_fm
            });
            vec![
                (
                    vec!["no evasion".to_string()],
                    ChapterOutcome::Fight(fd.clone(), Box::new(co.clone())),
                ),
                (
                    vec!["evasion".to_string()],
                    ChapterOutcome::Fight(nfd, Box::new(co.clone())),
                ),
            ]
        }
        Decision::Decisions(lst) => lst
            .iter()
            .flat_map(|(cdesc, d2)| {
                flatten_decision(ccst, cvar, d2).into_iter().map(|(mut adesc, res)| {
                    adesc.insert(0, cdesc.clone());
                    (adesc, res)
                })
            })
            .collect(),
        Decision::CanTake(i, q, nxt) => {
            if i == &Item::Gold {
                with_effect(&[SimpleOutcome::GainItem(Item::Gold, *q)], nxt)
            } else if *q == 0 {
                flatten_decision(ccst, cvar, nxt)
            } else if *q == 1 {
                let notake = || flatten_decision(ccst, cvar, nxt);
                match can_take(i, ccst, cvar) {
                    CanTake::Nope => notake(),
                    CanTake::SpaceAvailable(_) => with_effect(&[SimpleOutcome::GainItem(*i, 1)], nxt),
                    CanTake::MustDrop(lst) => {
                        if important_item(i, ccst, cvar) {
                            let mut out = Vec::new();
                            for l in lst {
                                out.extend(with_effect(
                                    &[SimpleOutcome::LoseItem(l, 1), SimpleOutcome::GainItem(*i, 1)],
                                    nxt,
                                ))
                            }
                            out
                        } else {
                            notake()
                        }
                    }
                }
            } else {
                let n = Decision::CanTake(*i, 1, Box::new(Decision::CanTake(*i, q - 1, nxt.clone())));
                flatten_decision(ccst, cvar, &n)
            }
        }
        Decision::RemoveItemFrom(Slot::Backpack, n, nxt) => {
            if cvar.cequipment.in_backpack().len() < *n as usize {
                Vec::new()
            } else {
                flatten_decision(ccst, cvar, &Decision::LoseItemFrom(Slot::Backpack, *n, nxt.clone()))
            }
        }
        Decision::LoseItemFrom(Slot::Backpack, n, nxt) => {
            let allbackpackitems: Vec<(Item, u8)> = cvar
                .cequipment
                .items()
                .into_iter()
                .filter(|(i, _)| i.slot() == Slot::Backpack)
                .collect();
            if *n == 0 || allbackpackitems.is_empty() {
                flatten_decision(ccst, cvar, nxt)
            } else {
                let mut out = Vec::new();
                for (to_drop, _) in allbackpackitems {
                    let n2 = Decision::LoseItemFrom(Slot::Backpack, n - 1, nxt.clone());
                    out.extend(with_effect(&[SimpleOutcome::LoseItem(to_drop, 1)], &n2))
                }
                out
            }
        }
        Decision::Cansell(item, price, nxt) => {
            let mut out = flatten_decision(ccst, cvar, nxt);
            let cnt = cvar.cequipment.get_item_count(item);
            // TODO, sell more than 1 item
            if cnt > 0 && !important_item(item, ccst, cvar) {
                out.extend(with_effect(
                    &[
                        SimpleOutcome::LoseItem(*item, 1),
                        SimpleOutcome::GainItem(Item::Gold, price.0),
                    ],
                    nxt,
                ))
            }
            out
        }
        Decision::Canbuy(item, price, nxt) => {
            let mut out = flatten_decision(ccst, cvar, nxt);
            if cvar.cequipment.get_item_count(&Item::Gold) >= price.0 && important_item(item, ccst, cvar) {
                match can_take(item, ccst, cvar) {
                    CanTake::Nope => (),
                    CanTake::SpaceAvailable(_) => out.extend(with_effect(
                        &[
                            SimpleOutcome::LoseItem(Item::Gold, price.0),
                            SimpleOutcome::GainItem(*item, 1),
                        ],
                        nxt,
                    )),
                    CanTake::MustDrop(lst) => {
                        for to_drop in lst {
                            out.extend(with_effect(
                                &[
                                    SimpleOutcome::LoseItem(to_drop, 1),
                                    SimpleOutcome::LoseItem(Item::Gold, price.0),
                                    SimpleOutcome::GainItem(*item, 1),
                                ],
                                nxt,
                            ));
                        }
                    }
                }
            }
            out
        }
        Decision::Special(SpecialChapter::B05S127) => {
            let borne = |x| {
                P::from_i64(
                    if x > 10 {
                        10
                    } else if x < 0 {
                        0
                    } else {
                        x
                    },
                    10,
                )
            };
            let b1 = borne(ccst.combat_skill as i64 - 10);
            let b2 = borne(20 - ccst.combat_skill as i64);
            vec![(
                vec!["No decision".to_string()],
                ChapterOutcome::Randomly(vec![
                    (b1, ChapterOutcome::Goto(ChapterId(159))),
                    (b2, ChapterOutcome::Goto(ChapterId(93))),
                ]),
            )]
        }
        Decision::Special(SpecialChapter::Cartwheel) => {
            let gold = cvar.cequipment.get_item_count(&Item::Gold);
            if gold >= MAX_CARTWHEEL {
                flatten_decision(ccst, cvar, &Decision::None(ChapterOutcome::Goto(ChapterId(136))))
                    .into_iter()
                    .map(|(mut dsc, out)| {
                        dsc.insert(0, "cartwheel ok".into());
                        (dsc, out)
                    })
                    .collect()
            } else {
                let start_target = std::cmp::min(std::cmp::max(20, gold), MAX_CARTWHEEL);
                (start_target..=MAX_CARTWHEEL)
                    .flat_map(|target| {
                        flatten_decision(
                            ccst,
                            cvar,
                            &Decision::None(ChapterOutcome::Randomly(
                                cartwheel(gold, target)
                                    .into_iter()
                                    .map(|pb| {
                                        (
                                            pb.p,
                                            ChapterOutcome::Simple(
                                                vec![
                                                    SimpleOutcome::LoseItemKind(vec![Slot::Pouch]),
                                                    SimpleOutcome::GainItem(Item::Gold, pb.v),
                                                ],
                                                Box::new(ChapterOutcome::Goto(ChapterId(136))),
                                            ),
                                        )
                                    })
                                    .collect(),
                            )),
                        )
                        .into_iter()
                        .map(move |(mut dsc, out)| {
                            dsc.insert(0, CARTWHEEL_LABEL[target as usize - 20].into());
                            (dsc, out)
                        })
                    })
                    .collect()
            }
        }
        // do not play portholes!
        Decision::Special(SpecialChapter::Portholes) => {
            flatten_decision(ccst, cvar, &Decision::None(ChapterOutcome::Goto(ChapterId(197))))
        }
        _ => todo!("{:?}", dec),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lonewolf::chapter::{BoolCond, ChapterId};
    use num_rational::BigRational;

    #[test]
    fn flatten_cond() {
        use ChapterOutcome::*;
        use Decision::*;
        let ccst = CharacterConstant {
            bookid: Book::Book01,
            combat_skill: 10,
            discipline: vec![],
            maxendurance: 20,
        };
        let cvar = CharacterVariable::new(20);
        let mut cvar2 = cvar.clone();
        cvar2.cequipment.add_item(&Item::GenBackpack(0), 1);
        let dec = Decision::Decisions::<BigRational>(vec![
            (
                "If you possess a rope".to_string(),
                Conditional(
                    BoolCond::HasItem(Item::GenBackpack(0), 1),
                    Box::new(None(Goto(ChapterId(305)))),
                ),
            ),
            (
                "otherwise".to_string(),
                Conditional(
                    BoolCond::Not(Box::new(BoolCond::HasItem(Item::GenBackpack(0), 1))),
                    Box::new(Decisions(vec![(
                        "if you do not possess a rope".to_string(),
                        None(Goto(ChapterId(387))),
                    )])),
                ),
            ),
        ]);
        let r1 = flatten_decision(&ccst, &cvar, &dec);
        let r2 = flatten_decision(&ccst, &cvar2, &dec);
        assert_eq!(
            r1,
            vec![(
                vec!["otherwise".to_string(), "if you do not possess a rope".to_string()],
                Goto(ChapterId(387))
            )]
        );
        assert_eq!(
            r2,
            vec![(vec!["If you possess a rope".to_string()], Goto(ChapterId(305)))]
        );
    }
}
