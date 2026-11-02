use crate::lonewolf::cartwheel::{CARTWHEEL_FREE_BET, CARTWHEEL_LABEL, MAX_CARTWHEEL, cartwheel_with_token};
use crate::lonewolf::chapter::{
    ChapterId, ChapterOutcome, Decision, Endurance, FightDetails, FightModifier, SimpleOutcome, SpecialChapter,
};
use crate::lonewolf::data::{WeaponSet, WeaponUse};
use crate::lonewolf::mini::{
    Book, CharacterConstant, CharacterVariableG, Discipline, Equipment, Flag, Item, Slot, StoredEquipment, Weapon,
    max_hp,
};
use crate::lonewolf::rules::{check, update_simple};
use crate::solver::rational::Rational;

/// A weapon that nothing can tell apart from another one of the same kind: the character has no skill with it, it is not
/// magical, it is not in the stored equipment, and it is not named in what remains to be played. Exchanging two of those
/// leaves a game that is identical up to their names, and so with the same score.
fn is_plain_weapon<PREV: StoredEquipment>(
    item: &Item,
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
    used: WeaponSet,
) -> bool {
    let stored: Equipment = cvar.cprevequipment.into();
    match item {
        Item::Weapon(Weapon::Sommerswerd | Weapon::MagicSpear) => false,
        Item::Weapon(w) => {
            !used.contains(*w) && !ccst.discipline.contains(&Discipline::WeaponSkill(*w)) && !stored.has_itemb(item)
        }
        _ => false,
    }
}

/// The items worth dropping to make room for `item`. Dropping any of the plain weapons has the same result, so that only
/// one of them is kept.
fn useful_drops<PREV: StoredEquipment>(
    item: &Item,
    lst: Vec<Item>,
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
    used: WeaponSet,
) -> Vec<Item> {
    let mut seen_plain = false;
    lst.into_iter()
        .filter(|l| {
            if l == item || !is_plain_weapon(l, ccst, cvar, used) {
                return true;
            }
            !std::mem::replace(&mut seen_plain, true)
        })
        .collect()
}

enum CanTake {
    SpaceAvailable,
    MustDrop(Vec<Item>),
    Nope,
}

/// In book 4, the Pickaxe and the Shovel "each occupy the same amount of space in your Backpack as two normal items".
fn backpack_slots(bookid: Book, item: &Item) -> usize {
    match item {
        Item::GenBackpack(3 | 4) if bookid == Book::Book04 => 2,
        _ => 1,
    }
}

fn can_take<PREV: Into<Equipment> + From<Equipment> + std::fmt::Debug>(
    item: &Item,
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
) -> CanTake {
    match item.slot() {
        Slot::Special => {
            if cvar.cequipment.has_itemb(item) {
                CanTake::Nope
            } else if item == &Item::SILVERHELMET && cvar.cequipment.has_itemb(&Item::Helmet) {
                CanTake::MustDrop(vec![Item::Helmet])
            } else if item == &Item::Helmet && cvar.cequipment.has_itemb(&Item::SILVERHELMET) {
                CanTake::MustDrop(vec![Item::SILVERHELMET])
            } else {
                CanTake::SpaceAvailable
            }
        }
        Slot::Pouch => CanTake::SpaceAvailable,
        Slot::Weapon => {
            let weapons = cvar.cequipment.weapons();
            match weapons.len() {
                0 => CanTake::SpaceAvailable,
                1 => CanTake::SpaceAvailable,
                _ => CanTake::MustDrop(weapons.into_iter().map(Item::Weapon).collect()),
            }
        }
        Slot::Backpack if !cvar.cequipment.has_itemb(&Item::Backpack) => CanTake::Nope,
        Slot::Backpack => {
            let mut bpitems = cvar.cequipment.in_backpack();
            if bpitems.len() > 8 {
                panic!("too many items in backpack :( bp={bpitems:?} cvar={cvar:?}")
            }
            let used: usize = bpitems.iter().map(|i| backpack_slots(ccst.bookid, i)).sum();
            if used + backpack_slots(ccst.bookid, item) <= 8 {
                CanTake::SpaceAvailable
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
        EnterFight(fd, _) => Some(fd),
        // a potion can only be used before the first round
        Round(_, _, _) => None,
        Randomly(choices) => choices.iter().map(|x| &x.1).flat_map(has_combat).next(),
        Conditionally(conds) => conds.iter().map(|x| &x.1).flat_map(has_combat).next(),
        Simple(_, o2) => has_combat(o2),
        Goto(_) => None,
        GameLost => None,
        GameWon => None,
        LoseItemFrom(_, _, o2) => has_combat(o2),
    }
}

pub fn flatten_decision<P: Rational, PREV: StoredEquipment>(
    wuse: &WeaponUse,
    ccst: &CharacterConstant,
    cvar: &CharacterVariableG<PREV>,
    dec: &Decision<P>,
) -> Vec<(Vec<String>, ChapterOutcome<P>)> {
    let with_effect = |sos: &[SimpleOutcome], nxt: &Decision<P>| -> Vec<(Vec<String>, ChapterOutcome<P>)> {
        let mut nv = cvar.clone();
        for so in sos {
            update_simple(&mut nv, ccst, so);
        }
        flatten_decision(wuse, ccst, &nv, nxt)
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
            let rdec: &Decision<P> = nxt;
            let ndec: Decision<P> = cvar
                .cequipment
                .items()
                .into_iter()
                // the backpack must be taken before what goes in it
                .rev()
                .fold(rdec.clone(), |acc, (i, q)| Decision::CanTake(i, q, Box::new(acc)));
            with_effect(&[SimpleOutcome::RetrieveEquipment], &ndec)
        }
        Decision::AfterCombat(nxt) => {
            let missing_hp = max_hp(ccst, cvar) - cvar.curendurance;
            let mut out = flatten_decision(wuse, ccst, cvar, nxt);

            if missing_hp > 0 {
                let usable_potions = [
                    (ccst.bookid == Book::Book05, Item::GenBackpack(2), 10),
                    (true, Item::Potion6Hp, 6),
                    (true, Item::Potion5Hp, 5),
                    (true, Item::Potion4Hp, 4),
                    (ccst.bookid != Book::Book01, Item::Laumspur, 4),
                    (true, Item::Potion2Hp, 2),
                ]
                .into_iter()
                .filter_map(|(usable, item, hpgained)| {
                    if usable && cvar.cequipment.has_itemb(&item) {
                        Some((item, hpgained))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
                // il faut conserver les potions qui restaurent trop de hp, si il y en a trop
                let (mut wasted, not_wasted) = usable_potions
                    .into_iter()
                    .partition::<Vec<_>, _>(|(_, hpgained)| *hpgained > missing_hp);
                let mut drinkable = not_wasted;
                if let Some(p) = wasted.pop() {
                    drinkable.push(p)
                }
                for (item, hpgained) in drinkable {
                    out.extend(with_effect(
                        &[
                            SimpleOutcome::HealPlayer(Endurance(hpgained)),
                            SimpleOutcome::LoseItem(item, 1),
                        ],
                        dec,
                    ));
                }
            }
            out
        }
        Decision::Conditional(bc, nxt) => {
            if check(ccst, cvar, bc) {
                flatten_decision(wuse, ccst, cvar, nxt)
            } else {
                Vec::new()
            }
        }
        Decision::Decisions(lst) => lst
            .iter()
            .flat_map(|(cdesc, d2)| {
                flatten_decision(wuse, ccst, cvar, d2)
                    .into_iter()
                    .map(|(mut adesc, res)| {
                        adesc.insert(0, cdesc.clone());
                        (adesc, res)
                    })
            })
            .collect(),
        Decision::CanTake(Item::Gold, q, nxt) => with_effect(&[SimpleOutcome::GainItem(Item::Gold, *q)], nxt),
        Decision::CanTake(_, 0, nxt) => flatten_decision(wuse, ccst, cvar, nxt),
        Decision::CanTake(i, 1, nxt) => {
            let notake = || flatten_decision(wuse, ccst, cvar, nxt);
            match can_take(i, ccst, cvar) {
                CanTake::Nope => notake(),
                CanTake::SpaceAvailable => with_effect(&[SimpleOutcome::GainItem(*i, 1)], nxt),
                CanTake::MustDrop(lst) => {
                    let mut out = notake();
                    let used = wuse.after(nxt);
                    let plain = is_plain_weapon(i, ccst, cvar, used);
                    for l in useful_drops(i, lst, ccst, cvar, used) {
                        // exchanging a plain weapon for another one is the same as not taking it
                        if backpack_slots(ccst.bookid, i) > 1 {
                            // dropping an item might not be enough
                            out.extend(with_effect(&[SimpleOutcome::LoseItem(l, 1)], dec))
                        } else if l != *i && !(plain && is_plain_weapon(&l, ccst, cvar, used)) {
                            out.extend(with_effect(
                                &[SimpleOutcome::LoseItem(l, 1), SimpleOutcome::GainItem(*i, 1)],
                                nxt,
                            ))
                        }
                    }
                    out
                }
            }
        }
        Decision::CanTake(i, q, nxt) => {
            let n = Decision::CanTake(*i, 1, Box::new(Decision::CanTake(*i, q - 1, nxt.clone())));
            flatten_decision(wuse, ccst, cvar, &n)
        }
        Decision::RemoveItemFrom(sl, n, nxt) => {
            let allslotitems: Vec<(Item, u8)> = cvar
                .cequipment
                .items()
                .into_iter()
                .filter(|(i, _)| i.slot() == *sl)
                .collect();
            if *n == 0 || allslotitems.is_empty() {
                flatten_decision(wuse, ccst, cvar, nxt)
            } else {
                let mut out = Vec::new();
                for (to_drop, _) in allslotitems {
                    let n2 = Decision::RemoveItemFrom(*sl, n - 1, nxt.clone());
                    out.extend(with_effect(&[SimpleOutcome::LoseItem(to_drop, 1)], &n2))
                }
                out
            }
        }
        Decision::Cansell(item, price, nxt) => {
            let mut out = flatten_decision(wuse, ccst, cvar, nxt);
            let cnt = cvar.cequipment.get_item_count(item);
            // TODO, sell more than 1 item
            if cnt > 0 {
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
            let mut out = flatten_decision(wuse, ccst, cvar, nxt);
            if cvar.cequipment.get_item_count(&Item::Gold) >= price.0 {
                match can_take(item, ccst, cvar) {
                    CanTake::Nope => (),
                    CanTake::SpaceAvailable => out.extend(with_effect(
                        &[
                            SimpleOutcome::LoseItem(Item::Gold, price.0),
                            SimpleOutcome::GainItem(*item, 1),
                        ],
                        nxt,
                    )),
                    CanTake::MustDrop(lst) => {
                        for to_drop in useful_drops(item, lst, ccst, cvar, wuse.after(nxt)) {
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
            let borne = |x: i64| P::from_i64(x.clamp(0, 10), 10);
            // the number picked, from 0 to 9, plus 10 must not be more than the combat skill
            let b1 = borne(ccst.combat_skill as i64 - 9);
            let b2 = borne(19 - ccst.combat_skill as i64);
            vec![(
                vec!["No decision".to_string()],
                ChapterOutcome::Randomly(vec![
                    (b1, ChapterOutcome::Goto(ChapterId(159))),
                    (
                        b2,
                        ChapterOutcome::Simple(
                            vec![SimpleOutcome::DamagePlayer(Endurance(1))],
                            Box::new(ChapterOutcome::Goto(ChapterId(93))),
                        ),
                    ),
                ]),
            )]
        }
        Decision::Special(SpecialChapter::Cartwheel) => {
            const FARE: u8 = 20;
            let gold = cvar.cequipment.get_item_count(&Item::Gold);
            let play = |label: &'static str, target: u8| {
                flatten_decision(
                    wuse,
                    ccst,
                    cvar,
                    &Decision::None(ChapterOutcome::Randomly(
                        cartwheel_with_token(gold, target)
                            .into_iter()
                            .map(|pb| {
                                let dest = if pb.v < FARE { 169 } else { 136 };
                                (
                                    pb.p,
                                    ChapterOutcome::Simple(
                                        vec![
                                            SimpleOutcome::LoseItemKind(vec![Slot::Pouch]),
                                            SimpleOutcome::GainItem(Item::Gold, pb.v),
                                        ],
                                        Box::new(ChapterOutcome::Goto(ChapterId(dest))),
                                    ),
                                )
                            })
                            .collect(),
                    )),
                )
                .into_iter()
                .map(move |(mut dsc, out)| {
                    dsc.insert(0, label.into());
                    (dsc, out)
                })
                .collect::<Vec<_>>()
            };
            let mut out = Vec::new();
            // with enough money for the fare, it is possible not to play at all, or to only bet the free token
            if gold >= FARE {
                out.extend(
                    flatten_decision(wuse, ccst, cvar, &Decision::None(ChapterOutcome::Goto(ChapterId(136))))
                        .into_iter()
                        .map(|(mut dsc, res)| {
                            dsc.insert(0, "cartwheel ok".into());
                            (dsc, res)
                        }),
                );
                out.extend(play(CARTWHEEL_FREE_BET, gold));
            }
            for target in std::cmp::max(FARE, gold + 1)..=MAX_CARTWHEEL {
                out.extend(play(CARTWHEEL_LABEL[target as usize - 20], target));
            }
            out
        }
        // do not play portholes!
        Decision::Special(SpecialChapter::Portholes) => {
            flatten_decision(wuse, ccst, cvar, &Decision::None(ChapterOutcome::Goto(ChapterId(197))))
        }
        _ => todo!("{:?}", dec),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lonewolf::{
        chapter::{BoolCond, Chapter, ChapterId, Price},
        combat::Memoz,
        mini::{CharacterVariable, NextStep},
        rules::update,
    };
    use num_rational::BigRational;
    use std::collections::HashMap;

    fn book_char(bookid: Book) -> CharacterConstant {
        CharacterConstant {
            bookid,
            combat_skill: 15,
            discipline: vec![],
            maxendurance: 20,
        }
    }

    fn goto(cid: u16) -> Decision<BigRational> {
        Decision::None(ChapterOutcome::Goto(ChapterId(cid)))
    }

    // every state that can be reached by making any of the choices
    fn choices_results(
        ccst: &CharacterConstant,
        cvar: &CharacterVariable,
        dec: &Decision<BigRational>,
    ) -> Vec<Vec<NextStep<Equipment>>> {
        let mut memo = Memoz::default();
        flatten_decision(&WeaponUse::default(), ccst, cvar, dec)
            .iter()
            .map(|(_, o)| {
                update(&mut memo, ccst, cvar, ChapterId(1), o)
                    .into_iter()
                    .map(|r| r.v)
                    .collect()
            })
            .collect()
    }

    #[test]
    fn retrieve_equipment() {
        let ccst = book_char(Book::Book05);
        let mut cvar = CharacterVariable::new(20);
        cvar.cprevequipment.add_item(&Item::Weapon(Weapon::Sword), 1);
        cvar.cprevequipment.add_item(&Item::Backpack, 1);
        cvar.cprevequipment.add_item(&Item::Meal, 2);
        cvar.cequipment.add_item(&Item::Gold, 3);
        cvar.cequipment.add_item(&Item::Potion2Hp, 1);
        let mut expected = CharacterVariable::new(20);
        expected.cequipment = cvar.cprevequipment;
        expected.cequipment.add_item(&Item::Gold, 3);
        expected.cequipment.add_item(&Item::Potion2Hp, 1);
        let results = choices_results(&ccst, &cvar, &Decision::RetrieveEquipment(Box::new(goto(58))));
        assert_eq!(results, vec![vec![NextStep::NewChapter(58, expected)]]);
    }

    #[test]
    fn drink_non_wasting_potions() {
        let ccst = book_char(Book::Book04);
        let mut cvar = CharacterVariable::new(14);
        for p in [Item::Potion6Hp, Item::Potion5Hp, Item::Potion4Hp, Item::Potion2Hp] {
            cvar.add_item(&p, 1);
        }
        let first_drink = |c: &CharacterVariable| -> Vec<String> {
            flatten_decision::<BigRational, _>(
                &WeaponUse::default(),
                &ccst,
                c,
                &Decision::AfterCombat(Box::new(goto(2))),
            )
            .into_iter()
            .filter_map(|(d, _)| d.first().cloned())
            .collect()
        };
        let drinks = first_drink(&cvar);
        for p in ["Potion6Hp", "Potion5Hp", "Potion4Hp", "Potion2Hp"] {
            assert!(drinks.iter().any(|d| d.contains(p)), "{p} not offered: {drinks:?}");
        }
        // 3 HP missing: only the smallest of the potions that heal too much is offered
        cvar.curendurance = 17;
        let drinks = first_drink(&cvar);
        assert!(drinks.iter().any(|d| d.contains("Potion2Hp")));
        assert!(drinks.iter().any(|d| d.contains("Potion4Hp")));
        assert!(!drinks.iter().any(|d| d.contains("Potion5Hp") || d.contains("Potion6Hp")));
    }

    #[test]
    fn can_decline_when_full() {
        let ccst = book_char(Book::Book04);
        let mut cvar = CharacterVariable::new(20);
        cvar.add_item(&Item::Backpack, 1);
        cvar.add_item(&Item::Meal, 7);
        cvar.add_item(&Item::Potion2Hp, 1);
        let dec = Decision::CanTake(Item::GenBackpack(5), 1, Box::new(goto(2)));
        let results = choices_results(&ccst, &cvar, &dec);
        assert_eq!(results.len(), 3);
        assert!(results.contains(&vec![NextStep::NewChapter(2, cvar)]));
    }

    #[test]
    fn cartwheel_outcomes() {
        let ccst = book_char(Book::Book02);
        let dec = Decision::Special(SpecialChapter::Cartwheel);
        let with_gold = |gold: i64| {
            let mut cvar = CharacterVariable::new(20);
            cvar.add_item(&Item::Gold, gold);
            cvar
        };
        // the free token makes it possible to play without any gold, but the fare can't be paid after losing
        for results in choices_results(&ccst, &with_gold(0), &dec) {
            assert!(results.contains(&NextStep::NewChapter(169, with_gold(0))));
            for r in results {
                if let NextStep::NewChapter(136, c) = r {
                    assert!(c.cequipment.has_item(&Item::Gold, 20));
                }
            }
        }
        let labels = |gold: i64| -> Vec<String> {
            flatten_decision::<BigRational, _>(&WeaponUse::default(), &ccst, &with_gold(gold), &dec)
                .into_iter()
                .map(|(d, _)| d[0].clone())
                .collect()
        };
        // the token is played first, so that it is possible to aim for the fare with 19 crowns
        assert_eq!(labels(19)[0], "Cartwheel target 20");
        // it is possible not to play
        let results = choices_results(&ccst, &with_gold(25), &dec);
        assert_eq!(results[0], vec![NextStep::NewChapter(136, with_gold(25))]);
        // or to only bet the token, which can't lose any crown
        assert_eq!(labels(25)[1], CARTWHEEL_FREE_BET);
        for r in &results[1] {
            let NextStep::NewChapter(136, c) = r else { panic!("{r:?}") };
            assert!(c.cequipment.has_item(&Item::Gold, 25));
        }
        // this is also possible with too much gold to aim for anything else
        assert_eq!(labels(35), vec!["cartwheel ok", CARTWHEEL_FREE_BET]);
        assert_eq!(labels(50), vec!["cartwheel ok", CARTWHEEL_FREE_BET]);
    }

    fn weapons_book(pchoice: Decision<BigRational>) -> (WeaponUse, Decision<BigRational>) {
        let chapter = |pchoice: Decision<BigRational>| Chapter {
            title: String::new(),
            desc: String::new(),
            pchoice,
        };
        let book: HashMap<ChapterId, Chapter<BigRational>> = [
            (ChapterId(1), chapter(pchoice.clone())),
            (ChapterId(2), chapter(Decision::None(ChapterOutcome::GameWon))),
        ]
        .into_iter()
        .collect();
        (WeaponUse::new(Book::Book02, &book), pchoice)
    }

    fn two_weapons() -> CharacterVariable {
        let mut cvar = CharacterVariable::new(20);
        cvar.add_item(&Item::Weapon(Weapon::Dagger), 1);
        cvar.add_item(&Item::Weapon(Weapon::Mace), 1);
        cvar
    }

    #[test]
    fn plain_weapons_are_not_exchanged() {
        let (wuse, dec) = weapons_book(Decision::CanTake(Item::Weapon(Weapon::Axe), 1, Box::new(goto(2))));
        let ccst = book_char(Book::Book02);
        assert_eq!(flatten_decision(&wuse, &ccst, &two_weapons(), &dec).len(), 1);
    }

    #[test]
    fn weapons_that_can_be_sold_are_exchanged() {
        // the Dagger is worth something later on, so that exchanging it for the Axe, or the opposite, changes the game
        let sell = |w: Weapon| Decision::Cansell(Item::Weapon(w), Price(3), Box::new(goto(2)));
        let (wuse, dec) = weapons_book(Decision::CanTake(
            Item::Weapon(Weapon::Axe),
            1,
            Box::new(sell(Weapon::Dagger)),
        ));
        let ccst = book_char(Book::Book02);
        let drops: Vec<String> = flatten_decision(&wuse, &ccst, &two_weapons(), &dec)
            .into_iter()
            .map(|(d, _)| d.join(" "))
            .collect();
        assert!(
            drops.iter().any(|d| d.contains("LoseItem(Weapon(Dagger), 1)")),
            "{drops:?}"
        );
        // the Mace can't be told apart from the Axe
        assert!(
            !drops.iter().any(|d| d.contains("LoseItem(Weapon(Mace), 1)")),
            "{drops:?}"
        );
    }

    #[test]
    fn skilled_weapon_is_not_plain() {
        let (wuse, dec) = weapons_book(Decision::CanTake(Item::Weapon(Weapon::Axe), 1, Box::new(goto(2))));
        let mut ccst = book_char(Book::Book02);
        ccst.discipline.push(Discipline::WeaponSkill(Weapon::Axe));
        // not taking it, or dropping either weapon, as these are no longer the same
        assert_eq!(flatten_decision(&wuse, &ccst, &two_weapons(), &dec).len(), 2);
    }

    #[test]
    fn weapon_named_later_is_not_plain() {
        let cond = Decision::Conditional(BoolCond::HasItem(Item::Weapon(Weapon::Axe), 1), Box::new(goto(2)));
        let any = Decision::Decisions(vec![("a".to_string(), cond), ("b".to_string(), goto(2))]);
        let (wuse, dec) = weapons_book(Decision::CanTake(Item::Weapon(Weapon::Axe), 1, Box::new(any)));
        let ccst = book_char(Book::Book02);
        let n = flatten_decision(&wuse, &ccst, &two_weapons(), &dec).len();
        assert!(n > 2, "{n}");
    }

    #[test]
    fn unknown_chapters_disable_the_filter() {
        let dec = Decision::CanTake(Item::Weapon(Weapon::Axe), 1, Box::new(goto(2)));
        let ccst = book_char(Book::Book02);
        assert_eq!(
            flatten_decision(&WeaponUse::default(), &ccst, &two_weapons(), &dec).len(),
            3
        );
    }

    #[test]
    fn all_items_can_be_sold_and_bought() {
        let mut ccst = book_char(Book::Book02);
        ccst.discipline.push(Discipline::WeaponSkill(Weapon::Dagger));
        let mut cvar = two_weapons();
        cvar.add_item(&Item::Gold, 5);
        let (wuse, dec) = weapons_book(Decision::Cansell(
            Item::Weapon(Weapon::Dagger),
            Price(1),
            Box::new(goto(2)),
        ));
        assert_eq!(flatten_decision(&wuse, &ccst, &cvar, &dec).len(), 2);
        // buying a weapon that nothing tells apart can still be useful to spend gold
        let (wuse, dec) = weapons_book(Decision::Canbuy(Item::Weapon(Weapon::Axe), Price(1), Box::new(goto(2))));
        assert_eq!(flatten_decision(&wuse, &ccst, &cvar, &dec).len(), 3);
    }

    #[test]
    fn canonical_merges_plain_weapons() {
        let (wuse, _) = weapons_book(goto(2));
        let ccst = book_char(Book::Book02);
        let cid = ChapterId(2);
        let with = |w: &[Weapon]| {
            let mut c = CharacterVariable::new(20);
            w.iter().for_each(|x| c.add_item(&Item::Weapon(*x), 1));
            c
        };
        let a = wuse.canonical(&ccst, cid, &with(&[Weapon::Axe, Weapon::Warhammer]));
        assert_eq!(a, wuse.canonical(&ccst, cid, &with(&[Weapon::Mace, Weapon::Dagger])));
        assert_eq!(a.cequipment.weapons().len(), 2);
        // the magical weapons and the skill keep their names, and the count is preserved
        let s = wuse.canonical(&ccst, cid, &with(&[Weapon::Sommerswerd, Weapon::Axe]));
        assert!(s.cequipment.has_itemb(&Item::Weapon(Weapon::Sommerswerd)));
        let mut skilled = book_char(Book::Book02);
        skilled.discipline.push(Discipline::WeaponSkill(Weapon::Axe));
        let k = wuse.canonical(&skilled, cid, &with(&[Weapon::Axe, Weapon::Mace]));
        assert!(k.cequipment.has_itemb(&Item::Weapon(Weapon::Axe)));
        assert_eq!(k.cequipment.weapons().len(), 2);
        // an unknown chapter renames nothing
        let c = with(&[Weapon::Axe]);
        assert_eq!(wuse.canonical(&ccst, ChapterId(99), &c), c);
    }

    fn liveness_book(pchoice: Decision<BigRational>, bookid: Book) -> (WeaponUse, ChapterId) {
        let chapter = |pchoice: Decision<BigRational>| Chapter {
            title: String::new(),
            desc: String::new(),
            pchoice,
        };
        let book: HashMap<ChapterId, Chapter<BigRational>> = [
            (ChapterId(1), chapter(pchoice)),
            (ChapterId(2), chapter(Decision::None(ChapterOutcome::GameWon))),
        ]
        .into_iter()
        .collect();
        (WeaponUse::new(bookid, &book), ChapterId(1))
    }

    fn full_character() -> CharacterVariable {
        let mut c = CharacterVariable::new(20);
        c.add_item(&Item::Backpack, 1);
        c.add_item(&Item::Meal, 2);
        c.add_item(&Item::Gold, 10);
        c.add_item(&Item::GenSpecial(1), 1);
        c.add_item(&Item::GenBackpack(1), 1);
        c.set_flag(Flag::Knowledge01);
        c.set_flag(Flag::HadCombat);
        c
    }

    #[test]
    fn what_is_never_read_is_forgotten() {
        let (wuse, cid) = liveness_book(goto(2), Book::Book02);
        let got = wuse.canonical(&book_char(Book::Book02), cid, &full_character());
        let mut expected = CharacterVariable::new(20);
        expected.add_item(&Item::Backpack, 1);
        expected.add_item(&Item::Meal, 2);
        expected.set_flag(Flag::HadCombat);
        assert_eq!(got, expected);
    }

    #[test]
    fn what_can_be_read_is_kept() {
        let c = full_character();
        let ccst = book_char(Book::Book02);
        let check = |cond: BoolCond, item: Option<Item>, flag: Option<Flag>| {
            let dec = Decision::Conditional(cond, Box::new(goto(2)));
            let (wuse, cid) = liveness_book(dec, Book::Book02);
            let got = wuse.canonical(&ccst, cid, &c);
            if let Some(i) = item {
                assert!(got.cequipment.has_itemb(&i), "{i:?}");
            }
            if let Some(f) = flag {
                assert!(got.flags.has(f), "{f:?}");
            }
        };
        check(BoolCond::HasItem(Item::Gold, 3), Some(Item::Gold), None);
        check(BoolCond::HasItem(Item::GenSpecial(1), 1), Some(Item::GenSpecial(1)), None);
        check(BoolCond::HasItem(Item::GenBackpack(1), 1), Some(Item::GenBackpack(1)), None);
        check(BoolCond::HasFlag(Flag::Knowledge01), None, Some(Flag::Knowledge01));
    }

    #[test]
    fn backpack_items_matter_while_space_matters() {
        let ccst = book_char(Book::Book02);
        let dec = Decision::CanTake(Item::Meal, 1, Box::new(goto(2)));
        let (wuse, cid) = liveness_book(dec, Book::Book02);
        let got = wuse.canonical(&ccst, cid, &full_character());
        assert!(got.cequipment.has_itemb(&Item::GenBackpack(1)));
        // a special item does not use any space
        assert!(!got.cequipment.has_itemb(&Item::GenSpecial(1)));
    }

    #[test]
    fn buying_needs_gold_and_a_sale_needs_the_item() {
        let ccst = book_char(Book::Book02);
        let (wuse, cid) = liveness_book(Decision::Canbuy(Item::GenSpecial(5), Price(3), Box::new(goto(2))), Book::Book02);
        assert!(wuse.canonical(&ccst, cid, &full_character()).cequipment.has_itemb(&Item::Gold));
        let (wuse, cid) = liveness_book(Decision::Cansell(Item::GenSpecial(1), Price(3), Box::new(goto(2))), Book::Book02);
        assert!(wuse.canonical(&ccst, cid, &full_character()).cequipment.has_itemb(&Item::GenSpecial(1)));
    }

    #[test]
    fn what_is_kept_for_the_next_book_is_not_forgotten() {
        // the gold is kept after book 1, and the flag after book 4
        let (wuse, cid) = liveness_book(goto(2), Book::Book01);
        assert!(wuse.canonical(&book_char(Book::Book01), cid, &full_character()).cequipment.has_itemb(&Item::Gold));
        let mut c = full_character();
        c.set_flag(Flag::FoughtElix);
        let (wuse, cid) = liveness_book(goto(2), Book::Book04);
        assert!(wuse.canonical(&book_char(Book::Book04), cid, &c).flags.has(Flag::FoughtElix));
    }

    #[test]
    fn what_scores_the_next_book_is_never_changed() {
        use crate::lonewolf::data::get_boundary;
        // a chapter that reads nothing, so that all that could be forgotten is
        for bookid in [Book::Book01, Book::Book02, Book::Book03, Book::Book04, Book::Book05] {
            let (wuse, cid) = liveness_book(goto(2), bookid);
            let ccst = book_char(bookid);
            let (items, flags) = get_boundary(bookid);
            for i in &items {
                let mut c = CharacterVariable::new(20);
                c.add_item(i, 3);
                let got = wuse.canonical(&ccst, cid, &c);
                assert_eq!(got.cequipment, c.cequipment, "{bookid:?} {i:?}");
            }
            for f in &flags {
                let mut c = CharacterVariable::new(20);
                c.set_flag(*f);
                assert!(wuse.canonical(&ccst, cid, &c).flags.has(*f), "{bookid:?} {f:?}");
            }
        }
    }

    #[test]
    fn gold_is_kept_up_to_what_can_be_used() {
        let buy = |i: Item, p: u8, d: Decision<BigRational>| Decision::Canbuy(i, Price(p), Box::new(d));
        let with_gold = |g: i64| {
            let mut c = CharacterVariable::new(20);
            c.add_item(&Item::Gold, g);
            c
        };
        let kept = |pchoice: Decision<BigRational>, bookid: Book, g: i64| {
            let (wuse, cid) = liveness_book(pchoice, bookid);
            wuse.canonical(&book_char(bookid), cid, &with_gold(g))
                .cequipment
                .get_item_count(&Item::Gold)
        };
        let shop = buy(Item::Meal, 5, buy(Item::Laumspur, 3, goto(2)));
        assert_eq!(kept(shop.clone(), Book::Book05, 20), 8);
        assert_eq!(kept(shop.clone(), Book::Book05, 6), 6);
        assert_eq!(kept(goto(2), Book::Book05, 20), 0);
        // it is part of the score of this book
        assert_eq!(kept(shop, Book::Book01, 20), 20);
        let cond = Decision::Conditional(BoolCond::Not(Box::new(BoolCond::HasItem(Item::Gold, 12))), Box::new(goto(2)));
        assert_eq!(kept(cond, Book::Book05, 20), 12);
        let spend = Decision::None(ChapterOutcome::Simple(
            vec![SimpleOutcome::LoseItem(Item::Gold, 7)],
            Box::new(ChapterOutcome::Goto(ChapterId(2))),
        ));
        assert_eq!(kept(spend, Book::Book05, 20), 7);
        assert_eq!(kept(Decision::Special(SpecialChapter::Cartwheel), Book::Book05, 20), 20);
        let lose = Decision::None(ChapterOutcome::LoseItemFrom(
            Slot::Pouch,
            1,
            Box::new(ChapterOutcome::Goto(ChapterId(2))),
        ));
        assert_eq!(kept(lose, Book::Book05, 20), 20);
    }

    #[test]
    fn what_needs_another_discipline_is_not_read() {
        let book_for = |pchoice: Decision<BigRational>| -> HashMap<ChapterId, Chapter<BigRational>> {
            [(
                ChapterId(1),
                Chapter {
                    title: String::new(),
                    desc: String::new(),
                    pchoice,
                },
            )]
            .into_iter()
            .chain([(
                ChapterId(2),
                Chapter {
                    title: String::new(),
                    desc: String::new(),
                    pchoice: Decision::None(ChapterOutcome::GameWon),
                },
            )])
            .collect()
        };
        let camouflage = || BoolCond::HasDiscipline(Discipline::Camouflage);
        let gold = || BoolCond::HasItem(Item::Gold, 1);
        let to2 = || ChapterOutcome::Goto(ChapterId(2));
        let without = book_char(Book::Book05);
        let with = CharacterConstant {
            discipline: vec![Discipline::Camouflage],
            ..book_char(Book::Book05)
        };
        let mut c = CharacterVariable::new(20);
        c.add_item(&Item::Gold, 3);
        let kept = |ccst: &CharacterConstant, book: &HashMap<ChapterId, Chapter<BigRational>>| {
            WeaponUse::for_character(ccst, book)
                .canonical(ccst, ChapterId(1), &c)
                .cequipment
                .has_itemb(&Item::Gold)
        };

        let gated = book_for(Decision::Conditional(
            BoolCond::CAnd(Box::new(camouflage()), Box::new(gold())),
            Box::new(goto(2)),
        ));
        assert!(!kept(&without, &gated));
        assert!(kept(&with, &gated));

        // only the first condition that holds is used
        let first = book_for(Decision::None(ChapterOutcome::Conditionally(vec![
            (camouflage(), to2()),
            (gold(), to2()),
            (BoolCond::Always(true), to2()),
        ])));
        assert!(kept(&without, &first));
        assert!(!kept(&with, &first));
    }

    #[test]
    fn unknown_chapters_forget_nothing() {
        let (wuse, _) = liveness_book(goto(2), Book::Book02);
        let c = full_character();
        assert_eq!(wuse.canonical(&book_char(Book::Book02), ChapterId(99), &c), c);
    }

    #[test]
    fn canonical_keeps_stored_weapons_consistent() {
        let (wuse, _) = weapons_book(goto(2));
        let ccst = book_char(Book::Book02);
        let cid = ChapterId(2);
        let with = |cur: &[Weapon], prev: &[Weapon]| {
            let mut c = CharacterVariable::new(20);
            cur.iter().for_each(|x| c.add_item(&Item::Weapon(*x), 1));
            prev.iter()
                .for_each(|x| c.cprevequipment.add_item(&Item::Weapon(*x), 1));
            c
        };
        use Weapon::*;
        let same = wuse.canonical(&ccst, cid, &with(&[Axe], &[Axe]));
        assert_eq!(same.cequipment.weapons(), same.cprevequipment.weapons());
        let diff = wuse.canonical(&ccst, cid, &with(&[Axe], &[Mace]));
        assert_ne!(diff.cequipment.weapons(), diff.cprevequipment.weapons());
        assert_eq!(same, wuse.canonical(&ccst, cid, &with(&[Spear], &[Spear])));
        assert_eq!(diff, wuse.canonical(&ccst, cid, &with(&[Spear], &[Dagger])));
    }

    #[test]
    fn having_any_weapon_names_none() {
        let any = Weapon::VALUES
            .iter()
            .map(|w| BoolCond::HasItem(Item::Weapon(*w), 1))
            .reduce(|a, b| BoolCond::COr(Box::new(a), Box::new(b)))
            .unwrap();
        let dec = Decision::CanTake(
            Item::Weapon(Weapon::Axe),
            1,
            Box::new(Decision::Conditional(any, Box::new(goto(2)))),
        );
        let (wuse, dec) = weapons_book(dec);
        // nothing names the weapons, so that exchanging them is pointless
        let ccst = book_char(Book::Book02);
        let mut cvar = two_weapons();
        cvar.cequipment.del_item(&Item::Weapon(Weapon::Mace), 1);
        cvar.cequipment.add_item(&Item::Weapon(Weapon::Spear), 1);
        let n = flatten_decision(&wuse, &ccst, &cvar, &dec).len();
        assert_eq!(n, 1);
    }

    #[test]
    fn locked_door() {
        let dec = Decision::Special(SpecialChapter::B05S127);
        let cvar = CharacterVariable::new(20);
        let opened = |combat_skill: u8| -> BigRational {
            let ccst = CharacterConstant {
                combat_skill,
                ..book_char(Book::Book05)
            };
            let res = update(
                &mut Memoz::default(),
                &ccst,
                &cvar,
                ChapterId(127),
                &flatten_decision(&WeaponUse::default(), &ccst, &cvar, &dec)[0].1,
            );
            let mut hurt = cvar;
            hurt.curendurance -= 1;
            assert!(res.iter().all(|r| r.p == BigRational::from_i64(0, 1)
                || r.v == NextStep::NewChapter(159, cvar)
                || r.v == NextStep::NewChapter(93, hurt)));
            res.into_iter()
                .filter(|r| r.v == NextStep::NewChapter(159, cvar))
                .map(|r| r.p)
                .next()
                .unwrap_or(BigRational::from_i64(0, 1))
        };
        assert_eq!(opened(9), BigRational::from_i64(0, 1));
        assert_eq!(opened(10), BigRational::from_i64(1, 10));
        assert_eq!(opened(15), BigRational::from_i64(6, 10));
        assert_eq!(opened(19), BigRational::from_i64(1, 1));
    }

    #[test]
    fn potent_strength_potion_is_a_backpack_item() {
        // book 3, section 311: "mark it on your Action Chart as a Backpack Item"
        assert_eq!(Item::StrengthPotion4.slot(), Slot::Backpack);
        let ccst = book_char(Book::Book03);
        let dec = Decision::CanTake(Item::StrengthPotion4, 1, Box::new(goto(2)));
        let cvar = CharacterVariable::new(20);
        assert_eq!(choices_results(&ccst, &cvar, &dec), vec![vec![NextStep::NewChapter(2, cvar)]]);
    }

    #[test]
    fn tools_use_two_backpack_slots() {
        let ccst = book_char(Book::Book04);
        let pick = Item::GenBackpack(3);
        let dec = Decision::CanTake(pick, 1, Box::new(goto(2)));
        let with_meals = |n: i64| {
            let mut c = CharacterVariable::new(20);
            c.add_item(&Item::Backpack, 1);
            c.add_item(&Item::Meal, n);
            c
        };
        let has_pick = |r: &Vec<NextStep<Equipment>>| {
            matches!(&r[0], NextStep::NewChapter(_, c) if c.cequipment.has_itemb(&pick))
        };
        // two free slots
        let results = choices_results(&ccst, &with_meals(6), &dec);
        assert_eq!(results.len(), 1);
        assert!(has_pick(&results[0]));
        // a single free slot: a meal must be dropped, or the tool refused
        let results = choices_results(&ccst, &with_meals(7), &dec);
        assert_eq!(results.len(), 2);
        assert_eq!(results.iter().filter(|r| has_pick(r)).count(), 1);
        // nothing free: two meals must be dropped
        let mut expected = with_meals(6);
        expected.add_item(&pick, 1);
        let results = choices_results(&ccst, &with_meals(8), &dec);
        assert!(results.contains(&vec![NextStep::NewChapter(2, expected)]));
        // it only takes one slot in the other books
        let results = choices_results(&book_char(Book::Book03), &with_meals(7), &dec);
        assert_eq!(results.len(), 1);
        assert!(has_pick(&results[0]));
    }

    #[test]
    fn backpack_items_need_a_backpack() {
        let ccst = book_char(Book::Book02);
        let cvar = CharacterVariable::new(20);
        let dec = Decision::CanTake(Item::Meal, 1, Box::new(goto(2)));
        assert_eq!(
            choices_results(&ccst, &cvar, &dec),
            vec![vec![NextStep::NewChapter(2, cvar)]]
        );
        // taken first, the backpack can hold it
        let dec = Decision::CanTake(Item::Backpack, 1, Box::new(dec));
        let mut expected = cvar;
        expected.add_item(&Item::Backpack, 1);
        expected.add_item(&Item::Meal, 1);
        assert_eq!(
            choices_results(&ccst, &cvar, &dec),
            vec![vec![NextStep::NewChapter(2, expected)]]
        );
    }

    #[test]
    fn losing_the_backpack_loses_its_content_and_armor_its_points() {
        let ccst = book_char(Book::Book02);
        let mut cvar = CharacterVariable::new(24);
        cvar.add_item(&Item::Backpack, 1);
        cvar.add_item(&Item::Meal, 2);
        cvar.add_item(&Item::BodyArmor, 1);
        let dec = Decision::None(ChapterOutcome::Simple(
            vec![
                SimpleOutcome::LoseItem(Item::Backpack, 1),
                SimpleOutcome::LoseItem(Item::BodyArmor, 1),
            ],
            Box::new(ChapterOutcome::Goto(ChapterId(2))),
        ));
        let mut expected = CharacterVariable::new(20);
        expected.curendurance = 20;
        assert_eq!(
            choices_results(&ccst, &cvar, &dec),
            vec![vec![NextStep::NewChapter(2, expected)]]
        );
    }

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
        let r1 = flatten_decision(&WeaponUse::default(), &ccst, &cvar, &dec);
        let r2 = flatten_decision(&WeaponUse::default(), &ccst, &cvar2, &dec);
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
