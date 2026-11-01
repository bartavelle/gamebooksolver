use crate::lonewolf::cartwheel::{CARTWHEEL_LABEL, MAX_CARTWHEEL, cartwheel};
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

fn can_take<PREV: Into<Equipment> + From<Equipment> + std::fmt::Debug>(
    item: &Item,
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
        Slot::Backpack => {
            let mut bpitems = cvar.cequipment.in_backpack();
            if bpitems.len() > 8 {
                panic!("too many items in backpack :( bp={bpitems:?} cvar={cvar:?}")
            }
            if bpitems.len() < 8 {
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
            match can_take(i, cvar) {
                CanTake::Nope => notake(),
                CanTake::SpaceAvailable => with_effect(&[SimpleOutcome::GainItem(*i, 1)], nxt),
                CanTake::MustDrop(lst) => {
                    let mut out = notake();
                    let used = wuse.after(nxt);
                    let plain = is_plain_weapon(i, ccst, cvar, used);
                    for l in useful_drops(i, lst, ccst, cvar, used) {
                        // exchanging a plain weapon for another one is the same as not taking it
                        if l != *i && !(plain && is_plain_weapon(&l, ccst, cvar, used)) {
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
                match can_take(item, cvar) {
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
            const FARE: u8 = 20;
            let gold = cvar.cequipment.get_item_count(&Item::Gold);
            if gold >= MAX_CARTWHEEL {
                flatten_decision(wuse, ccst, cvar, &Decision::None(ChapterOutcome::Goto(ChapterId(136))))
                    .into_iter()
                    .map(|(mut dsc, out)| {
                        dsc.insert(0, "cartwheel ok".into());
                        (dsc, out)
                    })
                    .collect()
            } else {
                // with enough money for the fare, it is possible not to play at all. Otherwise, the free token can't be
                // cashed, so the target must be at least one more than what it would be worth
                let no_play = (gold >= FARE).then_some(gold);
                let first_target = std::cmp::max(FARE, gold + 2);
                no_play
                    .into_iter()
                    .chain(first_target..=MAX_CARTWHEEL)
                    .flat_map(|target| {
                        let money = if target == gold { gold } else { gold + 1 };
                        flatten_decision(
                            wuse,
                            ccst,
                            cvar,
                            &Decision::None(ChapterOutcome::Randomly(
                                cartwheel(money, target)
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
                            dsc.insert(0, CARTWHEEL_LABEL[target as usize - 20].into());
                            (dsc, out)
                        })
                    })
                    .collect()
            }
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
        cvar.add_item(&Item::Meal, 7);
        cvar.add_item(&Item::Potion2Hp, 1);
        let dec = Decision::CanTake(Item::GenBackpack(3), 1, Box::new(goto(2)));
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
        // the token alone can't be turned into the missing crown
        let labels: Vec<_> = flatten_decision::<BigRational, _>(&WeaponUse::default(), &ccst, &with_gold(19), &dec)
            .into_iter()
            .map(|(d, _)| d[0].clone())
            .collect();
        assert_eq!(labels[0], "Cartwheel target 21");
        // it is possible not to play
        let results = choices_results(&ccst, &with_gold(25), &dec);
        assert_eq!(results[0], vec![NextStep::NewChapter(136, with_gold(25))]);
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
        (WeaponUse::new(&book), pchoice)
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
