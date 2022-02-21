use crate::lonewolf::mini::{max_hp, Discipline, Flag, Slot, Weapon};
use crate::lonewolf::rules::update_simple;
use crate::{
  Book, ChapterOutcome, CharacterConstant, CharacterVariable, Decision, Endurance, FightDetails,
  FightModifier, Item, SimpleOutcome,
};

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
  if cvar
    .cequipment
    .has_itemb(&Item::Weapon(Weapon::Sommerswerd))
  {
    if wskill == Some(Weapon::Sword)
      || wskill == Some(Weapon::ShortSword)
      || wskill == Some(Weapon::BroadSword)
    {
      UsedWeapon::WithSkill(Weapon::Sommerswerd)
    } else {
      UsedWeapon::WithoutSkill(Weapon::Sommerswerd)
    }
  } else {
    let weapons = cvar.cequipment.weapons();
    if weapons.is_empty() {
      UsedWeapon::NoWeapon
    } else if let Some(w) = weapons.iter().filter(|cw| Some(**cw) == wskill).next() {
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

fn has_combat(o: &ChapterOutcome) -> Option<&FightDetails> {
  use ChapterOutcome::*;
  match o {
    Fight(fd, _) => Some(fd),
    OneRound(fd, _, _, _) => Some(fd),
    Randomly(choices) => choices.iter().map(|x| &x.1).flat_map(has_combat).next(),
    Conditionally(conds) => conds.iter().map(|x| &x.1).flat_map(has_combat).next(),
    Simple(_, o2) => has_combat(o2),
    Goto(_) => None,
  }
}

pub fn flatten_decision(
  ccst: &CharacterConstant,
  cvar: &CharacterVariable,
  dec: &Decision,
) -> Vec<(Vec<String>, ChapterOutcome)> {
  // can be healed!
  if cvar.flags.has(Flag::Poisonned2) && cvar.cequipment.has_itemb(&Item::Laumspur) {
    let mut nv: CharacterVariable = cvar.clone();
    nv.flags.unset(Flag::Poisonned2);
    nv.cequipment.del_item(&Item::Laumspur, 1);
    return flatten_decision(ccst, &nv, dec);
  }
  let with_effect = |sos: &[SimpleOutcome], nxt: &Decision| -> Vec<(Vec<String>, ChapterOutcome)> {
    let mut nv = cvar.clone();
    for so in sos {
      update_simple(&mut nv, ccst, so);
    }
    flatten_decision(ccst, &nv, nxt).into_iter().map(|(mut dsc, out)| {
      dsc.insert(0, format!("{:?}", sos));
      (dsc, out)
    }).collect()
  };
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
      let rdec: &Decision = nxt;
      let ndec: Decision = cur.items().into_iter().fold(rdec.clone(), |acc, (i, q)| {
        Decision::CanTake(i, q, Box::new(acc))
      });
      flatten_decision(ccst, &nv, &ndec)
    }
    Decision::AfterCombat(nxt) => {
      let missing_hp = max_hp(ccst, cvar) - cvar.curendurance;
      let mut out = flatten_decision(ccst, cvar, nxt);
      if missing_hp >= 6 && cvar.cequipment.has_itemb(&Item::Potion6Hp) {
        out.extend(with_effect(&[
          SimpleOutcome::HealPlayer(Endurance(6)),
          SimpleOutcome::LoseItem(Item::Potion6Hp, 1),
        ], dec));
        return out;
      }
      if missing_hp >= 5 && cvar.cequipment.has_itemb(&Item::Potion5Hp) {
        out.extend(with_effect(&[
          SimpleOutcome::HealPlayer(Endurance(5)),
          SimpleOutcome::LoseItem(Item::Potion5Hp, 1),
        ], dec));
        return out;
      }
      if missing_hp >= 4 && cvar.cequipment.has_itemb(&Item::Potion4Hp) {
        out.extend(with_effect(&[
          SimpleOutcome::HealPlayer(Endurance(4)),
          SimpleOutcome::LoseItem(Item::Potion4Hp, 1),
        ], dec));
        return out;
      }
      if ccst.bookid != Book::Book01
        && missing_hp >= 4
        && cvar.cequipment.has_itemb(&Item::Laumspur)
      {
        out.extend(with_effect(&[
          SimpleOutcome::HealPlayer(Endurance(4)),
          SimpleOutcome::LoseItem(Item::Laumspur, 1),
        ], dec));
        return out;
      }
      if missing_hp >= 2 && cvar.cequipment.has_itemb(&Item::Potion2Hp) {
        out.extend(with_effect(&[
          SimpleOutcome::HealPlayer(Endurance(2)),
          SimpleOutcome::LoseItem(Item::Potion6Hp, 1),
        ], dec));
        return out;
      }
      if ccst.bookid == Book::Book05
        && missing_hp >= 10
        && cvar.cequipment.has_itemb(&Item::GenBackpack(2))
      {
        out.extend(with_effect(&[
          SimpleOutcome::HealPlayer(Endurance(10)),
          SimpleOutcome::LoseItem(Item::GenBackpack(2), 1),
        ], dec));
        return out;
      }
      return out;
    }
  }
}
