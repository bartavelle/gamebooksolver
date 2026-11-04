use crate::{ChapterStats, Output};
use gamebooksolver_base::lonewolf::chapter::*;
use gamebooksolver_base::lonewolf::mini::{Book, Discipline, Equipment, Flag, Flags, Item, Slot};
use gamebooksolver_base::solver::rational::Rational as _;
use gamebooksolver_base::solver::rational::r::MRational;
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

type Rational = MRational;

/// What is displayed in a node, in addition to the chapter name and the statistics.
#[derive(Debug, Deserialize)]
pub enum DotInfo {
    DExpectedAmount(Item),
    DHasItem(Item),
    DHasFlag(Flag),
    /// Written as `{"BackpackSize": []}`
    BackpackSize(#[allow(dead_code)] Vec<serde_json::Value>),
}

const fn url_part(book: Book) -> &'static str {
    match book {
        Book::Book01 => "01fftd",
        Book::Book02 => "02fotw",
        Book::Book03 => "03tcok",
        Book::Book04 => "04tcod",
        Book::Book05 => "05sots",
    }
}

fn item_name(book: Book, item: &Item) -> Option<&'static str> {
    use Item::{GenBackpack as B, GenSpecial as S};
    Some(match (book, item) {
        (Book::Book01, S(0)) => "Vordak Gem",
        (Book::Book01, B(0)) => "Torch and Tinderbox",
        (Book::Book01, S(1)) => "Golden Key",
        (Book::Book01, S(2)) => "Silver Key",
        (Book::Book01, S(3)) => "crystal Star Pendant",
        (Book::Book02, S(0)) => "Ticket",
        (Book::Book02, S(1)) => "Crystal star pendant",
        (Book::Book02, S(2)) => "Documents",
        (Book::Book02, S(3)) => "Seal of Hammerdal",
        (Book::Book02, S(4)) => "White pass",
        (Book::Book02, S(5)) => "Red pass",
        (Book::Book02, B(2)) => "Healing Potion (3)",
        (Book::Book03, S(0)) => "Ornate Silver Key",
        (Book::Book03, S(1)) => "Blue Stone Disc",
        (Book::Book03, S(2)) => "Blue Stone Triangle",
        (Book::Book03, S(3)) => "Firesphere",
        (Book::Book03, S(4)) => "Glowing Crystal",
        (Book::Book03, S(5)) => "Diamond",
        (Book::Book03, S(6)) => "Effigy",
        (Book::Book03, B(0)) => "Rope",
        (Book::Book03, B(1)) => "Potion of Green Gallowbrush",
        (Book::Book03, B(2)) => "Potion of Black Graveweed",
        (Book::Book03, Item::Potion5Hp) => "Red Potion of Laumspur",
        (Book::Book04, S(0)) => "Brass Key",
        (Book::Book04, S(1)) => "Firesphere",
        // this is also the Iron Key
        (Book::Book04, S(2)) => "Scroll",
        (Book::Book04, B(0)) => "Flask of Holy Water",
        (Book::Book04, B(1)) => "Torch",
        (Book::Book04, B(2)) => "Tinderbox",
        (Book::Book04, B(5)) => "Rope",
        (Book::Book05, S(1)) => "Blowpipe and Sleep Dart",
        (Book::Book05, S(2)) => "FireSphere",
        (Book::Book05, S(3)) => "Copper Key",
        (Book::Book05, S(4)) => "Gaoler's Keys",
        (Book::Book05, S(5)) => "Prism",
        (Book::Book05, S(6)) => "Sash",
        (Book::Book05, S(7)) => "Brass Whistle",
        (Book::Book05, S(8)) => "Black Crystal Cube",
        (Book::Book05, S(9)) => "Blue Stone Triangle",
        (Book::Book05, B(0)) => "Rope",
        (Book::Book05, B(2)) => "Oede herb",
        (Book::Book03 | Book::Book04 | Book::Book05, S(10)) => "Silver Helmet",
        (Book::Book04 | Book::Book05, S(11)) => "Onyx Medallion",
        _ => return None,
    })
}

fn raw_item(item: &Item) -> String {
    match item {
        Item::Weapon(w) => format!("Weapon {w:?}"),
        Item::GenSpecial(n) => format!("GenSpecial (GenCounter {n})"),
        Item::GenBackpack(n) => format!("GenBackpack (GenCounter {n})"),
        _ => format!("{item:?}"),
    }
}

fn show_item(book: Book, item: &Item) -> String {
    match (item, item_name(book, item)) {
        (Item::Weapon(w), _) => format!("{w:?}"),
        (_, Some(name)) => name.to_string(),
        _ => raw_item(item),
    }
}

fn show_flag(book: Book, flag: Flag) -> String {
    match (book, flag) {
        (Book::Book05, Flag::Special01) => "Jewelled Mace".to_string(),
        (Book::Book05, Flag::Knowledge01) => "Scroll".to_string(),
        (Book::Book03, Flag::Special01) => "KillKK1".to_string(),
        (Book::Book03, Flag::Special02) => "KillKK2".to_string(),
        (Book::Book03, Flag::Special03) => "KillKK3".to_string(),
        (Book::Book03, Flag::Special04) => "Backnar oil".to_string(),
        (Book::Book03, Flag::Knowledge01) => "GotA".to_string(),
        _ => format!("{flag:?}"),
    }
}

fn show_slot(slot: &Slot) -> String {
    format!("{slot:?}").to_lowercase()
}

fn item_desc(book: Book, item: &Item, count: u8) -> String {
    match item {
        Item::Gold => format!("{count}$"),
        Item::Weapon(w) => format!("{w:?}"),
        Item::Meal => format!("{count}M"),
        _ => show_item(book, item),
    }
}

fn cond_desc(book: Book, cond: &BoolCond) -> String {
    match cond {
        BoolCond::Always(true) => String::new(),
        BoolCond::Always(false) => "never".to_string(),
        BoolCond::HasItem(i, c) => format!("?{}", item_desc(book, i, *c)),
        BoolCond::Not(c) => format!("!{}", cond_desc(book, c)),
        BoolCond::COr(a, b) => format!("{}||{}", cond_desc(book, a), cond_desc(book, b)),
        BoolCond::CAnd(a, b) => format!("{}&&{}", cond_desc(book, a), cond_desc(book, b)),
        BoolCond::HasEndurance(e) => format!("endurance>={}", e.0),
        BoolCond::HasLevel(l) => format!("lvl {l:?}"),
        BoolCond::HasDiscipline(d) => match d {
            Discipline::SixthSense => "?6th".to_string(),
            Discipline::Hunting => "?hunt".to_string(),
            Discipline::MindOverMatter => "?MoM".to_string(),
            Discipline::Healing => "?heal".to_string(),
            Discipline::Tracking => "?trk".to_string(),
            Discipline::MindBlast => "?mb".to_string(),
            Discipline::Camouflage => "?camo".to_string(),
            Discipline::AnimalKinship => "?ak".to_string(),
            Discipline::WeaponSkill(w) => format!("?WeaponSkill {w:?}"),
            Discipline::MindShield => "?MindShield".to_string(),
        },
        BoolCond::HasFlag(f) => format!("f:{}", show_flag(book, *f)),
    }
}

fn simple_desc(book: Book, so: &SimpleOutcome) -> String {
    match so {
        SimpleOutcome::LoseItemKind(k) if k.len() == 4 => "LOSEALL".to_string(),
        SimpleOutcome::LoseItemKind(k) => {
            format!("lose all [{}]", k.iter().map(show_slot).collect::<Vec<_>>().join(","))
        }
        SimpleOutcome::LoseItem(i, c) => format!("-{}", item_desc(book, i, *c)),
        SimpleOutcome::GainItem(i, c) => format!("+{}", item_desc(book, i, *c as u8)),
        SimpleOutcome::MustEat(_) => "eat".to_string(),
        SimpleOutcome::HealPlayer(h) => format!("+{}HP", h.0),
        SimpleOutcome::DamagePlayer(h) => format!("-{}HP", h.0),
        SimpleOutcome::FullHeal => "+100%HP".to_string(),
        SimpleOutcome::HalfHeal => "+50%HP".to_string(),
        SimpleOutcome::SetFlag(f) => format!("+{}", show_flag(book, *f)),
        SimpleOutcome::ClearFlag(f) => format!("-{}", show_flag(book, *f)),
        SimpleOutcome::StoreEquipment => "store equipment".to_string(),
        SimpleOutcome::RetrieveEquipment => "retrieve equipment".to_string(),
    }
}

type Destinations = Vec<(u16, Vec<String>)>;

fn prepend(prefix: &[String], dests: Destinations) -> Destinations {
    dests
        .into_iter()
        .map(|(cid, descs)| (cid, prefix.iter().cloned().chain(descs).collect()))
        .collect()
}

fn fight_destinations(fd: &FightDetails<Rational>) -> Destinations {
    let by_kind = |desc: &str, select: &dyn Fn(&FightModifier<Rational>) -> Option<ChapterId>| -> Destinations {
        fd.fight_mod
            .iter()
            .filter_map(select)
            .map(|cid| (cid.0, vec![desc.to_string()]))
            .collect()
    };
    let mut o = by_kind("fake fight loss", &|m| match m {
        FightModifier::FakeFight(c) => Some(*c),
        _ => None,
    });
    o.extend(by_kind("on damage", &|m| match m {
        FightModifier::OnDamage(c) => Some(*c),
        _ => None,
    }));
    o.extend(by_kind("stopped fight", &|m| match m {
        FightModifier::StopFight(c) => Some(*c),
        _ => None,
    }));
    o.extend(by_kind("lost", &|m| match m {
        FightModifier::OnLose(c) => Some(*c),
        _ => None,
    }));
    o.extend(by_kind("slow fight", &|m| match m {
        FightModifier::Timed(_, sub) => match sub.as_ref() {
            FightModifier::OnNotYetWon(c) => Some(*c),
            _ => None,
        },
        _ => None,
    }));
    o.extend(by_kind("evaded", &|m| match m {
        FightModifier::Evaded(c) => Some(*c),
        _ => None,
    }));
    o.extend(by_kind("picked a one", &|m| match m {
        FightModifier::OnPickOne(c) => Some(*c),
        _ => None,
    }));
    o
}

fn outcome_destinations(book: Book, co: &ChapterOutcome<Rational>) -> Destinations {
    match co {
        ChapterOutcome::OneRound(_, w, e, l) => [w, e, l]
            .into_iter()
            .flat_map(|o| outcome_destinations(book, o))
            .collect(),
        ChapterOutcome::Simple(s, nxt) => {
            let descs: Vec<String> = s.iter().map(|so| simple_desc(book, so)).collect();
            prepend(&descs, outcome_destinations(book, nxt))
        }
        ChapterOutcome::LoseItemFrom(sl, n, nxt) => prepend(
            &[format!("lose {n} from {}", show_slot(sl))],
            outcome_destinations(book, nxt),
        ),
        ChapterOutcome::Fight(fd, nxt) => {
            let mut o = fight_destinations(fd);
            o.extend(outcome_destinations(book, nxt));
            o
        }
        ChapterOutcome::Randomly(lst) => lst
            .iter()
            .flat_map(|(p, o)| {
                let ratio = p.to_string();
                let ratio = if ratio.contains('/') { ratio } else { format!("{ratio}/1") };
                prepend(&[format!("r({ratio})")], outcome_destinations(book, o))
            })
            .collect(),
        ChapterOutcome::Conditionally(lst) => lst
            .iter()
            .flat_map(|(c, o)| prepend(&[cond_desc(book, c)], outcome_destinations(book, o)))
            .collect(),
        ChapterOutcome::Goto(c) => vec![(c.0, Vec::new())],
        ChapterOutcome::GameLost | ChapterOutcome::GameWon => Vec::new(),
        ChapterOutcome::EnterFight(_, _) | ChapterOutcome::Round(_, _, _) | ChapterOutcome::PassThrough(_, _) => {
            unreachable!("never found in a book file")
        }
    }
}

fn destinations(book: Book, d: &Decision<Rational>) -> Destinations {
    match d {
        Decision::Decisions(lst) => lst.iter().flat_map(|(_, sub)| destinations(book, sub)).collect(),
        Decision::CanTake(i, c, nxt) => prepend(&[format!("+{}", item_desc(book, i, *c))], destinations(book, nxt)),
        Decision::Cansell(_, _, nxt) => prepend(&["sell".to_string()], destinations(book, nxt)),
        Decision::Canbuy(i, c, nxt) => prepend(&[format!("$${}", item_desc(book, i, c.0))], destinations(book, nxt)),
        Decision::Conditional(c, nxt) => prepend(&[cond_desc(book, c)], destinations(book, nxt)),
        Decision::EvadeFight(_, cid, _, co) => {
            let mut o = vec![(cid.0, vec!["evade".to_string()])];
            o.extend(prepend(&["win".to_string()], outcome_destinations(book, co)));
            o
        }
        Decision::AfterCombat(nxt) | Decision::RetrieveEquipment(nxt) | Decision::RemoveItemFrom(_, _, nxt) => {
            destinations(book, nxt)
        }
        Decision::Special(SpecialChapter::Cartwheel) => {
            vec![(169, vec!["lost".to_string()]), (136, vec!["won".to_string()])]
        }
        Decision::Special(SpecialChapter::Portholes) => vec![(197, Vec::new())],
        Decision::Special(SpecialChapter::B05S127) => vec![(159, Vec::new()), (93, Vec::new())],
        Decision::None(co) => outcome_destinations(book, co),
    }
}

fn percent(r: &Rational) -> String {
    format!("{}%", r.clone().mul(&Rational::from(100)).to_fixed(3))
}

fn ratio_of(amount: Rational, total: &Rational) -> Rational {
    if total.is_z() { amount } else { amount.div(total) }
}

/// A field of a record label
enum Field {
    Label(String),
    Flip(Vec<Field>),
}

impl Field {
    fn pair(name: String, value: String) -> Self {
        Field::Flip(vec![Field::Label(name), Field::Label(value)])
    }

    fn render(&self, out: &mut String) {
        match self {
            Field::Label(l) => {
                for c in l.chars() {
                    if matches!(c, '{' | '}' | '|' | '<' | '>' | ' ') {
                        out.push('\\');
                    }
                    out.push(c);
                }
            }
            Field::Flip(fields) => {
                out.push('{');
                for (i, f) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push('|');
                    }
                    f.render(out);
                }
                out.push('}');
            }
        }
    }
}

fn quote(s: &str) -> String {
    format!("\"{}\"", s.replace('"', "\\\""))
}

fn info_field(book: Book, info: &DotInfo, stats: &ChapterStats) -> Option<Field> {
    let total_items: Rational = stats.items.values().cloned().sum();
    match info {
        DotInfo::DHasFlag(flag) => {
            let amount: Rational = stats
                .flags
                .iter()
                .filter(|(f, _)| Flags(**f).has(*flag))
                .map(|(_, p)| p.clone())
                .sum();
            let name = show_flag(book, *flag);
            presence_field(name, amount)
        }
        DotInfo::DHasItem(item) => {
            let amount: Rational = stats
                .items
                .iter()
                .filter(|(e, _)| e.has_itemb(item))
                .map(|(_, p)| p.clone())
                .sum();
            presence_field(show_item(book, item), ratio_of(amount, &total_items))
        }
        DotInfo::DExpectedAmount(item) => {
            let amount: Rational = stats
                .items
                .iter()
                .map(|(e, p)| p.clone().mul(&Rational::from(e.get_item_count(item) as i64)))
                .sum();
            let amount = ratio_of(amount, &total_items);
            if amount.is_z() {
                return None;
            }
            let value = amount.to_string();
            let value = if value.contains('/') { amount.to_fixed(2) } else { value };
            Some(Field::pair(show_item(book, item), value))
        }
        DotInfo::BackpackSize(_) => {
            let amount: Rational = stats.items.iter().map(|(e, p)| backpack_size(e, p)).sum();
            let total: Rational = stats.endurance.values().cloned().sum();
            Some(Field::pair(
                "Backpack items".to_string(),
                ratio_of(amount, &total).to_fixed(2),
            ))
        }
    }
}

fn presence_field(name: String, amount: Rational) -> Option<Field> {
    if amount.is_z() {
        None
    } else if amount == Rational::from(1) {
        Some(Field::pair(name, "yes".to_string()))
    } else {
        Some(Field::pair(name, percent(&amount)))
    }
}

fn backpack_size(e: &Equipment, p: &Rational) -> Rational {
    e.items()
        .iter()
        .filter(|(i, _)| i.slot() == Slot::Backpack)
        .map(|(_, q)| p.clone().mul(&Rational::from(*q as i64)))
        .sum()
}

/// The names and statistics of the opponents of a chapter
fn outcome_fights(co: &ChapterOutcome<Rational>, out: &mut Vec<String>) {
    let push = |fd: &FightDetails<Rational>, out: &mut Vec<String>| {
        let name = format!("{} {}/{}", fd.opponent, fd.combat_skill.0, fd.endurance.0);
        if !out.contains(&name) {
            out.push(name);
        }
    };
    match co {
        ChapterOutcome::Fight(fd, n) => {
            push(fd, out);
            outcome_fights(n, out);
        }
        ChapterOutcome::OneRound(fd, a, b, c) => {
            push(fd, out);
            [a, b, c].into_iter().for_each(|o| outcome_fights(o, out));
        }
        ChapterOutcome::Simple(_, n) | ChapterOutcome::LoseItemFrom(_, _, n) => outcome_fights(n, out),
        ChapterOutcome::Randomly(l) => l.iter().for_each(|(_, o)| outcome_fights(o, out)),
        ChapterOutcome::Conditionally(l) => l.iter().for_each(|(_, o)| outcome_fights(o, out)),
        _ => (),
    }
}

fn decision_fights(d: &Decision<Rational>, out: &mut Vec<String>) {
    match d {
        Decision::Decisions(l) => l.iter().for_each(|(_, n)| decision_fights(n, out)),
        Decision::CanTake(_, _, n)
        | Decision::Canbuy(_, _, n)
        | Decision::Cansell(_, _, n)
        | Decision::RemoveItemFrom(_, _, n)
        | Decision::Conditional(_, n)
        | Decision::AfterCombat(n)
        | Decision::RetrieveEquipment(n) => decision_fights(n, out),
        Decision::EvadeFight(_, _, fd, co) => {
            let name = format!("{} {}/{}", fd.opponent, fd.combat_skill.0, fd.endurance.0);
            if !out.contains(&name) {
                out.push(name);
            }
            outcome_fights(co, out);
        }
        Decision::None(co) => outcome_fights(co, out),
        Decision::Special(_) => (),
    }
}

/// Where a chapter can be evaded to
fn evasions(d: &Decision<Rational>, out: &mut BTreeSet<u16>) {
    match d {
        Decision::Decisions(l) => l.iter().for_each(|(_, n)| evasions(n, out)),
        Decision::CanTake(_, _, n)
        | Decision::Canbuy(_, _, n)
        | Decision::Cansell(_, _, n)
        | Decision::RemoveItemFrom(_, _, n)
        | Decision::Conditional(_, n)
        | Decision::AfterCombat(n)
        | Decision::RetrieveEquipment(n) => evasions(n, out),
        Decision::EvadeFight(_, cid, _, _) => {
            out.insert(cid.0);
        }
        Decision::None(_) | Decision::Special(_) => (),
    }
}

/// Whether the player has something to decide here, whatever the character
fn has_choice(d: &Decision<Rational>) -> bool {
    match d {
        Decision::None(_) | Decision::EvadeFight(_, _, _, _) => false,
        Decision::Conditional(_, n) | Decision::AfterCombat(n) => has_choice(n),
        _ => true,
    }
}

fn section_count(book: Book) -> u16 {
    if book == Book::Book05 { 400 } else { 350 }
}

/// A color between white and this one
fn tint(red: u8, green: u8, blue: u8, amount: f64) -> String {
    let mix = |c: u8| (255.0 - (255.0 - c as f64) * amount.clamp(0.0, 1.0)).round() as u8;
    format!("\"#{:02x}{:02x}{:02x}\"", mix(red), mix(green), mix(blue))
}

/// The statistics of a node, once the chapters that belong to it are merged
#[derive(Default)]
struct NodeStats {
    reached: Rational,
    value: Rational,
    deaths: Rational,
    evaded: Rational,
}

fn rational_percent(amount: &Rational, total: &Rational) -> String {
    percent(&ratio_of(amount.clone(), total))
}

fn node_label(
    book: Book,
    title: &str,
    fights: &[String],
    node: &NodeStats,
    stats: Option<&ChapterStats>,
    nstates: u64,
    display: &[DotInfo],
) -> Field {
    let mut fields = vec![Field::Label(title.to_string())];
    if !fights.is_empty() {
        fields.push(Field::Label(fights.join(", ")));
    }
    let reached = !node.reached.is_z();
    if let (true, Some(stats)) = (reached, stats) {
        let sum: Rational = stats
            .endurance
            .iter()
            .map(|(e, p)| p.clone().mul(&Rational::from(*e as i64)))
            .sum();
        fields.push(Field::Flip(vec![
            Field::pair("reached".to_string(), percent(&node.reached)),
            Field::pair("from here".to_string(), rational_percent(&node.value, &node.reached)),
            Field::pair("hp".to_string(), sum.div(&node.reached).to_fixed(2)),
            Field::pair("states".to_string(), nstates.to_string()),
        ]));
    }
    if !node.deaths.is_z() || !node.evaded.is_z() {
        let mut outcome = vec![Field::pair("die".to_string(), rational_percent(&node.deaths, &node.reached))];
        if !node.evaded.is_z() {
            outcome.push(Field::pair("evade".to_string(), rational_percent(&node.evaded, &node.reached)));
        }
        fields.push(Field::Flip(outcome));
    }
    if let (true, Some(stats)) = (reached, stats) {
        fields.extend(display.iter().filter_map(|i| info_field(book, i, stats)));
    }
    Field::Flip(fields)
}

fn write_statement(out: &mut String, head: &str, attrs: &[(&str, &str)]) {
    let padding = " ".repeat(head.len() + 5);
    write!(out, "    {head} [").unwrap();
    for (i, (name, value)) in attrs.iter().enumerate() {
        if i > 0 {
            write!(out, "\n{padding},").unwrap();
        }
        write!(out, "{name}={value}").unwrap();
    }
    out.push_str("];\n");
}

/// The node that shows each chapter. A chapter that was added to a book, without a choice, belongs to the only chapter that
/// leads to it. The chapters of the rounds of a fight belong to the chapter of the fight.
fn chapter_owners(
    book: Book,
    chapters: &[(ChapterId, Chapter<Rational>)],
    transitions: &BTreeMap<u16, BTreeMap<u16, Vec<String>>>,
    stats: &Output,
) -> BTreeMap<u16, u16> {
    let mut preds: BTreeMap<u16, BTreeSet<u16>> = BTreeMap::new();
    for (src, dsts) in transitions {
        for dst in dsts.keys() {
            preds.entry(*dst).or_default().insert(*src);
        }
    }
    let added: BTreeSet<u16> = chapters
        .iter()
        .filter(|(cid, c)| cid.0 > section_count(book) && !has_choice(&c.pchoice))
        .filter(|(cid, _)| preds.get(&cid.0).is_some_and(|p| p.len() == 1))
        .map(|(cid, _)| cid.0)
        .collect();
    let mut owners: BTreeMap<u16, u16> = BTreeMap::new();
    for (cid, _) in chapters {
        let mut cur = cid.0;
        while added.contains(&cur) {
            cur = *preds[&cur].first().expect("added chapters have a predecessor");
        }
        owners.insert(cid.0, cur);
    }
    let mut todo: Vec<u16> = owners.keys().copied().collect();
    while let Some(src) = todo.pop() {
        let owner = owners[&src];
        for dst in stats.res.get(&src).into_iter().flat_map(|s| s.transitions.keys()) {
            if !owners.contains_key(dst) {
                owners.insert(*dst, owner);
                todo.push(*dst);
            }
        }
    }
    owners
}

/// Renders the statistics of a solution over the graph of the chapters, in the dot format.
pub fn mkdot(
    chapters: &[(ChapterId, Chapter<Rational>)],
    display: &[DotInfo],
    stats: &Output,
    raw_rounds: bool,
) -> String {
    let book = stats.bookid;
    let by_id: BTreeMap<u16, &Chapter<Rational>> = chapters.iter().map(|(c, ch)| (c.0, ch)).collect();
    let mut transitions: BTreeMap<u16, BTreeMap<u16, Vec<String>>> = BTreeMap::new();
    for (cid, chapter) in chapters {
        // when several choices lead to the same chapter, the last one is described
        transitions.insert(cid.0, destinations(book, &chapter.pchoice).into_iter().collect());
    }
    let owners = if raw_rounds {
        BTreeMap::new()
    } else {
        chapter_owners(book, chapters, &transitions, stats)
    };
    let owner = |c: u16| owners.get(&c).copied().unwrap_or(c);

    // what happens at each node
    let mut nodes: BTreeMap<u16, NodeStats> = BTreeMap::new();
    let mut exits: BTreeMap<(u16, u16), Rational> = BTreeMap::new();
    let mut fights: BTreeMap<u16, Vec<String>> = BTreeMap::new();
    let mut evades: BTreeMap<u16, BTreeSet<u16>> = BTreeMap::new();
    for (cid, chapter) in chapters {
        let o = owner(cid.0);
        decision_fights(&chapter.pchoice, fights.entry(o).or_default());
        let mut targets = BTreeSet::new();
        evasions(&chapter.pchoice, &mut targets);
        evades.entry(o).or_default().extend(targets.into_iter().map(owner));
    }
    for (src, s) in &stats.res {
        let root = owner(*src);
        if root == *src {
            let node = nodes.entry(root).or_default();
            node.reached = s.endurance.values().cloned().sum();
            node.value = stats.values.get(src).cloned().unwrap_or_default();
        }
        for (dst, flow) in &s.transitions {
            let target = owner(*dst);
            if target != root {
                *exits.entry((root, target)).or_default() += flow.clone();
            }
        }
    }
    for (cid, dead) in &stats.deaths {
        nodes.entry(owner(*cid)).or_default().deaths += dead.clone();
    }
    for ((src, dst), flow) in &exits {
        if evades.get(src).is_some_and(|e| e.contains(dst)) {
            nodes.entry(*src).or_default().evaded += flow.clone();
        }
    }

    let mut out = String::from("digraph G {\n");
    let start = nodes.get(&1);
    let score = start.map(|n| percent(&n.value)).unwrap_or_default();
    let mut killers: Vec<(u16, Rational)> = nodes
        .iter()
        .filter(|(_, n)| !n.deaths.is_z())
        .map(|(c, n)| (*c, n.deaths.clone()))
        .collect();
    killers.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    let killers: Vec<String> = killers
        .iter()
        .take(5)
        .map(|(c, d)| {
            let who = fights.get(c).and_then(|f| f.first()).map(|f| format!(" {f}")).unwrap_or_default();
            format!("{c}{who} {}", percent(d))
        })
        .collect();
    write!(
        out,
        "    labelloc=t;\n    fontsize=24;\n    label={};\n",
        quote(&format!("Score {score}\\nDeaths, of all characters: {}", killers.join(", ")))
    )
    .unwrap();

    let default = NodeStats::default();
    for (cid, chapter) in chapters {
        if owner(cid.0) != cid.0 {
            continue;
        }
        let node = nodes.get(&cid.0).unwrap_or(&default);
        let nstates: u64 = owners
            .iter()
            .filter(|(_, o)| **o == cid.0)
            .filter_map(|(c, _)| stats.sttmap.get(c))
            .sum::<u64>()
            .max(stats.sttmap.get(&cid.0).copied().unwrap_or(0));
        let cstats = stats.res.get(&cid.0);
        let title = if cid.0 > section_count(book) {
            match transitions.iter().find(|(_, d)| d.contains_key(&cid.0)) {
                Some((p, _)) => format!("after {}", owner(*p)),
                None => cid.0.to_string(),
            }
        } else {
            chapter.title.clone()
        };
        let empty = Vec::new();
        let names = fights.get(&cid.0).unwrap_or(&empty);
        let mut label = String::new();
        node_label(book, &title, names, node, cstats, nstates, display).render(&mut label);
        let label = quote(&label);
        let section = if cid.0 > section_count(book) { title.trim_start_matches("after ").to_string() } else { chapter.title.clone() };
        let url = quote(&format!(
            "https://www.projectaon.org/en/xhtml/lw/{}/sect{section}.htm",
            url_part(book),
        ));
        let eating = chapter.pchoice.iter_outcomes().any(|co| {
            co.simple_outcomes().any(|so| matches!(so, SimpleOutcome::MustEat(_)))
        });
        let reached = !node.reached.is_z();
        let mut attrs: Vec<(&str, String)> = vec![
            ("shape", "record".to_string()),
            ("label", label),
            ("URL", url),
            ("style", "filled".to_string()),
        ];
        // the more characters die here, the more red, and otherwise the more reach it, the more blue
        let fill = if !node.deaths.is_z() {
            tint(0xe0, 0x30, 0x30, 0.15 + 0.85 * node.deaths.to_f64().sqrt())
        } else if reached {
            tint(0x60, 0xa0, 0xe0, 0.05 + 0.5 * node.reached.to_f64())
        } else {
            "white".to_string()
        };
        attrs.push(("fillcolor", fill));
        if !reached {
            attrs.push(("color", "\"#c0c0c0\"".to_string()));
            attrs.push(("fontcolor", "\"#a0a0a0\"".to_string()));
        } else if eating {
            attrs.push(("color", "\"#c09000\"".to_string()));
            attrs.push(("penwidth", "2".to_string()));
        } else if !names.is_empty() {
            attrs.push(("color", "\"#c02020\"".to_string()));
            attrs.push(("penwidth", "2".to_string()));
        } else if matches!(chapter.pchoice, Decision::Special(_)) {
            attrs.push(("color", "\"#2040c0\"".to_string()));
            attrs.push(("penwidth", "2".to_string()));
        }
        let attrs: Vec<(&str, &str)> = attrs.iter().map(|(n, v)| (*n, v.as_str())).collect();
        write_statement(&mut out, &cid.0.to_string(), &attrs);
    }

    // what the book says about the edges of a node
    let mut descs: BTreeMap<(u16, u16), String> = BTreeMap::new();
    for (src, dsts) in &transitions {
        for (dst, d) in dsts {
            if owner(*src) != owner(*dst) {
                descs.insert((owner(*src), owner(*dst)), d.join(" "));
            }
        }
    }
    let edge = |out: &mut String, src: u16, dst: u16, attrs: &[(&str, &str)], weight: &str| {
        let desc = descs.get(&(src, dst)).map(String::as_str).unwrap_or_default();
        let label = quote(format!("{weight} {desc}").trim());
        let head = format!("{src} -> {dst}");
        let mut all = attrs.to_vec();
        all.push(("label", &label));
        write_statement(out, &head, &all);
    };

    // the options that are never taken
    for &(src, dst) in descs.keys() {
        if !exits.contains_key(&(src, dst)) && by_id.contains_key(&src) && by_id.contains_key(&dst) {
            edge(
                &mut out,
                src,
                dst,
                &[("color", "\"#d0d0d0\""), ("fontcolor", "\"#b0b0b0\""), ("style", "dashed")],
                "",
            );
        }
    }
    for ((src, dst), flow) in &exits {
        let total = &nodes.get(src).unwrap_or(&default).reached;
        // how thick depends on how many characters go this way, and the label is out of those that are here
        let width = format!("{:.2}", 0.6 + 6.0 * flow.to_f64());
        edge(
            &mut out,
            *src,
            *dst,
            &[("color", "\"#303030\""), ("penwidth", &width)],
            &rational_percent(flow, total),
        );
    }
    out.push('}');
    out
}

#[cfg(test)]
mod test {
    use super::*;

    fn chapter(pchoice: Decision<Rational>) -> Chapter<Rational> {
        Chapter { title: String::new(), desc: String::new(), pchoice }
    }

    fn goto(c: u16) -> ChapterOutcome<Rational> {
        ChapterOutcome::Goto(ChapterId(c))
    }

    fn flows(l: &[(u16, i64, i64)]) -> ChapterStats {
        ChapterStats {
            transitions: l.iter().map(|(d, a, b)| (*d, Rational::from_i64(*a, *b))).collect(),
            ..ChapterStats::default()
        }
    }

    /// Chapter 1 is a fight of two rounds, that is won to 2, evaded to 3, or lost.
    fn example() -> (Vec<(ChapterId, Chapter<Rational>)>, Output) {
        let fd = FightDetails {
            opponent: "Thug".to_string(),
            combat_skill: CombatSkill(16),
            endurance: Endurance(25),
            fight_mod: vec![],
        };
        let mut chapters: Vec<(ChapterId, Chapter<Rational>)> = vec![
            (ChapterId(1), chapter(Decision::EvadeFight(Rounds(0), ChapterId(3), fd, goto(2)))),
            (ChapterId(2), chapter(Decision::None(ChapterOutcome::GameWon))),
            (ChapterId(3), chapter(Decision::None(ChapterOutcome::GameLost))),
            (ChapterId(4), chapter(Decision::None(goto(3)))),
        ];
        chapters[0].1.title = "1".to_string();
        let mut one = flows(&[(2000, 1, 1)]);
        one.endurance.insert(20, Rational::from(1));
        let mut res = BTreeMap::from([
            (1, one),
            (2000, flows(&[(2, 1, 2), (3, 1, 4), (2000, 1, 4)])),
        ]);
        res.get_mut(&2000).unwrap().endurance.insert(20, Rational::from(1));
        let mut two = flows(&[]);
        two.endurance.insert(20, Rational::from_i64(1, 2));
        res.insert(2, two);
        let mut three = flows(&[]);
        three.endurance.insert(20, Rational::from_i64(1, 4));
        res.insert(3, three);
        let output = Output {
            bookid: Book::Book02,
            res,
            sttmap: BTreeMap::new(),
            deaths: BTreeMap::from([(2000, Rational::from_i64(1, 4))]),
            values: BTreeMap::from([(1, Rational::from_i64(1, 2))]),
        };
        (chapters, output)
    }

    #[test]
    fn a_fight_is_a_single_node() {
        let (chapters, output) = example();
        let dot = mkdot(&chapters, &[], &output, false);
        assert!(!dot.contains("2000"), "{dot}");
        assert!(dot.contains("Thug\\ 16/25"), "{dot}");
        assert!(dot.contains("1 -> 2 ["), "{dot}");
        assert!(dot.contains("label=\"50.000% win\""), "{dot}");
        assert!(dot.contains("label=\"25.000% evade\""), "{dot}");
        // the chance to die and to evade, and the top of the graph
        assert!(dot.contains("{die|25.000%}|{evade|25.000%}"), "{dot}");
        assert!(dot.contains("Deaths, of all characters: 1 Thug 16/25 25.000%"), "{dot}");
    }

    #[test]
    fn never_taken_edges_are_faded() {
        let (chapters, output) = example();
        let dot = mkdot(&chapters, &[], &output, false);
        let edge = dot.split("4 -> 3 [").nth(1).expect("the edge is there");
        assert!(edge.starts_with("color=\"#d0d0d0\""), "{dot}");
    }

    #[test]
    fn rounds_can_be_shown() {
        let (chapters, output) = example();
        assert!(mkdot(&chapters, &[], &output, true).contains("1 -> 2000 ["));
    }
}
