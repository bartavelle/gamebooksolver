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
        (Book::Book01, S(1)) => "Golden Key",
        (Book::Book01, S(2)) => "Silver Key",
        (Book::Book01, S(3)) => "crystal Star Pendant",
        (Book::Book02, S(0)) => "Ticket",
        (Book::Book02, S(1)) => "Crystal star pendant",
        (Book::Book02, S(2)) => "Documents",
        (Book::Book02, S(3)) => "Seal of Hammerdal",
        (Book::Book02, S(4)) => "White pass",
        (Book::Book02, S(5)) => "Red pass",
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
    format!("{slot:?}Slot")
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
        BoolCond::HasEndurance(e) => format!("endurance > Endurance {{getEndurance = {}}}", e.0),
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
            format!("LoseItemKind [{}]", k.iter().map(show_slot).collect::<Vec<_>>().join(","))
        }
        SimpleOutcome::LoseItem(i, c) => format!("-{}", item_desc(book, i, *c)),
        SimpleOutcome::GainItem(i, c) => match i {
            Item::Weapon(_) | Item::GenSpecial(_) | Item::GenBackpack(_) => format!("GainItem ({}) {c}", raw_item(i)),
            _ => format!("GainItem {} {c}", raw_item(i)),
        },
        SimpleOutcome::MustEat(_) => "eat".to_string(),
        SimpleOutcome::HealPlayer(h) => format!("+{}HP", h.0),
        SimpleOutcome::DamagePlayer(h) => format!("-{}HP", h.0),
        SimpleOutcome::FullHeal => "+100%HP".to_string(),
        SimpleOutcome::HalfHeal => "+50%HP".to_string(),
        SimpleOutcome::SetFlag(f) => format!("+{}", show_flag(book, *f)),
        SimpleOutcome::ClearFlag(f) => format!("-{}", show_flag(book, *f)),
        SimpleOutcome::StoreEquipment => "StoreEquipment".to_string(),
        SimpleOutcome::RetrieveEquipment => "RetrieveEquipment".to_string(),
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
    o.extend(by_kind("fake fight loss", &|m| match m {
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
            &[format!("loseitem {} {n}", show_slot(sl))],
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
            o.extend(prepend(&["fight".to_string()], outcome_destinations(book, co)));
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

fn node_style(chapter: &Chapter<Rational>, visited: bool) -> &'static [(&'static str, &'static str)] {
    const FILLED: (&str, &str) = ("style", "filled");
    let d = &chapter.pchoice;
    let eating = d
        .iter_outcomes()
        .find_map(|co| {
            co.simple_outcomes().find_map(|so| match so {
                SimpleOutcome::MustEat(h) => Some(*h),
                _ => None,
            })
        });
    let combat = matches!(d, Decision::EvadeFight(_, _, _, _))
        || d.iter_outcomes().any(|co| matches!(co.as_ref(), ChapterOutcome::Fight(_, _)));
    let lost = d.iter_outcomes().any(|co| matches!(co.as_ref(), ChapterOutcome::GameLost));
    match eating {
        Some(CanHunt::NoHunt) => &[FILLED, ("color", "\"/svg/yellow\"")],
        Some(CanHunt::Hunt) => &[("color", "\"/svg/yellow\"")],
        None if combat => &[("color", "\"/svg/red\"")],
        None if lost => &[FILLED, ("fontcolor", "\"/svg/white\"")],
        None if matches!(d, Decision::Special(_)) => &[("color", "\"/svg/blue\"")],
        None if visited => &[("color", "\"/svg/gray\"")],
        None => &[FILLED],
    }
}

fn node_label(
    book: Book,
    chapter: &Chapter<Rational>,
    stats: Option<&ChapterStats>,
    nstates: u64,
    display: &[DotInfo],
) -> Field {
    let score = match stats {
        Some(stats) => {
            let total: Rational = stats.endurance.values().cloned().sum();
            let hp = if total.is_z() {
                String::new()
            } else {
                let sum: Rational = stats
                    .endurance
                    .iter()
                    .map(|(e, p)| p.clone().mul(&Rational::from(*e as i64)))
                    .sum();
                format!("{} hp", sum.div(&total).to_fixed(2))
            };
            Field::Flip(vec![
                Field::Label(format!("{nstates}stts")),
                Field::Label(percent(&stats.score)),
                Field::Label(hp),
            ])
        }
        None => Field::Label(format!("{nstates} states")),
    };
    let mut fields = vec![Field::Label(chapter.title.clone()), score];
    if let Some(stats) = stats {
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

/// Renders the statistics of a solution over the graph of the chapters, in the dot format.
pub fn mkdot(chapters: &[(ChapterId, Chapter<Rational>)], display: &[DotInfo], stats: &Output) -> String {
    let book = stats.bookid;
    let mut out = String::from("digraph G {\n");

    for (cid, chapter) in chapters {
        let cstats = stats.res.get(&cid.0);
        let nstates = stats.sttmap.get(&cid.0).copied().unwrap_or(0);
        let mut label = String::new();
        node_label(book, chapter, cstats, nstates, display).render(&mut label);
        let label = quote(&label);
        let url = quote(&format!(
            "https://www.projectaon.org/en/xhtml/lw/{}/sect{}.htm",
            url_part(book),
            chapter.title
        ));
        let mut attrs = vec![("shape", "record"), ("label", label.as_str()), ("URL", url.as_str())];
        attrs.extend_from_slice(node_style(chapter, cstats.is_some()));
        write_statement(&mut out, &cid.0.to_string(), &attrs);
    }

    let mut all_transitions: BTreeMap<u16, BTreeMap<u16, Vec<String>>> = BTreeMap::new();
    for (cid, chapter) in chapters {
        // when several choices lead to the same chapter, the last one is described
        all_transitions.insert(cid.0, destinations(book, &chapter.pchoice).into_iter().collect());
    }
    let edge_desc = |src: u16, dst: u16| -> String {
        all_transitions
            .get(&src)
            .and_then(|m| m.get(&dst))
            .into_iter()
            .flatten()
            .map(|d| format!(" {}", serde_json::Value::from(d.as_str())))
            .collect()
    };
    let write_edge = |out: &mut String, src: u16, dst: u16, color: &str, weight: String| {
        let label = quote(&format!("{weight}{}", edge_desc(src, dst)));
        let head = format!("{src} -> {dst}");
        write_statement(out, &head, &[("color", color), ("label", &label)]);
    };

    let solution_edges: BTreeSet<(u16, u16)> = stats
        .res
        .iter()
        .flat_map(|(src, s)| s.transitions.keys().map(|dst| (*src, *dst)))
        .collect();
    for (src, dsts) in &all_transitions {
        for dst in dsts.keys().filter(|dst| !solution_edges.contains(&(*src, **dst))) {
            write_edge(&mut out, *src, *dst, "\"/svg/darkgray\"", String::new());
        }
    }
    for (src, s) in &stats.res {
        let total: Rational = s.transitions.values().cloned().sum();
        for (dst, weight) in &s.transitions {
            let weight = ratio_of(weight.clone(), &total);
            write_edge(&mut out, *src, *dst, "\"/svg/black\"", percent(&weight));
        }
    }
    out.push('}');
    out
}
