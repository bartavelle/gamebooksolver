// Prints what the solver does on each case, see ../run.sh. The optimizations about weapons are not part of the Rocq model, and
// are disabled by the default WeaponUse.
use gamebooksolver_base::lonewolf::chapter::*;
use gamebooksolver_base::lonewolf::combat::Memoz;
use gamebooksolver_base::lonewolf::data::WeaponUse;
use gamebooksolver_base::lonewolf::evasion::expand_evasions;
use gamebooksolver_base::lonewolf::mini::*;
use gamebooksolver_base::lonewolf::solve::step;
use gamebooksolver_base::solver::rational::{JRatio, MF64, Rational};
use std::collections::HashMap;
use std::io::{BufRead, Write};

const BOOKS: [Book; 5] = [Book::Book01, Book::Book02, Book::Book03, Book::Book04, Book::Book05];
fn load(n: usize) -> HashMap<ChapterId, Chapter<MF64>> {
    let f = std::fs::read_to_string(format!("{}/../../../json-chapters/book0{n}.json", env!("CARGO_MANIFEST_DIR"))).unwrap();
    let raw: Vec<(ChapterId, Chapter<JRatio>)> = serde_json::from_str(&f).unwrap();
    let mut b: HashMap<ChapterId, Chapter<MF64>> = raw.into_iter().map(|(c, ch)| (c, ch.map_proba(&|r: &JRatio| MF64::from_jratio(r)))).collect();
    expand_evasions(BOOKS[n - 1], &mut b);
    b
}
fn iname(i: &Item) -> String {
    match i {
        Item::Weapon(w) => format!("{w:?}"),
        Item::GenSpecial(n) => format!("SGen{n}"),
        Item::GenBackpack(n) => format!("BGen{n}"),
        x => format!("{x:?}"),
    }
}
fn parse_items(s: &str) -> Equipment {
    let mut e = Equipment::default();
    for kv in s.split(',').filter(|x| !x.is_empty()) {
        let (k, v) = kv.split_once(':').unwrap();
        let it = Item::VALUES.iter().find(|i| iname(i) == k).unwrap_or_else(|| panic!("item {k}"));
        e.add_item(it, v.parse().unwrap());
    }
    e
}
fn show_items(e: &Equipment) -> String {
    let mut v: Vec<String> = e.items().iter().map(|(i, q)| format!("{}:{q}", iname(i))).collect();
    v.sort();
    v.join(",")
}
fn all_flags() -> Vec<Flag> {
    (0..32).filter_map(|n| Flags(1u32 << n).all().into_iter().next()).collect()
}
fn show_flags(f: &Flags) -> String {
    let mut v: Vec<String> = f.all().iter().map(|x| format!("{x:?}")).collect();
    v.sort();
    v.join(",")
}
fn show(ns: &NextStep<Equipment>) -> String {
    match ns {
        NextStep::HasLost(_) => "L".into(),
        NextStep::HasWon(c) => format!("W e:{} f:{} i:{} p:{}", c.curendurance, show_flags(&c.flags), show_items(&c.cequipment), show_items(&c.cprevequipment)),
        NextStep::NewChapter(ch, c) => format!("c:{ch} e:{} f:{} i:{} p:{}", c.curendurance, show_flags(&c.flags), show_items(&c.cequipment), show_items(&c.cprevequipment)),
    }
}
fn disc(s: &str) -> Discipline {
    use Discipline::*;
    match s {
        "Camouflage" => Camouflage, "Hunting" => Hunting, "SixthSense" => SixthSense, "Tracking" => Tracking, "Healing" => Healing,
        "MindShield" => MindShield, "MindBlast" => MindBlast, "AnimalKinship" => AnimalKinship, "MindOverMatter" => MindOverMatter,
        w => {
            let w = w.strip_prefix("WS:").unwrap();
            let i = parse_items(&format!("{w}:1"));
            WeaponSkill(i.weapons()[0])
        }
    }
}
fn main() {
    let a: Vec<String> = std::env::args().collect();
    if a[1] == "chapters" {
        for n in 1..=5 {
            let b = load(n);
            let mut k: Vec<u16> = b.keys().map(|c| c.0).collect();
            k.sort();
            println!("{n} {}", k.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
        }
        return;
    }
    let books: Vec<_> = (1..=5).map(load).collect();
    let fl = all_flags();
    let out = std::io::stdout();
    let mut out = std::io::BufWriter::new(out.lock());
    std::panic::set_hook(Box::new(|_| {}));
    for (lno, line) in std::fs::File::open(&a[1]).map(std::io::BufReader::new).unwrap().lines().enumerate() {
        let line = line.unwrap();
        let p: Vec<&str> = line.split('|').collect();
        let (bk, cs, maxe, cur, ch): (usize, u8, i8, i8, u16) = (p[0].parse().unwrap(), p[1].parse().unwrap(), p[2].parse().unwrap(), p[3].parse().unwrap(), p[4].parse().unwrap());
        let cc = CharacterConstant { bookid: BOOKS[bk - 1], combat_skill: cs, maxendurance: maxe, discipline: p[5].split(',').filter(|x| !x.is_empty()).map(disc).collect() };
        let mut cv: CharacterVariableG<Equipment> = CharacterVariableG::new(maxe);
        cv.curendurance = cur;
        cv.cequipment = parse_items(p[6]);
        cv.cprevequipment = parse_items(p[8]);
        for f in p[7].split(',').filter(|x| !x.is_empty()) {
            let x = fl.iter().find(|y| format!("{y:?}") == f).unwrap_or_else(|| panic!("flag {f}"));
            cv.flags.set(*x);
        }
        let ns = NextStep::NewChapter(ch, cv);
        let (order, wuse) = (HashMap::new(), WeaponUse::default());
        let b = &books[bk - 1];
        let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let mut memo = Memoz::default();
            step(&mut memo, &order, &wuse, b, &cc, &ns)
        }));
        let mut opts: Vec<String> = match r {
            Err(_) => vec![],
            Ok(cs) => cs.iter().map(|c| {
                let mut h: std::collections::BTreeMap<String, f64> = Default::default();
                for o in &c.res { *h.entry(show(&o.v)).or_default() += o.p.t_f64(); }
                h.iter().filter(|(_, p)| **p > 1e-9).map(|(k, p)| format!("{k}={p:.8}")).collect::<Vec<_>>().join(";")
            }).collect(),
        };
        opts.sort();
        writeln!(out, "{lno}|{}", opts.join(" || ")).unwrap();
    }
}
