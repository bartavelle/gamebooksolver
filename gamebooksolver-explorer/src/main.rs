mod lwexplore;

use bincode::Encode;
use clap::{Parser, Subcommand};
use gamebooksolver_base::lonewolf::chapter::*;
use gamebooksolver_base::lonewolf::data::Multistat;
use gamebooksolver_base::lonewolf::mini::CharacterVariableG;
use gamebooksolver_base::lonewolf::mini::CompactSolutionG;
use gamebooksolver_base::lonewolf::mini::GSolDump;
use gamebooksolver_base::lonewolf::mini::NoPrevEq;
use gamebooksolver_base::lonewolf::solve::solve_lws;
use gamebooksolver_base::solver::base::Proba;
use gamebooksolver_base::solver::base::optimize_outcome;
use gamebooksolver_base::solver::base::{ChoppedSolution, SolNode};
use lwexplore::explore_solution;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;

use gamebooksolver_base::lonewolf::mini::{
    Book, CVarState, CharacterConstant, CompactSolution, CompactState, Discipline, Equipment, Flag,
    Flags, Item, NextStep, SolDesc, SolutionDump, Weapon, mkchar,
};

use crate::lwexplore::explore_compact;

type Rational = gamebooksolver_base::solver::rational::r::MRational;

#[derive(Debug, Parser)]
struct OSolDesc {
    #[arg(long)]
    pub maxendurance: i8,
    #[arg(long = "skill")]
    pub combat_skill: u8,
    #[arg(long, short)]
    pub discipline: Vec<Discipline>,
    #[arg(long = "book")]
    pub bookid: Book,
    #[arg(long = "item", short = 'i')]
    pub items: Vec<Item>,
    #[arg(long = "gold")]
    pub gold: u8,
    #[arg(long = "flag", short = 'f')]
    pub flags: Vec<Flag>,
    #[arg(long)]
    pub finalchapters: Vec<u16>,
    #[arg(long)]
    pub bookpath: String,
    #[arg(long = "results")]
    pub resultspath: Option<String>,
    #[arg(long, default_value_t = 1)]
    pub startat: u16,
}

#[derive(Debug, Subcommand)]
enum PSub {
    LoadChapter,
    DumpStates {
        cid: u16,
    },
    CompareStates {
        otherpath: String,
        cid: u16,
    },
    Soldump(OSolDesc),
    Explore {
        bookpath: String,
    },
    Optimize {
        target: String,
        #[arg(long)]
        dummy: bool,
        #[arg(long)]
        bookpath: Option<String>,
    },
    ExploreCompact {
        bookpath: String,
    },
    /// soldump and optimize
    SoldumpOptimize(OSolDesc),
}

#[derive(Debug, Parser)]
struct Opt {
    /// Path to the solution file
    #[structopt(short, long)]
    solpath: String,
    /// Optional path to the .desc file
    #[structopt(long)]
    desc: Option<String>,
    /// Optional path to the .jot file
    #[structopt(long)]
    jot: Option<String>,
    /// Optional path to the .json file
    #[structopt(long)]
    json: Option<String>,
    /// Alternate commands
    #[structopt(subcommand)]
    cmd: Option<PSub>,
}

#[derive(Debug, Clone, serde::Serialize)]
struct ChapterStats {
    endurance: BTreeMap<i8, Rational>,
    flags: BTreeMap<u32, Rational>,
    items: BTreeMap<Equipment, Rational>,
    score: Rational,
    transitions: BTreeMap<u16, Rational>,
}

impl Default for ChapterStats {
    fn default() -> Self {
        ChapterStats {
            items: BTreeMap::new(),
            flags: BTreeMap::new(),
            endurance: BTreeMap::new(),
            transitions: BTreeMap::new(),
            score: Rational::from(0),
        }
    }
}

fn mksol<
    PREV: Into<Equipment> + From<Equipment> + std::hash::Hash + Ord + std::fmt::Debug + Clone,
>(
    ini: NextStep<PREV>,
    sttmap: HashMap<NextStep<PREV>, &ChoppedSolution<Rational, NextStep<PREV>>>,
) -> BTreeMap<u16, ChapterStats> {
    type Imap<PREV> = HashMap<NextStep<PREV>, BTreeMap<u16, ChapterStats>>;
    let mut memo: Imap<PREV> = HashMap::new();

    fn go<
        PREV: Into<Equipment> + From<Equipment> + std::hash::Hash + Ord + std::fmt::Debug + Clone,
    >(
        curmap: &mut Imap<PREV>,
        searchmap: &HashMap<NextStep<PREV>, &ChoppedSolution<Rational, NextStep<PREV>>>,
        curns: &NextStep<PREV>,
    ) -> BTreeMap<u16, ChapterStats> {
        if let Some(e) = curmap.get(curns) {
            return e.clone();
        }
        let src = match curns.chapter() {
            Some(cid) => cid,
            None => return BTreeMap::new(),
        };
        let cvar = match curns.cvar() {
            None => return BTreeMap::from([(src, ChapterStats::default())]),
            Some(s) => s,
        };

        // basic state
        let mut stats = ChapterStats::default();
        stats.items.insert(cvar.cequipment, Rational::from(1));
        stats.flags.insert(cvar.flags.0, Rational::from(1));
        stats.endurance.insert(cvar.curendurance, Rational::from(1));
        let mut out = BTreeMap::from([(src, stats)]);

        let cs = match searchmap.get(curns) {
            None => panic!("Missing state!?! {:?}", curns),
            Some(x) => x,
        };

        let mut update_choice =
            |p: Rational, cstt: &NextStep<PREV>, memo: &mut BTreeMap<u16, ChapterStats>| {
                let curmemo = go(curmap, searchmap, cstt);
                for (cid, curstats) in curmemo.into_iter() {
                    let e = memo.entry(cid).or_default();
                    for (i, ip) in curstats.items.into_iter() {
                        let ei = e.items.entry(i).or_default();
                        *ei += ip * &p;
                    }
                    for (f, ip) in curstats.flags.into_iter() {
                        let ei = e.flags.entry(f).or_default();
                        *ei += ip * &p;
                    }
                    for (en, ip) in curstats.endurance.into_iter() {
                        let ei = e.endurance.entry(en).or_default();
                        *ei += ip * &p;
                    }
                    for (dst, ip) in curstats.transitions.into_iter() {
                        let ei = e.transitions.entry(dst).or_default();
                        *ei += ip * &p;
                    }
                    e.score += curstats.score * &p;
                }

                if let Some(dst) = cstt.chapter() {
                    let e = memo.get_mut(&src).expect("there should be an entry here");
                    let e2 = e.transitions.entry(dst).or_default();
                    *e2 += p;
                }
            };

        match cs {
            ChoppedSolution::CLeafLost => {}
            ChoppedSolution::CLeaf(sc) => {
                let e = out.get_mut(&src).expect("there should be an entry here");
                e.score = sc.clone();
            }
            ChoppedSolution::CJump(_, stt) => {
                let e = out.get_mut(&src).expect("there should be an entry here");
                e.score = Rational::from(1);
                update_choice(Rational::from(1), stt, &mut out);
            }
            ChoppedSolution::CNode(_, pms) => {
                let e = out.get_mut(&src).expect("there should be an entry here");
                e.score = Rational::from(1);
                for (mstt, p) in pms.iter() {
                    if let Some(stt) = mstt {
                        update_choice(p.clone(), stt, &mut out);
                    }
                }
            }
        }
        curmap.insert(curns.clone(), out.clone());
        out
    }

    go(&mut memo, &sttmap, &ini)
}

fn count_states<A, PREV: Into<Equipment> + From<Equipment>>(
    cnt: &BTreeMap<NextStep<PREV>, A>,
) -> BTreeMap<u16, u64> {
    let mut o = BTreeMap::new();
    for (ns, _) in cnt.iter() {
        if let Some(cid) = ns.chapter() {
            let e = o.entry(cid).or_insert(0);
            *e += 1;
        }
    }
    o
}

#[derive(serde::Serialize)]
struct Output {
    bookid: String,
    res: BTreeMap<u16, ChapterStats>,
    sttmap: BTreeMap<u16, u64>,
}

fn load_soldump(
    pth: &str,
) -> anyhow::Result<gamebooksolver_base::lonewolf::mini::GSolDump<Rational>> {
    let file = File::open(pth)?;
    let mut dec = zstd::Decoder::new(file)?;
    let o = bincode::decode_from_std_read(&mut dec, bincode::config::standard())?;
    Ok(o)
}

fn chop_solution<PREV: Into<Equipment> + From<Equipment>>(
    sol: SolNode<Rational, NextStep<PREV>>,
) -> ChoppedSolution<Rational, NextStep<PREV>> {
    match sol {
        SolNode::Win(sc) => ChoppedSolution::CLeaf(sc),
        SolNode::Single(sc, u) => ChoppedSolution::CJump(sc, u),
        SolNode::Chosen(sc, outcome) => {
            ChoppedSolution::CNode(sc, outcome.into_iter().map(|x| (Some(x.v), x.p)).collect())
        }
    }
}

fn get_boundary(bookid: Book) -> (HashSet<Item>, HashSet<Flag>) {
    use Flag::*;
    use Item::{BodyArmor, Gold, Helmet, StrengthPotion4};

    match bookid {
        Book::Book01 => (
            [Helmet, BodyArmor, Gold].into_iter().collect(),
            HashSet::new(),
        ),
        Book::Book02 => ([Helmet, BodyArmor].into_iter().collect(), HashSet::new()),
        Book::Book03 => (
            [Item::Weapon(Weapon::Sommerswerd), Helmet, StrengthPotion4]
                .into_iter()
                .collect(),
            [HelmetIsSilver].into_iter().collect(),
        ),
        Book::Book04 => (
            [
                Item::Weapon(Weapon::Sommerswerd),
                Helmet,
                BodyArmor,
                StrengthPotion4,
            ]
            .into_iter()
            .collect(),
            [FoughtElix, PermanentSkillReduction2, HelmetIsSilver]
                .into_iter()
                .collect(),
        ),
        Book::Book05 => (HashSet::new(), HashSet::new()),
    }
}

fn load_results(
    disciplines: HashSet<Discipline>,
    iitems: &HashSet<Item>,
    iflags: &HashSet<Flag>,
    resdir: &str,
) -> anyhow::Result<HashMap<(Equipment, Flags), Rational>> {
    let mut o = HashMap::new();
    for fpath in std::fs::read_dir(resdir)? {
        let rpath = fpath?.path();
        let pth = rpath.to_str().ok_or_else(|| anyhow::anyhow!(":("))?;
        if !pth.ends_with(".json") {
            continue;
        }
        let f = File::open(pth)?;
        let ms: Multistat<Rational> = serde_json::de::from_reader(f)?;
        if ms.msentries.len() != 1 {
            eprintln!("invalid amount of entries in {}", pth);
            continue;
        }
        let curdiscs: HashSet<Discipline> = ms.msdisciplines.into_iter().collect();
        if curdiscs.difference(&disciplines).count() > 1 {
            continue;
        }
        let mut flags = Flags(0);
        for f in ms.variable.flags.into_iter().filter(|f| iflags.contains(f)) {
            flags.set(f);
        }
        let mut items = Equipment::default();
        for (i, q) in ms
            .variable
            .items
            .iter()
            .flatten()
            .filter(|(i, _)| iitems.contains(i))
        {
            items.add_item(i, *q);
        }
        if iitems.contains(&Item::Gold) {
            items.add_item(&Item::Gold, ms.variable.gold as i64);
        }
        let mentry: &Rational = &ms.msentries[0].mratio;
        let oentry: &mut Rational = o.entry((items, flags)).or_default();
        if mentry.cmp(oentry) == std::cmp::Ordering::Greater {
            *oentry = mentry.clone();
        }
    }
    Ok(o)
}

fn score_with(
    bookid: Book,
    mscoremap: &Option<HashMap<(Equipment, Flags), Rational>>,
    iitems: &HashSet<Item>,
    iflags: &HashSet<Flag>,
    e: Equipment,
    f: Flags,
) -> Rational {
    match mscoremap {
        None => Rational::from(1),
        Some(scoremap) => {
            let mut re = Equipment::default();
            for i in iitems {
                let ramount = e.get_item_count(i);
                let amount = if i == &Item::Gold {
                    match bookid {
                        Book::Book04 => {
                            if ramount < 4 {
                                0
                            } else {
                                15
                            }
                        }
                        _ => ramount,
                    }
                } else {
                    ramount
                };
                if amount > 0 {
                    re.add_item(i, amount as i64);
                }
            }
            let mut rf = Flags(0);
            for i in iflags {
                if f.has(*i) {
                    rf.set(*i);
                }
            }
            match scoremap.get(&(re, rf)) {
                None => {
                    eprintln!(
                        "Could not find matching combination for {:?} {:?}",
                        re.items(),
                        rf
                    );
                    eprintln!("know combinations are:");
                    for (ke, kf) in scoremap.keys() {
                        eprintln!(" * {:?} {:?}", ke.items(), kf);
                    }
                    panic!("failed :(");
                }
                Some(x) => x.clone(),
            }
        }
    }
}

fn compare_sols<
    PREV: Into<Equipment> + From<Equipment> + std::hash::Hash + Ord + Copy + std::fmt::Display,
>(
    m1: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>,
    m2: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>,
) {
    type Ocs<PREV> = Vec<(Rational, NextStep<PREV>)>;
    fn deloss<PREV: Into<Equipment> + From<Equipment>>(
        chapter: Option<u16>,
        ns: NextStep<PREV>,
    ) -> NextStep<PREV> {
        match &ns {
            NextStep::NewChapter(cid, cvar) => {
                if cvar.curendurance <= 0 {
                    NextStep::HasLost(chapter.unwrap_or(*cid))
                } else {
                    ns
                }
            }
            _ => ns,
        }
    }
    fn descored<PREV: Into<Equipment> + From<Equipment> + std::hash::Hash + Ord + Clone>(
        chapter: Option<u16>,
        cs: ChoppedSolution<Rational, NextStep<PREV>>,
    ) -> Ocs<PREV> {
        match cs {
            ChoppedSolution::CJump(_, ns) => vec![(Rational::from(1), deloss(chapter, ns.clone()))],
            ChoppedSolution::CNode(_, subs) => {
                let po: Vec<Proba<Rational, NextStep<PREV>>> = subs
                    .into_iter()
                    .filter_map(|(ms, p)| {
                        ms.map(|s| Proba {
                            p,
                            v: deloss(chapter, s),
                        })
                    })
                    .collect();
                let mut o = optimize_outcome(po)
                    .into_iter()
                    .map(|p| (p.p, p.v))
                    .collect::<Vec<_>>();
                o.sort();
                o
            }
            _ => Vec::new(),
        }
    }

    fn pretty_outcomes<PREV: Into<Equipment> + From<Equipment> + std::fmt::Display + Copy>(
        outcomes: &[(Rational, NextStep<PREV>)],
    ) {
        for (s, p) in outcomes {
            println!(" - {} {}", p, s);
        }
    }

    fn pretty_step<PREV: Into<Equipment> + From<Equipment> + std::fmt::Display + Copy>(
        ns: &NextStep<PREV>,
        outcomes: &[(Rational, NextStep<PREV>)],
    ) {
        println!("{}", ns);
        pretty_outcomes(outcomes);
    }

    let mkmap =
        |m: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>| -> HashMap<NextStep<PREV>, Ocs<PREV>> {
            m.into_iter()
                .map(|(ns, cs)| {
                    let ch = ns.chapter();
                    (ns, descored(ch, cs))
                })
                .collect()
        };

    let m1_ = mkmap(m1);
    let m2_ = mkmap(m2);

    let mut only1: Vec<(NextStep<PREV>, Ocs<PREV>)> = Vec::new();
    let mut only2: Vec<(NextStep<PREV>, Ocs<PREV>)> = Vec::new();
    let mut differing: Vec<(NextStep<PREV>, Ocs<PREV>, Ocs<PREV>)> = Vec::new();

    for (k1, v1) in m1_.iter() {
        match m2_.get(k1) {
            None => only1.push((k1.clone(), v1.clone())),
            Some(v2) => {
                if v1 != v2 {
                    differing.push((k1.clone(), v1.clone(), v2.clone()));
                }
            }
        }
    }
    for (k2, v2) in m2_ {
        if !m1_.contains_key(&k2) {
            only2.push((k2, v2));
        }
    }

    if !only1.is_empty() {
        println!("*** ONLY IN 1 ***");
        for (ns, outcomes) in only1.iter() {
            pretty_step(ns, outcomes);
        }
    }
    if !only2.is_empty() {
        println!("*** ONLY IN 2 ***");
        for (ns, outcomes) in only2.iter() {
            pretty_step(ns, outcomes);
        }
    }
    if !differing.is_empty() {
        println!("*** DIFF ***");
        for (k, v1, v2) in differing.iter() {
            println!("**** {} ****", k);
            pretty_outcomes(v1);
            println!("****");
            pretty_outcomes(v2);
        }
    }
}

fn diff_cvar<PREV: From<Equipment> + Into<Equipment>>(
    prev: &CharacterVariableG<PREV>,
    next: &CharacterVariableG<PREV>,
) -> String {
    let mut o: Vec<String> = Vec::new();
    match prev.curendurance.cmp(&next.curendurance) {
        std::cmp::Ordering::Greater => {
            o.push(format!("-{}hp", prev.curendurance - next.curendurance));
        }
        std::cmp::Ordering::Less => {
            o.push(format!("+{}hp", next.curendurance - prev.curendurance));
        }
        _ => (),
    }
    for i in &Item::VALUES {
        let pq = prev.cequipment.get_item_count(i);
        let nq = next.cequipment.get_item_count(i);
        match pq.cmp(&nq) {
            std::cmp::Ordering::Greater => {
                let desc = if pq - nq == 1 {
                    format!("-{:?}", i)
                } else {
                    format!("-{}{:?}", pq - nq, i)
                };
                o.push(desc);
            }
            std::cmp::Ordering::Less => {
                let desc = if nq - pq == 1 {
                    format!("+{:?}", i)
                } else {
                    format!("+{}{:?}", nq - pq, i)
                };
                o.push(desc);
            }
            _ => (),
        }
    }
    for f in &Flag::VALUES {
        if prev.flags.has(*f) && !next.flags.has(*f) {
            o.push(format!("-{:?}", f));
        }
        if !prev.flags.has(*f) && next.flags.has(*f) {
            o.push(format!("+{:?}", f));
        }
    }
    o.join(" ")
}

fn mkjot<
    PREV: From<Equipment>
        + Into<Equipment>
        + Default
        + std::fmt::Display
        + Copy
        + Eq
        + std::hash::Hash
        + std::fmt::Debug
        + Ord,
>(
    solpath: &str,
    soldump: &SolutionDump<Rational, PREV>,
    out: Option<&str>,
) -> anyhow::Result<()> {
    let ini = NextStep::NewChapter(1, mkchar(&soldump.soldesc.ccst, &soldump.soldesc.cvar));
    eprintln!("Starting condition: {:?} - {}", soldump.soldesc, ini);
    let sttmap = count_states(&soldump.content);
    let bookid = soldump.soldesc.ccst.bookid;
    let searchmap: HashMap<NextStep<PREV>, &ChoppedSolution<Rational, NextStep<PREV>>> = soldump
        .content
        .iter()
        .map(|(a, b)| (a.clone(), b))
        .collect();
    eprintln!("{} : {} states", solpath, searchmap.len());
    let res = mksol(ini, searchmap);
    let output = Output {
        bookid: format!("{:?}", bookid),
        res,
        sttmap,
    };
    match out {
        Some(path) => {
            let mut fs = std::fs::File::create(path)?;
            serde_json::to_writer_pretty(&mut fs, &output)?;
        }
        None => {
            let x = serde_json::to_string(&output)?;
            println!("{}", x);
        }
    }
    Ok(())
}

fn optimize<PREV: From<Equipment> + Into<Equipment> + Ord + Encode>(
    soldump: SolutionDump<Rational, PREV>,
    jname: &str,
    dummy: bool,
    bookpath: Option<&str>,
) -> anyhow::Result<CompactSolution<PREV>> {
    let useless_chapters = bookpath
        .map(|bookpath| {
            let fl = File::open(bookpath).unwrap();
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl).unwrap();
            book.into_iter()
                .filter_map(|(a, b)| {
                    if has_no_choice(&b.pchoice) {
                        None
                    } else {
                        Some(a.0)
                    }
                })
                .collect::<HashSet<_>>()
        })
        .unwrap_or_default();
    let mut content = if dummy {
        Vec::new()
    } else {
        soldump
            .content
            .into_iter()
            .filter_map(|(k, v)| CompactState::from_choppedsolution(k, v, &useless_chapters))
            .collect::<Vec<_>>()
    };
    content.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let compact = CompactSolution {
        soldesc: soldump.soldesc,
        content,
    };
    // save json recap
    let mut json_file = File::create(jname)?;
    serde_json::to_writer(&mut json_file, &compact.soldesc)?;

    Ok(compact)
}

fn dump_states<
    PREV: From<Equipment>
        + Into<Equipment>
        + Ord
        + Serialize
        + std::fmt::Display
        + Copy
        + std::fmt::Debug,
>(
    soldump: SolutionDump<Rational, PREV>,
    cid: u16,
) -> anyhow::Result<()> {
    for (ns, sol) in &soldump.content {
        if Some(cid) == ns.chapter() {
            match (ns, sol) {
                (
                    NextStep::NewChapter(_, pcvar),
                    ChoppedSolution::CJump(sc, NextStep::NewChapter(nid, ncvar)),
                ) => println!(
                    "{} -> JUMP {:.2}% ch:{} {}",
                    ns,
                    sc.to_f64() * 100.0,
                    nid,
                    &diff_cvar(pcvar, ncvar)
                ),
                (NextStep::NewChapter(_, pcvar), ChoppedSolution::CNode(sc, outcomes)) => {
                    println!("{} - {:.2}% ", ns, sc.to_f64() * 100.0);
                    for (mns, pb) in outcomes {
                        match mns {
                            Some(NextStep::NewChapter(nid, ncvar)) => {
                                println!(
                                    " [{:.2}%] ch:{} {}",
                                    pb.to_f64() * 100.0,
                                    nid,
                                    &diff_cvar(pcvar, ncvar)
                                )
                            }
                            _ => println!(" [{:.2}%] {:?}", pb.to_f64() * 100.0, mns),
                        }
                    }
                }
                _ => println!("{} -> {:?}", ns, sol),
            }
        }
    }
    Ok(())
}

fn compare_states<
    PREV: From<Equipment> + Into<Equipment> + Eq + std::hash::Hash + Ord + Copy + std::fmt::Display,
>(
    dump1: SolutionDump<Rational, PREV>,
    dump2: SolutionDump<Rational, PREV>,
    cid: u16,
) -> anyhow::Result<()> {
    println!("S1: {:?}", dump1.soldesc);
    let sols1: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>> = dump1
        .content
        .into_iter()
        .filter(|(ns, _)| Some(cid) == ns.chapter())
        .collect();
    println!("S2: {:?}", dump2.soldesc);
    let sols2: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>> = dump2
        .content
        .into_iter()
        .filter(|(ns, _)| Some(cid) == ns.chapter())
        .collect();
    compare_sols(sols1, sols2);
    Ok(())
}

fn soldump<
    PREV: From<Equipment>
        + Into<Equipment>
        + Default
        + std::fmt::Debug
        + std::hash::Hash
        + Copy
        + Ord
        + Serialize,
>(
    cnt: &OSolDesc,
    book: Vec<(ChapterId, Chapter<Rational>)>,
    json_path: &str,
) -> anyhow::Result<SolutionDump<Rational, PREV>> {
    let (iitems, iflags) = get_boundary(cnt.bookid);
    let mscoremap = cnt.resultspath.as_ref().and_then(|pth| {
        load_results(
            cnt.discipline.iter().copied().collect(),
            &iitems,
            &iflags,
            pth,
        )
        .ok()
    });

    let ccst = CharacterConstant {
        bookid: cnt.bookid,
        combat_skill: cnt.combat_skill,
        discipline: cnt.discipline.clone(),
        maxendurance: cnt.maxendurance,
    };
    let cvar = CVarState {
        flags: cnt.flags.clone(),
        gold: cnt.gold,
        items: if cnt.items.is_empty() {
            None
        } else {
            let mut mp: HashMap<Item, i64> = HashMap::new();
            for i in cnt.items.iter() {
                let e = mp.entry(*i).or_insert(0);
                *e += 1;
            }
            Some(mp.into_iter().collect())
        },
    };
    let finalchapters = if cnt.finalchapters.is_empty() {
        if ccst.bookid == Book::Book05 {
            vec![400]
        } else {
            vec![350]
        }
    } else {
        cnt.finalchapters.clone()
    };
    let fchapters: Vec<ChapterId> = finalchapters.iter().copied().map(ChapterId).collect();
    let soldesc = SolDesc {
        ccst,
        cvar,
        finalchapters,
    };
    let cvar = soldesc.cvariable::<PREV>();
    eprintln!("ini: {:?}", cvar);
    let sol = solve_lws(
        |e, f| score_with(soldesc.ccst.bookid, &mscoremap, &iitems, &iflags, e, f),
        &fchapters,
        &book,
        &soldesc.ccst,
        &cvar,
        cnt.startat,
    );
    let soldump = SolutionDump {
        soldesc,
        content: sol
            .into_iter()
            .map(|(ns, x)| (ns, chop_solution(x)))
            .collect(),
    };

    // save json summary
    let json_file = File::create(json_path)?;
    serde_json::to_writer_pretty(json_file, &Multistat::from_soldump(&soldump))?;

    Ok(soldump)
}

fn cmd_soldump(cnt: &OSolDesc, jsonpath: &str) -> anyhow::Result<GSolDump<Rational>> {
    let fl = File::open(&cnt.bookpath)?;
    let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl)?;
    let cos = book
        .iter()
        .flat_map(|x| x.1.pchoice.iter_outcomes())
        .collect::<Vec<_>>();
    let has_prev_eq = cos
        .iter()
        .flat_map(|co| co.simple_outcomes())
        .any(|so| matches!(so, SimpleOutcome::StoreEquipment));

    let sd = if has_prev_eq {
        soldump::<Equipment>(cnt, book, jsonpath).map(GSolDump::Prev)?
    } else {
        soldump::<NoPrevEq>(cnt, book, jsonpath).map(GSolDump::Noprev)?
    };
    Ok(sd)
}

fn cmd_optimize(
    soldump: GSolDump<Rational>,
    target: &str,
    jname: &str,
    dummy: bool,
    bookpath: Option<&str>,
) -> anyhow::Result<()> {
    let compact = match soldump {
        GSolDump::Prev(sd) => {
            optimize(sd, jname, dummy, bookpath).map(CompactSolutionG::WithStorage)?
        }
        GSolDump::Noprev(sd) => {
            optimize(sd, jname, dummy, bookpath).map(CompactSolutionG::NoStorage)?
        }
    };
    // save compact solution
    let file = File::create(target)?;
    let mut zwriter = zstd::Encoder::new(file, 9)?;
    bincode::encode_into_std_write(&compact, &mut zwriter, bincode::config::standard())?;
    zwriter.finish()?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::parse();

    match &opt.cmd {
        None => {
            let soldump = load_soldump(&opt.solpath)?;
            match soldump {
                GSolDump::Prev(sd) => mkjot(&opt.solpath, &sd, opt.jot.as_deref())?,
                GSolDump::Noprev(sd) => mkjot(&opt.solpath, &sd, opt.jot.as_deref())?,
            }
        }
        Some(PSub::Optimize {
            target,
            dummy,
            bookpath,
        }) => {
            let soldump = load_soldump(&opt.solpath)?;
            let jname = opt.desc.unwrap_or_else(|| target.to_owned() + ".desc");
            cmd_optimize(soldump, target, &jname, *dummy, bookpath.as_deref())?;
        }
        Some(PSub::Explore { bookpath }) => {
            let fl = File::open(bookpath)?;
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl)?;
            let soldump = load_soldump(&opt.solpath)?;
            match soldump {
                GSolDump::Prev(sd) => {
                    eprintln!("Starting condition: {:?}", sd.soldesc);
                    explore_solution(sd, &book);
                }
                GSolDump::Noprev(sd) => {
                    eprintln!("Starting condition: {:?}", sd.soldesc);
                    explore_solution(sd, &book);
                }
            }
        }
        Some(PSub::ExploreCompact { bookpath }) => {
            let fl = File::open(bookpath)?;
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl)?;
            let solfile = File::open(&opt.solpath)?;
            let mut dec = zstd::Decoder::new(solfile)?;
            let soldump: CompactSolutionG =
                bincode::decode_from_std_read(&mut dec, bincode::config::standard())?;
            match soldump {
                CompactSolutionG::WithStorage(cs) => explore_compact(&cs, &book),
                CompactSolutionG::NoStorage(cs) => explore_compact(&cs, &book),
            }
        }
        Some(PSub::LoadChapter) => {
            let file = File::open(opt.solpath.clone())?;
            let reader = BufReader::new(file);
            let u: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(reader)?;
            println!("{:?}", u);
        }
        Some(PSub::DumpStates { cid }) => {
            let soldump = load_soldump(&opt.solpath)?;
            match soldump {
                GSolDump::Prev(sd) => dump_states(sd, *cid)?,
                GSolDump::Noprev(sd) => dump_states(sd, *cid)?,
            }
        }
        Some(PSub::CompareStates { cid, otherpath }) => {
            let dump1 = load_soldump(&opt.solpath)?;
            let dump2 = load_soldump(otherpath)?;
            match (dump1, dump2) {
                (GSolDump::Prev(dump1), GSolDump::Prev(dump2)) => {
                    compare_states(dump1, dump2, *cid)?
                }
                (GSolDump::Noprev(dump1), GSolDump::Noprev(dump2)) => {
                    compare_states(dump1, dump2, *cid)?
                }
                _ => panic!("can't handle mixed solution dumps"),
            }
        }
        Some(PSub::Soldump(cnt)) => {
            let json_path = opt.json.unwrap_or_else(|| opt.solpath.to_owned() + ".json");
            let sd = cmd_soldump(cnt, &json_path)?;
            // save whole stuff
            let file = File::create(&opt.solpath)?;
            let mut zwriter = zstd::Encoder::new(file, 3)?;
            bincode::encode_into_std_write(&sd, &mut zwriter, bincode::config::standard())?;
            zwriter.finish()?;
        }
        Some(PSub::SoldumpOptimize(cnt)) => {
            let json_path = opt.json.unwrap_or_else(|| opt.solpath.to_owned() + ".json");
            let jname = opt.desc.unwrap_or_else(|| opt.solpath.to_owned() + ".desc");
            let sd = cmd_soldump(cnt, &json_path)?;
            match &sd {
                GSolDump::Prev(sd) => mkjot(&opt.solpath, sd, opt.jot.as_deref())?,
                GSolDump::Noprev(sd) => mkjot(&opt.solpath, sd, opt.jot.as_deref())?,
            }
            cmd_optimize(sd, &opt.solpath, &jname, false, Some(&cnt.bookpath))?;
        }
    }
    Ok(())
}

fn has_no_choice(d: &Decision<Rational>) -> bool {
    match d {
        Decision::Decisions(x) => x.len() == 1,
        Decision::RetrieveEquipment(_)
        | Decision::CanTake(_, _, _)
        | Decision::Canbuy(_, _, _)
        | Decision::Special(_)
        | Decision::EvadeFight(_, _, _, _)
        | Decision::RemoveItemFrom(_, _, _)
        | Decision::Cansell(_, _, _) => false,
        Decision::AfterCombat(d)
        | Decision::LoseItemFrom(_, _, d)
        | Decision::Conditional(_, d) => has_no_choice(d),
        Decision::None(_) => true,
    }
}
