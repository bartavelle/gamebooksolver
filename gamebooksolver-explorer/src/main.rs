mod dot;
mod lwexplore;
mod solfile;

use anyhow::Context;
use clap::{Parser, Subcommand};
use gamebooksolver_base::lonewolf::chapter::*;
use gamebooksolver_base::lonewolf::data::{Multistat, get_boundary};
use gamebooksolver_base::lonewolf::mini::CharacterVariableG;
use gamebooksolver_base::lonewolf::mini::CompactSolutionG;
use gamebooksolver_base::lonewolf::mini::GSolDump;
use gamebooksolver_base::lonewolf::mini::NoPrevEq;
use gamebooksolver_base::lonewolf::mini::SoldumpContent;
use gamebooksolver_base::lonewolf::mini::StoredEquipment;
use gamebooksolver_base::lonewolf::evasion::expand_evasions;
use gamebooksolver_base::lonewolf::solve::{
    BestPathState, PathStates, solve_lws, solve_lws_best_path, solve_lws_best_path_and_visit, solve_lws_best_path_starts,
    visit_best_path,
};
use gamebooksolver_base::solver::rational::MF64;
use std::io::Write;
use gamebooksolver_base::solver::base::Proba;
use gamebooksolver_base::solver::base::optimize_outcome;
use gamebooksolver_base::solver::base::{ChoppedSolution, SolNode};
use gamebooksolver_base::solver::rational::Rational as _;
use gamebooksolver_base::solver::rational::r::MRational;
use lwexplore::explore_solution;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;

use gamebooksolver_base::lonewolf::mini::{
    Book, CVarState, CharacterConstant, CompactSolution, CompactState, Discipline, Equipment, Flag, Flags, Item,
    NextStep, SolDesc, SolutionDump, mkchar,
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
    /// Keep all the states that can be reached, not only those on the best path. This is required for exploring what
    /// happens after other choices, but uses a lot more memory.
    #[arg(long)]
    pub allstates: bool,
}

#[derive(Debug, Subcommand)]
enum PSub {
    LoadChapter,
    DumpStates {
        cid: Vec<u16>,
    },
    CompareStates {
        otherpath: String,
        cid: u16,
    },
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
    /// solve a character for one starting gold, and write the solution to --solpath and the reports
    Solve(OSolDesc),
    /// write the reports of some of the starting gold of a solution file, in one go. In --jot, --desc and --dot, {gold} stands
    /// for the gold. Only the reports with a path are written.
    Extract {
        /// A list such as 10,20,50, or all
        #[arg(long)]
        golds: String,
        /// Path of the .dot files, which also needs --bookpath and --display
        #[arg(long)]
        dot: Option<String>,
        #[arg(long)]
        bookpath: Option<String>,
        #[arg(long)]
        display: Option<String>,
        /// Show the round by round chapters of the fights, instead of a node for each fight
        #[arg(long)]
        raw_rounds: bool,
    },
    /// show what a solution file holds
    SolutionInfo,
    /// solve once for a range of starting gold, from --gold to --gold-to: writes the solution to --solpath, the score of
    /// each to --json, where {gold} stands for the gold, and the ranges of consecutive amounts with the same score to --ranges
    GoldScores {
        #[command(flatten)]
        desc: OSolDesc,
        #[arg(long)]
        gold_to: u8,
        #[arg(long)]
        ranges: String,
    },
    /// write what can happen on the best path, with the value of all the choices, one state per line
    Decisions(OSolDesc),
    /// write a dot graph of a .jot file (the solution path), over the chapters of the book
    Dot {
        #[arg(long)]
        bookpath: String,
        /// Path to the description of what is displayed in the nodes
        #[arg(long)]
        display: String,
        /// Show the round by round chapters of the fights, instead of a node for each fight
        #[arg(long)]
        raw_rounds: bool,
    },
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
    /// The starting gold to read from a solution file that has several
    #[structopt(long)]
    start_gold: Option<u8>,
    /// Optional path where soldump writes the value of all the choices on the best path. It is compressed if it ends in .zst
    #[structopt(long)]
    decisions: Option<String>,
    /// Alternate commands
    #[structopt(subcommand)]
    cmd: Option<PSub>,
    #[structopt(long)]
    verbose: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
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

/// What happens in each chapter, for a character at the start. Goes through the states once, from the start, with the
/// probability of reaching each of them, so that nothing has to be kept for each state.
fn mksol<PREV: StoredEquipment>(
    ini: NextStep<PREV>,
    content: &SoldumpContent<Rational, PREV>,
) -> Flows {
    fn nexts<PREV: StoredEquipment>(
        cs: &ChoppedSolution<Rational, NextStep<PREV>>,
    ) -> Vec<(&NextStep<PREV>, Option<&Rational>)> {
        match cs {
            ChoppedSolution::CLeafLost | ChoppedSolution::CLeaf(_) => Vec::new(),
            ChoppedSolution::CJump(_, stt) => vec![(stt, None)],
            ChoppedSolution::CNode(_, pms) => pms.iter().filter_map(|(s, p)| s.as_ref().map(|s| (s, Some(p)))).collect(),
        }
    }
    let solution = |ns: &NextStep<PREV>| match content.get(ns) {
        None => panic!("Missing state!?! {:?}", ns),
        Some(x) => x,
    };
    // each state comes after all those that lead to it
    let mut order: Vec<&NextStep<PREV>> = Vec::new();
    let mut seen: HashSet<&NextStep<PREV>> = HashSet::new();
    let mut todo: Vec<(&NextStep<PREV>, bool)> = vec![(&ini, false)];
    while let Some((ns, children_done)) = todo.pop() {
        if children_done {
            order.push(ns);
            continue;
        }
        if !seen.insert(ns) {
            continue;
        }
        todo.push((ns, true));
        if ns.cvar().is_some() && ns.chapter().is_some() {
            todo.extend(nexts(solution(ns)).into_iter().map(|(n, _)| (n, false)));
        }
    }

    let mut out: BTreeMap<u16, ChapterStats> = BTreeMap::new();
    let mut deaths: BTreeMap<u16, Rational> = BTreeMap::new();
    let mut values: BTreeMap<u16, Rational> = BTreeMap::new();
    let mut reached: HashMap<&NextStep<PREV>, Rational> = HashMap::from([(&ini, Rational::from(1))]);
    for ns in order.into_iter().rev() {
        let weight = reached.remove(ns).unwrap_or_default();
        let Some(src) = ns.chapter() else { continue };
        let e = out.entry(src).or_default();
        let Some(cvar) = ns.cvar() else {
            *deaths.entry(src).or_default() += weight;
            continue;
        };
        *e.items.entry(cvar.cequipment).or_default() += weight.clone();
        *e.flags.entry(cvar.flags.0).or_default() += weight.clone();
        *e.endurance.entry(cvar.curendurance).or_default() += weight.clone();
        let cs = solution(ns);
        *values.entry(src).or_default() += weight.clone() * &cs.score();
        match cs {
            ChoppedSolution::CLeafLost => (),
            ChoppedSolution::CLeaf(sc) => e.score += weight.clone() * sc,
            ChoppedSolution::CJump(_, _) | ChoppedSolution::CNode(_, _) => e.score += weight.clone(),
        }
        for (next, p) in nexts(cs) {
            let flow = match p {
                Some(p) => weight.clone() * p,
                None => weight.clone(),
            };
            if let Some(dst) = next.chapter() {
                *e.transitions.entry(dst).or_default() += flow.clone();
            }
            *reached.entry(next).or_default() += flow;
        }
    }
    Flows { res: out, deaths, values }
}

fn count_states<A, PREV: StoredEquipment>(cnt: &BTreeMap<NextStep<PREV>, A>) -> BTreeMap<u16, u64> {
    let mut o = BTreeMap::new();
    for (ns, _) in cnt.iter() {
        if let Some(cid) = ns.chapter() {
            let e = o.entry(cid).or_insert(0);
            *e += 1;
        }
    }
    o
}

#[derive(serde::Serialize, serde::Deserialize)]
struct Output {
    bookid: Book,
    res: BTreeMap<u16, ChapterStats>,
    sttmap: BTreeMap<u16, u64>,
    /// How many characters die in each chapter
    #[serde(default)]
    deaths: BTreeMap<u16, Rational>,
    /// The score of the characters that reach each chapter, multiplied by how many they are
    #[serde(default)]
    values: BTreeMap<u16, Rational>,
}

/// What `mksol` finds
struct Flows {
    res: BTreeMap<u16, ChapterStats>,
    deaths: BTreeMap<u16, Rational>,
    values: BTreeMap<u16, Rational>,
}

fn chop_solution<PREV: StoredEquipment>(
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

fn load_results(
    disciplines: HashSet<Discipline>,
    iitems: &HashSet<Item>,
    iflags: &HashSet<Flag>,
    resdir: &str,
    verbose: bool,
) -> anyhow::Result<HashMap<(Equipment, Flags), Rational>> {
    if verbose {
        eprintln!("looking of results compatible with {disciplines:?}")
    }
    let mut o = HashMap::new();
    for fpath in std::fs::read_dir(resdir)? {
        let rpath = fpath?.path();
        let pth = rpath.to_str().ok_or_else(|| anyhow::anyhow!(":("))?;
        if !pth.ends_with(".json") {
            continue;
        }
        let f = File::open(pth)?;
        let ms: Multistat<Rational> =
            serde_json::de::from_reader(f).with_context(|| anyhow::anyhow!("loading {pth}"))?;
        if ms.msentries.len() != 1 {
            eprintln!("invalid amount of entries in {}", pth);
            continue;
        }
        let curdiscs: HashSet<Discipline> = ms.msdisciplines.into_iter().collect();
        if curdiscs.difference(&disciplines).count() > 1 {
            if verbose {
                eprintln!(" * ignoring  {}, invalid disciplines {curdiscs:?}", rpath.display())
            }
            continue;
        }
        let mut flags = Flags(0);
        for f in ms.variable.flags.into_iter().filter(|f| iflags.contains(f)) {
            flags.set(f);
        }
        let mut items = Equipment::default();
        for (i, q) in ms.variable.items.iter().flatten().filter(|(i, _)| iitems.contains(i)) {
            items.add_item(i, *q);
        }
        if iitems.contains(&Item::Gold) {
            items.add_item(&Item::Gold, ms.variable.gold as i64);
        }
        let mentry: &Rational = &ms.msentries[0].mratio;
        let oentry: &mut Rational = o.entry((items, flags)).or_default();
        if mentry.cmp(oentry) == std::cmp::Ordering::Greater {
            if verbose {
                eprintln!(" * accepting {}, invalid disciplines {curdiscs:?}", rpath.display())
            }
            *oentry = mentry.clone();
        }
    }
    Ok(o)
}

fn filter_final_state(
    bookid: Book,
    iitems: &HashSet<Item>,
    iflags: &HashSet<Flag>,
    e: Equipment,
    f: Flags,
) -> (Equipment, Flags) {
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
    (re, rf)
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
        Some(scoremap) if bookid == Book::Book01 => {
            let (re, rf) = filter_final_state(bookid, iitems, iflags, e, f);
            let mut score = MRational::from_i64(0, 1);
            for gainedgold in 10..=19 {
                let mut nre = re;
                nre.add_item(&Item::Gold, gainedgold);
                match scoremap.get(&(nre, rf)) {
                    None => {
                        eprintln!(
                            "Could not find matching combination for {:?} {:?}",
                            nre.items(),
                            rf.all()
                        );
                        eprintln!("know combinations are:");
                        for (ke, kf) in scoremap.keys() {
                            eprintln!(" * {:?} {:?}", ke.items(), kf.all());
                        }
                        panic!("failed :(");
                    }
                    Some(s) => {
                        score += s.clone();
                    }
                }
            }
            score = score.div(&MRational::from_i64(10, 1));
            score
        }
        Some(scoremap) => {
            let (re, rf) = filter_final_state(bookid, iitems, iflags, e, f);
            match scoremap.get(&(re, rf)) {
                None => {
                    eprintln!(
                        "Could not find matching combination for {:?} {:?}",
                        re.items(),
                        rf.all()
                    );
                    eprintln!("know combinations are:");
                    for (ke, kf) in scoremap.keys() {
                        eprintln!(" * {:?} {:?}", ke.items(), kf.all());
                    }
                    panic!("failed :(");
                }
                Some(x) => x.clone(),
            }
        }
    }
}

fn compare_sols<PREV: StoredEquipment>(
    m1: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>,
    m2: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>,
) {
    type Ocs<PREV> = Vec<(Rational, NextStep<PREV>)>;
    fn deloss<PREV: StoredEquipment>(chapter: Option<u16>, ns: NextStep<PREV>) -> NextStep<PREV> {
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
    fn descored<PREV: StoredEquipment>(
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
                let mut o = optimize_outcome(po).into_iter().map(|p| (p.p, p.v)).collect::<Vec<_>>();
                o.sort();
                o
            }
            _ => Vec::new(),
        }
    }

    fn pretty_outcomes<PREV: StoredEquipment>(outcomes: &[(Rational, NextStep<PREV>)]) {
        for (s, p) in outcomes {
            println!(" - {} {}", p, s);
        }
    }

    fn pretty_step<PREV: StoredEquipment>(ns: &NextStep<PREV>, outcomes: &[(Rational, NextStep<PREV>)]) {
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

fn jot_output<PREV: StoredEquipment>(soldump: &SolutionDump<Rational, PREV>) -> Output {
    let ini = NextStep::NewChapter(1, mkchar(&soldump.soldesc.ccst, &soldump.soldesc.cvar));
    let flows = mksol(ini, &soldump.content);
    Output {
        bookid: soldump.soldesc.ccst.bookid,
        res: flows.res,
        sttmap: count_states(&soldump.content),
        deaths: flows.deaths,
        values: flows.values,
    }
}

fn mkjot<PREV: StoredEquipment>(soldump: &SolutionDump<Rational, PREV>, out: Option<&str>) -> anyhow::Result<()> {
    let output = jot_output(soldump);
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

struct DotReport<'t> {
    path: &'t str,
    chapters: Vec<(ChapterId, Chapter<Rational>)>,
    display: Vec<dot::DotInfo>,
    raw_rounds: bool,
}

struct Reports<'t> {
    jot: Option<&'t str>,
    desc: Option<&'t str>,
    dot: Option<DotReport<'t>>,
}

/// The reports of some of the starting gold of a solution file.
fn extract<PREV: StoredEquipment>(f: &solfile::SolFile<PREV>, golds: &str, reports: &Reports) -> anyhow::Result<()> {
    let golds: Vec<u8> = if golds == "all" {
        f.golds.clone()
    } else {
        golds.split(',').map(str::parse).collect::<Result<_, _>>().context("--golds is a list of amounts, or all")?
    };
    let dot_path = reports.dot.as_ref().map(|d| d.path);
    let paths = [reports.jot, reports.desc, dot_path];
    anyhow::ensure!(paths.iter().any(|p| p.is_some()), "nothing to write: use --jot, --desc or --dot");
    anyhow::ensure!(
        golds.len() == 1 || paths.iter().flatten().all(|p| p.contains("{gold}")),
        "with several amounts of gold, the paths must contain {{gold}}"
    );
    for gold in golds {
        let at = |pattern: &str| pattern.replace("{gold}", &gold.to_string());
        let d = f.dump(Some(gold))?;
        if reports.jot.is_some() || reports.dot.is_some() {
            let output = jot_output(&d);
            if let Some(path) = reports.jot {
                serde_json::to_writer_pretty(File::create(at(path))?, &output)?;
            }
            if let Some(r) = &reports.dot {
                let mut text = dot::mkdot(&r.chapters, &r.display, &output, r.raw_rounds);
                text.push('\n');
                std::fs::write(at(r.path), text)?;
            }
        }
        if let Some(path) = reports.desc {
            write_desc(&d.soldesc, &at(path))?;
        }
    }
    Ok(())
}

fn optimize<PREV: StoredEquipment>(
    soldump: &SolutionDump<Rational, PREV>,
    jname: &str,
    dummy: bool,
    bookpath: Option<&str>,
) -> anyhow::Result<CompactSolution<PREV>> {
    let useless_chapters = bookpath
        .map(|bookpath| {
            let fl = File::open(bookpath).unwrap();
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl).unwrap();
            book.into_iter()
                .filter_map(|(a, b)| if has_no_choice(&b.pchoice) { None } else { Some(a.0) })
                .collect::<HashSet<_>>()
        })
        .unwrap_or_default();
    let mut content = if dummy {
        Vec::new()
    } else {
        soldump
            .content
            .iter()
            .filter_map(|(k, v)| CompactState::from_choppedsolution(k, v, &useless_chapters))
            .collect::<Vec<_>>()
    };
    let soldesc = soldump.soldesc.clone();
    content.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    write_desc(&soldesc, jname)?;
    let mut soldesc = soldesc;
    sort_soldesc(&mut soldesc);
    Ok(CompactSolution { soldesc, content })
}

fn sort_soldesc(soldesc: &mut SolDesc) {
    soldesc.ccst.discipline.sort();
    if let Some(l) = soldesc.mscoremap.as_mut() {
        l.sort()
    }
}

/// The json recap of a solution
fn write_desc(soldesc: &SolDesc, path: &str) -> anyhow::Result<()> {
    let mut soldesc = soldesc.clone();
    sort_soldesc(&mut soldesc);
    serde_json::to_writer(File::create(path)?, &soldesc)?;
    Ok(())
}

fn dump_states<PREV: StoredEquipment>(soldump: SolutionDump<Rational, PREV>, cids: &[u16]) -> anyhow::Result<()> {
    for &cid in cids {
        println!("CID{cid}");
        for (ns, sol) in &soldump.content {
            if Some(cid) == ns.chapter() {
                match (ns, sol) {
                    (NextStep::NewChapter(_, pcvar), ChoppedSolution::CJump(sc, NextStep::NewChapter(nid, ncvar))) => {
                        println!(
                            "{} -> JUMP {:.2}% ch:{} {}",
                            ns,
                            sc.to_f64() * 100.0,
                            nid,
                            &diff_cvar(pcvar, ncvar)
                        )
                    }
                    (NextStep::NewChapter(_, pcvar), ChoppedSolution::CNode(sc, outcomes)) => {
                        println!("{} - {:.2}% ", ns, sc.to_f64() * 100.0);
                        for (mns, pb) in outcomes {
                            match mns {
                                Some(NextStep::NewChapter(nid, ncvar)) => {
                                    println!(" [{:.2}%] ch:{} {}", pb.to_f64() * 100.0, nid, &diff_cvar(pcvar, ncvar))
                                }
                                _ => println!(" [{:.2}%] {:?}", pb.to_f64() * 100.0, mns),
                            }
                        }
                    }
                    _ => println!("{} -> {:?}", ns, sol),
                }
            }
        }
    }
    Ok(())
}

fn compare_states<PREV: StoredEquipment>(
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

type ScoreMap = HashMap<(Equipment, Flags), Rational>;

/// What is needed to solve a book for this character, and what the next book says about how it ends.
fn prepare_soldesc(
    cnt: &OSolDesc,
    verbose: bool,
) -> anyhow::Result<(SolDesc, Vec<ChapterId>, Option<ScoreMap>, HashSet<Item>, HashSet<Flag>)> {
    let (iitems, iflags) = get_boundary(cnt.bookid);
    let mscoremap = cnt
        .resultspath
        .as_ref()
        .map(|pth| load_results(cnt.discipline.iter().copied().collect(), &iitems, &iflags, pth, verbose))
        .transpose()?;
    if verbose && let Some(ms) = &mscoremap {
        eprintln!("mscoremap");
        for ((e, f), s) in ms {
            eprintln!("* {:?} {:?} {:.3}", e.items(), f.all(), 100.0 * s.to_f64());
        }
    }

    let ccst = CharacterConstant {
        bookid: cnt.bookid,
        combat_skill: cnt.combat_skill,
        discipline: cnt.discipline.clone(),
        maxendurance: cnt.maxendurance,
    };
    let cvar = CVarState {
        flags: cnt.flags.iter().cloned().collect(),
        gold: cnt.gold,
        items: if cnt.items.is_empty() {
            None
        } else {
            let mut mp: BTreeMap<Item, i64> = BTreeMap::new();
            for i in cnt.items.iter() {
                let e = mp.entry(*i).or_insert(0);
                *e += 1;
            }
            Some(mp)
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
        mscoremap: mscoremap
            .as_ref()
            .map(|mp| mp.iter().map(|((k1, k2), v)| (*k1, *k2, v.clone())).collect()),
    };
    Ok((soldesc, fchapters, mscoremap, iitems, iflags))
}

#[derive(serde::Serialize)]
struct GoldRange {
    from: u8,
    to: u8,
    score: f64,
    ratio: Rational,
}

/// Solves once for all the starting gold from `--gold` to `gold_to`. Writes the solution to `solpath`, the score of each to
/// `json_pattern`, where `{gold}` stands for the gold, and to `ranges_path` the ranges of consecutive amounts that have the
/// same score.
fn gold_scores<PREV: StoredEquipment + solfile::Kind + bincode::Encode>(
    cnt: &OSolDesc,
    gold_to: u8,
    book: &[(ChapterId, Chapter<Rational>)],
    solpath: &str,
    json_pattern: &str,
    ranges_path: &str,
    verbose: bool,
) -> anyhow::Result<()> {
    anyhow::ensure!(cnt.gold <= gold_to, "--gold must not be more than --gold-to");
    anyhow::ensure!(json_pattern.contains("{gold}"), "--json must contain {{gold}}");
    let (soldesc, fchapters, mscoremap, iitems, iflags) = prepare_soldesc(cnt, verbose)?;
    let golds: Vec<u8> = (cnt.gold..=gold_to).collect();
    let descs: Vec<SolDesc> = golds
        .iter()
        .map(|gold| {
            let mut desc = soldesc.clone();
            desc.cvar.gold = *gold;
            desc
        })
        .collect();
    let cvars: Vec<CharacterVariableG<PREV>> = descs.iter().map(|d| d.cvariable()).collect();
    let scorer = |e, f| score_with(soldesc.ccst.bookid, &mscoremap, &iitems, &iflags, e, f);
    let sol = solve_lws_best_path_starts(scorer, &fchapters, book, &soldesc.ccst, &cvars, cnt.startat, verbose);
    let content: SoldumpContent<Rational, PREV> = sol.into_iter().map(|(ns, x)| (ns, chop_solution(x))).collect();
    let hash = solfile::book_hash(&std::fs::read(&cnt.bookpath)?);
    solfile::save(solpath, &soldesc, &content, &golds, cnt.startat, Some(hash), solved_origin())?;
    let mut ranges: Vec<GoldRange> = Vec::new();
    for (desc, cvar) in descs.iter().zip(cvars) {
        let start = NextStep::NewChapter(cnt.startat, cvar);
        let score = content.get(&start).context("a start was not solved")?.score();
        let path = json_pattern.replace("{gold}", &desc.cvar.gold.to_string());
        let file = File::create(&path).with_context(|| anyhow::anyhow!("creating {path}"))?;
        serde_json::to_writer_pretty(file, &Multistat::new(desc, score.clone(), content.len()))?;
        match ranges.last_mut() {
            Some(last) if last.ratio == score => last.to = desc.cvar.gold,
            _ => ranges.push(GoldRange {
                from: desc.cvar.gold,
                to: desc.cvar.gold,
                score: score.to_f64(),
                ratio: score,
            }),
        }
    }
    for r in &ranges {
        println!("{:>2} to {:>2}: {:.4}%", r.from, r.to, r.score * 100.0);
    }
    // written last, as it says that everything is there
    serde_json::to_writer_pretty(File::create(ranges_path)?, &ranges)?;
    Ok(())
}

fn solved_origin() -> solfile::Origin {
    solfile::Origin::Solved {
        commit: std::env::var("GBS_COMMIT").ok(),
    }
}

fn soldump<PREV: StoredEquipment>(
    cnt: &OSolDesc,
    book: Vec<(ChapterId, Chapter<Rational>)>,
    json_path: &str,
    decisions_path: Option<&str>,
    verbose: bool,
) -> anyhow::Result<SolutionDump<Rational, PREV>> {
    anyhow::ensure!(
        !(cnt.allstates && decisions_path.is_some()),
        "--decisions only works with the best path, and not with --allstates"
    );
    let (soldesc, fchapters, mscoremap, iitems, iflags) = prepare_soldesc(cnt, verbose)?;
    let cvar = soldesc.cvariable::<PREV>();
    let scorer = |e, f| score_with(soldesc.ccst.bookid, &mscoremap, &iitems, &iflags, e, f);
    let sol = if cnt.allstates {
        solve_lws(scorer, &fchapters, &book, &soldesc.ccst, &cvar, cnt.startat, verbose)
    } else if let Some(path) = decisions_path {
        let mut writer = DecisionsWriter::new(path, soldesc.ccst.bookid, &book)?;
        let sol = solve_lws_best_path_and_visit(
            scorer,
            &fchapters,
            &book,
            &soldesc.ccst,
            &cvar,
            cnt.startat,
            verbose,
            &mut |st, leaf| writer.write(st, leaf),
        );
        writer.finish()?;
        sol
    } else {
        solve_lws_best_path(scorer, &fchapters, &book, &soldesc.ccst, &cvar, cnt.startat, verbose)
    };
    let soldump = SolutionDump {
        soldesc,
        content: sol.into_iter().map(|(ns, x)| (ns, chop_solution(x))).collect(),
    };

    // save json summary
    let json_file = File::create(json_path)?;
    serde_json::to_writer_pretty(json_file, &Multistat::from_soldump(&soldump))?;

    Ok(soldump)
}

fn cmd_soldump(
    cnt: &OSolDesc,
    jsonpath: &str,
    decisions: Option<&str>,
    verbose: bool,
) -> anyhow::Result<GSolDump<Rational>> {
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
        soldump::<Equipment>(cnt, book, jsonpath, decisions, verbose).map(GSolDump::Prev)?
    } else {
        soldump::<NoPrevEq>(cnt, book, jsonpath, decisions, verbose).map(GSolDump::Noprev)?
    };
    Ok(sd)
}

/// Writes the states of the best path with the value of all the choices, one per line. A file ending in .zst is compressed.
struct DecisionsWriter {
    out: Box<dyn Write>,
    opponents: HashMap<u16, (String, i8)>,
    failure: Option<std::io::Error>,
}

impl DecisionsWriter {
    fn new<P: gamebooksolver_base::solver::rational::Rational>(
        path: &str,
        bookid: Book,
        book: &[(ChapterId, Chapter<P>)],
    ) -> anyhow::Result<Self> {
        let mut expanded: HashMap<ChapterId, Chapter<P>> = book.iter().cloned().collect();
        expand_evasions(bookid, &mut expanded);
        let opponents = expanded
            .iter()
            .filter_map(|(cid, c)| round_details(&c.pchoice).map(|fd| (cid.0, (fd.opponent.clone(), fd.endurance.0))))
            .collect();
        let file = File::create(path)?;
        let out: Box<dyn Write> = if path.ends_with(".zst") {
            Box::new(zstd::Encoder::new(file, 9)?.auto_finish())
        } else {
            Box::new(std::io::BufWriter::new(file))
        };
        Ok(DecisionsWriter {
            out,
            opponents,
            failure: None,
        })
    }

    fn write<P: gamebooksolver_base::solver::rational::Rational, PREV: StoredEquipment>(
        &mut self,
        st: BestPathState<P, PREV>,
        leaf: Option<&P>,
    ) {
        let round = st.state.chapter().and_then(|c| self.opponents.get(&c));
        let line = serde_json::json!({
            "state": st.state.to_string(),
            "chapter": st.state.chapter(),
            "cvar": st.state.cvar().map(|c| serde_json::json!({
                "endurance": c.curendurance,
                "flags": c.flags.all().iter().map(|f| format!("{f:?}")).collect::<Vec<_>>(),
                "items": c.cequipment.to_string(),
                "stored": c.cprevequipment.into().to_string(),
            })),
            "opponent": round.map(|r| &r.0),
            "opponent_endurance": round.map(|r| r.1),
            "score": leaf.map(|s| s.t_f64()),
            "best": st.best,
            "choices": st.choices.iter().map(|(desc, score, res)| serde_json::json!({
                "desc": desc,
                "score": score.t_f64(),
                "next": res.iter().map(|o| (o.p.t_f64(), o.v.to_string())).collect::<Vec<_>>(),
            })).collect::<Vec<_>>(),
        });
        if self.failure.is_none()
            && let Err(e) = writeln!(self.out, "{line}")
        {
            self.failure = Some(e);
        }
    }

    fn finish(mut self) -> anyhow::Result<()> {
        match self.failure {
            Some(e) => Err(e.into()),
            None => Ok(self.out.flush()?),
        }
    }
}

fn write_decisions<PREV: StoredEquipment>(
    cnt: &OSolDesc,
    book: Vec<(ChapterId, Chapter<Rational>)>,
    path: &str,
    verbose: bool,
) -> anyhow::Result<()> {
    let (iitems, iflags) = get_boundary(cnt.bookid);
    let mscoremap = cnt
        .resultspath
        .as_ref()
        .map(|pth| load_results(cnt.discipline.iter().copied().collect(), &iitems, &iflags, pth, verbose))
        .transpose()?;
    let ccst = CharacterConstant {
        bookid: cnt.bookid,
        combat_skill: cnt.combat_skill,
        discipline: cnt.discipline.clone(),
        maxendurance: cnt.maxendurance,
    };
    let mut items: BTreeMap<Item, i64> = BTreeMap::new();
    for i in &cnt.items {
        *items.entry(*i).or_insert(0) += 1;
    }
    let cvar = CVarState {
        flags: cnt.flags.iter().cloned().collect(),
        gold: cnt.gold,
        items: Some(items),
    };
    let cvar = mkchar::<PREV>(&ccst, &cvar);
    let fchapters: Vec<ChapterId> = if !cnt.finalchapters.is_empty() {
        cnt.finalchapters.iter().copied().map(ChapterId).collect()
    } else if ccst.bookid == Book::Book05 {
        vec![ChapterId(400)]
    } else {
        vec![ChapterId(350)]
    };
    // an exact score isn't needed for this, and takes a lot more memory
    let book: Vec<(ChapterId, Chapter<MF64>)> = book
        .into_iter()
        .map(|(cid, c)| (cid, c.map_proba(&|p: &Rational| MF64::f_f64(p.to_f64()))))
        .collect();
    let mut writer = DecisionsWriter::new(path, ccst.bookid, &book)?;
    visit_best_path(
        |e, f| MF64::f_f64(score_with(ccst.bookid, &mscoremap, &iitems, &iflags, e, f).to_f64()),
        &fchapters,
        &book,
        &ccst,
        &cvar,
        cnt.startat,
        verbose,
        &mut [(PathStates::Actual, &mut |st, leaf| writer.write(st, leaf))],
    );
    writer.finish()
}

fn round_details<P>(d: &Decision<P>) -> Option<&FightDetails<P>> {
    match d {
        Decision::None(ChapterOutcome::Round(fd, _, _)) => Some(fd),
        Decision::Decisions(lst) => lst.iter().find_map(|(_, d)| round_details(d)),
        _ => None,
    }
}

fn cmd_optimize(
    soldump: &GSolDump<Rational>,
    target: &str,
    jname: &str,
    dummy: bool,
    bookpath: Option<&str>,
) -> anyhow::Result<()> {
    let compact = match soldump {
        GSolDump::Prev(sd) => optimize(sd, jname, dummy, bookpath).map(CompactSolutionG::WithStorage)?,
        GSolDump::Noprev(sd) => optimize(sd, jname, dummy, bookpath).map(CompactSolutionG::NoStorage)?,
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
            let soldump = solfile::load_dump(&opt.solpath, opt.start_gold)?;
            match soldump {
                GSolDump::Prev(sd) => mkjot(&sd, opt.jot.as_deref())?,
                GSolDump::Noprev(sd) => mkjot(&sd, opt.jot.as_deref())?,
            }
        }
        Some(PSub::Optimize {
            target,
            dummy,
            bookpath,
        }) => {
            let soldump = solfile::load_dump(&opt.solpath, opt.start_gold)?;
            let jname = opt.desc.unwrap_or_else(|| target.to_owned() + ".desc");
            cmd_optimize(&soldump, target, &jname, *dummy, bookpath.as_deref())?;
        }
        Some(PSub::Explore { bookpath }) => {
            let fl = File::open(bookpath)?;
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl)?;
            let soldump = solfile::load_dump(&opt.solpath, opt.start_gold)?;
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
            let soldump: CompactSolutionG = bincode::decode_from_std_read(&mut dec, bincode::config::standard())?;
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
            let soldump = solfile::load_dump(&opt.solpath, opt.start_gold)?;
            match soldump {
                GSolDump::Prev(sd) => dump_states(sd, cid)?,
                GSolDump::Noprev(sd) => dump_states(sd, cid)?,
            }
        }
        Some(PSub::CompareStates { cid, otherpath }) => {
            let dump1 = solfile::load_dump(&opt.solpath, opt.start_gold)?;
            let dump2 = solfile::load_dump(otherpath, opt.start_gold)?;
            match (dump1, dump2) {
                (GSolDump::Prev(dump1), GSolDump::Prev(dump2)) => compare_states(dump1, dump2, *cid)?,
                (GSolDump::Noprev(dump1), GSolDump::Noprev(dump2)) => compare_states(dump1, dump2, *cid)?,
                _ => panic!("can't handle mixed solution dumps"),
            }
        }
        Some(PSub::Decisions(cnt)) => {
            let fl = File::open(&cnt.bookpath)?;
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(fl)?;
            let has_prev_eq = book
                .iter()
                .flat_map(|x| x.1.pchoice.iter_outcomes())
                .collect::<Vec<_>>()
                .iter()
                .flat_map(|co| co.simple_outcomes())
                .any(|so| matches!(so, SimpleOutcome::StoreEquipment));
            if has_prev_eq {
                write_decisions::<Equipment>(cnt, book, &opt.solpath, opt.verbose)?;
            } else {
                write_decisions::<NoPrevEq>(cnt, book, &opt.solpath, opt.verbose)?;
            }
        }
        Some(PSub::Dot { bookpath, display, raw_rounds }) => {
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(BufReader::new(File::open(bookpath)?))?;
            let display: Vec<dot::DotInfo> = serde_json::from_reader(BufReader::new(File::open(display)?))?;
            let stats: Output = serde_json::from_reader(BufReader::new(File::open(&opt.solpath)?))
                .with_context(|| anyhow::anyhow!("loading {}", opt.solpath))?;
            println!("{}", dot::mkdot(&book, &display, &stats, *raw_rounds));
        }
        Some(PSub::GoldScores { desc, gold_to, ranges }) => {
            let json = opt.json.context("--json is required")?;
            let book: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(File::open(&desc.bookpath)?)?;
            let cos = book.iter().flat_map(|x| x.1.pchoice.iter_outcomes()).collect::<Vec<_>>();
            let has_prev_eq = cos
                .iter()
                .flat_map(|co| co.simple_outcomes())
                .any(|so| matches!(so, SimpleOutcome::StoreEquipment));
            if has_prev_eq {
                gold_scores::<Equipment>(desc, *gold_to, &book, &opt.solpath, &json, ranges, opt.verbose)?;
            } else {
                gold_scores::<NoPrevEq>(desc, *gold_to, &book, &opt.solpath, &json, ranges, opt.verbose)?;
            }
        }
        Some(PSub::Solve(cnt)) => {
            let cur = std::time::Instant::now();
            let json_path = opt.json.unwrap_or_else(|| opt.solpath.to_owned() + ".json");
            let jname = opt.desc.unwrap_or_else(|| opt.solpath.to_owned() + ".desc");
            let sd = cmd_soldump(cnt, &json_path, opt.decisions.as_deref(), opt.verbose)?;
            let soldump_duration = cur.elapsed();
            let hash = Some(solfile::book_hash(&std::fs::read(&cnt.bookpath)?));
            let cur = std::time::Instant::now();
            let soldesc = match &sd {
                GSolDump::Prev(d) => {
                    let golds = [d.soldesc.cvar.gold];
                    solfile::save(&opt.solpath, &d.soldesc, &d.content, &golds, cnt.startat, hash, solved_origin())?;
                    &d.soldesc
                }
                GSolDump::Noprev(d) => {
                    let golds = [d.soldesc.cvar.gold];
                    solfile::save(&opt.solpath, &d.soldesc, &d.content, &golds, cnt.startat, hash, solved_origin())?;
                    &d.soldesc
                }
            };
            let save_duration = cur.elapsed();
            write_desc(soldesc, &jname)?;
            let cur = std::time::Instant::now();
            let (nstates, winrate) = match &sd {
                GSolDump::Prev(d) => {
                    mkjot(d, opt.jot.as_deref())?;
                    start_stats(&d.content)
                }
                GSolDump::Noprev(d) => {
                    mkjot(d, opt.jot.as_deref())?;
                    start_stats(&d.content)
                }
            };
            println!(
                "states:{nstates} win:{:.2}% sd[{soldump_duration:?}] save[{save_duration:?}] jot[{:?}]",
                winrate * 100.0,
                cur.elapsed()
            )
        }
        Some(PSub::Extract { golds, dot, bookpath, display, raw_rounds }) => {
            let dot = match (dot, bookpath, display) {
                (None, _, _) => None,
                (Some(path), Some(book), Some(display)) => {
                    let chapters: Vec<(ChapterId, Chapter<Rational>)> = serde_json::from_reader(BufReader::new(File::open(book)?))?;
                    let display: Vec<dot::DotInfo> = serde_json::from_reader(BufReader::new(File::open(display)?))?;
                    Some(DotReport { path, chapters, display, raw_rounds: *raw_rounds })
                }
                _ => anyhow::bail!("--dot needs --bookpath and --display"),
            };
            let reports = Reports { jot: opt.jot.as_deref(), desc: opt.desc.as_deref(), dot };
            match solfile::load(&opt.solpath)? {
                solfile::Loaded::Prev(f) => extract(&f, golds, &reports)?,
                solfile::Loaded::Noprev(f) => extract(&f, golds, &reports)?,
            }
        }
        Some(PSub::SolutionInfo) => match solfile::load(&opt.solpath)? {
            solfile::Loaded::Prev(f) => print_solution_info(&f),
            solfile::Loaded::Noprev(f) => print_solution_info(&f),
        },
    }
    Ok(())
}

fn print_solution_info<PREV: StoredEquipment>(f: &solfile::SolFile<PREV>) {
    println!("book: {:?}, starting gold: {:?}", f.desc.ccst.bookid, f.golds);
    println!("states: {}, origin: {:?}, book hash: {:?}", f.content.len(), f.origin, f.book_hash);
}

fn start_stats<PREV: StoredEquipment>(content: &SoldumpContent<Rational, PREV>) -> (usize, f64) {
    let win = content
        .iter()
        .find(|(ns, _)| ns.chapter() == Some(1))
        .map(|(_, cs)| cs.score().to_f64())
        .unwrap_or(-1.0);
    (content.len(), win)
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
        Decision::AfterCombat(d) | Decision::Conditional(_, d) => has_no_choice(d),
        Decision::None(_) => true,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // computes the same thing from the end, and needs a table for each state
    fn mksol_backward<PREV: StoredEquipment>(
        ini: NextStep<PREV>,
        sttmap: HashMap<NextStep<PREV>, &ChoppedSolution<Rational, NextStep<PREV>>>,
    ) -> BTreeMap<u16, ChapterStats> {
        type Imap<PREV> = HashMap<NextStep<PREV>, BTreeMap<u16, ChapterStats>>;
        let mut memo: Imap<PREV> = HashMap::new();

        fn go<PREV: StoredEquipment>(
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

            let mut update_choice = |p: Rational, cstt: &NextStep<PREV>, memo: &mut BTreeMap<u16, ChapterStats>| {
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

    #[test]
    fn stats_from_the_start_are_the_same_as_from_the_end() {
        type State = NextStep<NoPrevEq>;
        let chapter = |c: u16, e: i8| -> State { NextStep::NewChapter(c, CharacterVariableG::new(e)) };
        let r = |n, d| Rational::from_i64(n, d);
        let won: State = NextStep::HasWon(CharacterVariableG::new(3));
        let content: SoldumpContent<Rational, NoPrevEq> = BTreeMap::from([
            (
                chapter(1, 10),
                ChoppedSolution::CNode(
                    r(1, 2),
                    vec![
                        (Some(chapter(2, 8)), r(1, 3)),
                        (Some(chapter(2, 9)), r(1, 3)),
                        (Some(NextStep::HasLost(1)), r(1, 3)),
                        (None, r(0, 1)),
                    ],
                ),
            ),
            (chapter(2, 8), ChoppedSolution::CJump(r(1, 2), chapter(3, 8))),
            (chapter(2, 9), ChoppedSolution::CNode(r(1, 2), vec![(Some(chapter(3, 8)), r(1, 4)), (Some(won.clone()), r(3, 4))])),
            (chapter(3, 8), ChoppedSolution::CLeaf(r(2, 3))),
            (won, ChoppedSolution::CLeaf(r(1, 1))),
            (NextStep::HasLost(1), ChoppedSolution::CLeafLost),
        ]);
        let searchmap = content.iter().map(|(a, b)| (a.clone(), b)).collect();
        let backward = mksol_backward(chapter(1, 10), searchmap);
        let forward = mksol(chapter(1, 10), &content).res;
        assert_eq!(serde_json::to_value(&forward).unwrap(), serde_json::to_value(&backward).unwrap());
    }
}
