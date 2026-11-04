//! The solution files. A header that is not compressed, then a zstd stream holding the rest, encoded with bincode:
//! - a magic number, a version, and whether the character has stored equipment;
//! - the character, what the next book scores, and the origin of the file;
//! - the starting states, one for each starting gold;
//! - each distinct fraction once, in binary;
//! - the sorted states, and for each of them what happens next, using the indices of the states and of the fractions.

use anyhow::Context;
use bincode::{Decode, Encode};
use gamebooksolver_base::lonewolf::mini::{
    CVarState, CharacterConstant, Equipment, Flags, GSolDump, Item, NextStep, NoPrevEq, SolDesc, SolutionDump,
    SoldumpContent, StoredEquipment,
};
use gamebooksolver_base::solver::base::ChoppedSolution;
use gamebooksolver_base::solver::rational::r::MRational;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs::File;
use std::io::{Read, Write};

const MAGIC: &[u8; 8] = b"GBSOLVE\n";
const VERSION: u16 = 1;
// these files are written once, and read many times
const ZSTD_LEVEL: i32 = 19;

pub trait Kind {
    const KIND: u8;
}

impl Kind for NoPrevEq {
    const KIND: u8 = 0;
}

impl Kind for Equipment {
    const KIND: u8 = 1;
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub enum Origin {
    Solved { commit: Option<String> },
    /// Converted from a format that did not tell what it was computed from, as for Books 3 to 5.
    Converted,
}

#[derive(Debug, Encode, Decode)]
struct Fraction {
    negative: bool,
    numerator: Vec<u8>,
    denominator: Vec<u8>,
}

#[derive(Debug, Encode, Decode)]
enum Node {
    /// A state that is only mentioned, because it is not on a best path.
    Unknown,
    Lost,
    Leaf(u32),
    Jump(u32, u32),
    /// What comes next is 0 when there is nothing, or the index of the state plus 1.
    Choice(u32, Vec<(u32, u32)>),
}

#[derive(Debug, Encode, Decode)]
struct Payload<PREV: StoredEquipment> {
    finalchapters: Vec<u16>,
    ccst: CharacterConstant,
    items: Option<BTreeMap<Item, i64>>,
    flags: BTreeSet<gamebooksolver_base::lonewolf::mini::Flag>,
    scoremap: Option<Vec<(Equipment, Flags, u32)>>,
    startat: u16,
    book_hash: Option<u64>,
    origin: Origin,
    starts: Vec<(u8, u32)>,
    fractions: Vec<Fraction>,
    states: Vec<NextStep<PREV>>,
    nodes: Vec<Node>,
}

/// Tells apart the versions of a book. It is not meant to resist anything but mistakes.
pub fn book_hash(content: &[u8]) -> u64 {
    content.iter().fold(0xcbf29ce484222325, |h, b| (h ^ *b as u64).wrapping_mul(0x100000001b3))
}

#[derive(Default)]
struct Fractions {
    index: HashMap<MRational, u32>,
    all: Vec<Fraction>,
}

impl Fractions {
    fn get(&mut self, r: &MRational) -> u32 {
        if let Some(i) = self.index.get(r) {
            return *i;
        }
        let i = u32::try_from(self.all.len()).expect("too many fractions");
        let (negative, numerator, denominator) = r.to_parts();
        self.all.push(Fraction { negative, numerator, denominator });
        self.index.insert(r.clone(), i);
        i
    }
}

fn start_state<PREV: StoredEquipment>(desc: &SolDesc, startat: u16, gold: u8) -> NextStep<PREV> {
    let mut d = desc.clone();
    d.cvar.gold = gold;
    NextStep::NewChapter(startat, d.cvariable())
}

/// Saves the states reachable from the starting states with these amounts of gold.
pub fn save<PREV: StoredEquipment + Kind + Encode>(
    path: &str,
    desc: &SolDesc,
    content: &SoldumpContent<MRational, PREV>,
    golds: &[u8],
    startat: u16,
    hash: Option<u64>,
    origin: Origin,
) -> anyhow::Result<()> {
    let mut known: BTreeSet<&NextStep<PREV>> = content.keys().collect();
    for node in content.values() {
        match node {
            ChoppedSolution::CNode(_, l) => known.extend(l.iter().filter_map(|(s, _)| s.as_ref())),
            ChoppedSolution::CJump(_, s) => {
                known.insert(s);
            }
            ChoppedSolution::CLeafLost | ChoppedSolution::CLeaf(_) => (),
        }
    }
    let index: HashMap<&NextStep<PREV>, u32> = known
        .iter()
        .enumerate()
        .map(|(i, s)| (*s, u32::try_from(i).expect("too many states")))
        .collect();
    let mut fractions = Fractions::default();
    let nodes = known
        .iter()
        .map(|s| match content.get(*s) {
            None => Node::Unknown,
            Some(ChoppedSolution::CLeafLost) => Node::Lost,
            Some(ChoppedSolution::CLeaf(p)) => Node::Leaf(fractions.get(p)),
            Some(ChoppedSolution::CJump(p, n)) => Node::Jump(fractions.get(p), index[n]),
            Some(ChoppedSolution::CNode(p, l)) => Node::Choice(
                fractions.get(p),
                l.iter()
                    .map(|(s, q)| (s.as_ref().map_or(0, |s| index[s] + 1), fractions.get(q)))
                    .collect(),
            ),
        })
        .collect();
    let starts = golds
        .iter()
        .map(|g| {
            let start = start_state::<PREV>(desc, startat, *g);
            let i = index.get(&start).with_context(|| format!("no start state with {g} gold"))?;
            Ok((*g, *i))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let scoremap = desc
        .mscoremap
        .as_ref()
        .map(|m| m.iter().map(|(e, f, s)| (*e, *f, fractions.get(s))).collect());
    let payload = Payload::<PREV> {
        finalchapters: desc.finalchapters.clone(),
        ccst: desc.ccst.clone(),
        items: desc.cvar.items.clone(),
        flags: desc.cvar.flags.clone(),
        scoremap,
        startat,
        book_hash: hash,
        origin,
        starts,
        fractions: fractions.all,
        states: known.into_iter().cloned().collect(),
        nodes,
    };
    let mut file = File::create(path).with_context(|| format!("creating {path}"))?;
    file.write_all(MAGIC)?;
    file.write_all(&VERSION.to_le_bytes())?;
    file.write_all(&[PREV::KIND])?;
    let mut zwriter = zstd::Encoder::new(file, ZSTD_LEVEL)?;
    bincode::encode_into_std_write(&payload, &mut zwriter, bincode::config::standard())?;
    zwriter.finish()?;
    Ok(())
}

/// A solution file, that can hold several starting states.
pub struct SolFile<PREV: StoredEquipment> {
    pub desc: SolDesc,
    pub content: SoldumpContent<MRational, PREV>,
    pub golds: Vec<u8>,
    pub startat: u16,
    pub book_hash: Option<u64>,
    pub origin: Origin,
}

fn load_payload<PREV: StoredEquipment + Decode<()>>(reader: impl Read) -> anyhow::Result<SolFile<PREV>> {
    let mut dec = zstd::Decoder::new(reader)?;
    let p: Payload<PREV> = bincode::decode_from_std_read(&mut dec, bincode::config::standard())?;
    let fractions: Vec<MRational> = p
        .fractions
        .iter()
        .map(|f| MRational::from_parts(f.negative, &f.numerator, &f.denominator))
        .collect();
    let mut content = BTreeMap::new();
    for (i, node) in p.nodes.iter().enumerate() {
        let fr = |n: &u32| fractions[*n as usize].clone();
        let st = |n: &u32| p.states[*n as usize].clone();
        let sol = match node {
            Node::Unknown => continue,
            Node::Lost => ChoppedSolution::CLeafLost,
            Node::Leaf(s) => ChoppedSolution::CLeaf(fr(s)),
            Node::Jump(s, n) => ChoppedSolution::CJump(fr(s), st(n)),
            Node::Choice(s, l) => ChoppedSolution::CNode(
                fr(s),
                l.iter()
                    .map(|(n, q)| (n.checked_sub(1).map(|n| st(&n)), fr(q)))
                    .collect(),
            ),
        };
        content.insert(p.states[i].clone(), sol);
    }
    let golds: Vec<u8> = p.starts.iter().map(|(g, _)| *g).collect();
    let desc = SolDesc {
        finalchapters: p.finalchapters,
        ccst: p.ccst,
        cvar: CVarState {
            items: p.items,
            gold: *golds.first().context("a solution file without a start")?,
            flags: p.flags,
        },
        mscoremap: p
            .scoremap
            .map(|m| m.into_iter().map(|(e, f, s)| (e, f, fractions[s as usize].clone())).collect()),
    };
    Ok(SolFile {
        desc,
        content,
        golds,
        startat: p.startat,
        book_hash: p.book_hash,
        origin: p.origin,
    })
}

impl<PREV: StoredEquipment> SolFile<PREV> {
    /// The solution of one starting gold, as if it had been solved on its own. It is optional when there is only one.
    pub fn dump(&self, gold: Option<u8>) -> anyhow::Result<SolutionDump<MRational, PREV>> {
        let gold = match (gold, self.golds.as_slice()) {
            (Some(g), golds) if golds.contains(&g) => g,
            (Some(g), golds) => anyhow::bail!("no start with {g} gold, there are {golds:?}"),
            (None, [g]) => *g,
            (None, golds) => anyhow::bail!("this file has many starting gold, choose with --start-gold: {golds:?}"),
        };
        let mut soldesc = self.desc.clone();
        soldesc.cvar.gold = gold;
        let mut content = BTreeMap::new();
        let mut todo = vec![start_state::<PREV>(&soldesc, self.startat, gold)];
        while let Some(s) = todo.pop() {
            if content.contains_key(&s) {
                continue;
            }
            if let Some(node) = self.content.get(&s) {
                match node {
                    ChoppedSolution::CNode(_, l) => todo.extend(l.iter().filter_map(|(n, _)| n.clone())),
                    ChoppedSolution::CJump(_, n) => todo.push(n.clone()),
                    ChoppedSolution::CLeafLost | ChoppedSolution::CLeaf(_) => (),
                }
                content.insert(s, node.clone());
            }
        }
        Ok(SolutionDump { soldesc, content })
    }
}

fn read_kind(path: &str, file: &mut File) -> anyhow::Result<u8> {
    let mut header = [0u8; 11];
    file.read_exact(&mut header).with_context(|| format!("reading {path}"))?;
    anyhow::ensure!(&header[..8] == MAGIC, "{path} is not a solution file");
    let version = u16::from_le_bytes([header[8], header[9]]);
    anyhow::ensure!(version == VERSION, "{path} has version {version}, and this program reads {VERSION}");
    Ok(header[10])
}

pub enum Loaded {
    Prev(SolFile<Equipment>),
    Noprev(SolFile<NoPrevEq>),
}

pub fn load(path: &str) -> anyhow::Result<Loaded> {
    let mut file = File::open(path).with_context(|| format!("opening {path}"))?;
    match read_kind(path, &mut file)? {
        NoPrevEq::KIND => load_payload(file).map(Loaded::Noprev),
        Equipment::KIND => load_payload(file).map(Loaded::Prev),
        k => anyhow::bail!("{path} has an unknown kind: {k}"),
    }
}

/// The solution of one starting gold of a file.
pub fn load_dump(path: &str, gold: Option<u8>) -> anyhow::Result<GSolDump<MRational>> {
    Ok(match load(path)? {
        Loaded::Prev(f) => GSolDump::Prev(f.dump(gold)?),
        Loaded::Noprev(f) => GSolDump::Noprev(f.dump(gold)?),
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use gamebooksolver_base::lonewolf::mini::{Book, CharacterVariableG, Discipline};
    use gamebooksolver_base::solver::rational::Rational;

    fn desc() -> SolDesc {
        SolDesc {
            finalchapters: vec![350],
            ccst: CharacterConstant {
                maxendurance: 20,
                combat_skill: 10,
                discipline: vec![Discipline::Healing],
                bookid: Book::Book02,
            },
            cvar: CVarState {
                items: None,
                gold: 3,
                flags: BTreeSet::new(),
            },
            mscoremap: Some(vec![(Equipment::default(), Flags(0), MRational::from_i64(1, 3))]),
        }
    }

    type Content = SoldumpContent<MRational, NoPrevEq>;

    fn start(gold: u8) -> NextStep<NoPrevEq> {
        start_state(&desc(), 1, gold)
    }

    /// Two starts that lead to a common state, and one state that is not on the best path of the first.
    fn content() -> Content {
        let common: NextStep<NoPrevEq> = NextStep::NewChapter(9, CharacterVariableG::new(5));
        let other: NextStep<NoPrevEq> = NextStep::NewChapter(8, CharacterVariableG::new(5));
        let half = MRational::from_i64(1, 2);
        Content::from([
            (start(3), ChoppedSolution::CJump(half.clone(), common.clone())),
            (
                start(4),
                ChoppedSolution::CNode(half.clone(), vec![(Some(common.clone()), half.clone()), (Some(other.clone()), half.clone()), (None, MRational::from_i64(0, 1))]),
            ),
            (common, ChoppedSolution::CLeaf(half)),
            (other, ChoppedSolution::CLeafLost),
        ])
    }

    fn path(name: &str) -> String {
        let dir = std::env::temp_dir();
        dir.join(format!("gbsolve-{}-{name}", std::process::id())).to_str().unwrap().to_string()
    }

    #[test]
    fn saved_files_read_back() {
        let p = path("roundtrip");
        save(&p, &desc(), &content(), &[3, 4], 1, Some(7), Origin::Solved { commit: Some("abc".into()) }).unwrap();
        let Loaded::Noprev(back) = load(&p).unwrap() else { panic!("wrong kind") };
        assert_eq!(back.content, content());
        assert_eq!(back.golds, [3, 4]);
        assert_eq!(back.desc, desc());
        assert_eq!(back.book_hash, Some(7));
        assert_eq!(back.origin, Origin::Solved { commit: Some("abc".into()) });
        std::fs::remove_file(p).unwrap();
    }

    #[test]
    fn one_start_is_like_a_separate_solve() {
        let p = path("start");
        save(&p, &desc(), &content(), &[3, 4], 1, None, Origin::Converted).unwrap();
        let Loaded::Noprev(f) = load(&p).unwrap() else { panic!("wrong kind") };
        let d = f.dump(Some(3)).unwrap();
        // the second start, and what only it reaches, are gone
        assert_eq!(d.content.len(), 2);
        assert_eq!(d.soldesc.cvar.gold, 3);
        assert!(f.dump(None).is_err());
        assert!(f.dump(Some(5)).is_err());
        std::fs::remove_file(p).unwrap();
    }

    #[test]
    fn bad_headers_are_refused() {
        let p = path("header");
        std::fs::write(&p, b"not a solution file").unwrap();
        assert!(load(&p).err().unwrap().to_string().contains("not a solution file"));
        let mut other = MAGIC.to_vec();
        other.extend((VERSION + 1).to_le_bytes());
        other.push(0);
        std::fs::write(&p, other).unwrap();
        assert!(load(&p).err().unwrap().to_string().contains("version"));
        std::fs::remove_file(p).unwrap();
    }
}
