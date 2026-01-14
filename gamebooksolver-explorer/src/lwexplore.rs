use gamebooksolver_base::lonewolf::chapter::{Chapter, ChapterId};
use gamebooksolver_base::lonewolf::combat::Memoz;
use gamebooksolver_base::lonewolf::mini::{
    CharacterConstant, CompactSolution, NextStep, SolutionDump, StoredEquipment, mkchar,
};
use gamebooksolver_base::lonewolf::solve::step;
use gamebooksolver_base::solver::base::{ChoppedSolution, Proba};
use gamebooksolver_base::solver::rational::MF64;
use std::collections::HashMap;
use std::io::stdin;

type Rational = gamebooksolver_base::solver::rational::r::MRational;

fn rational_to_mf64(r: &Rational) -> MF64 {
    <MF64 as gamebooksolver_base::solver::rational::Rational>::f_f64(r.to_f64())
}

pub fn explore_compact<PREV: StoredEquipment>(
    soldump: &CompactSolution<PREV>,
    book: &[(ChapterId, Chapter<Rational>)],
) {
    println!("DESC: {:?}", &soldump.soldesc);
    fn convert_chapter(c: Chapter<Rational>) -> Chapter<MF64> {
        c.map_proba(&rational_to_mf64)
    }

    let ini = NextStep::NewChapter(1, mkchar(&soldump.soldesc.ccst, &soldump.soldesc.cvar));
    let mut memo = Memoz::default();
    let mbook: HashMap<ChapterId, Chapter<MF64>> = book.iter().cloned().map(|(k, c)| (k, convert_chapter(c))).collect();
    let order: HashMap<ChapterId, u32> = HashMap::new();
    let mut memo_score = HashMap::new();

    go_compact(
        &mut memo,
        &mut memo_score,
        &soldump.soldesc.ccst,
        &mbook,
        &order,
        soldump,
        &ini,
    );
}

pub fn explore_solution<PREV: StoredEquipment>(
    soldump: SolutionDump<Rational, PREV>,
    book: &[(ChapterId, Chapter<Rational>)],
) {
    let ini = NextStep::NewChapter(1, mkchar(&soldump.soldesc.ccst, &soldump.soldesc.cvar));
    let mut memo = Memoz::default();
    let mbook: HashMap<ChapterId, Chapter<Rational>> = book.iter().cloned().collect();
    let order: HashMap<ChapterId, u32> = HashMap::new();
    let content: HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>> =
        soldump.content.into_iter().collect();

    go(&mut memo, &soldump.soldesc.ccst, &mbook, &order, &content, &ini);
}

fn select_between<A, F>(choices: &[A], fmt: F) -> &A
where
    F: Fn(&A) -> String,
{
    for (i, c) in choices.iter().enumerate() {
        println!("{:2} - {}", i, fmt(c));
    }
    if choices.len() == 1 {
        &choices[0]
    } else {
        loop {
            let mut user_input = String::new();
            stdin().read_line(&mut user_input).unwrap();
            match user_input.trim().parse::<usize>() {
                Ok(n) => {
                    if n < choices.len() {
                        return &choices[n];
                    }
                }
                Err(rr) => {
                    println!("err: {}", rr);
                }
            }
        }
    }
}

fn compute_score<PREV: StoredEquipment>(
    solmap: &HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>,
    outcome: &[Proba<Rational, NextStep<PREV>>],
) -> Rational {
    let mut o = Rational::from(0);
    for p in outcome {
        if let Some(s) = solmap.get(&p.v) {
            o += s.score() * &p.p;
        }
    }
    o
}

fn go<PREV: StoredEquipment>(
    memo: &mut Memoz<Rational>,
    ccst: &CharacterConstant,
    mbook: &HashMap<ChapterId, Chapter<Rational>>,
    order: &HashMap<ChapterId, u32>,
    solmap: &HashMap<NextStep<PREV>, ChoppedSolution<Rational, NextStep<PREV>>>,
    ns: &NextStep<PREV>,
) {
    match ns {
        NextStep::HasLost(cid) => {
            println!("lost {}", cid);
            return;
        }
        NextStep::HasWon(cvar) => {
            println!("won {}", cvar);
            return;
        }
        NextStep::NewChapter(cid, cvar) => {
            println!("cid={} - {}", cid, cvar);
        }
    };

    let steps = step(memo, order, mbook, ccst, ns);

    let next_step = select_between(&steps, |s| {
        let score = compute_score(solmap, &s.res);
        format!("{:2.2}% {:?}", score.to_f64() * 100.0, s.desc)
    });

    let highest_proba = next_step
        .res
        .iter()
        .map(|p| &p.p)
        .max()
        .cloned()
        .unwrap_or_else(|| Rational::from(0));

    let outcome = select_between(&next_step.res, |s| {
        let score = solmap
            .get(&s.v)
            .map(|s| s.score())
            .unwrap_or_else(|| Rational::from(-1));
        let ishighest = if s.p == highest_proba { "*" } else { " " };
        format!(
            "{} p:{:2.2}% s:{:2.2}% {}",
            ishighest,
            s.p.to_f64() * 100.0,
            score.to_f64() * 100.0,
            s.v
        )
    });
    go(memo, ccst, mbook, order, solmap, &outcome.v);
}

fn go_compact<PREV: StoredEquipment>(
    memo: &mut Memoz<MF64>,
    memo_score: &mut HashMap<NextStep<PREV>, f64>,
    ccst: &CharacterConstant,
    mbook: &HashMap<ChapterId, Chapter<MF64>>,
    order: &HashMap<ChapterId, u32>,
    solmap: &CompactSolution<PREV>,
    ns: &NextStep<PREV>,
) {
    match ns {
        NextStep::HasLost(cid) => {
            println!("lost {}", cid);
            return;
        }
        NextStep::HasWon(cvar) => {
            println!("won {}", cvar);
            return;
        }
        NextStep::NewChapter(cid, cvar) => {
            println!("cid={} - {}", cid, cvar);
        }
    };

    let r_steps = step(memo, order, mbook, ccst, ns);

    let mut steps = Vec::new();

    for step in r_steps {
        let score = compute_score_compact(solmap, memo, memo_score, ccst, mbook, order, &step.res);
        steps.push((step, score));
    }

    let next_step = select_between(&steps, |(s, score)| format!("{:2.2}% {:?}", score * 100.0, s.desc));

    let highest_proba = next_step
        .0
        .res
        .iter()
        .map(|p| &p.p)
        .max()
        .cloned()
        .unwrap_or_else(MF64::default);

    let mut outcomes = Vec::new();
    for s in &next_step.0.res {
        let score = score_of_step(solmap, memo, memo_score, ccst, mbook, order, &s.v);
        outcomes.push((s, score))
    }

    let outcome = select_between(&outcomes, |(s, score)| {
        let ishighest = if s.p == highest_proba { "*" } else { " " };
        format!(
            "{} p:{:2.2}% s:{:2.2}% {}",
            ishighest,
            gamebooksolver_base::solver::rational::Rational::t_f64(&s.p) * 100.0,
            score * 100.0,
            s.v
        )
    });
    go_compact(memo, memo_score, ccst, mbook, order, solmap, &outcome.0.v);
}

fn score_of_step<PREV: StoredEquipment>(
    solmap: &CompactSolution<PREV>,
    memo: &mut Memoz<MF64>,
    memo_score: &mut HashMap<NextStep<PREV>, f64>,
    ccst: &CharacterConstant,
    mbook: &HashMap<ChapterId, Chapter<MF64>>,
    order: &HashMap<ChapterId, u32>,
    ns: &NextStep<PREV>,
) -> f64 {
    let out = match memo_score.get(ns).copied().or_else(|| solmap.get_score(ns)) {
        Some(x) => x,
        None => {
            let v = step(memo, order, mbook, ccst, ns);
            let mut max = 0.0;
            for s in v {
                let cur = compute_score_compact(solmap, memo, memo_score, ccst, mbook, order, &s.res);
                if cur > max {
                    max = cur
                }
            }
            max
        }
    };
    memo_score.insert(ns.clone(), out);
    out
}

fn compute_score_compact<PREV: StoredEquipment>(
    solmap: &CompactSolution<PREV>,
    memo: &mut Memoz<MF64>,
    memo_score: &mut HashMap<NextStep<PREV>, f64>,
    ccst: &CharacterConstant,
    mbook: &HashMap<ChapterId, Chapter<MF64>>,
    order: &HashMap<ChapterId, u32>,
    outcome: &[Proba<MF64, NextStep<PREV>>],
) -> f64 {
    let mut o = 0.0;
    for p in outcome {
        let curscore = score_of_step(solmap, memo, memo_score, ccst, mbook, order, &p.v);
        o += curscore * gamebooksolver_base::solver::rational::Rational::t_f64(&p.p);
    }
    o
}
