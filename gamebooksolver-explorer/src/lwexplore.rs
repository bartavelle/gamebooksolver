use gamebooksolver_base::lonewolf::chapter::{Chapter, ChapterId};
use gamebooksolver_base::lonewolf::combat::Memoz;
use gamebooksolver_base::lonewolf::mini::{mkchar, CharacterConstant, NextStep, SolutionDump};
use gamebooksolver_base::lonewolf::solve::step;
use gamebooksolver_base::solver::base::{ChoppedSolution, Proba};
use rug::Rational;
use std::collections::HashMap;
use std::io::stdin;

pub fn explore_solution(soldump: SolutionDump<Rational>, book: &[(ChapterId, Chapter<Rational>)]) {
    let ini = NextStep::NewChapter(1, mkchar(&soldump.soldesc.ccst, &soldump.soldesc.cvar));
    let mut memo = Memoz::default();
    let mbook: HashMap<ChapterId, Chapter<Rational>> = book.iter().cloned().collect();
    let order: HashMap<ChapterId, u32> = HashMap::new();
    let content: HashMap<NextStep, ChoppedSolution<Rational, NextStep>> = soldump.content.into_iter().collect();

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

fn compute_score(
    solmap: &HashMap<NextStep, ChoppedSolution<Rational, NextStep>>,
    outcome: &[Proba<Rational, NextStep>],
) -> Rational {
    let mut o = Rational::from(0);
    for p in outcome {
        if let Some(s) = solmap.get(&p.v) {
            o += s.score() * &p.p;
        }
    }
    o
}

fn go(
    memo: &mut Memoz<Rational>,
    ccst: &CharacterConstant,
    mbook: &HashMap<ChapterId, Chapter<Rational>>,
    order: &HashMap<ChapterId, u32>,
    solmap: &HashMap<NextStep, ChoppedSolution<Rational, NextStep>>,
    ns: &NextStep,
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
