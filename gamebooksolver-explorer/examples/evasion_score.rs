//! Solves small books given as one JSON object per line on the standard input, with a win worth 1, and prints the exact
//! score of each. Used by `rocq/difftest/evasion.py`.

use gamebooksolver_base::lonewolf::chapter::{Chapter, ChapterId};
use gamebooksolver_base::lonewolf::mini::{Book, CharacterConstant, CharacterVariableG, NextStep, NoPrevEq};
use gamebooksolver_base::lonewolf::solve::solve_lws;
use gamebooksolver_base::solver::base::SolNode;
use gamebooksolver_base::solver::rational::Rational as _;
use gamebooksolver_base::solver::rational::r::MRational;
use serde::Deserialize;
use std::io::BufRead;

#[derive(Deserialize)]
struct Case {
    book: Vec<(ChapterId, Chapter<MRational>)>,
    start: u16,
    skill: u8,
    maxend: i8,
    hp: i8,
    disc: Vec<String>,
    items: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    for line in std::io::stdin().lock().lines() {
        let case: Case = serde_json::from_str(&line?)?;
        let ccst = CharacterConstant {
            bookid: Book::Book04,
            combat_skill: case.skill,
            maxendurance: case.maxend,
            discipline: case.disc.iter().map(|d| d.parse().unwrap()).collect(),
        };
        let mut cvar = CharacterVariableG::<NoPrevEq>::new(case.hp);
        for i in &case.items {
            cvar.add_item(&i.parse().unwrap(), 1);
        }
        let sol = solve_lws(|_, _| MRational::from_i64(1, 1), &[], &case.book, &ccst, &cvar, case.start, false);
        let score = match sol.get(&NextStep::NewChapter(case.start, cvar)) {
            Some(SolNode::Chosen(p, _)) | Some(SolNode::Single(p, _)) | Some(SolNode::Win(p)) => p.clone(),
            None => panic!("not solved"),
        };
        println!("{}", serde_json::to_string(&score)?);
    }
    Ok(())
}
