use num_rational::BigRational;
use num_traits::cast::ToPrimitive;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

use gamebooksolver_base::lonewolf::chapter::{Chapter, ChapterId};
use gamebooksolver_base::lonewolf::combat::Memoz;
use gamebooksolver_base::lonewolf::data::order_chapters;
use gamebooksolver_base::lonewolf::mini::{CharacterVariable, CompactSolution, NextStep, SolDesc};
use gamebooksolver_base::lonewolf::solve::step;
use gamebooksolver_base::solver::base::{Choice, Outcome, Proba};
use gamebooksolver_base::solver::rational::{JRatio, Rational};

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn init_hooks() {
  std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
pub struct WSolDesc(SolDesc);

#[wasm_bindgen]
impl WSolDesc {
  #[wasm_bindgen(constructor)]
  pub fn new(buf: &[u8]) -> Self {
    WSolDesc(minicbor::decode(&buf).unwrap())
  }

  pub fn ini_cvar(&self) -> WCharacterVariable {
    WCharacterVariable(self.0.cvariable())
  }
}

#[wasm_bindgen]
pub struct WCharacterVariable(CharacterVariable);

#[wasm_bindgen]
pub struct WChoices(Vec<Choice<BigRational, String, NextStep>>);

#[wasm_bindgen]
impl WChoices {
  pub fn count(&self) -> usize {
    self.0.len()
  }

  pub fn nth(&self, idx: usize) -> Option<WChoice> {
    self.0.get(idx).cloned().map(WChoice)
  }
}

#[wasm_bindgen]
pub struct WChoice(Choice<BigRational, String, NextStep>);

#[wasm_bindgen]
impl WChoice {
  #[wasm_bindgen(getter)]
  pub fn desc(&self) -> String {
    self.0.desc.clone()
  }

  #[wasm_bindgen(getter)]
  pub fn res(&self) -> WOutcome {
    WOutcome(self.0.res.clone())
  }
}

#[wasm_bindgen]
pub struct WOutcome(Outcome<BigRational, NextStep>);

#[wasm_bindgen]
impl WOutcome {
  pub fn count(&self) -> usize {
    self.0.len()
  }

  pub fn nth(&self, idx: usize) -> Option<PNS> {
    self.0.get(idx).cloned().map(PNS)
  }
}

#[wasm_bindgen]
pub struct PNS(Proba<BigRational, NextStep>);

#[wasm_bindgen]
impl PNS {
  #[wasm_bindgen(getter)]
  pub fn p(&self) -> f64 {
    self.0.p.to_f64().unwrap()
  }

  #[wasm_bindgen(getter)]
  pub fn v(&self) -> WNS {
    WNS(self.0.v.clone())
  }
}

#[wasm_bindgen]
pub struct WNS(NextStep);

#[wasm_bindgen]
impl WNS {
  #[wasm_bindgen(constructor)]
  pub fn new(cid: u16, wc: &WCharacterVariable) -> Self {
    WNS(NextStep::NewChapter(cid, wc.0.clone()))
  }

  pub fn tp(&self) -> String {
    match self.0 {
      NextStep::HasLost(_) => "lost",
      NextStep::HasWon(_) => "won",
      NextStep::NewChapter(_, _) => "chapter",
    }
    .into()
  }

  pub fn lost(&self) -> Option<u16> {
    if let NextStep::HasLost(cid) = self.0 {
      Some(cid)
    } else {
      None
    }
  }

  pub fn won(&self) -> Option<WCharacterVariable> {
    if let NextStep::HasWon(sc) = &self.0 {
      Some(WCharacterVariable(sc.clone()))
    } else {
      None
    }
  }

  pub fn chapter(&self) -> Option<WNewChapter> {
    if let NextStep::NewChapter(cid, cv) = &self.0 {
      Some(WNewChapter {
        chapter: *cid,
        cvar: cv.clone(),
      })
    } else {
      None
    }
  }
}

#[wasm_bindgen]
pub struct WNewChapter {
  chapter: u16,
  cvar: CharacterVariable,
}

#[wasm_bindgen]
impl WNewChapter {
  #[wasm_bindgen(getter)]
  pub fn chapter(&self) -> u16 {
    self.chapter
  }

  #[wasm_bindgen(getter)]
  pub fn cvar(&self) -> WCharacterVariable {
    WCharacterVariable(self.cvar.clone())
  }
}

#[wasm_bindgen]
impl WCharacterVariable {
  pub fn desc(&self) -> String {
    format!("{}", self.0)
  }
}

#[wasm_bindgen]
impl WCharacterVariable {
  #[wasm_bindgen(getter)]
  pub fn endurance(&self) -> i8 {
    self.0.curendurance
  }
  #[wasm_bindgen(getter)]
  pub fn flags(&self) -> Vec<JsValue> {
    self
      .0
      .flags
      .all()
      .into_iter()
      .map(|f| JsValue::from_serde(&f).unwrap())
      .collect()
  }
  #[wasm_bindgen(getter)]
  pub fn items(&self) -> Vec<JsValue> {
    self
      .0
      .cequipment
      .items()
      .into_iter()
      .map(|p| JsValue::from_serde(&p).unwrap())
      .collect()
  }
}

#[wasm_bindgen]
pub struct WState {
  soldesc: SolDesc,
  scores: HashMap<(u16, CharacterVariable), f32>,
  memo: Memoz<BigRational>,
  order: HashMap<ChapterId, u32>,
  chapters: HashMap<ChapterId, Chapter<BigRational>>,
}

#[wasm_bindgen]
impl WState {
  #[wasm_bindgen(constructor)]
  pub fn new(buf: &[u8], bk: &str) -> WState {
    let sol: CompactSolution = minicbor::decode(&buf).unwrap();
    let book: Vec<(ChapterId, Chapter<JRatio>)> = serde_json::from_str(&bk).unwrap();
    let chapters: HashMap<ChapterId, Chapter<BigRational>> = book
      .into_iter()
      .map(|(cid, c)| (cid, c.map_proba(&|p: &JRatio| BigRational::from_jratio(p))))
      .collect();
    let order = order_chapters(&chapters);
    let soldesc = sol.soldesc;
    let scores = sol
      .content
      .into_iter()
      .map(|cs| ((cs.chapter, cs.character), cs.score))
      .collect();

    WState {
      soldesc,
      scores,
      memo: Memoz::default(),
      order,
      chapters,
    }
  }

  pub fn step(&mut self, ns: &WNS) -> WChoices {
    WChoices(step(
      &mut self.memo,
      &self.order,
      &self.chapters,
      &self.soldesc.ccst,
      &ns.0,
    ))
  }

  pub fn score(&self, ns: &WNS) -> Option<f32> {
    match &ns.0 {
      NextStep::HasLost(_) => Some(0.0),
      NextStep::HasWon(sc) => Some(1.0),
      NextStep::NewChapter(chapter, cv) => self.scores.get(&(*chapter, cv.clone())).copied(),
    }
  }

  pub fn ini_cvar(&self) -> WCharacterVariable {
    WCharacterVariable(self.soldesc.cvariable())
  }
}
