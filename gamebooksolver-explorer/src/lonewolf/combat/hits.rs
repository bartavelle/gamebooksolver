pub fn hits(ratio: i8) -> &'static [HitResult; 10] {
  let nratio: usize = if ratio < -6 {
    0
  } else if ratio > 6 {
    12
  } else {
    (ratio + 6) as usize
  };
  &HITSCHART[nratio]
}

pub struct HitResult {
  pub op: u8,
  pub lw: u8,
}

const HITSCHART: [[HitResult; 10]; 13] = [
  [
    // -6
    HitResult { op: 0, lw: 100 },
    HitResult { op: 0, lw: 100 },
    HitResult { op: 0, lw: 8 },
    HitResult { op: 0, lw: 8 },
    HitResult { op: 1, lw: 7 },
    HitResult { op: 2, lw: 6 },
    HitResult { op: 3, lw: 5 },
    HitResult { op: 4, lw: 4 },
    HitResult { op: 5, lw: 3 },
    HitResult { op: 6, lw: 0 },
  ],
  [
    // -5
    HitResult { op: 0, lw: 100 },
    HitResult { op: 0, lw: 8 },
    HitResult { op: 0, lw: 7 },
    HitResult { op: 1, lw: 7 },
    HitResult { op: 2, lw: 6 },
    HitResult { op: 3, lw: 6 },
    HitResult { op: 4, lw: 5 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 0 },
  ],
  [
    // -4
    HitResult { op: 0, lw: 8 },
    HitResult { op: 0, lw: 7 },
    HitResult { op: 1, lw: 6 },
    HitResult { op: 2, lw: 6 },
    HitResult { op: 3, lw: 5 },
    HitResult { op: 4, lw: 5 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 2 },
    HitResult { op: 8, lw: 0 },
  ],
  [
    // -3
    HitResult { op: 0, lw: 6 },
    HitResult { op: 1, lw: 6 },
    HitResult { op: 2, lw: 5 },
    HitResult { op: 3, lw: 5 },
    HitResult { op: 4, lw: 4 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 2 },
    HitResult { op: 8, lw: 0 },
    HitResult { op: 9, lw: 0 },
  ],
  [
    // -2
    HitResult { op: 1, lw: 6 },
    HitResult { op: 2, lw: 5 },
    HitResult { op: 3, lw: 5 },
    HitResult { op: 4, lw: 4 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 2 },
    HitResult { op: 8, lw: 1 },
    HitResult { op: 9, lw: 0 },
    HitResult { op: 10, lw: 0 },
  ],
  [
    // -1
    HitResult { op: 2, lw: 5 },
    HitResult { op: 3, lw: 5 },
    HitResult { op: 4, lw: 4 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 2 },
    HitResult { op: 8, lw: 2 },
    HitResult { op: 9, lw: 1 },
    HitResult { op: 10, lw: 0 },
    HitResult { op: 11, lw: 0 },
  ],
  [
    // 0
    HitResult { op: 3, lw: 5 },
    HitResult { op: 4, lw: 4 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 2 },
    HitResult { op: 8, lw: 2 },
    HitResult { op: 10, lw: 1 },
    HitResult { op: 10, lw: 0 },
    HitResult { op: 11, lw: 0 },
    HitResult { op: 12, lw: 0 },
  ],
  [
    // 1
    HitResult { op: 4, lw: 5 },
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 3 },
    HitResult { op: 8, lw: 2 },
    HitResult { op: 9, lw: 2 },
    HitResult { op: 11, lw: 1 },
    HitResult { op: 11, lw: 0 },
    HitResult { op: 12, lw: 0 },
    HitResult { op: 14, lw: 0 },
  ],
  [
    // 2
    HitResult { op: 5, lw: 4 },
    HitResult { op: 6, lw: 3 },
    HitResult { op: 7, lw: 3 },
    HitResult { op: 8, lw: 2 },
    HitResult { op: 9, lw: 2 },
    HitResult { op: 10, lw: 2 },
    HitResult { op: 12, lw: 1 },
    HitResult { op: 12, lw: 0 },
    HitResult { op: 14, lw: 0 },
    HitResult { op: 16, lw: 0 },
  ],
  [
    // 3
    HitResult { op: 6, lw: 4 },
    HitResult { op: 7, lw: 3 },
    HitResult { op: 8, lw: 3 },
    HitResult { op: 9, lw: 2 },
    HitResult { op: 10, lw: 2 },
    HitResult { op: 11, lw: 1 },
    HitResult { op: 14, lw: 0 },
    HitResult { op: 14, lw: 0 },
    HitResult { op: 16, lw: 0 },
    HitResult { op: 18, lw: 0 },
  ],
  [
    // 4
    HitResult { op: 7, lw: 4 },
    HitResult { op: 8, lw: 3 },
    HitResult { op: 9, lw: 2 },
    HitResult { op: 10, lw: 2 },
    HitResult { op: 11, lw: 2 },
    HitResult { op: 12, lw: 1 },
    HitResult { op: 14, lw: 0 },
    HitResult { op: 16, lw: 0 },
    HitResult { op: 18, lw: 0 },
    HitResult { op: 100, lw: 0 },
  ],
  [
    // 5
    HitResult { op: 8, lw: 3 },
    HitResult { op: 9, lw: 3 },
    HitResult { op: 10, lw: 2 },
    HitResult { op: 11, lw: 2 },
    HitResult { op: 12, lw: 2 },
    HitResult { op: 14, lw: 1 },
    HitResult { op: 16, lw: 0 },
    HitResult { op: 18, lw: 0 },
    HitResult { op: 100, lw: 0 },
    HitResult { op: 100, lw: 0 },
  ],
  [
    // 6
    HitResult { op: 9, lw: 3 },
    HitResult { op: 10, lw: 2 },
    HitResult { op: 11, lw: 2 },
    HitResult { op: 12, lw: 2 },
    HitResult { op: 14, lw: 1 },
    HitResult { op: 16, lw: 1 },
    HitResult { op: 18, lw: 0 },
    HitResult { op: 100, lw: 0 },
    HitResult { op: 100, lw: 0 },
    HitResult { op: 100, lw: 0 },
  ],
];
