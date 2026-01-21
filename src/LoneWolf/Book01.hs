module LoneWolf.Book01 where

import Control.Lens
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.RawBook.Book01 (chapters)
import LoneWolf.Simplify (extractMultiFight)

pchapters :: [(ChapterId, Chapter)]
pchapters = base ++ extra
  where
    base = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
    max_base_chapter = maximum (map fst base)
    hill_climb_abort = max_base_chapter + 1
    hill_climb_start = hill_climb_abort + 1
    max_hill_attempts = 10
    extra = hill_climb
    hill_climb =
      ( hill_climb_abort,
        Chapter
          "140b"
          "abort hill climbing"
          ( Decisions
              [ ("If you take the south path, turn to 14.", NoDecision (Goto 14)),
                ("If you take the east path, turn to 252.", NoDecision (Goto 252)),
                ("If you take the southwest path, turn to 215.", NoDecision (Goto 215))
              ]
          )
      )
        : [ ( hill_climb_start + attempt,
              Chapter
                ("h" ++ show attempt)
                "attemp to climb the hill"
                ( Decisions
                    [ ("try climbing", NoDecision (Randomly [(1 % 2, Goto 323), (1 % 2, Simple [DamagePlayer 2] (Goto (if attempt == max_hill_attempts then hill_climb_abort else hill_climb_start + attempt + 1)))])),
                      ("abandon", NoDecision (Goto hill_climb_abort))
                    ]
                )
            )
          | attempt <- [0 .. max_hill_attempts]
          ]
    -- blacklisted = S.fromList [8, 130, 201, 15, 207, 35, 86, 238, 42, 28, 147, 42, 130, 68, 15]
    blacklisted = S.fromList []
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    -- tower climbing tricks, so that we don't go back to climbing the a previous known state after climbing the hill
    patch (36, c) = (36, c & pchoice .~ NoDecision (Goto hill_climb_start))
    patch (323, c) =
      ( 323,
        c
          & pchoice
            .~ Decisions
              [ ("If you wish to open this box, turn to 290.", NoDecision (Goto 290)),
                ("If you would prefer to descend the ladder and leave the tower, taking care to use only the good rungs, turn to 140.", NoDecision (Goto hill_climb_abort))
              ]
      )
    patch (290, c) =
      ( 290,
        c
          & pchoice
            .~ CanTake
              (Weapon Quarterstaff)
              1
              (NoDecision (Goto hill_climb_abort))
      )
    -- useless branches
    patch (125, c) = (125, c & pchoice .~ NoDecision (Goto 27))
    -- whole labythintic zone
    patch (70, c) =
      ( 70,
        c
          & pchoice
            .~ CanTake
              (Weapon Sword)
              1
              ( Decisions
                  [ ("Exit through 167", NoDecision (Simple [MustEat Hunt] (Goto 167))),
                    ("Exit through 30", NoDecision (Simple [MustEat Hunt] (Goto 30))),
                    ("Exit through 6", NoDecision (Simple [MustEat Hunt] (Goto 6)))
                  ]
              )
      )
    patch x = x
