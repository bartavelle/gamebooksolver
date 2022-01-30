module LoneWolf.Book01 where

import Control.Lens
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Simplify (extractMultiFight)
import LoneWolf.RawBook.Book01 (chapters)

pchapters :: [(ChapterId, Chapter)]
pchapters = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
  where
    blacklisted = S.fromList [8, 130, 201, 15, 207, 35, 86, 238, 42, 28, 147, 42, 130, 68, 15]
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    -- tower climbing tricks : pretend that going to 36 means you wanna climb the tower at all cost
    patch (36, c) =
      ( 36,
        c & pchoice
          .~ NoDecision
            ( Randomly
                [ (1 % 2, Goto 290),
                  (1 % 4, Simple [DamagePlayer 2] (Goto 290)),
                  (1 % 8, Simple [DamagePlayer 4] (Goto 290)),
                  (1 % 16, Simple [DamagePlayer 6] (Goto 290)),
                  (1 % 32, Simple [DamagePlayer 8] (Goto 290)),
                  (1 % 64, Simple [DamagePlayer 10] (Goto 290)),
                  (1 % 128, Simple [DamagePlayer 12] (Goto 290)),
                  (1 % 256, Simple [DamagePlayer 14] (Goto 290)),
                  (1 % 512, Simple [DamagePlayer 16] (Goto 290)),
                  (1 % 512, Simple [DamagePlayer 18] (Goto 290))
                ]
            )
      )
    patch (290, c) =
      ( 290,
        c & pchoice
          .~ CanTake
            (Weapon Quarterstaff)
            1
            ( Decisions
                [ ("south", NoDecision (Goto 14)),
                  ("east", NoDecision (Goto 252)),
                  ("southwest", NoDecision (Goto 215))
                ]
            )
      )
    -- useless branches
    patch (125, c) = (125, c & pchoice .~ NoDecision (Goto 27))
    -- whole labythintic zone
    patch (70, c) =
      ( 70,
        c & pchoice
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
