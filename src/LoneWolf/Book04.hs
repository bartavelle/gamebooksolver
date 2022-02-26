module LoneWolf.Book04 where

import Control.Lens
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.RawBook.Book04 (chapters)
import LoneWolf.Simplify (extractMultiFight)

pchapters :: [(ChapterId, Chapter)]
pchapters = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
  where
    blacklisted = S.fromList []
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    patch :: (ChapterId, Chapter) -> (ChapterId, Chapter)
    -- remove brass key once it is not in use
    patch (23, c) = (23, c & pchoice . _Outcome %~ Simple [LoseItem brassKeyB04 1])
    patch (50, c) = (50, c & pchoice . _Outcome %~ Simple [LoseItem brassKeyB04 1])
    patch (342, c) = (342, c & pchoice . _Outcome %~ Simple [LoseItem brassKeyB04 1])
    patch (308, c) = (308, c & pchoice . _Outcome %~ Simple [LoseItem brassKeyB04 1])
    -- remove brass key at chapter 152, as it can't be used later
    patch (152, c) =
      ( 152,
        c & pchoice
          .~ CanTake
            Gold
            6
            ( CanTake (Weapon Sword) 1 (CanTake Meal 2 (NoDecision (Goto 81)))
            )
      )
    -- remove iron key as it is used once, just after being picket
    patch (268, c) =
      ( 268,
        c & pchoice
          .~ CanTake
            Gold
            4
            ( CanTake
                Meal
                2
                ( CanTake
                    (Weapon Spear)
                    1
                    ( CanTake (Weapon BroadSword) 1 (CanTake (GenSpecial (GenCounter 0)) 1 (CanTake Laumspur 1 (Decisions [("If you wish to examine the iron door, turn to 118.", NoDecision (Goto 118)), ("If you wish to ascend the spiral stairs, turn to 170.", NoDecision (Goto 170)), ("If you wish to descend the spiral stairs, turn to 228.", NoDecision (Goto 228))])))
                    )
                )
            )
      )
    patch (118, c) =
      ( 118,
        c & pchoice
          .~ Decisions
            [ ("If you possess an Iron Key, you may open the door by turning to 308.", NoDecision (Goto 308)),
              ("If you wish to ascend the stairs, turn to 170.", NoDecision (Goto 170)),
              ("If you wish to descend the stairs, turn to 228.", NoDecision (Goto 228))
            ]
      )
    -- drop picks when not in use
    patch (54, c) = (54, c & pchoice . _Outcome %~ Simple [LoseItem pickAB04 1, LoseItem pickBB04 1])
    patch (115, c) = (115, c & pchoice . _Outcome %~ Simple [LoseItem pickAB04 1, LoseItem pickBB04 1])
    patch (173, c) = (173, c & pchoice . _Outcome %~ Simple [LoseItem pickAB04 1, LoseItem pickBB04 1])
    patch (224, c) = (224, c & pchoice . _Outcome %~ Simple [LoseItem pickAB04 1, LoseItem pickBB04 1])
    patch (244, c) = (244, c & pchoice . _Outcome %~ Simple [LoseItem pickAB04 1, LoseItem pickBB04 1])
    patch x = x
