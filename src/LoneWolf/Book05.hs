{-# LANGUAGE FlexibleContexts #-}

module LoneWolf.Book05 where

import Control.Lens
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.RawBook.Book05 (chapters)
import LoneWolf.Simplify (extractMultiFight)

pchapters :: [(ChapterId, Chapter)]
pchapters = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
  where
    blacklisted = S.fromList []
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    patch (207, Chapter n d _) = (207, Chapter n d (NoDecision (Goto 224))) -- useless item
    patch (131, c) =
      (131, c & pchoice .~ CanTake Meal 3 (CanTake prismB05 1 (CanTake Laumspur 1 (CanTake (Weapon Dagger) 1 (NoDecision (Goto 58))))))
    patch x = x
