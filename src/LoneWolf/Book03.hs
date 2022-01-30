module LoneWolf.Book03 where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.RawBook.Book03 (chapters)
import LoneWolf.Simplify (extractMultiFight)

pchapters :: [(ChapterId, Chapter)]
pchapters = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
  where
    blacklisted = S.fromList []
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    patch (181, _) = (181, Chapter "181" "DD" (Decisions [("attack", NoDecision (Goto 208)), ("go back (skip)", NoDecision (Goto 292))]))
    patch x = x
