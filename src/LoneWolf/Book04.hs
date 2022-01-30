module LoneWolf.Book04 where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.RawBook.Book04 (chapters)
import LoneWolf.Simplify (extractMultiFight)

pchapters :: [(ChapterId, Chapter)]
pchapters = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
  where
    blacklisted = S.fromList []
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    patch x = x
