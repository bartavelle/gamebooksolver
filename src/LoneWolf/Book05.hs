{-# LANGUAGE FlexibleContexts #-}

module LoneWolf.Book05 where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.RawBook.Book05 (chapters)
import LoneWolf.Simplify (extractMultiFight)

pchapters :: [(ChapterId, Chapter)]
pchapters = mapMaybe (fmap patch . mignore) (extractMultiFight chapters)
  where
    blacklisted = S.fromList []
    mignore (cid, c) = if cid `S.member` blacklisted then Nothing else Just (cid, c)
    patch (207, Chapter n d _) = (207, Chapter n d (NoDecision (Goto 224))) -- useless item
    patch x = x
