{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LoneWolf.XML where

import Control.Lens hiding (children)
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.XML.Gen
import LoneWolf.XML.XML03 (book03gen, extraChapters03)
import LoneWolf.XML.XML04
import LoneWolf.XML.XML05 (book05gen, extraChapters05)
import Text.XML.Expat.Lens.Generic (children, named, parameterized, (./))
import Text.XML.Expat.Tree (UNode, defaultParseOptions, parse)

parseChapter :: Book -> UNode String -> Maybe (ChapterId, Chapter)
parseChapter b =
  case b of
    Book01 -> parseChapterBookGen b book01gen
    Book02 -> error "Book 2 has been manally converted, please do not edit again"
    Book03 -> parseChapterBookGen b book03gen
    Book04 -> parseChapterBookGen b book04gen
    Book05 -> parseChapterBookGen b book05gen

extraChapters :: Book -> [(ChapterId, Chapter)]
extraChapters b =
  case b of
    Book03 -> extraChapters03
    Book04 -> extraChapters04
    Book05 -> extraChapters05
    _ -> []

loadXML :: Book -> FilePath -> IO String
loadXML book xmlpath =
  unlines
    . intersperse "    , "
    . map mkChapterModule
    . (++ extraChapters book)
    . mapMaybe (parseChapter book)
    . toListOf getChapters
    . fst
    . parse defaultParseOptions
    <$> L.readFile xmlpath
  where
    mkChapterModule (cid, Chapter ttl cdesc pch) =
      "  (" ++ show cid ++ ", Chapter " ++ unwords [show ttl, show cdesc, "\n       ", "(", show pch, ")"] ++ ")"
    getChapters = children . traverse . parameterized "id" "title" ./ named "data" ./ parameterized "class" "numbered" ./ named "data" ./ named "section" . parameterized "class" "numbered"

-- todo verify that we are unarmed at chapter 260
book01gen :: ChapterId -> [AC] -> Decision -> Maybe Decision
book01gen _ _ _ = error "now hand made"