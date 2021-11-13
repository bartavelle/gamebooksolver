-- | This module can be used to generate the LoneWolf.Book02 module.
--
-- In order to work, you must download the XML description from the Aon
-- project.
module Main where

import LoneWolf.XML

main :: IO ()
main = do
  s <- loadXML "xml/02fotw.xml"
  putStrLn $
    unlines
      [ "module LoneWolf.Book02 where",
        "",
        "import LoneWolf.Chapter",
        "import LoneWolf.Character",
        "import Data.Ratio",
        "",
        "chapters :: [(ChapterId, Chapter)]",
        "chapters = [",
        s,
        "    ]"
      ]
