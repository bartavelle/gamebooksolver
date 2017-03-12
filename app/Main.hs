module Main where

import LoneWolf.XML

main :: IO ()
main = do
   s <- loadXML "xml/02fotw.xml"
   putStrLn $ unlines [ "module LoneWolf.Book02 where"
                      , ""
                      , "import LoneWolf.Chapter"
                      , "import LoneWolf.Character"
                      , "import Data.Ratio"
                      , ""
                      , "chapters :: [(ChapterId, Chapter)]"
                      , "chapters = ["
                      , s
                      , "    ]"
                      ]

