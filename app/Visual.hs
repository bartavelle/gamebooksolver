module Main (main) where

import Control.Lens (has, preview)
import Control.Monad (forM_, guard)
import Data.Data.Lens (biplate)
import qualified Data.IntMap.Strict as IM
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified LoneWolf.Book01
import qualified LoneWolf.Book02
import qualified LoneWolf.Book03
import qualified LoneWolf.Book04
import qualified LoneWolf.Book05
import LoneWolf.Chapter
import LoneWolf.Character (Book (..))
import LoneWolf.Various (getDestinations)

pchapters :: Book -> [(ChapterId, Chapter)]
pchapters book = case book of
  Book01 -> LoneWolf.Book01.pchapters
  Book02 -> LoneWolf.Book02.pchapters
  Book03 -> LoneWolf.Book03.pchapters
  Book04 -> LoneWolf.Book04.pchapters
  Book05 -> LoneWolf.Book05.pchapters

orderChapters :: Book -> IM.IntMap Chapter -> M.Map ChapterId Int
orderChapters bid book = M.fromList $ zip (reverse orderedlist) [1 ..]
  where
    orderedlist = go startedges [] edgemap
    startedges = filter (`M.notMember` edgemap) (IM.keys book)
    edgemap = M.fromListWith (++) $ do
      (cid, Chapter _ _ d) <- IM.toList book
      (dst, _) <- getDestinations bid d
      guard (dst /= cid)
      pure (dst, [cid])
    go [] out _ = out
    go (x : xs) out emap = go (xs ++ newedges) (x : out) emap'
      where
        pruned = fmap (filter (/= x)) emap
        (newedgesmap, emap') = M.partition null pruned
        newedges = M.keys newedgesmap

main :: IO ()
main = do
  let book = Book03
      urlpart = case book of
        Book01 -> "01fftd"
        Book02 -> "02fotw"
        Book03 -> "03tcok"
        Book04 -> "04tcod"
        Book05 -> "05sots"
  putStrLn "digraph G {"
  putStrLn "rankdir=LR;"
  let chapters = pchapters book
  let rank = orderChapters book (IM.fromList chapters)
  forM_ chapters $ \(cnumber, Chapter ctitle _ cdesc) -> do
    let mhasCombat = has (outcomePlate . biplate . _Fight) cdesc || has (biplate . _EvadeFight) cdesc
        eating = preview (outcomePlate . biplate . _MustEat) cdesc
        style
          | eating == Just NoHunt = "color=yellow style=filled"
          | eating == Just Hunt = "color=yellow"
          | mhasCombat = "color=red"
          | has (biplate . _GameLost) cdesc = "fillcolor=black style=filled fontcolor=white"
          | has _Special cdesc = "shape=square color=blue"
          | otherwise = ""
        label
          | has (biplate . _GameLost) cdesc = ctitle ++ " :("
          | otherwise = ctitle
        rnk = maybe "??" show (M.lookup cnumber rank)
    putStrLn
      ( show (show cnumber) ++ " ["
          ++ style
          ++ " label="
          ++ show (label ++ " " ++ rnk)
          ++ " URL=\"https://www.projectaon.org/en/xhtml/lw/"
          ++ urlpart
          ++ "/sect"
          ++ ctitle
          ++ ".htm\"];"
      )
    forM_ (nub $ getDestinations book cdesc) $ \(dest, comment) ->
      putStrLn (show (show cnumber) ++ " -> " ++ show (show dest) ++ " [label=" ++ show (unwords (nub comment)) ++ " color=black]")
  putStrLn "}"
