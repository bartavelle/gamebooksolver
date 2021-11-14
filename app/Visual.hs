module Main (main) where

import Control.Lens
import Control.Monad
import Data.Data.Lens
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import LoneWolf.Book02
import LoneWolf.Chapter
import LoneWolf.Various (getDestinations)

orderChapters :: IM.IntMap Chapter -> M.Map ChapterId Int
orderChapters book = M.fromList $ zip (reverse orderedlist) [1 ..]
  where
    orderedlist = go startedges [] edgemap
    startedges = filter (`M.notMember` edgemap) (IM.keys book)
    edgemap = M.fromListWith (++) $ do
      (cid, Chapter _ _ d) <- IM.toList book
      (dst, _) <- getDestinations d
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
  putStrLn "digraph G {"
  putStrLn "rankdir=LR;"
  let rank = orderChapters (IM.fromList pchapters)
  forM_ pchapters $ \(cnumber, Chapter ctitle _ cdesc) -> do
    let hasCombat = has (outcomePlate . biplate . _Fight) cdesc || has (biplate . _EvadeFight) cdesc
        eating = preview (outcomePlate . biplate . _MustEat) cdesc
        style
          | eating == Just NoHunt = "color=yellow style=filled"
          | eating == Just Hunt = "color=yellow"
          | hasCombat = "color=red"
          | has (biplate . _GameLost) cdesc = "fillcolor=black style=filled fontcolor=white"
          | has _Special cdesc = "shape=square color=blue"
          | otherwise = ""
        label
          | has (biplate . _GameLost) cdesc = ctitle ++ " :("
          | otherwise = ctitle
        rnk = case M.lookup cnumber rank of
          Nothing -> "??"
          Just r -> show r
    putStrLn (show ctitle ++ " [" ++ style ++ " label=" ++ show (label ++ " " ++ rnk) ++ " URL=\"https://www.projectaon.org/en/xhtml/lw/02fotw/sect" ++ ctitle ++ ".htm\"];")
    forM_ (nub $ getDestinations cdesc) $ \(dest, comment) ->
      putStrLn (show ctitle ++ " -> " ++ show (show dest) ++ " [label=" ++ show (unwords (nub comment)) ++ " color=black]")
  putStrLn "}"
