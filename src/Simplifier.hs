{-# LANGUAGE  ScopedTypeVariables #-}
module Simplifier where

import qualified Data.Set as S
import qualified Data.Graph as G
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Ord
import Control.Monad.State.Strict

data LinkType = Neutral
              | Good
              | Bad
              | Both
              deriving (Show, Eq, Ord)

instance Monoid LinkType where
    mempty = Neutral
    Neutral `mappend` b = b
    a `mappend` Neutral = a
    a `mappend` b = if a == b
                        then a
                        else Both

bubbleTree :: forall chapterid priority. (Ord chapterid, Ord priority)
           => (chapterid -> priority)
           -> (chapterid -> S.Set chapterid)
           -> chapterid
           -> chapterid
           -> Maybe (G.Tree (chapterid, chapterid))
bubbleTree priority childs start end =
    case unfoldr (\k -> step priority childs k end) start of
      [] -> Nothing
      [x] -> x
      lst -> Just (G.Node (start, end) (catMaybes lst))

step :: forall chapterid priority. (Ord chapterid, Ord priority)
     => (chapterid -> priority)
     -> (chapterid -> S.Set chapterid)
     -> chapterid
     -> chapterid
     -> Maybe (Maybe (G.Tree (chapterid, chapterid)), chapterid)
step priority childs start end
  | start == end = Nothing
  | otherwise =
      let schilds = childs start
      in  case S.toList schilds of
          [] -> Nothing
          [x] -> Just (Nothing, x)
          lst -> do
            x <- findClosestDominated priority childs schilds
            let content = mapMaybe (\k -> bubbleTree priority childs k x) lst
            return (Just (G.Node (start, x) content), x)

findClosestDominated :: (Ord chapterid, Ord priority)
             => (chapterid -> priority)
             -> (chapterid -> S.Set chapterid)
             -> S.Set chapterid
             -> Maybe chapterid
findClosestDominated prio childs = go
  where
    go st
      | null st = Nothing
      | otherwise =
          let best = minimumBy (comparing prio) st
              st' = S.delete best st
          in  if null st'
                then Just best
                else go (st' <> childs best)

edgesBetween :: forall chapterid. (Show chapterid, Ord chapterid)
             => (chapterid -> S.Set chapterid)
             -> chapterid
             -> chapterid
             -> S.Set (chapterid, chapterid)
edgesBetween childs start end = evalState (go start) mempty
  where
    go :: chapterid -> State (S.Set chapterid) (S.Set (chapterid, chapterid))
    go i | i == end = return mempty
    go i = do
      visited <- get
      if i `S.member` visited
        then return mempty
        else do
          modify (S.insert i)
          let cs = S.toList (childs i)
              this = S.fromList [(i,c) | c <- cs ]
          nxt <- mconcat <$> mapM go cs
          return (this <> nxt)

graphMap :: forall chapterid. (Show chapterid, Ord chapterid)
         => (chapterid -> S.Set chapterid)
         -> chapterid
         -> chapterid
         -> String
graphMap childs start end = unlines ("digraph \"foo\" {" : edges ++ ["}"])
  where
    edges = [ show i ++ " -> " ++ show c | (i,c) <- S.toList (edgesBetween childs start end) ]
