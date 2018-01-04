{-# LANGUAGE  ScopedTypeVariables #-}
module Simplifier where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Graph as G
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Ord
import Control.Monad.State.Strict
import Control.Arrow ((&&&))

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

uselessEdges
  :: forall chapterid priority. (Ord chapterid, Ord priority, Show chapterid)
  => (chapterid -> priority)
  -> (chapterid -> M.Map (S.Set chapterid) LinkType)
  -> chapterid
  -> chapterid
  -> S.Set (chapterid, chapterid)
uselessEdges priority childs istart iend
    = foldMap go simplTree
  where
    childs' = foldMap id . M.keysSet . childs
    simplTree = bubbleTree priority childs' istart iend
    go :: G.Tree (chapterid, chapterid) -> S.Set (chapterid, chapterid)
    go (G.Node (start, end) subs) =
       let forbidden = foldMap go subs
       in  prunePathes childs forbidden start end

moreThan :: Int -> [a] -> Bool
moreThan n lst = n < 0 || (not (null lst) && moreThan (n-1) (tail lst))

prunePathes
  :: forall chapterid. (Ord chapterid, Show chapterid)
  => (chapterid -> M.Map (S.Set chapterid) LinkType)
  -> S.Set (chapterid, chapterid)
  -> chapterid
  -> chapterid
  -> S.Set (chapterid, chapterid)
prunePathes childs forbidden start end
    | null allpathes = error ("Bad length for " ++ show start ++ " -> " ++ show end)
    | moreThan 10000 allpathes = forbidden
    | any ( any ((> 1) .  S.size) . fst ) allpathes = forbidden
    | pathTypes == S.singleton Neutral = keep allpathes
    | not (withType Both) && withType Good = keep (filterT Good)
    | not (withType Both) && withType Neutral = keep (take 1 (filterT Neutral))
    | otherwise = forbidden
  where
    filterT :: LinkType -> [ ([S.Set chapterid], LinkType) ]
    filterT t = filter ( (== t) . snd ) allpathes
    withType :: LinkType -> Bool
    withType t = S.member t pathTypes
    allpathes :: [ ([S.Set chapterid], LinkType) ]
    allpathes = map ((S.singleton start :) . map fst &&& foldMap snd) $ findAllPathes childs forbidden start end
    pathTypes :: S.Set LinkType
    pathTypes = S.fromList (map snd allpathes)
    keep :: [([S.Set chapterid], LinkType)] -> S.Set (chapterid, chapterid)
    keep kept = forbidden <> S.difference (edges allpathes) (edges (onlyOneNeutral kept))
    onlyOneNeutral lst = let (neutral, others) = partition ( (== Neutral) . snd ) lst
                         in  take 1 neutral ++ others
    edges :: [([S.Set chapterid], LinkType)] -> S.Set (chapterid, chapterid)
    edges = foldMap (edges' . fst)
    edges' :: [S.Set chapterid] -> S.Set (chapterid, chapterid)
    edges' [] = mempty
    edges' [_] = mempty
    edges' (s1 : s2 : ss) = edges' (s2 : ss) <> S.fromList ( do
      a <- S.toList s1
      b <- S.toList s2
      return (a,b)
      )

findAllPathes
  :: forall chapterid. Ord chapterid
  => (chapterid -> M.Map (S.Set chapterid) LinkType)
  -> S.Set (chapterid, chapterid)
  -> chapterid
  -> chapterid
  -> [ [(S.Set chapterid, LinkType)] ]
findAllPathes childs forbidden s e
  | s == e = [ [] ]
  | otherwise = do
      path@(ks, _) <- M.toList (childs s)
      let  transitions = S.fromList [(s, i) | i <- S.toList ks]
      guard (not (transitions `S.isSubsetOf` forbidden))
      n <- S.toList ks
      nxt <- findAllPathes childs forbidden n e
      return (path : nxt)

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

edgesBetween :: forall chapterid. (Ord chapterid)
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

wfs :: forall chapterid. Ord chapterid
    => (chapterid -> S.Set chapterid)
    -> chapterid
    -> [chapterid]
wfs childs = go mempty . (:[])
  where
    go :: S.Set chapterid -> [chapterid] -> [chapterid]
    go _ [] = []
    go visited (x:xs)
      | x `S.member` visited = go visited xs
      | otherwise = x : go (S.insert x visited) (xs ++ S.toList (childs x))

dfs :: forall chapterid. Ord chapterid
      => (chapterid -> S.Set chapterid)
      -> chapterid
      -> M.Map chapterid Int
dfs childs start = execState (go 0 start) mempty
  where
    go :: Int -> chapterid -> State (M.Map chapterid Int) ()
    go depth x = do
      visited <- get
      case M.lookup x visited of
        Just depth' | depth' >= depth -> return ()
        _ -> do
          modify (M.insert x depth)
          mapM_ (go (depth + 1)) (S.toList (childs x))

graphMap
  :: forall chapterid priority. (Show chapterid, Ord chapterid, Ord priority, Show priority)
  => (chapterid -> priority)
  -> (chapterid -> M.Map (S.Set chapterid) LinkType)
  -> (chapterid -> S.Set chapterid)
  -> chapterid
  -> chapterid
  -> String
graphMap priority childs childsSimple start end = unlines ("digraph \"foo\" {" : edges ++ nodes ++ ["}"])
  where
    allEdges = S.toList (edgesBetween childsSimple start end)
    allNodes = S.fromList (concatMap (\(a,b) -> [a,b]) allEdges)
    nodes = map showNode (S.toList allNodes)
    edges = map showEdge allEdges
    showNode n = show n ++ " [ label=\"" ++ show (n,priority n) ++ "\" URL=\"https://www.projectaon.org/en/xhtml/lw/02fotw/sect" ++ show n ++ ".htm\" ];"
    showEdge (i,c) = show i ++ " -> " ++ show c ++ if S.member (i,c) useless
                                                     then " [color=red];"
                                                     else ";"
    useless = uselessEdges priority childs start end
