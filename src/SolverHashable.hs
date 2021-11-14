{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SolverHashable where

{- a solver that works under the assumtion that the state is a Word64 -}

import Control.Monad (forM_)
import qualified Control.Monad.ST as ST
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.Class as STC
import qualified Data.HashTable.ST.Basic as ST
import Data.Hashable (Hashable)
import Data.List
import Data.Ord (comparing)
import SimpleSolver
import Solver (Choice, Proba, Probably, Score (..))

data SearchNode state description
  = OK !(Solution state description)
  | SS ![Choice state description]

data UL a = UNull | UCons a (UL a)

data UP a b = UP !a !b

type UProbably a = UL (UP a Proba)

type UChoice state description = UP description (Probably state)

solveHST ::
  forall state description.
  (Eq state, Hashable state, Show state, Show description) =>
  (state -> [Choice state description]) ->
  (state -> Score) ->
  state ->
  Solution state description
solveHST getChoice score start = ST.runST $ do
  visited <- ST.newSized 10000000
  let go [] = pure ()
      go (stt : q') = do
        mknown <- ST.lookup visited stt
        case (mknown, score stt) of
          (Just (OK _), _) -> do
            go q'
          (_, Known x) -> do
            ST.insert visited stt (OK (Leaf x stt))
            go q'
          (Nothing, Unknown) -> do
            let choices = getChoice stt
                allstates = concatMap (\(_, ps) -> map fst ps) choices
            unknownstates <- ST.new
            forM_ allstates $ \stt' -> do
              c <- ST.lookup unknownstates stt'
              case c of
                Nothing -> do
                  d <- ST.lookup visited stt'
                  case d of
                    Nothing -> ST.insert unknownstates stt' ()
                    Just _ -> pure ()
                Just _ -> pure ()
              ST.insert visited stt (SS choices)
            lst <- map fst <$> STC.toList unknownstates
            go (lst ++ (stt : q'))
          (Just (SS chs), _) -> do
            let extractSol (d, ps) = do
                  let getSttSol (stt', proba) = do
                        lk <- ST.lookup visited stt'
                        case lk of
                          Nothing -> error ("unvisited state " ++ show stt ++ " -> " ++ show stt')
                          Just (SS _) -> error ("loop on state " ++ show stt)
                          Just (OK s) -> pure (s, proba)
                  psols <- mapM getSttSol ps
                  let scr = sum (map (\(csol, proba) -> getSolScore csol * proba) psols)
                  pure (Node d stt scr psols)
            solved <- mapM extractSol chs
            let sol =
                  if null solved
                    then LeafLost
                    else maximumBy (comparing _score) solved
            ST.insert visited stt (OK sol)
            go q'

  go [start]
  s <- ST.lookup visited start
  case s of
    Nothing -> error "??"
    Just (SS _) -> error ":("
    Just (OK r) -> pure r
{-# INLINEABLE solveHST #-}

solveH ::
  forall state description.
  (Eq state, Hashable state, Show state) =>
  (state -> [Choice state description]) ->
  (state -> Score) ->
  state ->
  Solution state description
solveH getChoice score start = case HM.lookup start (go HM.empty [start]) of
  Nothing -> error "??"
  Just (SS _) -> error ":("
  Just (OK s) -> s
  where
    go :: HM.HashMap state (SearchNode state description) -> [state] -> HM.HashMap state (SearchNode state description)
    go visited q =
      case q of
        [] -> visited
        stt : q' ->
          case (HM.lookup stt visited, score stt) of
            (Just (OK _), _) ->
              -- already resolved
              go visited q'
            (_, Known x) -> go (HM.insert stt (OK (Leaf x stt)) visited) q'
            (Nothing, _) ->
              -- new node, nothing known about it
              let choices = getChoice stt
                  newstates = HM.fromList $ do
                    (_, ps) <- choices
                    (s, _) <- ps
                    pure (s, ())
                  unknownstates = HM.difference newstates visited
               in go (HM.insert stt (SS choices) visited) (HM.keys unknownstates ++ q)
            (Just (SS chs), _) ->
              -- all the child nodes must be solved now
              let solved = map extractSol chs
                  extractSol (d, ps) =
                    let psols = map getSttSol ps
                        getSttSol (stt', proba) =
                          case HM.lookup stt' visited of
                            Nothing -> error "unvisited state"
                            Just (SS _) -> error ("loop on state " ++ show stt)
                            Just (OK s) -> (s, proba)
                        scr = sum (map (\(csol, proba) -> getSolScore csol * proba) psols)
                     in Node d stt scr psols
                  sol =
                    if null solved
                      then LeafLost
                      else maximumBy (comparing _score) solved
               in go (HM.insert stt (OK sol) visited) q'
{-# INLINEABLE solveH #-}