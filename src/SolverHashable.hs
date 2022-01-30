{-# LANGUAGE ScopedTypeVariables #-}

module SolverHashable where

{- a solver that works under the assumtion that the state is a Word64 -}

import Control.Monad (forM_)
import qualified Control.Monad.ST as ST
import qualified Data.HashTable.Class as STC
import qualified Data.HashTable.ST.Basic as ST
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import SimpleSolver
import Solver (Choice, Proba, Probably, Score (..))
import Data.Foldable (maximumBy)

data SearchNode state description
  = OK !(Solution state description)
  | SS ![Choice state description]

data UL a = UNull | UCons a (UL a)

data UP a b = UP !a !b

type UProbably a = UL (UP a Proba)

type UChoice state description = UP description (Probably state)

solveHST ::
  forall state description.
  (Eq state, Hashable state, Show state) =>
  (state -> [Choice state description]) ->
  (state -> Score) ->
  state ->
  (Solution state description, [(state, Solution state description)])
solveHST getChoice score start = ST.runST $ do
  visited <- ST.newSized 50000000
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
  let filterSolution (_, SS _) = Nothing
      filterSolution (k, OK sl) = Just (k, sl)
  solmap <- mapMaybe filterSolution <$> STC.toList visited
  case s of
    Nothing -> error "??"
    Just (SS _) -> error ":("
    Just (OK r) -> pure (r, solmap)
{-# INLINEABLE solveHST #-}
