{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module SolverHashable where

{- a solver that works under the assumtion that the state is a Word64 -}

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List
import Data.Ord (comparing)
import Data.Word (Word64)
import SimpleSolver
import Solver (Choice, Proba, Probably, Score (..))

data Word128 = Word128 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Generic)

instance Hashable Word128

data SearchNode state description
  = OK !(Solution state description)
  | SS ![Choice state description]

data UL a = UNull | UCons a (UL a)

data UP a b = UP !a !b

type UProbably a = UL (UP a Proba)

type UChoice state description = UP description (Probably state)

solveH ::
  forall state description.
  (Eq state, Hashable state) =>
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
                  newstates = HS.fromList $ do
                    (_, ps) <- choices
                    (s, _) <- ps
                    pure s
               in go (HM.insert stt (SS choices) visited) (HS.toList newstates ++ q)
            (Just (SS chs), _) ->
              -- all the child nodes must be solved now
              let solved = map extractSol chs
                  extractSol (d, ps) =
                    let psols = map getSttSol ps
                        getSttSol (stt', proba) =
                          case HM.lookup stt' visited of
                            Nothing -> error "unvisited state"
                            Just (SS _) -> error "loop"
                            Just (OK s) -> (s, proba)
                        scr = sum (map (\(csol, proba) -> getSolScore csol * proba) psols)
                     in Node d stt scr psols
                  sol =
                    if null solved
                      then LeafLost
                      else maximumBy (comparing _score) solved
               in go (HM.insert stt (OK sol) visited) q'

solveW128 ::
  forall description.
  (Word128 -> [Choice Word128 description]) ->
  (Word128 -> Score) ->
  Word128 ->
  Solution Word128 description
solveW128 getChoice score start = case HM.lookup start (go HM.empty [start]) of
  Nothing -> error "??"
  Just (SS _) -> error ":("
  Just (OK s) -> s
  where
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
                  newstates = HS.fromList $ do
                    (_, ps) <- choices
                    (s, _) <- ps
                    pure s
               in go (HM.insert stt (SS choices) visited) (HS.toList newstates ++ q)
            (Just (SS chs), _) ->
              -- all the child nodes must be solved now
              let solved = map extractSol chs
                  extractSol (d, ps) =
                    let psols = map getSttSol ps
                        getSttSol (stt', proba) =
                          case HM.lookup stt' visited of
                            Nothing -> error "unvisited state"
                            Just (SS _) -> error "loop"
                            Just (OK s) -> (s, proba)
                        scr = sum (map (\(csol, proba) -> getSolScore csol * proba) psols)
                     in Node d stt scr psols
                  sol =
                    if null solved
                      then LeafLost
                      else maximumBy (comparing _score) solved
               in go (HM.insert stt (OK sol) visited) q'
