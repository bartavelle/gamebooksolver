{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleSolver where

import Codec.Serialise (Serialise)
import Control.Lens (Bifunctor (bimap))
import Data.Aeson (FromJSON (..), Options (..), SumEncoding (ObjectWithSingleField), ToJSON (..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Bifunctor (first)
import Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Solver (Choice, Probably, Score (..), SolMap, certain, mapProbably, regroup)
import Control.DeepSeq (NFData)

data Solution state description
  = Node
      { _desc :: description,
        _stt :: state,
        _score :: Rational,
        _outcome :: Probably (Solution state description)
      }
  | LeafLost
  | Leaf Rational state
  deriving (Show, Eq, Generic)

instance (NFData state, NFData description) => NFData (Solution state description)

data ChoppedSolution state
  = CNode
      { _cscore :: Rational,
        _coutcome :: Probably (Maybe state)
      }
  | CJump !Rational state
  | CLeafLost
  | CLeaf !Rational
  deriving (Show, Eq, Generic)

choppedScore :: ChoppedSolution state -> Rational
choppedScore cs =
  case cs of
    CNode s _ -> s
    CJump s _ -> s
    CLeafLost -> 0
    CLeaf s -> s

instance (Serialise state) => Serialise (ChoppedSolution state)

choppedSolutionOptions :: Options
choppedSolutionOptions =
  defaultOptions
    { fieldLabelModifier = drop 2,
      constructorTagModifier = drop 1,
      sumEncoding = ObjectWithSingleField
    }

instance (ToJSON state) => ToJSON (ChoppedSolution state) where
  toJSON = genericToJSON choppedSolutionOptions
  toEncoding = genericToEncoding choppedSolutionOptions

instance (FromJSON state) => FromJSON (ChoppedSolution state) where
  parseJSON = genericParseJSON choppedSolutionOptions

chopSolution :: Solution state description -> ChoppedSolution state
chopSolution = \case
  LeafLost -> CLeafLost
  Leaf sc _ -> CLeaf sc
  Node _ _ c o -> case map (first mstate) o of
    [(Just r, p)] | p == 1 -> CJump c r
    ms -> CNode c ms
  where
    mstate = \case
      Leaf _ stt -> Just stt
      LeafLost -> Nothing
      Node _ stt' _ _ -> Just stt'

toSolMap :: forall state description. Ord state => state -> Solution state description -> SolMap state
toSolMap loststate = go 1 M.empty
  where
    go :: Rational -> SolMap state -> Solution state description -> SolMap state
    go curp mp n = case n of
      LeafLost -> insertTuple loststate (curp, mempty) mp
      Leaf r stt -> insertTuple stt (r * curp, mempty) mp
      Node _ stt sc outcome ->
        let outcomemap = foldl' (\cmp (sol, p) -> go (curp * p) cmp sol) mp outcome
         in insertTuple stt (sc * curp, map (bimap extractState (* curp)) outcome) outcomemap
    insertTuple :: state -> (Rational, Probably state) -> SolMap state -> SolMap state
    insertTuple = M.insertWith combine
    combine (np, nps) (op, ops) = (np + op, regroup (nps <> ops))
    extractState :: Solution state description -> state
    extractState s = case s of
      LeafLost -> loststate
      Leaf _ stt -> stt
      Node _ stt _ _ -> stt
{-# INLINEABLE toSolMap #-}

lmapSol :: (s1 -> s2) -> Solution s1 desc -> Solution s2 desc
lmapSol f s =
  case s of
    LeafLost -> LeafLost
    Leaf sc st -> Leaf sc (f st)
    Node d st sc o -> Node d (f st) sc (mapProbably (lmapSol f) o)

getSolScore :: Solution state description -> Rational
getSolScore s = case s of
  LeafLost -> 0
  Leaf x _ -> x
  Node _ _ x _ -> x

winStates :: Ord state => Solution state description -> Probably state
winStates s = case s of
  LeafLost -> mempty
  Leaf _ st -> certain st
  Node _ _ _ ps -> regroup $ concatMap (\(o, p) -> fmap (* p) <$> winStates o) ps

solve ::
  Memo.Memo state ->
  (state -> [Choice state description]) -> -- the choice function
  (state -> Score) ->
  state ->
  Solution state description
solve memo getChoice score = go
  where
    go = memo solve'
    solve' stt =
      case score stt of
        Known x -> Leaf x stt
        Unknown ->
          if null choices
            then LeafLost
            else maximumBy (comparing getSolScore) scored
      where
        choices = getChoice stt
        scored = fmap scoreTree choices
        scoreTree (cdesc, pstates) =
          let ptrees = fmap (first go) pstates
           in Node cdesc stt (sum (fmap (\(o, p) -> p * getSolScore o) ptrees)) ptrees
{-# INLINEABLE solve #-}
