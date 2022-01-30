module LoneWolf.Solve (solveLWs, orderChapters, step) where

import Control.Lens ((^.))
import Control.Monad (guard)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Choices (flattenDecision)
import LoneWolf.Rules
  ( NextStep (..),
    getMaxHp,
    update,
  )
import LoneWolf.Various (getDestinations)
import qualified SimpleSolver as S
import Solver (Probably, Score (..), certain)
import SolverHashable (solveHST)

orderChapters :: Book -> IM.IntMap Chapter -> M.Map ChapterId Int
orderChapters bkid book = M.fromList $ zip (reverse orderedlist) [1 ..]
  where
    orderedlist = go startedges [] edgemap
    startedges = filter (`M.notMember` edgemap) (IM.keys book)
    edgemap = M.fromListWith (++) $ do
      (cid, Chapter _ _ d) <- IM.toList book
      (dst, _) <- getDestinations bkid d
      guard (dst /= cid)
      pure (dst, [cid])
    go [] out _ = out
    go (x : xs) out emap = go (xs ++ newedges) (x : out) emap'
      where
        pruned = fmap (filter (/= x)) emap
        (newedgesmap, emap') = M.partition null pruned
        newedges = M.keys newedgesmap

step :: M.Map ChapterId Int -> IM.IntMap Chapter -> CharacterConstant -> NextStep -> [(String, Probably NextStep)]
step order chapters cconstant xx@(NewChapter cid curvariable)
  | curvariable ^. curendurance > getMaxHp cconstant curvariable = error (show xx)
  | otherwise =
    case IM.lookup cid chapters of
      Nothing -> error ("Unknown chapter: " ++ show cid)
      Just (Chapter _ _ d) -> map snd $
        sortBy (comparing fst) $ do
          (cdesc, outcome) <- flattenDecision cconstant curvariable (if curvariable ^. flag HadCombat then AfterCombat d else d)
          let res = update cconstant curvariable cid outcome
              score = stepprio res
          return (score, (unwords cdesc, res))
  where
    stepprio :: Probably NextStep -> Int
    stepprio = maximum . map (stepi . fst)
    stepi ns = case ns of
      HasLost _ -> 0
      HasWon _ -> 0
      NewChapter xid _ -> M.findWithDefault 0 xid order
step _ _ _ (HasWon c) = [("won", certain (HasWon c))]
step _ _ _ (HasLost cid) = [("lost", certain (HasLost cid))]
{-# INLINE step #-}

getScore :: (S.Set Item -> S.Set Flag -> Rational) -> IS.IntSet -> NextStep -> Score
getScore scorer target ns =
  case ns of
    NewChapter x cvar ->
      if x `IS.member` target
        then Known (scorefor cvar)
        else Unknown
    HasWon cvar -> Known (scorefor cvar)
    HasLost _ -> Known 0
  where
    scorefor cvar =
      let itms = S.fromList (map fst (items (cvar ^. equipment)))
          flgs = S.fromList (allFlags cvar)
       in scorer itms flgs
{-# INLINE getScore #-}

solveLWs ::
  -- | scoring function
  (S.Set Item -> S.Set Flag -> Rational) ->
  -- | book to solve
  Book ->
  -- | target chapters
  [ChapterId] ->
  -- | book content
  [(ChapterId, Chapter)] ->
  CharacterConstant ->
  CharacterVariable ->
  (S.Solution NextStep String, [(NextStep, S.Solution NextStep String)])
solveLWs scorer bkid target book cconstant cvariable = solveHST (step order chapters cconstant) (getScore scorer starget) (NewChapter 1 cvariable)
  where
    order = orderChapters bkid chapters
    chapters = IM.fromList book
    starget = IS.fromList target
