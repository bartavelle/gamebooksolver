module LoneWolf.Solve where

import Control.Monad (guard)
import Data.Bits (Bits (clearBit, setBit, testBit))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import Data.Ord (comparing)
import Data.Word (Word16, Word64)
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Choices (flattenDecision)
import LoneWolf.Rules
  ( HadCombat (DidFight, Didn'tFight),
    NextStep (..),
    update,
  )
import LoneWolf.Various (getDestinations)
import qualified SimpleSolver as S
import Solver (Probably, Score (..), Solution, certain, solve)
import SolverHashable (solveHST)

startConstant :: CharacterConstant
startConstant = CharacterConstant 25 15 [Hunting, WeaponSkill ShortSword, MindBlast, SixthSense, MindShield]

startVariable :: CharacterVariable
startVariable = mkCharacter 25 (inventoryFromList [(Weapon ShortSword, 1), (Gold, 15), (Meal, 2), (SealHammerdalVol2, 1)])

memoState :: Memo.Memo NextStep
memoState = Memo.wrap fromWord64 toWord64 (Memo.pair Memo.bits Memo.bits)
{-# INLINE memoState #-}

toWord64 :: NextStep -> (Word16, Word64)
toWord64 s = case s of
  HasLost cid -> (fromIntegral cid, 0)
  HasWon cvariable -> toWord64 (NewChapter 0 cvariable Didn'tFight)
  NewChapter cid (CharacterVariable cvalue) hadfight ->
    let cidb16 = fromIntegral cid
        cid16 =
          if hadfight == Didn'tFight
            then cidb16
            else setBit cidb16 15
     in (cid16, cvalue)
{-# INLINE toWord64 #-}

fromWord64 :: (Word16, Word64) -> NextStep
fromWord64 (cid, 0) = HasLost (fromIntegral cid)
fromWord64 (cid16, cvalue) =
  let cid = fromIntegral (clearBit cid16 15)
      hadfight =
        if testBit cid16 15
          then DidFight
          else Didn'tFight
      cvariable = CharacterVariable cvalue
   in if cid == 0
        then HasWon cvariable
        else NewChapter cid cvariable hadfight
{-# INLINE fromWord64 #-}

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

step :: M.Map ChapterId Int -> IM.IntMap Chapter -> CharacterConstant -> NextStep -> [(String, Probably NextStep)]
step order chapters cconstant (NewChapter cid curvariable m) =
  case IM.lookup cid chapters of
    Nothing -> error ("Unknown chapter: " ++ show cid)
    Just (Chapter _ _ d) -> map snd $
      sortBy (comparing fst) $ do
        (cdesc, outcome) <- flattenDecision cconstant curvariable (if m == DidFight then AfterCombat d else d)
        let res = update cconstant curvariable cid outcome
            score = stepprio res
        return (score, (unwords cdesc, res))
  where
    stepprio :: Probably NextStep -> Int
    stepprio = maximum . map (stepi . fst)
    stepi ns = case ns of
      HasLost _ -> 0
      HasWon _ -> 0
      NewChapter xid _ _ -> M.findWithDefault 0 xid order
step _ _ _ (HasWon c) = [("won", certain (HasWon c))]
step _ _ _ (HasLost cid) = [("lost", certain (HasLost cid))]
{-# INLINE step #-}

getScore :: IS.IntSet -> NextStep -> Score
getScore target ns =
  case ns of
    NewChapter x _ _ -> if x `IS.member` target then Known 1 else Unknown
    HasWon _ -> Known 1
    HasLost _ -> Known 0
{-# INLINE getScore #-}

solveLW :: [ChapterId] -> [(ChapterId, Chapter)] -> CharacterConstant -> CharacterVariable -> Solution NextStep String
solveLW target book cconstant cvariable = solve memoState 1 (step order chapters cconstant) (getScore starget) (NewChapter 1 cvariable Didn'tFight)
  where
    order = orderChapters chapters
    chapters = IM.fromList book
    starget = IS.fromList target

solveLWs :: [ChapterId] -> [(ChapterId, Chapter)] -> CharacterConstant -> CharacterVariable -> S.Solution NextStep String
solveLWs target book cconstant cvariable = solveHST (step order chapters cconstant) (getScore starget) (NewChapter 1 cvariable Didn'tFight)
  where
    order = orderChapters chapters
    chapters = IM.fromList book
    starget = IS.fromList target
