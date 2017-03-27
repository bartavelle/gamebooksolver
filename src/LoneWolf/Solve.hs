module LoneWolf.Solve where

import Solver
import qualified SimpleSolver as S
import LoneWolf.Choices
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Rules
import qualified Data.MemoCombinators as Memo
import qualified Data.IntSet as IS
import Data.Word
import Data.Bits

import qualified Data.IntMap.Strict as IM

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
                        cid16 = if hadfight == Didn'tFight
                                    then cidb16
                                    else setBit cidb16 15
                    in  (cid16, cvalue)
{-# INLINE toWord64 #-}

fromWord64 :: (Word16, Word64) -> NextStep
fromWord64 (cid, 0) = HasLost (fromIntegral cid)
fromWord64 (cid16, cvalue) =
    let cid = fromIntegral (clearBit cid16 15)
        hadfight = if testBit cid16 15
                       then DidFight
                       else Didn'tFight
        cvariable = CharacterVariable cvalue
    in  if cid == 0
            then HasWon cvariable
            else NewChapter cid cvariable hadfight
{-# INLINE fromWord64 #-}


step :: IM.IntMap Chapter -> CharacterConstant -> NextStep -> [(String, Probably NextStep)]
step chapters cconstant (NewChapter cid curvariable m) =
        case IM.lookup cid chapters of
            Nothing -> error ("Unknown chapter: " ++ show cid)
            Just (Chapter _ _ d) -> do
                (cdesc, outcome) <- flattenDecision cconstant curvariable (if m == DidFight then AfterCombat d else d)
                return (unwords cdesc, update cconstant curvariable cid outcome)
step _ _ (HasWon c) = [("won", certain (HasWon c))]
step _ _ (HasLost cid) = [("lost", certain (HasLost cid))]
{-# INLINE step #-}

getScore :: IS.IntSet -> NextStep -> Score
getScore target ns =
    case ns of
      NewChapter x _ _ -> if x `IS.member` target then Known 1 else Unknown
      HasWon _ -> Known 1
      HasLost _ -> Known 0
{-# INLINE getScore #-}

solveLW :: [ChapterId] -> [(ChapterId, Chapter)] -> CharacterConstant -> CharacterVariable -> Solution NextStep String
solveLW target book cconstant cvariable = solve memoState 1 (step chapters cconstant) (getScore starget) (NewChapter 1 cvariable Didn'tFight)
  where
    chapters = IM.fromList book
    starget = IS.fromList target

solveLWs :: [ChapterId] -> [(ChapterId, Chapter)] -> CharacterConstant -> CharacterVariable -> S.Solution NextStep String
solveLWs target book cconstant cvariable = S.solve memoState (step chapters cconstant) (getScore starget) (NewChapter 1 cvariable Didn'tFight)
  where
    chapters = IM.fromList book
    starget = IS.fromList target

