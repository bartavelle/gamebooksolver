module LoneWolf.Solve where

import Solver
import LoneWolf.Choices
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Rules

import qualified Data.IntMap.Strict as IM

startConstant :: CharacterConstant
startConstant = CharacterConstant 25 15 [Hunting, WeaponSkill ShortSword]

startVariable :: CharacterVariable
startVariable = CharacterVariable 25 (foldr (uncurry addItem) emptyInventory [(Weapon ShortSword, 1), (Gold, 15), (Meal, 2)])

solveLW :: [(ChapterId, Chapter)] -> CharacterConstant -> CharacterVariable -> Solution NextStep String
solveLW book cconstant cvariable = solve step getScore (NewChapter 1 cvariable Didn'tFight)
  where
    chapters = IM.fromList book
    step :: NextStep -> [(String, Probably NextStep)]
    step (NewChapter cid curvariable m) = case IM.lookup cid chapters of
                                              Nothing -> error ("Unknown chapter: " ++ show cid)
                                              Just (Chapter _ _ d) -> do
                                                  (desc, outcome) <- flattenDecision cconstant curvariable (if m == DidFight then AfterCombat d else d)
                                                  return (unwords desc, update cconstant curvariable outcome)
    step (HasWon c) = [("won", certain (HasWon c))]
    step HasLost = [("lost", certain HasLost)]
    getScore ns = case ns of
                      NewChapter 197 _ _ -> Win 1
                      HasWon _ -> Win 1
                      HasLost -> Lose
                      _ -> Unknown

