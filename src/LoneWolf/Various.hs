module LoneWolf.Various where

import Control.Monad
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Combat

sampleFight :: FightDetails
sampleFight = FightDetails "opponent" 22 30 [EnemyMindblast, Undead, MindblastImmune]

survival :: Endurance -> CombatSkill -> [Discipline] -> Weapon -> FightDetails -> Double
survival end cs discs wpn fdetails = fromRational proba
  where
    cst = CharacterConstant end cs discs
    var = mkCharacter end (addItem (Weapon wpn) 1 emptyInventory)
    proba = sum $ do
      (r, p) <- fight cst var fdetails
      guard (r > 0)
      pure p

combatMap :: [Discipline] -> Weapon -> FightDetails -> [(Int, Int, Double)]
combatMap discs wpn details = do
  raw_end <- [20 .. 30]
  raw_skll <- [10 .. 20]
  pure (raw_end, raw_skll, survival (Endurance raw_end) (CombatSkill raw_skll) discs wpn details)