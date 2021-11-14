module LoneWolf.Various where

import Data.Maybe
import Data.Ratio
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


getDestinations :: Decision -> [(ChapterId, [String])]
getDestinations d =
  case d of
    Decisions lst -> concatMap (getDestinations . snd) lst
    CanTake i c d' -> append (takeDesc i c) $ getDestinations d'
    Cansell _ _ d' -> append "sell" $ getDestinations d'
    Canbuy _ _ d' -> append "buy" $ getDestinations d'
    Conditional c d' -> append (condDesc c) $ getDestinations d'
    EvadeFight _ cid _ co -> (cid, ["evade"]) : append "fight" (getDestinationsO co)
    AfterCombat d' -> getDestinations d'
    Special s -> case s of
      Cartwheel -> [(169, ["lost"]), (186, ["won"])]
      Portholes -> [(197, [])]
    NoDecision co -> getDestinationsO co
  where
    itemDesc i c = case i of
      Gold -> show c ++ "$"
      Weapon _ -> "w"
      TicketVol2 -> "TKT"
      Meal -> show c ++ "M"
      _ -> show i
    takeDesc i c = '+' : itemDesc i c
    condDesc c = case c of
      Always True -> ""
      Always False -> "never"
      HasItem i cnt -> '?' : itemDesc i cnt
      Not co -> '!' : condDesc co
      COr a b -> condDesc a ++ "||" ++ condDesc b
      CAnd a b -> condDesc a ++ "&&" ++ condDesc b
      HasDiscipline di ->
        '?' : case di of
          SixthSense -> "6th"
          Hunting -> "hunt"
          MindOverMatter -> "MoM"
          Healing -> "heal"
          Tracking -> "trk"
          MindBlast -> "mb"
          Camouflage -> "camo"
          AnimalKinship -> "ak"
          _ -> show di
    append s = map (fmap (s :))
    sdesc s = case s of
      LoseItemKind k | length k == 4 -> Just "LOSEALL"
      LoseItem i c -> Just ('-' : itemDesc i c)
      MustEat _ -> Just "eat"
      HealPlayer h -> Just ('+' : show (getEndurance h) ++ "HP")
      DamagePlayer h -> Just ('-' : show (getEndurance h) ++ "HP")
      FullHeal -> Just "+100%HP"
      HalfHeal -> Just "+50%HP"
      _ -> Just (show s)
    getDestinationsO co = case co of
      Simple s co' -> map (fmap (mapMaybe sdesc s ++)) (getDestinationsO co')
      Fight _ co' -> getDestinationsO co'
      Randomly lst -> concatMap (\(p, o) -> append ("r(" ++ show (numerator p) ++ "/" ++ show (denominator p) ++ ")") (getDestinationsO o)) lst
      Conditionally lst -> concatMap (\(c, o) -> append (condDesc c) $ getDestinationsO o) lst
      Goto c -> [(c, [])]
      GameLost -> []
      GameWon -> []
