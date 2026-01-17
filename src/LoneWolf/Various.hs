{-# LANGUAGE TupleSections #-}

module LoneWolf.Various where

import Control.Lens (to, (^..), _2)
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Combat (winchance)

sampleFight :: FightDetails
sampleFight = FightDetails "opponent" 22 30 [EnemyMindblast, Undead, MindblastImmune]

survival :: Endurance -> CombatSkill -> [Discipline] -> Weapon -> FightDetails -> Double
survival end cs discs wpn fdetails = fromRational (winchance cst var fdetails)
  where
    cst = CharacterConstant end cs discs Book01
    var = mkCharacter end (addItem (Weapon wpn) 1 emptyInventory)

combatMap :: [Discipline] -> Weapon -> FightDetails -> [(Int, Int, Double)]
combatMap discs wpn details = do
  raw_end <- [20 .. 30]
  raw_skll <- [10 .. 20]
  pure (raw_end, raw_skll, survival (Endurance (fromIntegral raw_end)) (CombatSkill raw_skll) discs wpn details)

getDestinations :: Book -> Decision -> [(ChapterId, [String])]
getDestinations book d =
  case d of
    Decisions lst -> concatMap (getDestinations book . snd) lst
    CanTake i c d' -> append (takeDesc i c) $ getDestinations book d'
    Cansell _ _ d' -> append ("sell" :: String) $ getDestinations book d'
    Canbuy i c d' -> append ("$$" ++ itemDesc i c) $ getDestinations book d'
    Conditional c d' -> append (condDesc c) $ getDestinations book d'
    EvadeFight _ cid _ co -> (cid, ["evade"]) : append "fight" (getDestinationsO co)
    AfterCombat d' -> getDestinations book d'
    Special s -> case s of
      Cartwheel -> [(169, ["lost"]), (186, ["won"])]
      Portholes -> [(197, [])]
      B05S127 -> [(159, []), (93, [])]
      B05S357 -> [(293, []), (207, []), (224, [])]
    NoDecision co -> getDestinationsO co
    RetrieveEquipment d' -> getDestinations book d'
    RemoveItemFrom _ _ d' -> getDestinations book d'
    LoseItemFrom _ _ d' -> getDestinations book d'
  where
    itemDesc i c = case i of
      Gold -> show c ++ "$"
      Weapon w -> show w
      Meal -> show c ++ "M"
      _ -> showItem book i
    takeDesc i c = '+' : itemDesc i c
    condDesc c = case c of
      Always True -> ""
      Always False -> "never"
      HasItem i cnt -> '?' : itemDesc i cnt
      Not co -> '!' : condDesc co
      COr a b -> condDesc a ++ "||" ++ condDesc b
      CAnd a b -> condDesc a ++ "&&" ++ condDesc b
      HasEndurance e -> "endurance > " ++ show e
      HasLevel lvl -> "lvl " ++ show lvl
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
      HasFlag f -> "f:" ++ showFlag book f
    append s = map (fmap (s :))
    sdesc s = case s of
      LoseItemKind k | length k == 4 -> Just "LOSEALL"
      LoseItem i c -> Just ('-' : itemDesc i c)
      MustEat _ -> Just "eat"
      HealPlayer h -> Just ('+' : show (getEndurance h) ++ "HP")
      DamagePlayer h -> Just ('-' : show (getEndurance h) ++ "HP")
      FullHeal -> Just "+100%HP"
      HalfHeal -> Just "+50%HP"
      SetFlag f -> Just ('+' : showFlag book f)
      ClearFlag f -> Just ('-' : showFlag book f)
      _ -> Just (show s)
    getDestinationsO co = case co of
      OneRound _ w e l -> getDestinationsO w ++ getDestinationsO e ++ getDestinationsO l
      Simple s co' -> map (fmap (mapMaybe sdesc s ++)) (getDestinationsO co')
      Fight fd co' ->
        let slow = fd ^.. fightMod . traverse . _Timed . _2 . _OnNotYetWon . to (,["slow fight"])
            fake = fd ^.. fightMod . traverse . _FakeFight . to (,["fake fight loss"])
            ondamage = fd ^.. fightMod . traverse . _OnDamage . to (,["fake fight loss"])
            stopfight = fd ^.. fightMod . traverse . _StopFight . to (,["stopped fight"])
            lose = fd ^.. fightMod . traverse . _OnLose . to (,["lost"])
            evaded = fd ^.. fightMod . traverse . _Evaded . to (,["evaded"]) -- should not happen
         in fake ++ ondamage ++ stopfight ++ lose ++ slow ++ evaded ++ getDestinationsO co'
      Randomly lst -> concatMap (\(p, o) -> append ("r(" ++ show (numerator p) ++ "/" ++ show (denominator p) ++ ")") (getDestinationsO o)) lst
      Conditionally lst -> concatMap (\(c, o) -> append (condDesc c) $ getDestinationsO o) lst
      Goto c -> [(c, [])]
      GameLost -> []
      GameWon -> []

showFlag :: Book -> Flag -> String
showFlag Book05 Special01 = "Jewelled Mace"
showFlag Book05 Knowledge01 = "Scroll"
showFlag Book03 Special01 = "KillKK1"
showFlag Book03 Special02 = "KillKK2"
showFlag Book03 Special03 = "KillKK3"
showFlag Book03 Special04 = "Backnar oil"
showFlag Book03 Knowledge01 = "GotA"
showFlag _ f = show f
