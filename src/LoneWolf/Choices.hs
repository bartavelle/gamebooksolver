{-# LANGUAGE TupleSections #-}
module LoneWolf.Choices where

import Control.Lens
import Data.List

import LoneWolf.Character
import LoneWolf.Chapter
import LoneWolf.Rules (check, updateSimple)
import qualified LoneWolf.Cartwheel

importantItem :: Item -> CharacterConstant -> CharacterVariable -> Bool
importantItem i cconstant cvariable =
    case i of
        Weapon Sommerswerd -> True
        Weapon MagicSpear -> True
        Weapon w -> case usedWeapon cconstant cvariable of
                        WithSkill _ -> False
                        NoWeapon -> True
                        WithoutSkill _ -> WeaponSkill w `elem` cconstant ^. discipline
        _ -> True

data CanTake = SpaceAvailable
             | Mustdrop [Item]
             | Nope
             deriving (Show, Eq)

canTake :: Item -> CharacterConstant -> CharacterVariable -> CanTake
canTake item cconstant cvariable =
    case itemSlot item of
        BackpackSlot | itemsInBackpack > 8 -> error "canTake: too many items in backpack"
                     | itemsInBackpack == 8 -> Mustdrop (map fst backpackItems)
                     | otherwise -> SpaceAvailable
        WeaponSlot -> case weapons of
                          [] -> SpaceAvailable
                          [_] -> SpaceAvailable
                          lst -> Mustdrop $ map Weapon $ case usedWeapon cconstant cvariable of
                                                             WithSkill w -> filter (/= w) lst
                                                             WithoutSkill Sommerswerd -> filter (/= Sommerswerd) lst
                                                             _ -> lst
        PouchSlot -> SpaceAvailable
        SpecialSlot -> if hasItem item inventory
                           then Nope
                           else SpaceAvailable
  where
   inventory = cvariable ^. equipment
   weapons = getWeapons inventory
   backpackItems = filter ( (== BackpackSlot) . itemSlot . fst ) (items inventory)
   itemsInBackpack = sum (map snd backpackItems)

flattenDecision :: CharacterConstant -> CharacterVariable -> Decision -> [([String], ChapterOutcome)]
flattenDecision cconstant cvariable d =
  let inventory = cvariable ^. equipment
      takeItem item  = withEffects [GainItem item 1]
      withEffects effects nxt = do
        let nvariable = foldl' (updateSimple cconstant) cvariable effects
        (dsc, o) <- flattenDecision cconstant nvariable nxt
        return ( show effects : dsc, Simple effects o)
  in  case d of
        AfterCombat nxt
          | hasItem HealingPotion inventory && missingHp >= 4
              -> nopotion ++ withEffects [HealPlayer 4, LoseItem HealingPotion 1] nxt
          | hasItem PotentPotion inventory && missingHp >= 5
              -> nopotion ++ withEffects [HealPlayer 5, LoseItem PotentPotion 1] nxt
          | otherwise -> nopotion
          where
            nopotion = flattenDecision cconstant cvariable nxt
            missingHp = cconstant ^. maxendurance - cvariable ^. curendurance
        Conditional bc d' -> if check cconstant cvariable bc
                                then flattenDecision cconstant cvariable d'
                                else []
        Special Portholes -> [([], Goto 197)]
        Special Cartwheel -> cartwheel (cvariable ^. equipment . gold)
        NoDecision o -> [([], o)]
        EvadeFight nrounds cid fdetails co -> [ (["no evasion"], Fight fdetails co)
                                              , (["evasion"], Fight (fdetails & fightMod %~ (Timed nrounds (Evaded cid) :)) co)
                                              ]
        Decisions lst -> do
            (desc, d') <- lst
            (alldesc, o) <- flattenDecision cconstant cvariable d'
            return (desc : alldesc, o)
        CanTake item count nxt
            | item == Gold
                -> withEffects [GainItem Gold count] nxt
            | count == 0
                -> flattenDecision cconstant cvariable nxt
            | otherwise ->
                case canTake item cconstant cvariable of
                    Nope -> notake
                    SpaceAvailable -> takeItem item nxt'
                    Mustdrop drops
                      | not (importantItem item cconstant cvariable) -> notake
                      | otherwise -> do
                          itemDropped <- drops
                          withEffects [LoseItem itemDropped 1, GainItem item 1] nxt'
          where
            notake = flattenDecision cconstant cvariable nxt
            nxt' = CanTake item (count - 1) nxt
        Canbuy item price nxt
            | cvariable ^. equipment . gold < price
                -> nobuy
            | not (importantItem item cconstant cvariable)
                -> nobuy
            | otherwise ->
              case canTake item cconstant cvariable of
                  Nope -> nobuy
                  SpaceAvailable -> withEffects [LoseItem Gold price, GainItem item 1] nxt
                  Mustdrop drops -> do
                      itemDropped <- drops
                      withEffects [LoseItem Gold price, LoseItem itemDropped 1, GainItem item 1] nxt
          where nobuy = flattenDecision cconstant cvariable nxt
        Cansell item price nxt
            | not (hasItem item inventory)
                -> flattenDecision cconstant cvariable nxt
            | otherwise -> case usedWeapon cconstant cvariable of
                              WithSkill w -> if Weapon w == item then nosell else sell
                              WithoutSkill w -> if Weapon w == item then nosell else sell
                              _ -> nosell
          where nosell = flattenDecision cconstant cvariable nxt
                sell = withEffects [GainItem Gold price, LoseItem item 1] nxt

cartwheel :: Price -> [([String], ChapterOutcome)]
cartwheel m
    | m == 50 = [([], Goto 186)]
    | m == 0 = [([], Goto 169)]
    | otherwise = do
        target <- [max m 22 .. min 50 (m + 40)]
        let desc = "target: " ++ show target
            res = do
                (newmoney, proba) <- LoneWolf.Cartwheel.solveFor target m
                return $ (proba,) $ if newmoney == 0
                             then GameLost
                             else Simple [GainItem Gold (newmoney - m)] (Goto 186)
        return ([desc], Randomly res)
