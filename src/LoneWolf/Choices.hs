{-# LANGUAGE TupleSections #-}

module LoneWolf.Choices where

import Control.Lens
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified LoneWolf.Cartwheel
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Combat (getRatio)
import LoneWolf.CombatChart (hits)
import LoneWolf.Rules (check, getMaxHp, updateSimple)
import Solver (regroup)

importantItem :: Item -> CharacterConstant -> CharacterVariable -> Bool
importantItem i cconstant cvariable =
  case i of
    Weapon Sommerswerd -> True
    Weapon MagicSpear -> True
    -- there is a magic mace in book 05, but due the way the chapters are made we can't decide in advance if this mace is the magic mace or not :(
    Weapon Mace | _bookid cconstant == Book05 -> True
    Weapon w -> case usedWeapon cconstant cvariable of
      WithSkill _ -> False
      NoWeapon -> True
      WithoutSkill _ -> WeaponSkill w `elem` cconstant ^. discipline
    _ -> True

-- some items will force branches (particularly in book05, with the black crystal cube, and should not always be picked up)
potentiallybad :: Item -> Bool
potentiallybad i = i == blackCubeB05

data CanTake
  = SpaceAvailable Int
  | Mustdrop [Item]
  | Nope
  deriving (Show, Eq)

canTake :: Item -> CharacterConstant -> CharacterVariable -> CanTake
canTake item cconstant cvariable =
  case itemSlot item of
    BackpackSlot
      | itemsInBackpack > 8 -> error ("canTake: too many items in backpack -> " ++ show cvariable)
      | itemsInBackpack == 8 -> Mustdrop (map fst backpackItems)
      | otherwise -> SpaceAvailable (8 - itemsInBackpack)
    WeaponSlot -> case weapons of
      [] -> SpaceAvailable 2
      [_] -> SpaceAvailable 1
      lst -> Mustdrop $
        map Weapon $ case usedWeapon cconstant cvariable of
          WithSkill w -> filter (/= w) lst
          WithoutSkill Sommerswerd -> filter (/= Sommerswerd) lst
          _ -> lst
    PouchSlot -> SpaceAvailable 50
    SpecialSlot ->
      if hasItem item inventory
        then Nope
        else SpaceAvailable 1
  where
    inventory = cvariable ^. equipment
    weapons = getWeapons inventory
    backpackItems = filter ((== BackpackSlot) . itemSlot . fst) (items inventory)
    itemsInBackpack = sum (map snd backpackItems)

flattenDecision :: CharacterConstant -> CharacterVariable -> Decision -> [([String], ChapterOutcome)]
flattenDecision cconstant cvariable d =
  let inventory = cvariable ^. equipment
      takeItem item = withEffects [GainItem item 1]
      withEffects effects nxt = do
        let nvariable = foldl' (updateSimple cconstant) cvariable effects
        (dsc, o) <- flattenDecision cconstant nvariable nxt
        return (show effects : dsc, Simple effects o)
   in case d of
        _ -- can be healed, finally!
          | hasFlag Poisonned2 cvariable && hasItem Laumspur inventory ->
            let nvariable = updateSimple cconstant cvariable (HealPlayer 4) & equipment %~ delItem Laumspur 1 & flag Poisonned2 .~ False
             in flattenDecision cconstant nvariable d
        NoDecision o
          | Fight (FightDetails _ _ _ mds) _ <- o, NoPotion `elem` mds -> [([], o)]
          | hasCombat o && hasItem StrengthPotion4 inventory && hasItem StrengthPotion inventory ->
            [ (["don't use strength potion"], o),
              (["use strength potion"], Simple [LoseItem StrengthPotion4 1, SetFlag PotentStrengthPotionActive] o),
              (["use potent strength potion"], Simple [LoseItem StrengthPotion4 1, SetFlag PotentStrengthPotionActive] o)
            ]
          | hasCombat o && hasItem StrengthPotion inventory ->
            [ (["don't use strength potion"], o),
              (["use strength potion"], Simple [LoseItem StrengthPotion 1, SetFlag StrengthPotionActive] o)
            ]
          | hasCombat o && hasItem StrengthPotion4 inventory ->
            [ (["don't use strength potion"], o),
              (["use potent strength potion"], Simple [LoseItem StrengthPotion4 1, SetFlag PotentStrengthPotionActive] o)
            ]
          | otherwise -> [([], o)]
        RetrieveEquipment nxt ->
          -- there should be less items in the current inventory than in the stored, that's why it is done that way
          let CharacterVariable e flgs cure stoe = cvariable
              itms = items cure
           in flattenDecision cconstant (CharacterVariable e flgs stoe 0) (foldr (uncurry CanTake) nxt itms)
        AfterCombat nxt
          | hasItem Potion6Hp inventory && missingHp >= 6 ->
            nopotion ++ withEffects [HealPlayer 6, LoseItem Potion6Hp 1] d
          | hasItem Potion5Hp inventory && missingHp >= 5 ->
            nopotion ++ withEffects [HealPlayer 5, LoseItem Potion5Hp 1] d
          | not book01 && hasItem Laumspur inventory && missingHp >= 4 ->
            nopotion ++ withEffects [HealPlayer 4, LoseItem Laumspur 1] d
          | hasItem Potion4Hp inventory && missingHp >= 4 ->
            nopotion ++ withEffects [HealPlayer 4, LoseItem Potion4Hp 1] d
          | hasItem Potion2Hp inventory && missingHp >= 2 ->
            nopotion ++ withEffects [HealPlayer 2, LoseItem Potion2Hp 1] d
          | _bookid cconstant == Book05 && hasItem oedeHerb 1 && missingHp >= 10 ->
            nopotion ++ withEffects [HealPlayer 10, LoseItem oedeHerb 1] d
          | otherwise -> nopotion
          where
            nopotion = flattenDecision cconstant cvariable nxt
            missingHp = getMaxHp cconstant cvariable - cvariable ^. curendurance
            book01 = _bookid cconstant == Book01
        Conditional bc d' ->
          if check cconstant cvariable bc
            then flattenDecision cconstant cvariable d'
            else []
        Special Portholes -> [([], Goto 197)]
        Special Cartwheel -> cartwheel (cvariable ^. equipment . gold)
        EvadeFight nrounds cid fdetails co ->
          [ (["no evasion"], Fight fdetails co),
            (["evasion"], Fight (fdetails & fightMod %~ ((if nrounds > 0 then Timed nrounds (Evaded cid) else Evaded cid) :)) co)
          ]
        Decisions lst -> do
          (cdesc, d') <- lst
          (alldesc, o) <- flattenDecision cconstant cvariable d'
          return (cdesc : alldesc, o)
        CanTake item count nxt
          | item == Gold -> withEffects [GainItem Gold count] nxt
          | count == 0 -> flattenDecision cconstant cvariable nxt
          | count == 1 ->
            case canTake item cconstant cvariable of
              Nope -> notake
              SpaceAvailable n | n <= 0 -> error "space available invariant wrong"
              SpaceAvailable _ -> takeItem item nxt'
              Mustdrop drops
                | not (importantItem item cconstant cvariable) -> notake
                | otherwise -> do
                  itemDropped <- drops
                  withEffects [LoseItem itemDropped 1, GainItem item 1] nxt'
          | otherwise -> flattenDecision cconstant cvariable (CanTake item 1 (CanTake item (count - 1) nxt))
          where
            notake = flattenDecision cconstant cvariable nxt
            nxt' = CanTake item (count - 1) nxt
        Canbuy item price nxt
          | cvariable ^. equipment . gold < price ->
            nobuy
          | not (importantItem item cconstant cvariable) ->
            nobuy
          | otherwise ->
            case canTake item cconstant cvariable of
              Nope -> nobuy
              SpaceAvailable _ -> withEffects [LoseItem Gold price, GainItem item 1] d
              Mustdrop drops -> do
                itemDropped <- drops
                withEffects [LoseItem Gold price, LoseItem itemDropped 1, GainItem item 1] d
          where
            nobuy = flattenDecision cconstant cvariable nxt
        Cansell item price nxt
          | not (hasItem item inventory) ->
            flattenDecision cconstant cvariable nxt
          | otherwise -> nosell ++ sell
          where
            nosell = flattenDecision cconstant cvariable nxt
            sell = withEffects [GainItem Gold price, LoseItem item 1] nxt
        Special B05S127 ->
          let cs = fromIntegral (cconstant ^. combatSkill)
              borne x
                | x > 1 = 1
                | x < 0 = 0
                | otherwise = x
              b1 = borne ((cs - 10) / 10)
              b2 = borne ((20 - cs) / 10)
           in if b1 + b2 /= 1
                then error "SPECIAL B05S127"
                else [(["No decision"], Randomly [(b1, Goto 159), (b2, Goto 93)])]
        RemoveItemFrom BackpackSlot n nxt
          | n <= 0 -> flattenDecision cconstant cvariable nxt
          | otherwise ->
            let allbackpackitems = filter ((== BackpackSlot) . itemSlot . fst) (items inventory)
             in if sum (map snd allbackpackitems) < n
                  then []
                  else do
                    (todrop, _) <- allbackpackitems
                    let nvariable = updateSimple cconstant cvariable (LoseItem todrop 1)
                    (dsc, o) <- flattenDecision cconstant nvariable (RemoveItemFrom BackpackSlot (n - 1) nxt)
                    return (("drop " ++ show todrop) : dsc, Simple [LoseItem todrop 1] o)
        LoseItemFrom BackpackSlot n nxt ->
          let allbackpackitems = filter ((== BackpackSlot) . itemSlot . fst) (items inventory)
           in if n <= 0 || null allbackpackitems
                then flattenDecision cconstant cvariable nxt
                else do
                  (todrop, _) <- allbackpackitems
                  let nvariable = updateSimple cconstant cvariable (LoseItem todrop 1)
                  (dsc, o) <- flattenDecision cconstant nvariable (LoseItemFrom BackpackSlot (n - 1) nxt)
                  return (("lost " ++ show todrop) : dsc, Simple [LoseItem todrop 1] o)
        Special B05S357 ->
          let lwe = cvariable ^. curendurance
              ratio = getRatio cconstant cvariable (FightDetails "Platform Sentry" 15 23 [CombatBonus (-2)])
              result n = Randomly $ do
                ((lwe', mcid), p) <- bk05c357map M.! (ratio, lwe, 23)
                let outcome c = Simple [DamagePlayer (lwe - lwe')] (Goto c)
                pure (p, outcome (fromMaybe n mcid))
           in [(["Search the sentry body"], result 207), (["Ignore the sentry body"], result 224)]
        x -> error ("TODO: " ++ show x)

bk05c357map :: M.Map (CombatSkill, Endurance, Endurance) [((Endurance, Maybe ChapterId), Rational)]
bk05c357map = M.fromList [((cs, elw, een), bk05c357 cs elw een) | cs <- [-10 .. 40], elw <- [0 .. 40], een <- [0 .. 24]]

bk05c357 :: CombatSkill -> Endurance -> Endurance -> [((Endurance, Maybe ChapterId), Rational)]
bk05c357 ratio lwhp ophp
  | lwhp <= 0 = [((0, Nothing), 1)]
  | ophp <= 0 = [((lwhp, Nothing), 1)]
  | otherwise = ((lwhp, Just 293), 1 / 10) : regroup nxt
  where
    curp = 9 / 100
    nxt = do
      (dmgopponent, dmglw) <- hits ratio
      let lwhp' = lwhp - dmglw
          ophp' = ophp - dmgopponent
      if lwhp' <= 0
        then [((0, Nothing), curp)]
        else
          if ophp' <= 0
            then [((lwhp', Nothing), curp)]
            else do
              (o, p) <- case M.lookup (ratio, lwhp', ophp') bk05c357map of
                Just x -> x
                Nothing -> error ("Could not find key " ++ show (ratio, lwhp', ophp'))
              pure (o, p * curp)

cartwheel :: Price -> [([String], ChapterOutcome)]
cartwheel m
  | m >= 22 = [([], Goto 136)]
  | otherwise =
    let cmoney = m + 1 -- free token!
        target = 22
        cdesc = "target: " ++ show target
        res = do
          (newmoney, proba) <- LoneWolf.Cartwheel.solveFor target cmoney
          return $
            (proba,) $
              if newmoney == 0
                then GameLost
                else Simple [GainItem Gold (newmoney - m)] (Goto 136)
     in [([cdesc], Randomly res)]
