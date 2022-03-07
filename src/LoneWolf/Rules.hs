{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module LoneWolf.Rules where

import Codec.Serialise (Serialise)
import Control.DeepSeq
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable
import Data.List
import GHC.Generics
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Combat
import Solver

data NextStep
  = HasLost !ChapterId
  | HasWon !CharacterVariable
  | NewChapter !ChapterId !CharacterVariable
  deriving (Show, Eq, Generic, Ord)

instance Serialise NextStep

instance Hashable NextStep

instance ToJSON NextStep

instance FromJSON NextStep

makePrisms ''NextStep

instance NFData NextStep

check :: CharacterConstant -> CharacterVariable -> BoolCond -> Bool
check cconstant cvariable cond =
  case cond of
    Always b -> b
    HasDiscipline d -> d `elem` cconstant ^. discipline
    HasEndurance e -> cvariable ^. curendurance >= e
    HasFlag flg -> cvariable ^. flag flg
    Not c -> not (check cconstant cvariable c)
    COr c1 c2 -> check cconstant cvariable c1 || check cconstant cvariable c2
    CAnd c1 c2 -> check cconstant cvariable c1 && check cconstant cvariable c2
    HasLevel lvl -> getLevel cconstant >= lvl
    HasItem i n -> case i of
      Gold -> cvariable ^. equipment . gold >= n
      Meal -> cvariable ^. equipment . meals >= n
      _ -> hasItem i (cvariable ^. equipment)

updateSimple :: CharacterConstant -> CharacterVariable -> SimpleOutcome -> CharacterVariable
updateSimple cconstant cvariable soutcome =
  case soutcome of
    DamagePlayer dmg -> cvariable & curendurance %~ \hp -> max 0 (hp - dmg)
    HealPlayer heal -> cvariable & curendurance %~ \hp -> min maxhp (hp + heal)
    FullHeal -> cvariable & curendurance .~ maxhp
    HalfHeal -> cvariable & curendurance %~ \hp -> (hp + maxhp) `div` 2
    GainItem item count -> cvariable & equipment %~ addItem item count
    LoseItem item lost -> cvariable & equipment %~ delItem item lost & updateMaxHp
    LoseItemKind slots -> cvariable & equipment %~ delItemSlot slots & updateMaxHp
    SetFlag flg -> cvariable & flag flg .~ True
    ClearFlag flg -> cvariable & flag flg .~ False
    StoreEquipment ->
      let CharacterVariable e flgs pe _ = cvariable
       in CharacterVariable e flgs 0 pe
    MustEat canhunt
      | canhunt == Hunt && Hunting `elem` _discipline cconstant ->
        cvariable
      | book01 && hasItem Laumspur eqp && (maxhp - curhp >= 3) -> laumspuru
      | hasItem Meal eqp -> updates [LoseItem Meal 1]
      | book01 && hasItem Laumspur eqp -> laumspuru
      | otherwise -> updates [DamagePlayer 3]
  where
    book01 = _bookid cconstant == Book01
    laumspuru = updates [HealPlayer 3, LoseItem Laumspur 1]
    curhp = cvariable ^. curendurance
    maxhp = getMaxHp cconstant cvariabl../gamebooksolver-webapp-alpine/e
    eqp = cvariable ^. equipment
    updates = foldl' (updateSimple cconstant) cvariable
    updateMaxHp cv = cv & curendurance %~ min maxhp

update :: CharacterConstant -> CharacterVariable -> ChapterId -> ChapterOutcome -> Probably NextStep
update cconstant cvariable cid outcome =
  case outcome of
    _ | cvariable ^. curendurance <= 0 -> certain (HasLost cid)
    Goto cid'
      | cid < maxchapter && hasFlag Poisonned2 cvariable ->
        let nvariable = updateSimple cconstant cvariable (DamagePlayer 2)
         in if nvariable ^. curendurance > 0
              then certain (NewChapter cid' (nvariable & flag HadCombat .~ False))
              else certain (HasLost cid)
      | cid < maxchapter && has (discipline . traverse . _Healing) cconstant && not (cvariable ^. flag HadCombat) ->
        certain (NewChapter cid' (updateSimple cconstant cvariable (HealPlayer 1)))
      | otherwise -> certain (NewChapter cid' cvariable)
    GameLost -> certain (HasLost cid)
    GameWon -> certain (HasWon cvariable)
    Simple effects nxt ->
      let nvariable = foldl' (updateSimple cconstant) cvariable effects
       in if nvariable ^. curendurance <= 0
            then certain (HasLost cid)
            else update cconstant nvariable cid nxt
    Conditionally conditions -> update cconstant cvariable cid (uCheck cconstant cvariable conditions)
    Randomly rands -> regroup $ do
      (p, o) <- rands
      fmap (* p) <$> update cconstant cvariable cid o
    OneRound fd lose eq win -> regroup $ do
      ((lwhp, ophp), p) <- fightRound cconstant cvariable fd
      let lwloss = cvariable ^. curendurance - lwhp
          oploss = fd ^. fendurance - ophp
          tgt
            | lwloss > oploss = lose
            | lwloss < oploss = win
            | otherwise = eq
      fmap (* p)
        <$> update
          cconstant
          ( cvariable
              & curendurance .~ lwhp
              & flag StrengthPotionActive .~ False
              & flag PotentStrengthPotionActive .~ False
              & flag HadCombat .~ True
          )
          cid
          tgt
    Fight fd nxt -> regroup $ do
      (echarendurance, p) <- fight cconstant cvariable fd
      let (noutcome, charendurance) = case echarendurance of
            HasEscaped ecid n -> (Goto ecid, n)
            LateWin ecid n -> (Goto ecid, n)
            NotEscaped n -> (nxt, n)
            Lost lostchapter -> (Goto lostchapter, 1)
            Stopped ecid n -> (Goto ecid, n)
          -- desactivate potion at the end of the fights
          cvariable_popo = if has (fightMod . traverse . _MultiFight) fd then cvariable else cvariable & flag StrengthPotionActive .~ False & flag PotentStrengthPotionActive .~ False
          cvariable' = cvariable_popo & flag HadCombat .~ True
      case fd ^? fightMod . traverse . _FakeFight of
        Nothing ->
          if charendurance <= 0
            then [(HasLost cid, p)]
            else fmap (* p) <$> update cconstant (cvariable' & curendurance .~ charendurance) cid noutcome
        Just cid' ->
          if charendurance <= 0
            then [(NewChapter cid' (cvariable & flag HadCombat .~ False), p)]
            else fmap (* p) <$> update cconstant cvariable' cid noutcome
  where
    maxchapter = if _bookid cconstant == Book05 then 400 else 350

uCheck :: CharacterConstant -> CharacterVariable -> [(BoolCond, t)] -> t
uCheck cconstant cvariable conds = case conds of
  [] -> error "Can't happen"
  [(_, o)] -> o
  ((c, o) : cs) ->
    if check cconstant cvariable c
      then o
      else uCheck cconstant cvariable cs

getMaxHp :: CharacterConstant -> CharacterVariable -> Endurance
getMaxHp cconstant cvariable =
  let eq = cvariable ^. equipment
   in cconstant ^. maxendurance
        + (if hasItem BodyArmor eq then 4 else 0)
        + (if hasItem Helmet eq && not (hasFlag HelmetIsSilver cvariable) then 2 else 0)
