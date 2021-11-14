{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module LoneWolf.Rules where

import Control.DeepSeq
import Control.Lens
import Data.Hashable
import Data.List
import GHC.Generics
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Combat
import Solver

data HadCombat
  = Didn'tFight
  | DidFight
  deriving (Show, Eq, Generic, Ord)

instance Hashable HadCombat

instance NFData HadCombat

data NextStep
  = HasLost !ChapterId
  | HasWon !CharacterVariable
  | NewChapter !ChapterId !CharacterVariable !HadCombat
  deriving (Show, Eq, Generic, Ord)

instance Hashable NextStep

makePrisms ''NextStep

instance NFData NextStep

check :: CharacterConstant -> CharacterVariable -> BoolCond -> Bool
check cconstant cvariable cond =
  case cond of
    Always b -> b
    HasDiscipline d -> d `elem` cconstant ^. discipline
    Not c -> not (check cconstant cvariable c)
    COr c1 c2 -> check cconstant cvariable c1 || check cconstant cvariable c2
    CAnd c1 c2 -> check cconstant cvariable c1 && check cconstant cvariable c2
    HasItem i n -> case i of
      Gold -> cvariable ^. equipment . gold >= n
      Meal -> cvariable ^. equipment . meals >= n
      _ -> hasItem i (cvariable ^. equipment)

updateSimple :: CharacterConstant -> CharacterVariable -> SimpleOutcome -> CharacterVariable
updateSimple cconstant cvariable soutcome =
  case soutcome of
    DamagePlayer dmg -> cvariable & curendurance -~ dmg
    HealPlayer heal -> cvariable & curendurance %~ \hp -> max maxhp (hp + heal)
    FullHeal -> cvariable & curendurance .~ (cconstant ^. maxendurance)
    HalfHeal -> cvariable & curendurance %~ \hp -> (hp + maxhp) `div` 2
    GainItem item count -> cvariable & equipment %~ addItem item count
    LoseItem item lost -> cvariable & equipment %~ delItem item lost
    LoseItemKind slots -> cvariable & equipment %~ delItemSlot slots
    MustEat canhunt
      | canhunt == Hunt && Hunting `elem` _discipline cconstant ->
        cvariable
      | hasItem Laumspur eqp && (maxhp - curhp >= 3) -> laumspur
      | hasItem Meal eqp -> updates [LoseItem Meal 1]
      | hasItem Laumspur eqp -> laumspur
      | otherwise -> updates [DamagePlayer 3]
  where
    laumspur = updates [HealPlayer 3, LoseItem Laumspur 1]
    curhp = cvariable ^. curendurance
    maxhp = getMaxHp cconstant cvariable
    eqp = cvariable ^. equipment
    updates lst = foldl' (updateSimple cconstant) cvariable lst

update :: CharacterConstant -> CharacterVariable -> ChapterId -> ChapterOutcome -> Probably NextStep
update cconstant cvariable cid outcome =
  case outcome of
    Goto cid' -> certain (NewChapter cid' cvariable Didn'tFight)
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
    Fight fd nxt -> regroup $
      (traverse . _1 . _NewChapter . _3 .~ DidFight) $ do
        (charendurance, p) <- fight cconstant cvariable fd
        case fd ^? fightMod . traverse . _FakeFight of
          Nothing ->
            if charendurance <= 0
              then [(HasLost cid, p)]
              else fmap (* p) <$> update cconstant (cvariable & curendurance .~ charendurance) cid nxt
          Just cid' ->
            if charendurance <= 0
              then [(NewChapter cid' cvariable Didn'tFight, p)]
              else map (fmap (* p)) $
                update cconstant cvariable cid $
                  case fd ^? fightMod . traverse . _Evaded of
                    Just evasionDestination -> Goto evasionDestination
                    Nothing -> nxt

uCheck :: CharacterConstant -> CharacterVariable -> [(BoolCond, t)] -> t
uCheck cconstant cvariable conds = case conds of
  [] -> error "Can't happen"
  [(_, o)] -> o
  ((c, o) : cs) ->
    if check cconstant cvariable c
      then o
      else uCheck cconstant cvariable cs

getMaxHp :: CharacterConstant -> CharacterVariable -> Endurance
getMaxHp cconstant cvariable = cconstant ^. maxendurance + if hasItem ChainMail (cvariable ^. equipment) then 4 else 0
