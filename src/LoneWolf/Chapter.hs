{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-
---
title: "Solving a gamebook : chapters"
series: Gamebook solver
date: 2017-03-14
---

Chapter modelisation
====================

This article is a literate Haskell representation of the `LoneWolf.Chapter` module at the time of the writing.

It doesn't include any code, only type definitions, and will certainly be the least interesting post of this serie.
It however shows a possible encoding of the chapter's descriptions, which the reader might find interesting on its own.

The last part discusses how the chapter definitions were partially generated.

Extensions and imports
----------------------

Clearly not the best part :)
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LoneWolf.Chapter where

import Control.Lens
import Data.Aeson
import Data.Data
import Data.Data.Lens
import qualified Data.Function.Memoize as M
import GHC.Generics (Generic)
import LoneWolf.Character

{-
Basic types
-----------

The following types should get a newtype, but to be honest, it is much more handy that way right now, because of the vicious code generation that is happening later ...
-}

-- each chapter has an identifier, with a value ranging from 1 to 350
type ChapterId = Int

-- some combat effects are timed, for example some fights can be evaded after a certain number of rounds
type Rounds = Int

-- price of stuff, in gold coins
type Price = Int

{-
What's in a chapter?
--------------------

Lone Wolf chapters usually have a descriptive text, some figures, a description of what happens to the payer, and a list of choices. The `title` of the chapter serves no real purpose here, as it is identical (in `String` form) to the associated `ChapterId`.
-}

type Chapter = ChapterG Rational

data ChapterG a = Chapter
  { _title :: String,
    _desc :: String,
    _pchoice :: DecisionG a
  }
  deriving (Show, Eq, Generic, Functor)

instance ToJSON a => ToJSON (ChapterG a) where
  toJSON = genericToJSON jsonOptions

{-
Here is what an actual chapter looks like:

```haskell
 Chapter "346"
         "The driver nods and hands back the ticket. The inn is warm but poorly furnished..."
         (Decisions
          [ ( "Buy a meal, and a room"
            , Conditional
                (HasItem Gold 2)
                (NoDecision (LoseItem Gold 2 (Goto 280))))
          , ( "Just the room"
            , Conditional
                (HasItem Gold 1)
                (NoDecision (LoseItem Gold 1 (MustEat NoHunt (Goto 280)))))
          , ("Nothing", NoDecision (MustEat NoHunt (Goto 280)))
          ])
```

The important part of this model is that the `Decision` type encodes the **player choices**, and contains `ChapterOutcome` fields that encode the **outcome of the choices**.
This is a critical distinction, as it is structural to how the solver works.

The Decision type
-----------------

In details, the `Decision` types has the following constructors:
-}

type Decision = DecisionG Rational

data DecisionG a
  = Decisions [(String, DecisionG a)]
  | RetrieveEquipment (DecisionG a)
  | CanTake Item Int (DecisionG a)
  | Canbuy Item Price (DecisionG a)
  | Cansell Item Price (DecisionG a)
  | Conditional BoolCond (DecisionG a)
  | Special SpecialChapter
  | NoDecision (ChapterOutcomeG a)
  | EvadeFight Rounds ChapterId (FightDetailsG a) (ChapterOutcomeG a)
  | AfterCombat (DecisionG a)
  | RemoveItemFrom Slot Int (DecisionG a) -- must have that least that many items
  | LoseItemFrom Slot Int (DecisionG a) -- always succeeds
  deriving (Show, Eq, Typeable, Data, Generic, Functor)

instance ToJSON a => ToJSON (DecisionG a) where
  toJSON = genericToJSON jsonOptions

data SpecialChapter
  = Cartwheel
  | Portholes
  | B05S127
  | B05S357
  deriving (Show, Eq, Typeable, Data, Generic)

instance ToJSON SpecialChapter where
  toJSON = genericToJSON jsonOptions

{-
 * The `Decisions` constructor represents a list of possible choices for the player, with, for each case, a textual and programmatic description.

 * The `CanTake` and `CanBuy` constructors represent situations where the player can take, or buy, items.
   `CanSell` serves an identical purpose, for selling items.
   These constructors are not necessary, as they could be encoded using other, simpler, primitives. The following are equivalent, with regards to the choices they encode:

    ```haskell
    gaingold = CanTake Gold 10 (NoDecision (Goto 20))

    gaingold' = Decisions
      [ ("Take the gold", NoDecision (GainItem Gold 10 (Goto 20)))
      , ("Take 9 coins", NoDecision (GainItem Gold 9 (Goto 20)))
      , ("Take 8 coins", NoDecision (GainItem Gold 8 (Goto 20)))
      , ...
      , ("Leave the gold", NoDecision (Goto 20)
      ]
    ```

    Having specialized constructors helps a lot. First it makes it easier to write the chapter definitions, and makes them clearer.
    But, more importantly, it will help reducing the computational complexity by deciding locally with specific heuristics.

 * Some decisions are only accessible in certain conditions, for example:

    ```haskell
    (Decisions
     [ ( "If you have the Kai Discipline of Tracking, turn to 13."
         , Conditional (HasDiscipline Tracking) (NoDecision (Goto 13)))
     , ( "If you wish to take the left fork, turn to 155."
         , Conditional
             (Not (HasDiscipline Tracking))
             (NoDecision (Goto 155)))
     , ( "If you wish to take the right fork, turn to 293."
         , Conditional
             (Not (HasDiscipline Tracking))
             (NoDecision (Goto 293)))
     ])
    ```

    In this example, if the player has the `Tracking` discipline, he will be forced to jump to chapter 13. If not, he will be able to choose between chapters 155 and 293.

 * Some chapters are special, and will be handled specifically. More on this later!

 * The `NoDecision` constructor indicates that there is no further choice for the player, and is at the leaf of most `Decision` structures.
   I currently have flagged two chapters as special, but this might evolve in the future.

The ChapterOutcome type
-----------------------

A `ChapterOutcome` describe what happens to the player once he has made a decision.
-}

type ChapterOutcome = ChapterOutcomeG Rational

data ChapterOutcomeG a
  = Fight (FightDetailsG a) (ChapterOutcomeG a)
  | -- | lw loss, equal, lw win
    OneRound (FightDetailsG a) (ChapterOutcomeG a) (ChapterOutcomeG a) (ChapterOutcomeG a)
  | Randomly [(a, ChapterOutcomeG a)]
  | Conditionally [(BoolCond, ChapterOutcomeG a)]
  | Simple [SimpleOutcome] (ChapterOutcomeG a)
  | Goto ChapterId
  | GameLost
  | GameWon
  deriving (Show, Eq, Typeable, Data, Generic, Functor)

instance ToJSON a => ToJSON (ChapterOutcomeG a) where
  toJSON x = case x of
    GameLost -> "GameLost"
    GameWon -> "GameWon"
    Fight a b -> object [("Fight", toJSON (a, b))]
    Simple a b -> object [("Simple", toJSON (a, b))]
    Conditionally l -> object [("Conditionally", toJSON l)]
    Randomly l -> object [("Randomly", toJSON l)]
    Goto d -> object [("Goto", toJSON d)]
    OneRound a b c d -> object [("OneRound", toJSON (a, b, c, d))]

data SimpleOutcome
  = DamagePlayer Endurance
  | HealPlayer Endurance
  | FullHeal
  | HalfHeal
  | GainItem Item Int
  | LoseItem Item Int
  | LoseItemKind [Slot]
  | MustEat CanHunt
  | StoreEquipment -- drop all equipment, saving it in case we find it again
  | SetFlag Flag
  | ClearFlag Flag
  deriving (Show, Eq, Typeable, Data, Generic)

instance ToJSON SimpleOutcome where
  toJSON x = case x of
    DamagePlayer a -> object [("DamagePlayer", toJSON a)]
    HealPlayer a -> object [("HealPlayer", toJSON a)]
    LoseItemKind a -> object [("LoseItemKind", toJSON a)]
    MustEat a -> object [("MustEat", toJSON a)]
    SetFlag a -> object [("SetFlag", toJSON a)]
    ClearFlag a -> object [("ClearFlag", toJSON a)]
    FullHeal -> "FullHeal"
    HalfHeal -> "HalfHeal"
    StoreEquipment -> "StoreEquipment"
    GainItem a b -> object [("GainItem", toJSON (a, b))]
    LoseItem a b -> object [("LoseItem", toJSON (a, b))]

hasCombat :: ChapterOutcome -> Bool
hasCombat o =
  case o of
    Fight _ _ -> True
    OneRound {} -> True
    Randomly probs -> any (hasCombat . snd) probs
    Conditionally choices -> any (hasCombat . snd) choices
    Simple _ o' -> hasCombat o'
    Goto _ -> False
    GameLost -> False
    GameWon -> False

{-
A constructor of interest is the `MustEat` constructor.
It represents chapters where the player loses some hit points if he doesn't have a meal in his backpack.
In the Lone Wolf series, the `Hunting` discipline let the player ignore these events.
But in this specific book, there are a couple of chapters where `Hunting` can't be used!
In a previous tentative, I used a flag in the game state to indicate whether or not hunting was permitted (it's explicitely told when it stops and when it starts).
In general, it is better to make the game state as simple as possible, so as to get good memoization performance, so I decided to manually track the chapters where hunting was disabled, and mark them as such.

```haskell
Conditionally
 [ (HasItem Laumspur 1, Goto 145)
 , (HasDiscipline Healing, Goto 210)
 , (Always True, Goto 275)
 ]
```

Note how it works like Haskell's guards, and ends up with the equivalent of `otherwise`, which I called ... `botherwise`:
-}

botherwise :: BoolCond
botherwise = Always True

data CanHunt = Hunt | NoHunt
  deriving (Show, Eq, Typeable, Data, Generic)

instance ToJSON CanHunt where
  toJSON = genericToJSON jsonOptions

data BoolCond
  = HasDiscipline Discipline
  | Not BoolCond
  | COr BoolCond BoolCond
  | CAnd BoolCond BoolCond
  | HasItem Item Int
  | Always Bool
  | HasEndurance Endurance
  | HasFlag Flag
  | HasLevel KaiLevel
  deriving (Show, Eq, Typeable, Data, Generic)

instance ToJSON BoolCond where
  toJSON = genericToJSON jsonOptions

(.&&.) :: BoolCond -> BoolCond -> BoolCond
(.&&.) = CAnd

{-
The boolean conditions type (`BoolCond`) is pretty self-explanatory.

Combat description
------------------

The three main parts of a combat descriptions are the name of the opponent, its combat skill, and its endurance.
There are however many combat situations that involve special modifiers which are detailled here.
Fights against consecutive opponents are handled by chaining the `Fight` and `EvadeFight` constructors.
-}

type FightDetails = FightDetailsG Rational

data FightDetailsG a = FightDetails
  { _opponent :: String,
    _fcombatSkill :: CombatSkill,
    _fendurance :: Endurance,
    _fightMod :: [FightModifierG a]
  }
  deriving (Show, Eq, Typeable, Data, Generic, Functor)

instance ToJSON a => ToJSON (FightDetailsG a) where
  toJSON = genericToJSON jsonOptions

type FightModifier = FightModifierG Rational

data FightModifierG a
  = Undead
  | MindblastImmune
  | Timed Int (FightModifierG a)
  | CombatBonus CombatSkill
  | BareHanded
  | FakeFight ChapterId
  | EnemyMindblast
  | ForceEMindblast -- forced loss of 2 pts/turn
  | PlayerInvulnerable
  | DoubleDamage -- chapter 306
  | Evaded ChapterId
  | OnDamage ChapterId
  | OnNotYetWon ChapterId -- if won in less than that amount of rounds
  | MultiFight -- set when this is only part of a single chapter fight
  | EnemyInvulnerable -- rounds during which the enemy is invulnerable
  | OnLose ChapterId -- if the fight is lost, do not die, but jump
  | StopFight ChapterId -- immediately stop the fight and jump
  | DPR Endurance -- damage per round inflicted to the opponent
  | NoPotion -- can't take a potion for this fight
  | Poisonous a -- opponent doesn't damage, but can instakill on damage (cf b03s088)
  deriving (Show, Eq, Typeable, Data, Generic, Ord, Functor)

instance ToJSON a => ToJSON (FightModifierG a) where
  toJSON x = case x of
    Undead -> String "Undead"
    MindblastImmune -> String "MindblastImmune"
    Timed t m -> object [("Timed", toJSON (t, m))]
    CombatBonus a -> object [("CombatBonus", toJSON a)]
    BareHanded -> String "BareHanded"
    FakeFight a -> object [("FakeFight", toJSON a)]
    EnemyMindblast -> String "EnemyMindblast"
    ForceEMindblast -> String "ForceEMindblast"
    PlayerInvulnerable -> String "PlayerInvulnerable"
    DoubleDamage -> String "DoubleDamage"
    Evaded a -> object [("Evaded", toJSON a)]
    OnDamage a -> object [("OnDamage", toJSON a)]
    OnNotYetWon a -> object [("OnNotYetWon", toJSON a)]
    MultiFight -> String "MultiFight"
    EnemyInvulnerable -> String "EnemyInvulnerable"
    OnLose a -> object [("OnLose", toJSON a)]
    StopFight a -> object [("StopFight", toJSON a)]
    DPR a -> object [("DPR", toJSON a)]
    NoPotion -> String "NoPotion"
    Poisonous a -> object [("Poisonous", toJSON a)]

data KaiLevel
  = Novice
  | Intuite
  | Doan
  | Acolyte
  | Initiate
  | Aspirant
  | Guardian
  | Warmarn
  | Savant
  | Master
  deriving (Show, Eq, Ord, Enum, Bounded, Data, Generic)

instance ToJSON KaiLevel where
  toJSON = genericToJSON jsonOptions

getLevel :: CharacterConstant -> KaiLevel
getLevel ccst = case length (_discipline ccst) of
  1 -> Novice
  2 -> Intuite
  3 -> Doan
  4 -> Acolyte
  5 -> Initiate
  6 -> Aspirant
  7 -> Guardian
  8 -> Warmarn
  9 -> Savant
  10 -> Master
  _ -> error ("invalid amount of disciplines: " ++ show (_discipline ccst))

{-
 * `Undead`: undead creatures take double damage from the `Sommerswerd`.
 * `MindblastImmune`: the opponent is not affected by the `MindBlast` ability of the player.
 * `CombatBonus`: the player receives a combat bonus for this fight.
 * `BareHanded`: the player is forced to fight bare handed, reducing his combat skill by 4 points.
 * `FakeFight`: this isn't a real fight, and the player will have his endurance restored at the end of the fight. The `ChapterId` is provided as the destination if he loses the fight. This happens in [chapter 276](https://www.projectaon.org/en/xhtml/lw/02fotw/sect276.htm).
 * `EnemyMindblast`: the enemy has the `MindBlast` ability!
 * `PlayerInvulnerable` the player cannot be harmed.
 * `DoubleDamage`: happens in [chapter 306](https://www.projectaon.org/en/xhtml/lw/02fotw/sect306.htm), where the player inflicts double damage.
 * `Timed`: the effect only lasts a given number of rounds.
-}

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_'),
      allNullaryToStringTag = True,
      unwrapUnaryRecords = True,
      tagSingleConstructors = False,
      sumEncoding = ObjectWithSingleField
    }

{-
Lenses, plates and utilities
----------------------------
-}

makePrisms ''ChapterOutcomeG
makePrisms ''SimpleOutcome
makePrisms ''DecisionG
makePrisms ''FightModifierG
makeLenses ''FightDetailsG
makeLenses ''ChapterG

moneyCond :: Int -> ChapterOutcome -> Decision
moneyCond price = Conditional (HasItem Gold price) . NoDecision . Simple [LoseItem Gold price]

outcomePlate :: Traversal' Decision ChapterOutcome
outcomePlate = biplate

_Outcome :: Traversal' Decision ChapterOutcome
_Outcome = outcomePlate

{-
All of these are mainly useful in the `LoneWolf.XML` module.

The actual chapters data
========================

The chapters are described in the `LoneWolf.Book02` module, but it's all generated code!
The `LoneWolf.XML` module contains all the code that is necessary to parse the XML file provided by project Aon, compute a baseline, adapt all chapters that have special rules and generate code.

In previous iterations of this project, I used to manually generate all the chapter's definition, and chapter 346, that could be seen at the beginning of this article, would look like that:

```haskell
(346, UnsetFlag CanHunt
    ( Decisions [ ("buy meal", ItemPay Gold 1 (Outcome (Goto 1104)))
                , ("eat own meal", ItemPay Meal 1 (Outcome (Goto 1104)))
                , ("starve", Outcome (ODamage 3 (Goto 1104))) ] ))
```

Compared to the current version, the chapter's text is missing, and the choice descriptions are not as nice. It was also harder to debug, and one can see the `UnsetFlag CanHunt` situation I mentioned earlier.

The current situation is a net gain, as it has all the original text content, and also wrote most of the code for me. It was however still a **lot** of work to read all the chapters and make sure they were properly encoded.
I also realized my previous encoding was imperfect in parts (I never realized there was a chapter where you could sell your weapons!), and I have no illusions there are mistakes in the current definitions.

Next time will have actual code, with a rules interpreter!

-}

M.deriveMemoizable ''FightModifierG