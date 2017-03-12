---
title: "Solving a game book : modelling the Lone Wolf character state"
series: Game book solver
date: 2017-03-11
---

This module holds the base types for representing the game state, and specifically the player state. This is a literate Haskell file!

Language extensions, imports
----------------------------

Nothing much to say here, except perhaps the not that common option `DeriveDataTypeable`, which will turn out to be very handy for using `biplate`.

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DeriveDataTypeable #-}
>
> module LoneWolf.Character where
>
> import GHC.Generics
> import Data.HashMap.Strict (HashMap)
> import Data.Hashable
> import Data.Data

Character sheet
---------------

A character is made up of two parts : one is constant, decided at the beginning of the game book. This is an oversimplification, as the Lone Wolf rules support an experience counter that can increase the combat skill of the player.
As a simplification, I have decided to ignore it.

> data Character = Character { _constantData :: CharacterConstant
>                            , _variableData :: CharacterVariable
>                            } deriving (Generic, Eq, Show, Read)

In the constant part, the combat skill and endurance are randomly determined when the adventure begins. The list of disciplines is choosen by the player.

> newtype CombatSkill = CombatSkill { getCombatSkill :: Int }
>                     deriving (Show, Eq, Read, Num, Typeable, Data)
>
> newtype Endurance = Endurance { getEndurance :: Int }
>                     deriving (Show, Eq, Read, Num, Typeable, Data)
>
> data CharacterConstant = CharacterConstant
>       { _maxendurance :: Endurance
>       , _combatSkill  :: CombatSkill
>       , _discipline   :: [Discipline]
>       } deriving (Generic, Eq, Show, Read)

The variable part holds the player inventory, and current health points.

> data CharacterVariable = CharacterVariable
>       { _curendurance :: Endurance
>       , _equipment    :: (HashMap Item Int)
>       } deriving (Generic, Eq, Show, Read)

Five disciplines must be picked before the game starts. Most of them can turn out to be useful during the adventure, but the `WeaponSkill` deserves a special treatment.
During the adventure, magic weapons can be found, in the form of a sword and a spear.
As a result, if a `WeaponSkill` should be picked, it should be related to swords or spears. There are several types of swords, but I'll choose the `ShortSword` to start with, as it's cheaper than the others and easier to procure.

> data Discipline = Camouflage
>                 | Hunting
>                 | SixthSense
>                 | Tracking
>                 | Healing
>                 | WeaponSkill Weapon
>                 | MindShield
>                 | MindBlast
>                 | AnimalKinship
>                 | MindOverMatter
>                 deriving (Show, Eq, Generic, Read, Ord, Typeable, Data)

Two weapons are selected by the player before the story starts. The `MagicSpear` and `Sommerswerd` can't be picked when the game begins, but can be found during the adventure.
Given that these weapons are magic and more powerful than their mundane counterparts, and given the preference for `WeaponSkill` selection, The `ShortSword` and `Spear` will always be picked at the start of the adventure.

> data Weapon = Dagger
>             | Spear
>             | Mace
>             | ShortSword
>             | Warhammer
>             | Sword
>             | Axe
>             | Quarterstaff
>             | BroadSword
>             | MagicSpear
>             | Sommerswerd
>             deriving (Show, Eq, Generic, Ord, Enum, Bounded, Read, Typeable, Data)

> data Item = Weapon !Weapon
>           | Backpack
>           | Helmet
>           | Shield
>           | Meal
>           | ChainMail
>           | HealingPotion
>           | PotentPotion -- heals 5 after combat
>           | Gold
>           | Torch
>           | Rope
>           | Laumspur -- works as meal, heals 3hp (chapter 113)
>           | StrengthPotion -- Combat skill +2 for a single combat
>           | Scroll193
>           | Message267
>           | PerfumedSoap315
>           | TinderBox347
>           | CrystalStar
>           | TicketVol2
>           | PasswordVol2
>           | DocumentsVol2
>           | SealHammerdalVol2
>           | WhitePassVol2
>           | RedPassVol2
>           deriving (Show, Eq, Generic, Ord, Read, Typeable, Data)

> instance Hashable Item
> instance Hashable Weapon

> data Slot = WeaponSlot
>           | BackpackSlot
>           | SpecialSlot
>           | PouchSlot
>           deriving (Show, Eq, Generic, Typeable, Data)

> slotSize :: Slot -> Maybe Int
> slotSize s = case s of
>               WeaponSlot   -> Just 2
>               BackpackSlot -> Just 8
>               _            -> Nothing

> itemSlot :: Item -> Slot
> itemSlot (Weapon _)        = WeaponSlot
> itemSlot Helmet            = SpecialSlot
> itemSlot Meal              = BackpackSlot
> itemSlot ChainMail         = SpecialSlot
> itemSlot HealingPotion     = BackpackSlot
> itemSlot Gold              = PouchSlot
> itemSlot Laumspur          = BackpackSlot
> itemSlot StrengthPotion    = BackpackSlot
> itemSlot Torch             = BackpackSlot
> itemSlot Scroll193         = BackpackSlot
> itemSlot Message267        = BackpackSlot
> itemSlot PerfumedSoap315   = BackpackSlot
> itemSlot TinderBox347      = BackpackSlot
> itemSlot CrystalStar       = SpecialSlot
> itemSlot TicketVol2        = SpecialSlot
> itemSlot Backpack          = SpecialSlot
> itemSlot PotentPotion      = BackpackSlot
> itemSlot Rope              = BackpackSlot
> itemSlot PasswordVol2      = SpecialSlot
> itemSlot DocumentsVol2     = SpecialSlot
> itemSlot WhitePassVol2     = SpecialSlot
> itemSlot RedPassVol2       = SpecialSlot
> itemSlot SealHammerdalVol2 = SpecialSlot
> itemSlot Shield            = SpecialSlot

