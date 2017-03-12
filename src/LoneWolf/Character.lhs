> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module LoneWolf.Character where

> import GHC.Generics
> import Data.HashMap.Strict (HashMap)
> import Data.Hashable
> import Data.Data

Pick 2

> data Weapon = Dagger | Spear | Mace | ShortSword | Warhammer | Sword | Axe | Quarterstaff | BroadSword | MagicSpear | Sommerswerd
>          deriving (Show, Eq, Generic, Ord, Enum, Bounded, Read, Typeable, Data)

Pick 5

> data Discipline = Camouflage | Hunting | SixthSense | Tracking | Healing | WeaponSkill !Weapon | MindShield | MindBlast | AnimalKinship | MindOverMatter
>          deriving (Show, Eq, Generic, Read, Ord, Typeable, Data)

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

> newtype CombatSkill = CombatSkill { getCombatSkill :: Int }
>                     deriving (Show, Eq, Read, Num, Typeable, Data)

> newtype Endurance = Endurance { getEndurance :: Int }
>                     deriving (Show, Eq, Read, Num, Typeable, Data)

> data Character = Character { _constantData :: CharacterConstant
>                            , _variableData :: CharacterVariable
>                            } deriving (Generic, Eq, Show, Read)

> data CharacterConstant = CharacterConstant
>       { _maxendurance :: Endurance
>       , _combatSkill  :: CombatSkill
>       , _discipline   :: [Discipline]
>       } deriving (Generic, Eq, Show, Read)
>

> data CharacterVariable = CharacterVariable
>       { _curendurance :: Endurance
>       , _equipment    :: (HashMap Item Int)
>       , _canHunt      :: Bool
>       } deriving (Generic, Eq, Show, Read)

