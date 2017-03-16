This module holds the base types for representing the game state, and specifically the player state. This is a literate Haskell file!

Language extensions, imports
----------------------------

Nothing much to say here, except perhaps the not that common option `DeriveDataTypeable`, which will turn out to be very handy for using `biplate`.

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE TemplateHaskell #-}
>
> module LoneWolf.Character where
>
> import GHC.Generics
> import Data.Hashable
> import Data.Data
> import Data.Word
> import Control.Lens
> import qualified Data.Discrimination.Grouping as D
> import Data.Bits
> import Data.List

Character sheet
---------------

A character is made up of two parts : one is constant, decided at the beginning of the game book. This is an oversimplification, as the Lone Wolf rules support an experience counter that can increase the combat skill of the player.
As a simplification, I have decided to ignore it.

> data Character = Character { _constantData :: CharacterConstant
>                            , _variableData :: CharacterVariable
>                            } deriving (Generic, Eq, Show, Read)

In the constant part, the combat skill and endurance are randomly determined when the adventure begins. The list of disciplines is choosen by the player.

> newtype CombatSkill = CombatSkill { getCombatSkill :: Int }
>                     deriving (Show, Eq, Read, Num, Typeable, Data, Ord, Integral, Real, Enum, Generic)
>
> newtype Endurance = Endurance { getEndurance :: Int }
>                     deriving (Show, Eq, Read, Num, Typeable, Data, Ord, Integral, Real, Enum, Generic)
>
> instance D.Grouping Endurance
>
> data CharacterConstant = CharacterConstant
>       { _maxendurance :: Endurance
>       , _combatSkill  :: CombatSkill
>       , _discipline   :: [Discipline]
>       } deriving (Generic, Eq, Show, Read)

The variable part holds the player inventory, and current health points.

> data CharacterVariable = CharacterVariable
>       { _curendurance :: Endurance
>       , _equipment    :: Inventory
>       } deriving (Generic, Eq, Show, Read)
>
> instance D.Grouping CharacterVariable

> data Inventory = Inventory { _singleItems :: Word32
>                            , _gold        :: Word8
>                            , _meals       :: Word8
>                            } deriving (Generic, Eq, Show, Read)
>
> instance D.Grouping Inventory
>
> emptyInventory :: Inventory
> emptyInventory = Inventory 0 0 0

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

Not all items that are referenced in the books are described here.
I decided to let go of all items that were not useful.

> data Item = Weapon !Weapon
>           | Backpack
>           | Helmet
>           | Shield
>           | Meal
>           | ChainMail
>           | HealingPotion
>           | PotentPotion -- heals 5 after combat
>           | Gold
>           | Laumspur
>           | TicketVol2
>           | PasswordVol2
>           | DocumentsVol2
>           | SealHammerdalVol2
>           | WhitePassVol2
>           | RedPassVol2
>           deriving (Show, Eq, Generic, Ord, Read, Typeable, Data)

> instance Bounded Item where
>   minBound = Backpack
>   maxBound = Weapon maxBound

> instance Enum Item where
>   fromEnum x = case x of
>     Backpack          -> 0
>     Helmet            -> 1
>     Shield            -> 2
>     ChainMail         -> 3
>     HealingPotion     -> 4
>     PotentPotion      -> 5
>     Laumspur          -> 6
>     TicketVol2        -> 7
>     PasswordVol2      -> 8
>     DocumentsVol2     -> 9
>     SealHammerdalVol2 -> 10
>     WhitePassVol2     -> 11
>     RedPassVol2       -> 12
>     Gold              -> 13
>     Meal              -> 14
>     Weapon w          -> fromEnum w + 15

>   toEnum n = case n of
>     0  -> Backpack
>     1  -> Helmet
>     2  -> Shield
>     3  -> ChainMail
>     4  -> HealingPotion
>     5  -> PotentPotion
>     6  -> Laumspur
>     7  -> TicketVol2
>     8  -> PasswordVol2
>     9  -> DocumentsVol2
>     10 -> SealHammerdalVol2
>     11 -> WhitePassVol2
>     12 -> RedPassVol2
>     13 -> Gold
>     14 -> Meal
>     _  -> Weapon (toEnum (n - 15))

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
> itemSlot TicketVol2        = SpecialSlot
> itemSlot Backpack          = SpecialSlot
> itemSlot PotentPotion      = BackpackSlot
> itemSlot PasswordVol2      = SpecialSlot
> itemSlot DocumentsVol2     = SpecialSlot
> itemSlot WhitePassVol2     = SpecialSlot
> itemSlot RedPassVol2       = SpecialSlot
> itemSlot SealHammerdalVol2 = SpecialSlot
> itemSlot Shield            = SpecialSlot

> makeLenses ''CharacterVariable
> makeLenses ''CharacterConstant
> makeLenses ''Character
> makePrisms ''Discipline

> gold :: Lens' Inventory Int
> gold f inventory = (\ng -> inventory { _gold = fromIntegral ng }) <$> f (fromIntegral (_gold inventory))
> {-# INLINE gold #-}

> meals :: Lens' Inventory Int
> meals f inventory = (\ng -> inventory { _meals = fromIntegral ng }) <$> f (fromIntegral (_meals inventory))
> {-# INLINE meals #-}

> singleItems :: Lens' Inventory Word32
> singleItems f inventory = (\ni -> inventory { _singleItems = ni }) <$> f (_singleItems inventory)
> {-# INLINE singleItems #-}

> hasItem :: Item -> Inventory -> Bool
> hasItem i inv =
>   case i of
>      Gold -> inv ^. gold > 0
>      Meal -> inv ^. meals > 0
>      _ -> testBit (inv ^. singleItems) (fromEnum i)
>
> addItem :: Item -> Int -> Inventory -> Inventory
> addItem i count inv
>   | count < 0 = delItem i (negate count) inv
>   | count == 0 = inv
>   | otherwise = case i of
>                   Gold -> inv & gold +~ fromIntegral count
>                   Meal -> inv & meals +~ fromIntegral count
>                   _    -> inv & singleItems %~ flip setBit (fromEnum i)
>
> delItem :: Item -> Int -> Inventory -> Inventory
> delItem i count inv
>   | count < 0 = addItem i (negate count) inv
>   | count == 0 = inv
>   | otherwise = case i of
>                   Gold -> inv & gold -~ fromIntegral count
>                   Meal -> inv & meals -~ fromIntegral count
>                   _    -> inv & singleItems %~ flip clearBit (fromEnum i)

> delItemSlot :: [Slot] -> Inventory -> Inventory
> delItemSlot = flip (foldl' delSlot)
>   where delSlot inv s = case s of
>                               PouchSlot -> inv & gold .~ 0
>                               SpecialSlot -> removeAll inv [Helmet, ChainMail, TicketVol2, PasswordVol2, DocumentsVol2, WhitePassVol2, RedPassVol2, SealHammerdalVol2, Shield]
>                               WeaponSlot -> removeAll inv (map Weapon [minBound .. maxBound])
>                               BackpackSlot -> removeAll (inv & meals .~ 0) [HealingPotion, Laumspur, PotentPotion]
>         removeAll = foldl' (\inv itm -> delItem itm 1 inv)

> items :: Inventory -> [(Item, Int)]
> items inventory@(Inventory _ gld mls) = filter ( (> 0) . snd )
>       ( (Gold, fromIntegral gld) : (Meal, fromIntegral mls) :
>         [ (item, if hasItem item inventory then 1 else 0) | item <- standardItems ] )
>  where
>    standardItems = filter (`notElem` [Gold, Meal]) [minBound .. maxBound]

> getWeapons :: Inventory -> [Weapon]
> getWeapons inventory = filter (\w -> hasItem (Weapon w) inventory) [minBound .. maxBound]

> data UsedWeapon = WithSkill Weapon
>                 | WithoutSkill Weapon
>                 | NoWeapon
>                 deriving (Show, Eq)

> usedWeapon :: CharacterConstant -> CharacterVariable -> UsedWeapon
> usedWeapon cconstant cvariable
>    | hasItem (Weapon Sommerswerd) inventory =
>           if any (`elem` wskills) [Sword, ShortSword, BroadSword]
>                       then WithSkill Sommerswerd
>                       else WithoutSkill Sommerswerd
>    | otherwise = case filter ((`hasItem` inventory) . Weapon) wskills of
>                    (x:_) -> WithSkill x
>                    [] -> if null weapons
>                               then NoWeapon
>                               else WithoutSkill (head weapons)
>  where
>    inventory = cvariable ^. equipment
>    wskills = cconstant ^.. discipline . traverse . _WeaponSkill
>    weapons = getWeapons inventory
