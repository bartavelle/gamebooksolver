{-
This module holds the base types for representing the game state, and specifically the player state. This is a literate Haskell file!

Language extensions, imports
----------------------------

Nothing much to say here, except perhaps the not that common option `DeriveDataTypeable`, which will turn out to be very handy for using `biplate`.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module LoneWolf.Character where

import Codec.Serialise (Serialise)
import Control.DeepSeq
import Control.Lens
import Data.Aeson (FromJSON, FromJSONKey, Options (fieldLabelModifier), ToJSON (toJSON), ToJSONKey, defaultOptions, genericParseJSON, genericToJSON, withText)
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Bits
import Data.Bits.Lens (bitAt)
import Data.Data
import qualified Data.Function.Memoize as M
import Data.Hashable
import Data.Int (Int16)
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Word
import GHC.Generics
import Text.Read (readMaybe)

{-
Character sheet
---------------

A character is made up of two parts : one is constant, decided at the beginning of the game book. This is an oversimplification, as the Lone Wolf rules support an experience counter that can increase the combat skill of the player.
As a simplification, I have decided to ignore it.
-}
data Character = Character
  { _constantData :: CharacterConstant,
    _variableData :: CharacterVariable
  }
  deriving (Generic, Eq, Show)

-- Book id
data Book = Book01 | Book02 | Book03 | Book04 | Book05 deriving (Show, Generic, Eq, Ord, Read, Enum, Bounded)

instance ToJSON Book

instance FromJSON Book

instance Serialise Book

{-
In the constant part, the combat skill and endurance are randomly determined when the adventure begins. The list of disciplines is choosen by the player.
-}
newtype CombatSkill = CombatSkill {getCombatSkill :: Int}
  deriving (Show, Eq, Read, Num, Typeable, Data, Ord, Integral, Real, Enum, Generic, Bits, ToJSON, FromJSON, Serialise, M.Memoizable)

newtype Endurance = Endurance {getEndurance :: Int16}
  deriving (Show, Eq, Read, Num, Typeable, Data, Ord, Integral, Real, Enum, Generic, Bits, ToJSON, FromJSON, Serialise, Hashable, NFData, Bounded, FromJSONKey, ToJSONKey)

instance M.Memoizable Endurance where
  memoize = M.memoizeFinite

data CharacterConstant = CharacterConstant
  { _maxendurance :: !Endurance,
    _combatSkill :: !CombatSkill,
    _discipline :: [Discipline],
    _bookid :: Book
  }
  deriving (Generic, Eq, Show, Read)

instance FromJSON CharacterConstant

instance ToJSON CharacterConstant

instance Serialise CharacterConstant

{-
The variable part holds the player inventory, and current health points.
-}

data CharacterVariable = CharacterVariable
  { _curendurance :: !Endurance,
    _flags :: !Flags,
    _cequipment :: !Inventory,
    _cprevequipment :: !Inventory
  }
  deriving (Generic, Eq, Ord, Show)

instance Hashable CharacterVariable

instance NFData CharacterVariable

instance ToJSON CharacterVariable where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON CharacterVariable where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance Serialise CharacterVariable

{- Character flags as bits -}
newtype Flags = Flags {getFlags :: Word32} deriving (Eq, Ord, Num, Bits, Hashable, NFData, ToJSON, FromJSON, Serialise, FromJSONKey, ToJSONKey)

instance Show Flags where
  show flgs = show $ filter (\f -> view (bitAt (fromEnum f)) flgs) ([minBound .. maxBound] :: [Flag])

-- newtype CharacterVariable = CharacterVariable {getCharacterVariable :: Word64}
--   deriving (Generic, Eq, Bits, Ord, Hashable, NFData, ToJSON, FromJSON, Serialise)

mkCharacter :: Endurance -> Inventory -> CharacterVariable
mkCharacter e i = CharacterVariable e 0 i 0

curendurance :: Lens' CharacterVariable Endurance
curendurance f (CharacterVariable e flgs i p) = (\e' -> CharacterVariable e' flgs i p) <$> f e
{-# INLINE curendurance #-}

equipment :: Lens' CharacterVariable Inventory
equipment f (CharacterVariable e flgs i p) = (\i' -> CharacterVariable e flgs i' p) <$> f i
{-# INLINE equipment #-}

prevequipment :: Lens' CharacterVariable Inventory
prevequipment f (CharacterVariable e flgs i p) = CharacterVariable e flgs i <$> f p
{-# INLINE prevequipment #-}

flags :: Lens' CharacterVariable Flags
flags f (CharacterVariable e flgs i p) = (\flgs' -> CharacterVariable e flgs' i p) <$> f flgs
{-# INLINE flags #-}

newtype Inventory = Inventory {getInventory :: Word64}
  deriving (Generic, Eq, Bits, Ord, Num, Hashable, NFData, ToJSON, FromJSON, Serialise, FromJSONKey, ToJSONKey)

instance Show Inventory where
  show i = "(inventoryFromList " ++ show (items i) ++ ")"

emptyInventory :: Inventory
emptyInventory = Inventory 0

data Flag
  = PermanentSkillReduction -- happens in book 01
  | StrengthPotionActive
  | FoughtElix -- happens in book 04, used in book 05
  | LimbDeath -- book 05
  | ReceivedCrystalStarPendant -- happens in book 01, checked in book 05
  | Knowledge01
  | Knowledge02
  | Knowledge03
  | Knowledge04
  | Special01
  | Special02
  | Special03
  | Special04
  | Poisonned2
  | HadCombat
  | PermanentSkillReduction2 -- happens in book 02
  deriving (Show, Eq, Ord, Enum, Bounded, Data, Read, Generic)

instance ToJSON Flag

instance FromJSON Flag

instance Serialise Flag

flag :: Flag -> Lens' CharacterVariable Bool
flag flg = flags . bitAt (fromEnum flg)

setFlags :: Foldable t => t Flag -> CharacterVariable -> CharacterVariable
setFlags flgs cvar = foldl' setFlag cvar flgs
  where
    setFlag c f = c & flag f .~ True

allFlags :: CharacterVariable -> [Flag]
allFlags cvar = filter (\f -> cvar ^. flag f) [minBound .. maxBound]

{-
Five disciplines must be picked before the game starts. Most of them can turn out to be useful during the adventure, but the `WeaponSkill` deserves a special treatment.
During the adventure, magic weapons can be found, in the form of a sword and a spear.
As a result, if a `WeaponSkill` should be picked, it should be related to swords or spears. There are several types of swords, but I'll choose the `ShortSword` to start with, as it's cheaper than the others and easier to procure.
-}
data Discipline
  = Camouflage
  | Hunting
  | SixthSense
  | Tracking
  | Healing
  | WeaponSkill Weapon
  | MindShield
  | MindBlast
  | AnimalKinship
  | MindOverMatter
  deriving (Show, Eq, Generic, Read, Ord, Typeable, Data)

instance ToJSON Discipline

instance FromJSON Discipline

instance Serialise Discipline

{-
Two weapons are selected by the player before the story starts. The `MagicSpear` and `Sommerswerd` can't be picked when the game begins, but can be found during the adventure.
Given that these weapons are magic and more powerful than their mundane counterparts, and given the preference for `WeaponSkill` selection, The `ShortSword` and `Spear` will always be picked at the start of the adventure.
-}
data Weapon
  = Dagger
  | Spear
  | Mace
  | ShortSword
  | Warhammer
  | Sword
  | Axe
  | Quarterstaff
  | BroadSword
  | MagicSpear
  | Sommerswerd
  deriving (Show, Eq, Generic, Ord, Enum, Bounded, Read, Typeable, Data)

instance ToJSON Weapon

instance FromJSON Weapon

instance Serialise Weapon

newtype GenCounter = GenCounter Word8 deriving (Show, Eq, Ord, Read, Data, Enum, Num, FromJSON, ToJSON, Serialise, Integral, Real)

instance Bounded GenCounter where
  minBound = 0
  maxBound = 11

{-
Not all items that are referenced in the books are described here.
I decided to let go of all items that were not useful.
-}
data Item
  = Weapon !Weapon
  | Backpack
  | Helmet
  | Shield
  | ChainMail
  | Potion2Hp
  | Potion4Hp
  | Potion5Hp
  | Potion6Hp
  | StrengthPotion
  | GenSpecial GenCounter
  | GenBackpack GenCounter
  | Meal
  | Gold
  | Laumspur
  | SilverHelm -- book 03, combatskill + 1
  deriving (Show, Eq, Generic, Ord, Read, Typeable, Data)

itemNames :: M.Map Book (M.Map String Item)
itemNames =
  M.fromList
    [ ( Book01,
        M.fromList
          [ ("Vordak Gem", GenSpecial 0),
            ("Golden Key", GenSpecial 1),
            ("Silver Key", GenSpecial 2),
            ("crystal Star Pendant", GenSpecial 3)
          ]
      ),
      ( Book03,
        M.fromList
          [ ("Ornate Silver Key", ornateSilverKeyB03),
            ("Blue Stone Disc", blueStoneDiscB03),
            ("Blue Stone Triangle", blueStoneTriangleB03),
            ("Rope", ropeB03),
            ("Potion of Black Graveweed", graveWeedB03),
            ("Potion of Green Gallowbrush", gallowBrushB03),
            ("Firesphere", fireSphereB03),
            ("Diamond", diamondB03),
            ("Effigy", effigyB03),
            ("Red Potion of Laumspur", Potion5Hp),
            ("Sommerswerd", Weapon Sommerswerd),
            ("Glowing Crystal", glowingCrystalB03)
          ]
      ),
      ( Book04,
        M.fromList
          [ ("Onyx Medallion", onyxMedallion),
            ("Brass Key", brassKeyB04),
            ("Firesphere", fireSphereB04),
            ("Sommerswerd", Weapon Sommerswerd),
            ("Flask of Holy Water", flaskHolyWaterB04),
            ("Iron Key", ironKeyB04),
            ("Scroll", scrollB04),
            ("Rope", ropeB04),
            ("Torch", torchB04),
            ("Tinderbox", tinderBoxB04)
          ]
      ),
      ( Book05,
        M.fromList
          [ ("Gaoler's Keys", gaolerKeyB05),
            ("Copper Key", copperKeyB05),
            ("Black Crystal Cube", tinctureGraveweed),
            ("Blowpipe and Sleep Dart", blowpipeSleepDart),
            ("Oede herb", oedeHerb),
            ("Blue Stone Triangle", blueStoneTriangleB05),
            ("Sash", sashB05),
            ("Black Crystal Cube", blackCubeB05),
            ("Brass Whistle", brassWhistleB05),
            -- from other books:
            ("Rope", ropeB05),
            ("Prism", prismB05), -- can be retrieved anyway
            ("FireSphere", fireSphereB05), -- useless
            ("Sommerswerd", Weapon Sommerswerd),
            ("Onyx Medallion", onyxMedallion) -- useful if you do not have animal kinship
          ]
      )
    ]

itemIds :: M.Map Book (M.Map Item String)
itemIds = fmap rev itemNames
  where
    rev = M.fromList . map swap . M.toList

showItem :: Book -> Item -> String
showItem bk i = case (i, M.lookup bk itemIds >>= M.lookup i) of
  (Weapon w, _) -> show w
  (_, Just d) -> d
  _ -> show i

-- book 01
vordakGem :: Item
vordakGem = GenSpecial 0

goldenKey :: Item
goldenKey = GenSpecial 1

silverKey :: Item
silverKey = GenSpecial 2

crystalStarPendantB01 :: Item
crystalStarPendantB01 = GenSpecial 3

torch :: Item
torch = GenBackpack 0

-- book 02

passwordVol2 :: Flag
passwordVol2 = Knowledge01

ticketVol2 :: Item
ticketVol2 = GenSpecial 0

crystalStarPendantB02 :: Item
crystalStarPendantB02 = GenSpecial 1

documentsVol2 :: Item
documentsVol2 = GenSpecial 2

sealHammerdalVol2 :: Item
sealHammerdalVol2 = GenSpecial 3

whitePassVol2 :: Item
whitePassVol2 = GenSpecial 4

redPassVol2 :: Item
redPassVol2 = GenSpecial 5

-- book 03
killKL11 :: Flag -- killed first kalkoth
killKL11 = Special01

killKL10 :: Flag -- killed second kalkoth
killKL10 = Special02

killKL8 :: Flag -- killed third kalkoth
killKL8 = Special03

baknarOilB03 :: Flag
baknarOilB03 = Special04

visitedGraveyardAncientsB03 :: Flag
visitedGraveyardAncientsB03 = Knowledge01

ornateSilverKeyB03 :: Item
ornateSilverKeyB03 = GenSpecial 0

blueStoneDiscB03 :: Item
blueStoneDiscB03 = GenSpecial 1

blueStoneTriangleB03 :: Item
blueStoneTriangleB03 = GenSpecial 2

fireSphereB03 :: Item
fireSphereB03 = GenSpecial 3

glowingCrystalB03 :: Item
glowingCrystalB03 = GenSpecial 4

diamondB03 :: Item
diamondB03 = GenSpecial 5

effigyB03 :: Item
effigyB03 = GenSpecial 6

ropeB03 :: Item
ropeB03 = GenBackpack 0

gallowBrushB03 :: Item
gallowBrushB03 = GenBackpack 1

graveWeedB03 :: Item
graveWeedB03 = GenBackpack 2

-- book 04

captainDvalSword :: Flag
captainDvalSword = Special01

visitedGornCove :: Flag
visitedGornCove = Special02

brassKeyB04 :: Item
brassKeyB04 = GenSpecial 0

fireSphereB04 :: Item -- from book 03
fireSphereB04 = GenSpecial 1

flaskHolyWaterB04 :: Item
flaskHolyWaterB04 = GenBackpack 0

torchB04 :: Item
torchB04 = GenBackpack 1

scrollB04 :: Item
scrollB04 = GenSpecial 2

tinderBoxB04 :: Item
tinderBoxB04 = GenBackpack 2

ironKeyB04 :: Item
ironKeyB04 = GenSpecial 2

pickAB04 :: Item
pickAB04 = GenBackpack 3

pickBB04 :: Item
pickBB04 = GenBackpack 4

ropeB04 :: Item
ropeB04 = GenBackpack 5

-- onyxMedallion = GenSpecial 11

-- book 05
blowpipeSleepDart :: Item -- in this book
blowpipeSleepDart = GenSpecial 1

fireSphereB05 :: Item -- from book 03
fireSphereB05 = GenSpecial 2

copperKeyB05 :: Item -- in this book
copperKeyB05 = GenSpecial 3

gaolerKeyB05 :: Item -- in this book
gaolerKeyB05 = GenSpecial 4

prismB05 :: Item -- in this book, but also in book 03
prismB05 = GenSpecial 5

bronzeDoorSecretB05 :: Flag -- this book
bronzeDoorSecretB05 = Knowledge01

jewelledMaceB05 :: Flag -- this book
jewelledMaceB05 = Special01

needOedeB05 :: Flag -- this book
needOedeB05 = LimbDeath

ropeB05 :: Item -- book 01
ropeB05 = GenBackpack 0

tinctureGraveweed :: Item -- this book
tinctureGraveweed = GenBackpack 1

oedeHerb :: Item -- this book
oedeHerb = GenBackpack 2

sashB05 :: Item -- this book
sashB05 = GenSpecial 6

brassWhistleB05 :: Item -- this book, useless
brassWhistleB05 = GenSpecial 7

blackCubeB05 :: Item -- this book
blackCubeB05 = GenSpecial 8

blueStoneTriangleB05 :: Item -- from book 03, useless
blueStoneTriangleB05 = GenSpecial 9

onyxMedallion :: Item -- from book 04
onyxMedallion = GenSpecial 11

instance ToJSON Item where
  toJSON i = case i of
    Weapon w -> toJSON w
    GenSpecial n -> toJSON ("S" ++ show n)
    GenBackpack n -> toJSON ("B" ++ show n)
    _ -> toJSON (show i)

instance FromJSON Item where
  parseJSON = withText "Item" $ \s -> case T.unpack s of
    'S' : nums | Just n <- readMaybe nums -> pure (GenSpecial n)
    'B' : nums | Just n <- readMaybe nums -> pure (GenBackpack n)
    ss
      | Just x <- readMaybe ss -> pure x
      | Just x <- readMaybe ss -> pure (Weapon x)
      | otherwise -> fail ":("

instance Serialise Item

instance Bounded Item where
  minBound = Backpack
  maxBound = Meal

instance Enum Item where
  fromEnum x = case x of
    Backpack -> 0
    Helmet -> 1
    Shield -> 2
    ChainMail -> 3
    Potion2Hp -> 4
    Potion4Hp -> 5
    Potion5Hp -> 6
    Potion6Hp -> 7
    StrengthPotion -> 8
    Weapon w -> 9 + fromEnum w
    GenSpecial n -> 20 + fromIntegral n
    GenBackpack n -> 32 + fromIntegral n
    SilverHelm -> 44
    Laumspur -> 45
    Gold -> 46
    Meal -> 47

  -- 11 weapons

  toEnum n = case n of
    0 -> Backpack
    1 -> Helmet
    2 -> Shield
    3 -> ChainMail
    4 -> Potion2Hp
    5 -> Potion4Hp
    6 -> Potion5Hp
    7 -> Potion6Hp
    8 -> StrengthPotion
    44 -> SilverHelm
    45 -> Laumspur
    46 -> Gold
    47 -> Meal
    _ | n >= 9 && n < 20 -> Weapon (toEnum (n - 9))
    _ | n >= 20 && n < 32 -> GenSpecial (fromIntegral (n - 20))
    _ | n >= 32 && n < 44 -> GenBackpack (fromIntegral (n - 32))
    _ -> error ("oob item: " ++ show n)

data Slot
  = WeaponSlot
  | BackpackSlot
  | SpecialSlot
  | PouchSlot
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Slot where
  toJSON = genericToJSON defaultOptions

slotSize :: Slot -> Maybe Int
slotSize s = case s of
  WeaponSlot -> Just 2
  BackpackSlot -> Just 8
  _ -> Nothing

itemSlot :: Item -> Slot
itemSlot (Weapon _) = WeaponSlot
itemSlot Helmet = SpecialSlot
itemSlot Meal = BackpackSlot
itemSlot ChainMail = SpecialSlot
itemSlot Potion2Hp = BackpackSlot
itemSlot Potion4Hp = BackpackSlot
itemSlot Potion5Hp = BackpackSlot
itemSlot Potion6Hp = BackpackSlot
itemSlot Gold = PouchSlot
itemSlot Laumspur = BackpackSlot
itemSlot Backpack = SpecialSlot
itemSlot Shield = SpecialSlot
itemSlot SilverHelm = SpecialSlot
itemSlot (GenSpecial _) = SpecialSlot
itemSlot (GenBackpack _) = BackpackSlot
itemSlot StrengthPotion = BackpackSlot

laumspur :: Lens' Inventory Int
laumspur f (Inventory w) = (\ng -> Inventory ((w .&. 0x0fffffffffffffff) .|. (fromIntegral ng `shiftL` 60))) <$> f (fromIntegral ((w `shiftR` 60) .&. 0xf))
{-# INLINE laumspur #-}

gold :: Lens' Inventory Int
gold f (Inventory w) = (\ng -> Inventory ((w .&. 0xf00fffffffffffff) .|. (fromIntegral ng `shiftL` 52))) <$> f (fromIntegral ((w `shiftR` 52) .&. 0xff))
{-# INLINE gold #-}

meals :: Lens' Inventory Int
meals f (Inventory w) = (\nm -> Inventory ((w .&. 0xfff0ffffffffffff) .|. (fromIntegral nm `shiftL` 48))) <$> f (fromIntegral ((w `shiftR` 48) .&. 0xf))
{-# INLINE meals #-}

singleItems :: Lens' Inventory Word64
singleItems f (Inventory w) = (\ni -> Inventory ((w .&. 0xffff000000000000) .|. fromIntegral ni)) <$> f (fromIntegral (w .&. 0xffffffffffff))
{-# INLINE singleItems #-}

hasItem :: Item -> Inventory -> Bool
hasItem i inv =
  case i of
    Gold -> inv ^. gold > 0
    Meal -> inv ^. meals > 0
    Laumspur -> inv ^. laumspur > 0
    _ -> testBit (inv ^. singleItems) (fromEnum i)

hasFlag :: Flag -> CharacterVariable -> Bool
hasFlag flg = view (flag flg)

itemCount :: Item -> Inventory -> Int
itemCount i inv =
  case i of
    Gold -> inv ^. gold
    Meal -> inv ^. meals
    Laumspur -> inv ^. laumspur
    _ -> if testBit (inv ^. singleItems) (fromEnum i) then 1 else 0

addItem :: Item -> Int -> Inventory -> Inventory
addItem i count inv
  | count < 0 = delItem i (negate count) inv
  | count == 0 = inv
  | otherwise = case i of
    Gold -> inv & gold %~ \curgold -> min 50 (curgold + fromIntegral count)
    Meal -> inv & meals +~ fromIntegral count
    Laumspur -> inv & laumspur +~ fromIntegral count
    _ -> inv & singleItems %~ flip setBit (fromEnum i)

delItem :: Item -> Int -> Inventory -> Inventory
delItem i count inv
  | count < 0 = addItem i (negate count) inv
  | count == 0 = inv
  | otherwise = case i of
    Gold -> inv & gold %~ drp
    Meal -> inv & meals %~ drp
    Laumspur -> inv & laumspur %~ drp
    _ -> inv & singleItems %~ flip clearBit (fromEnum i)
  where
    drp cur = max 0 (cur - fromIntegral count)

delItemSlot :: [Slot] -> Inventory -> Inventory
delItemSlot = flip (foldl' delSlot)
  where
    delSlot inv s = case s of
      PouchSlot -> inv & gold .~ 0
      SpecialSlot -> removeAll inv (filter ((== SpecialSlot) . itemSlot) [minBound .. maxBound])
      WeaponSlot -> removeAll inv (map Weapon [minBound .. maxBound])
      BackpackSlot -> removeAll (inv & meals .~ 0 & laumspur .~ 0) (filter ((== BackpackSlot) . itemSlot) [minBound .. maxBound])
    removeAll = foldl' (\inv itm -> delItem itm 1 inv)

items :: Inventory -> [(Item, Int)]
items inventory =
  filter
    ((> 0) . snd)
    ( (Gold, inventory ^. gold) :
      (Meal, inventory ^. meals) :
      (Laumspur, inventory ^. laumspur) :
        [(item, if hasItem item inventory then 1 else 0) | item <- standardItems]
    )
  where
    standardItems = filter (`notElem` [Gold, Meal, Laumspur]) [minBound .. maxBound]

inventoryFromList :: [(Item, Int)] -> Inventory
inventoryFromList = foldr (uncurry addItem) emptyInventory

getWeapons :: Inventory -> [Weapon]
getWeapons inventory = filter (\w -> hasItem (Weapon w) inventory) [minBound .. maxBound]

data UsedWeapon
  = WithSkill Weapon
  | WithoutSkill Weapon
  | NoWeapon
  deriving (Show, Eq)

makeLenses ''CharacterConstant
makeLenses ''Character
makePrisms ''Discipline

usedWeapon :: CharacterConstant -> CharacterVariable -> UsedWeapon
usedWeapon cconstant cvariable
  | hasItem (Weapon Sommerswerd) inventory =
    if any (`elem` wskills) [Sword, ShortSword, BroadSword]
      then WithSkill Sommerswerd
      else WithoutSkill Sommerswerd
  | otherwise = case filter ((`hasItem` inventory) . Weapon) wskills of
    (x : _) -> WithSkill x
    [] ->
      if null weapons
        then NoWeapon
        else WithoutSkill (head weapons)
  where
    inventory = cvariable ^. equipment
    wskills = cconstant ^.. discipline . traverse . _WeaponSkill
    weapons = getWeapons inventory

M.deriveMemoizable ''Weapon
M.deriveMemoizable ''Discipline
M.deriveMemoizable ''Flag