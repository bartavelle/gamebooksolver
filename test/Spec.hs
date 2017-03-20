module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Lens hiding (elements)
import Data.Ratio

import LoneWolf.Character
import LoneWolf.Chapter
import LoneWolf.Combat
import LoneWolf.Solve
import LoneWolf.Rules (NextStep(..), HadCombat(..))

aitem :: Gen Item
aitem = arbitraryBoundedEnum

uitem :: Gen Item
uitem = suchThat aitem (`notElem` [Gold, Meal])

nstep :: Gen NextStep
nstep = oneof [ pure HasLost, HasWon <$> cvar, NewChapter <$> elements [1..350] <*> cvar <*> elements [DidFight, Didn'tFight] ]

cvar :: Gen CharacterVariable
cvar = mkCharacter <$> elements [0..25] <*> inventory

inventory :: Gen Inventory
inventory = foldr (\i inv -> addItem i 1 inv) emptyInventory <$> listOf sitem
  where
   sitem = oneof [ elements [Backpack .. RedPassVol2]
                 , Weapon <$> arbitraryBoundedEnum
                 ]

main :: IO ()
main = hspec $ do
    let defConstant = CharacterConstant 25 15 []
        defVariable = mkCharacter 25 (addItem (Weapon BroadSword) 1 emptyInventory)
        defCombat   = FightDetails "def" 28 30 []
    describe "Item enum instance" $ do
        it "fromEnum" $ map fromEnum [minBound .. maxBound :: Item] `shouldBe` [0..25]
        it "toEnum" $ map toEnum [0..25] `shouldBe` [minBound .. maxBound :: Item]
        it "toEnum . fromEnum == id" $ forAll aitem $ \i -> toEnum (fromEnum i) == i
    describe "Inventory" $ do
        prop "empty Inventory has no item" $ forAll aitem $ \i -> not (hasItem i emptyInventory)
        prop "single item Inventory" $ forAll aitem $ \i -> items (addItem i 1 emptyInventory) == [(i, 1)]
        prop "multi item inventory" $ forAll inventory $ \i -> inventoryFromList (items i) == i
    describe "getRatio" $ do
        it "default combat" $ getRatio defConstant defVariable defCombat `shouldBe` -13
        it "no weapons (normal)" $ getRatio defConstant (defVariable & equipment %~ delItem (Weapon BroadSword) 1) defCombat `shouldBe` -17
        it "no weapons (special)" $ getRatio defConstant defVariable (defCombat & fightMod .~ [BareHanded]) `shouldBe` -17
        it "good weapon" $ getRatio (defConstant & discipline .~ [WeaponSkill BroadSword]) defVariable defCombat `shouldBe` -11
        it "mindblast" $ getRatio (defConstant & discipline .~ [MindBlast]) defVariable defCombat `shouldBe` -11
        it "mindblast (countered)" $ getRatio (defConstant & discipline .~ [MindBlast]) defVariable (defCombat & fightMod .~ [MindblastImmune]) `shouldBe` -13
        it "shield" $ getRatio defConstant (defVariable & equipment %~ addItem Shield 1) defCombat `shouldBe` -11
        it "sommerswerd" $ getRatio defConstant (defVariable & equipment %~ addItem (Weapon Sommerswerd) 1) defCombat `shouldBe` -3
        it "sommerswerd + skill" $ getRatio (defConstant & discipline .~ [WeaponSkill BroadSword]) (defVariable & equipment %~ addItem (Weapon Sommerswerd) 1) defCombat `shouldBe` -1
    describe "fight" $ do
        it "vanilla test" $ fight defConstant defVariable defCombat `shouldMatchList` [(0,19879281 % 20000000),(1,6883 % 10000000),(2,8139 % 20000000),(3,9043 % 10000000),(4,11197 % 20000000),(5,649 % 800000),(6,2719 % 5000000),(7,91 % 200000),(8,147 % 500000),(9,111 % 500000),(10,253 % 2000000),(11,87 % 400000),(12,99 % 1000000),(13,451 % 2000000),(14,33 % 250000),(15,27 % 200000),(16,1 % 12500),(17,11 % 200000),(18,7 % 200000),(19,1 % 50000),(20,1 % 200000),(21,1 % 200000),(22,1 % 200000),(25,1 % 100000)]
    describe "state memo" $ do
        prop "memo works" $ forAll nstep $ \ns -> fromWord64 (toWord64 ns) == ns
