{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding (elements)
import Control.Monad
import Data.Aeson (decode, encode)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Ratio
import qualified Data.Set as S
import LoneWolf.Book02 (chapters)
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Choices (flattenDecision)
import LoneWolf.Combat
import LoneWolf.Data
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Simplify (extractMultiFight)
import LoneWolf.Solve
import qualified LoneWolf.StateSelector as SS
import qualified SimpleSolver as S
import Solver
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

aitem :: Gen Item
aitem = arbitraryBoundedEnum

uitem :: Gen Item
uitem = suchThat aitem (`notElem` [Gold, Meal, Laumspur])

nstep :: Gen NextStep
nstep =
  oneof
    [ HasLost <$> elements [1 .. 350],
      HasWon <$> cvar,
      NewChapter <$> elements [1 .. 350] <*> cvar
    ]

cvar :: Gen CharacterVariable
cvar = mkCharacter <$> elements [1 .. 25] <*> inventory

inventory :: Gen Inventory
inventory = foldr (`addItem` 1) emptyInventory <$> listOf sitem
  where
    sitem = arbitraryBoundedEnum

proba :: Gen Proba
proba = elements (map (/ 100) [0 .. 100])

addProba :: [a] -> Gen (Probably a)
addProba [] = return []
addProba lst = do
  probas <- suchThat (replicateM (length lst) proba) ((/= 0) . sum)
  let sprobas = sum probas
      normalized = map (/ sprobas) probas
  return (zip lst normalized)

solution :: Gen (Solution Int Int)
solution = do
  let lsolution = frequency [(1, solution), (20, pure LeafLost), (5, Leaf <$> proba <*> arbitrary)]
  aleaves <- suchThat (listOf lsolution) (\l -> not (null l) && length l < 10)
  leaves <- addProba aleaves
  Node <$> arbitrary <*> arbitrary <*> pure (mkPScore leaves) <*> pure leaves

sumScore :: [(Solution a b, Proba)] -> Rational
sumScore = sum . map (\(s, p) -> getSolScore s * p)

main :: IO ()
main = hspec $ do
  let solveLW e b c v = fst (solveLWsString (\_ _ -> 1) Book01 e b c v)
  let defConstant = CharacterConstant 25 15 [] Book01
      defVariable = mkCharacter 25 (addItem (Weapon BroadSword) 1 emptyInventory)
      defCombat = FightDetails "def" 28 30 []
  describe "Item enum instance" $ do
    it "fromEnum" $ map fromEnum [minBound .. maxBound :: Item] `shouldBe` [0 .. 47]
    it "toEnum" $ map toEnum [0 .. 47] `shouldBe` [minBound .. maxBound :: Item]
    it "toEnum . fromEnum == id" $ forAll aitem $ \i -> toEnum (fromEnum i) == i
  describe "Flags" $ do
    prop "flags fit in a 32 bit word" $ fromEnum (maxBound :: Flag) < 32
  describe "Inventory" $ do
    prop "empty Inventory has no item" $ forAll aitem $ \i -> not (hasItem i emptyInventory)
    prop "single item Inventory" $ forAll aitem $ \i -> items (addItem i 1 emptyInventory) == [(i, 1)]
    prop "multi item inventory" $ forAll inventory $ \i -> inventoryFromList (items i) == i
  describe "countable items" $ do
    let citemsinfo = do
          uitms <- map (,1) . S.toList . S.fromList <$> listOf uitem
          g <- choose (0, 50)
          ls <- choose (0, 7)
          ml <- choose (0, 7)
          pure ([(Gold, g) | g > 0] ++ [(Laumspur, ls) | ls > 0] ++ [(Meal, ml) | ml > 0] ++ uitms)
    prop "check" $ forAll citemsinfo $ \lst -> S.fromList (items (inventoryFromList lst)) `shouldBe` S.fromList lst
  describe "mkCharacter" $ do
    let cinfo = do
          uitms <- map (,1) . S.toList . S.fromList <$> listOf uitem
          uitms2 <- map (,1) . S.toList . S.fromList <$> listOf uitem
          e <- Endurance <$> choose (0, 40)
          uflags <- S.toList . S.fromList <$> listOf arbitraryBoundedEnum
          g <- choose (0, 50)
          let acvar = mkCharacter e (inventoryFromList uitms) & equipment %~ addItem Gold g & setFlags uflags & prevequipment .~ inventoryFromList uitms2
          pure (uitms, uitms2, e, uflags :: [Flag], g, acvar)
    prop "frm endurance" $
      forAll cinfo $ \(_, _, e, _, _, acvar) -> acvar ^. curendurance `shouldBe` e
    prop "frm items" $
      forAll cinfo $ \(itms, _, _, _, g, acvar) ->
        let eitems = S.fromList ([(Gold, g) | g > 0] ++ itms)
            aitems = S.fromList (items (acvar ^. equipment))
         in aitems `shouldBe` eitems
    prop "frm items 2" $
      forAll cinfo $ \(_, itms2, _, _, _, acvar) ->
        let eitems = S.fromList itms2
            aitems = S.fromList (items (acvar ^. prevequipment))
         in aitems `shouldBe` eitems
    prop "frm flags" $
      forAll cinfo $ \(_, _, _, flgs, _, acvar) ->
        let aflags = S.fromList (allFlags acvar)
         in aflags `shouldBe` S.fromList flgs
  describe "getRatio" $ do
    it "default combat" $ getRatio defConstant defVariable defCombat `shouldBe` -13
    it "no weapons (normal)" $ getRatio defConstant (defVariable & equipment %~ delItem (Weapon BroadSword) 1) defCombat `shouldBe` -17
    it "no weapons (special)" $ getRatio defConstant defVariable (defCombat & fightMod .~ [BareHanded]) `shouldBe` -17
    it "good weapon" $ getRatio (defConstant & discipline .~ [WeaponSkill BroadSword]) defVariable defCombat `shouldBe` -11
    it "mindblast" $ getRatio (defConstant & discipline .~ [MindBlast]) defVariable defCombat `shouldBe` -11
    it "mindblast (countered)" $ getRatio (defConstant & discipline .~ [MindBlast]) defVariable (defCombat & fightMod .~ [MindblastImmune]) `shouldBe` -13
    it "shield" $ getRatio defConstant (defVariable & equipment %~ addItem Shield 1) defCombat `shouldBe` -11
    it "sommerswerd" $ getRatio defConstant (defVariable & equipment %~ addItem (Weapon Sommerswerd) 1) defCombat `shouldBe` -5
    it "sommerswerd + skill" $ getRatio (defConstant & discipline .~ [WeaponSkill BroadSword]) (defVariable & equipment %~ addItem (Weapon Sommerswerd) 1) defCombat `shouldBe` -3
  describe "fight" $ do
    it "vanilla test" $ fight defConstant defVariable defCombat `shouldMatchList` [(NotEscaped e, p) | (e, p) <- [(0, 19879281 % 20000000), (1, 6883 % 10000000), (2, 8139 % 20000000), (3, 9043 % 10000000), (4, 11197 % 20000000), (5, 649 % 800000), (6, 2719 % 5000000), (7, 91 % 200000), (8, 147 % 500000), (9, 111 % 500000), (10, 253 % 2000000), (11, 87 % 400000), (12, 99 % 1000000), (13, 451 % 2000000), (14, 33 % 250000), (15, 27 % 200000), (16, 1 % 12500), (17, 11 % 200000), (18, 7 % 200000), (19, 1 % 50000), (20, 1 % 200000), (21, 1 % 200000), (22, 1 % 200000), (25, 1 % 100000)]]
  describe "PScore" $ do
    prop "PScore returns a proper score" $ forAll (listOf solution >>= addProba) $ \sols -> getCertain (mkPScore sols) == sumScore sols
  describe "Solver" $ do
    let randomBook =
          [ (1, Chapter "1" "1" (NoDecision (Randomly [(1 / 2, Goto 2), (1 / 2, Goto 3)]))),
            (2, Chapter "w" "w" (NoDecision GameWon)),
            (3, Chapter "w" "w" (NoDecision GameLost))
          ]
    it "Should score a random book properly" $ S.getSolScore (solveLW [5] randomBook defConstant defVariable) `shouldBe` (1 / 2)
    let choiceBook =
          [ (1, Chapter "1" "1" (Decisions [("x", NoDecision (Goto 2)), ("y", NoDecision (Goto 3))])),
            (2, Chapter "w" "w" (NoDecision GameWon)),
            (3, Chapter "w" "w" (NoDecision GameLost))
          ]
    it "Should score a pure choice book properly" $ S.getSolScore (solveLW [5] choiceBook defConstant defVariable) `shouldBe` 1
    let mixedBook =
          [ (1, Chapter "1" "1" (Decisions [("x", NoDecision (Goto 2)), ("y", NoDecision (Goto 3))])),
            (2, Chapter "w" "w" (NoDecision (Randomly [(1 / 3, GameWon), (2 / 3, GameLost)]))),
            (3, Chapter "w" "w" (NoDecision (Randomly [(2 / 3, GameWon), (1 / 3, GameLost)])))
          ]
    it "Should score a mixed choice book properly" $ S.getSolScore (solveLW [5] mixedBook defConstant defVariable) `shouldBe` (2 / 3)
    let fightBook =
          [ ( 1,
              Chapter
                "1"
                "1"
                ( Decisions
                    [ ("x", NoDecision (Fight (FightDetails "hard" 30 30 []) GameWon)),
                      ("y", NoDecision (Fight (FightDetails "easy" 10 10 []) GameWon))
                    ]
                )
            )
          ]
    let fsol = solveLW [5] fightBook defConstant defVariable
    it "Should choose the easiest fight" $ S._desc fsol `shouldBe` "y"
  describe "Generated chapters" $ do
    it "Auto random should be consistent" $
      let lst = map fst (filter (wrongRandom . snd) chapters)
          wrongRandom (Chapter _ _ pc) = case pc ^? _NoDecision . _Randomly of
            Nothing -> False
            Just l -> sum (map fst l) /= 1
       in lst `shouldBe` []
  describe "Multifights simplifier" $
    it "seems to work" $ do
      let book =
            [ (296, Chapter "296" "lala" (EvadeFight 0 88 fd1 (Fight fd1 (Fight fd2 (Goto 221))))),
              (12, Chapter "12" "xx" (NoDecision (Fight fd3 (Fight fd4 (Goto 12))))),
              (50, Chapter "50" "xx" (NoDecision (Fight fd4 (Goto 33))))
            ]
          fd1 = FightDetails "F1" 13 22 []
          fd2 = FightDetails "F2" 12 21 []
          fd3 = FightDetails "F3" 11 20 []
          fd4 = FightDetails "F4" 10 19 []
      M.fromList (extractMultiFight book)
        `shouldBe` M.fromList
          [ (296, Chapter "296" "lala" (EvadeFight 0 88 fd1 (Simple [SetFlag HadCombat] (Goto 297)))),
            (297, Chapter "297" "Dummy fight chapter 297" (NoDecision (Fight (fd1 & fightMod .~ [MultiFight]) (Goto 298)))),
            (298, Chapter "298" "Dummy fight chapter 298" (NoDecision (Fight fd2 (Goto 299)))),
            (299, Chapter "299" "Last fight chapter 299" (AfterCombat (NoDecision (Simple [SetFlag HadCombat] (Goto 221))))),
            (12, Chapter "12" "xx" (NoDecision (Simple [SetFlag HadCombat] (Goto 300)))),
            (300, Chapter "300" "Dummy fight chapter 300" (NoDecision (Fight (fd3 & fightMod .~ [MultiFight]) (Goto 301)))),
            (301, Chapter "301" "Dummy fight chapter 301" (NoDecision (Fight fd4 (Goto 302)))),
            (302, Chapter "302" "Last fight chapter 302" (AfterCombat (NoDecision (Simple [SetFlag HadCombat] (Goto 12))))),
            (50, Chapter "50" "xx" (NoDecision (Simple [SetFlag HadCombat] (Goto 303)))),
            (303, Chapter "303" "Dummy fight chapter 303" (NoDecision (Fight fd4 (Goto 304)))),
            (304, Chapter "304" "Last fight chapter 304" (AfterCombat (NoDecision (Simple [SetFlag HadCombat] (Goto 33)))))
          ]
  describe "Character selector" $
    forM_
      [ ("chapter 12", SS.P (SS.InChapter 12)),
        ("Sword && special 3", SS.And (SS.P (SS.HasItem (Weapon Sword))) (SS.P (SS.HasItem sealHammerdalVol2)))
      ]
      $ \(i, e) ->
        it i (SS.parseSelector i `shouldBe` Right e)
  describe "Evasion" $ do
    let rawfd = FightDetails "F1" 20 20 []
        fdesc n = EvadeFight n 88 rawfd (Goto 2)
        cconst = CharacterConstant 25 15 [] Book01
        scvar = mkCharacter 25 (inventoryFromList [(Weapon ShortSword, 1)])
        stepper n = step id order schapters cconst (NewChapter 1 scvar)
          where
            schapters = IM.fromList [(1, Chapter "1" "dummy" (fdesc n)), (2, Chapter "2" "dummy" (NoDecision GameLost)), (88, Chapter "88" "dummy" (NoDecision GameWon))]
            order = orderChapters Book01 schapters
    it "Properly flattened" $ do
      flattenDecision id cconst scvar (fdesc 0) `shouldBe` [(["no evasion"], Fight rawfd (Goto 2)), (["evasion"], Fight (FightDetails "F1" 20 20 [Evaded 88]) (Goto 2))]
      flattenDecision id cconst scvar (fdesc 2) `shouldBe` [(["no evasion"], Fight rawfd (Goto 2)), (["evasion"], Fight (FightDetails "F1" 20 20 [Timed 2 (Evaded 88)]) (Goto 2))]
    it "Fight result" $ do
      let fd = FightDetails "F1" 20 20 [Evaded 88]
      getRatio cconst scvar fd `shouldBe` (-5)
      fight cconst scvar fd
        `shouldBe` [ (HasEscaped 88 19, 1 % 5),
                     (HasEscaped 88 20, 1 % 5),
                     (HasEscaped 88 21, 1 % 5),
                     (HasEscaped 88 22, 1 % 10),
                     (HasEscaped 88 23, 1 % 10),
                     (HasEscaped 88 25, 1 % 5)
                   ]
    it "Ratio" $ getRatio cconst scvar rawfd `shouldBe` (-5)
    it "Escapes after a round" $ do
      let r = stepper 1
      length r `shouldBe` 2
      let healthloss = [6, 6, 5, 5, 4, 4, 3, 2, 0, 0] -- combat ratio = -5
          expectedResult = regroup $ do
            l1 <- healthloss
            l2 <- healthloss
            pure (NewChapter 88 (scvar & curendurance -~ (l1 + l2) & flag HadCombat .~ True), 1 % 100)
      case filter ((== "evasion") . fst) r of
        [x] -> x `shouldBe` ("evasion", expectedResult)
        _ -> fail "no evasion?"
  describe "Fight round" $ do
    it "Standard fight" $ do
      let r = fightRound defConstant defVariable defCombat
      S.fromList r `shouldBe` S.fromList [((0, 30), 1 % 5), ((17, 30), 1 % 5), ((18, 29), 1 % 10), ((19, 28), 1 % 10), ((20, 27), 1 % 10), ((21, 26), 1 % 10), ((22, 25), 1 % 10), ((25, 24), 1 % 10)]
  describe "Histostats" $ do
    it "Encode/decode" $ do
      let d1 :: DecisionStat
          d1 = DecisionStat (singletonbag 45) (singletonbag 3) (singletonbag 8) (singletonbag (GoldWin 3 4)) (singletonbag 8)
      decode (encode d1) `shouldBe` Just d1
