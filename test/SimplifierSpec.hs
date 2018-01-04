module SimplifierSpec where

import Test.Hspec

import LoneWolf.Book02 (chapters)
import Simplifier
import LoneWolf.Simplify
import LoneWolf.Character
import LoneWolf.Chapter (ChapterId)

import qualified Data.Graph as G
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

priority :: ChapterId -> Int
childs :: ChapterId -> S.Set ChapterId
childsComplete :: ChapterId -> M.Map (S.Set ChapterId) LinkType
(priority, childs, childsComplete) = forSimplification (CharacterConstant 25 25 []) (IM.fromList chapters)

tests :: SpecWith ()
tests = do
  describe "findClosestDominated" $ do
    let fc k = findClosestDominated priority childs (childs k)
    it "Works for 1" $ fc 1 `shouldBe` Just 160
    it "Works for 160" $ fc 160 `shouldBe` Just 268
    it "Works for 273" $ fc 273 `shouldBe` Just 160
  describe "bubbleTree" $ do
    let btree = bubbleTree priority childs
    it "Works for part 1-160" $
      btree 1 160 `shouldBe` Just (G.Node (1, 160) [] )
    it "Works for part 1-300" $
      btree 1 300 `shouldBe` Just (G.Node (1, 300) [G.Node (1, 160) [], G.Node (268, 300) [G.Node (333, 300) []]] )
  describe "findAllPathes" $
    it "Works for part 1-160" $
      findAllPathes childsComplete mempty 1 160 `shouldBe` [[(S.fromList [160],Neutral)],[(S.fromList [273],Neutral),(S.fromList [160],Neutral)]]

foo :: Int -> String
foo = graphMap priority childsComplete childs 1
