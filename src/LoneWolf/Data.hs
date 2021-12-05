{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This file is used for file generation/interop
module LoneWolf.Data where

import Codec.Serialise (Serialise)
import Control.Lens (makeLenses, to, view, (^.))
import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Word (Word64)
import GHC.Generics (Generic)
import LoneWolf.Chapter (ChapterId, FightDetails)
import LoneWolf.Character
import LoneWolf.Combat (expectedEndurance, winchance)
import LoneWolf.Rules (NextStep (NewChapter))
import Options.Applicative
import SimpleSolver (ChoppedSolution, choppedScore)

data Multistat = Multistat
  { _msdisciplines :: [Discipline],
    _variable :: CVarState,
    _msentries :: [MultistatEntry]
  }
  deriving (Generic, Show)

instance ToJSON Multistat

instance FromJSON Multistat

data MultistatEntry = MultistatEntry
  { _meendurance :: Endurance,
    _mskill :: CombatSkill,
    _mscore :: Double,
    _mratio :: Rational,
    _states :: Int
  }
  deriving (Generic, Show)

instance ToJSON MultistatEntry

instance FromJSON MultistatEntry

data CVarState = CVarState
  { _cvitems :: [(Item, Int)],
    _cvgold :: Int,
    _cvmeals :: Int
  }
  deriving (Generic, Show, Eq)

instance Serialise CVarState

mkchar :: CharacterConstant -> CVarState -> CharacterVariable
mkchar cst (CVarState sitems gld cmeals) = mkCharacter (_maxendurance cst) (inventoryFromList allitems)
  where
    allitems = sitems ++ [(Gold, gld), (Meal, cmeals)]

instance ToJSON CVarState

instance FromJSON CVarState where
  parseJSON = withObject "CVarState" $ \o ->
    CVarState
      <$> (o .: "_cvitems" <|> o .: "_items")
      <*> (o .: "_cvgold" <|> o .: "_gold")
      <*> (o .: "_cvmeals" <|> o .: "_meals")

defaultItems :: [(Item, Int)]
defaultItems = [(Weapon ShortSword, 1), (SealHammerdalVol2, 1), (Shield, 1)]

defaultCVarState :: CVarState
defaultCVarState = CVarState defaultItems 15 0

cvariable :: Parser CVarState
cvariable =
  CVarState
    <$> fmap (cdefaultItems . M.toList . M.fromListWith (+) . map (,1)) (many (option auto (long "item" <> short 'i' <> help "Starting items (default items if empty)")))
    <*> option auto (long "gold" <> help "Starting gold" <> value (_cvgold defaultCVarState))
    <*> option auto (long "meals" <> help "Starting meals" <> value (_cvmeals defaultCVarState))
  where
    cdefaultItems [] = _cvitems defaultCVarState
    cdefaultItems itms = itms

eqcvarstate :: CVarState -> CVarState -> Bool
eqcvarstate (CVarState i1 g1 m1) (CVarState i2 g2 m2) = g1 == g2 && m1 == m2 && M.fromListWith (+) i1 == M.fromListWith (+) i2

makeLenses ''Multistat
makeLenses ''MultistatEntry
makeLenses ''CVarState

data SolDesc = SolDesc
  { _finalchapters :: [ChapterId],
    _ccst :: CharacterConstant,
    _cvar :: CVarState
  }
  deriving (Generic, Eq)

data SolutionDump = SolutionDump SolDesc [(NextStep, ChoppedSolution NextStep)]
  deriving (Generic, Eq)

instance ToJSON SolDesc

instance FromJSON SolDesc

instance Serialise SolDesc

instance ToJSON SolutionDump

instance FromJSON SolutionDump

instance Serialise SolutionDump

discnames :: M.Map Discipline String
discnames =
  M.fromList
    [ (Camouflage, "CA"),
      (Hunting, "HU"),
      (SixthSense, "6S"),
      (Tracking, "TR"),
      (Healing, "HL"),
      (WeaponSkill Spear, "SP"),
      (WeaponSkill ShortSword, "SS"),
      (MindShield, "MS"),
      (MindBlast, "MB"),
      (AnimalKinship, "AK"),
      (MindOverMatter, "MO")
    ]

rdiscnames :: M.Map String Discipline
rdiscnames = M.fromList $ map swap $ M.toList discnames

soldumpSummary :: SolutionDump -> Multistat
soldumpSummary (SolutionDump (SolDesc _ ccst cvar) sol) =
  let nstates = length sol
      firstChapter ns = case ns of
        NewChapter 1 _ _ -> True
        _ -> False
      (_, csol) = case filter (firstChapter . fst) sol of
        x : _ -> x
        _ -> error "More than one first chapter?"
      score = choppedScore csol
   in Multistat (_discipline ccst) cvar [MultistatEntry (_maxendurance ccst) (_combatSkill ccst) (fromRational score) score nstates]

data GoldWin = GoldWin
  { _gold :: Int,
    _win :: Int
  }
  deriving (Show, Eq, Ord)

toGW :: Word64 -> GoldWin
toGW w =
  let (g64, w64) = divMod w 10
   in GoldWin (fromIntegral g64) (fromIntegral w64)

fromGW :: GoldWin -> Word64
fromGW (GoldWin gld win) = fromIntegral gld * 10 + fromIntegral win

data DecisionStat = DecisionStat
  { _dsHp :: !(Bagged Endurance),
    _dsGold :: !(Bagged Int),
    _dsExpectedEndurance :: !(Bagged Endurance), -- scaled between 0/10
    _dsGoldWin :: !(Bagged GoldWin), -- also scalled between 0/10 for winning
    _dsSpecItems :: !(Bagged Inventory)
  }
  deriving (Show, Eq)

instance Semigroup DecisionStat where
  DecisionStat h1 g1 w1 gw1 i1 <> DecisionStat h2 g2 w2 gw2 i2 = DecisionStat (h1 <> h2) (g1 <> g2) (w1 <> w2) (gw1 <> gw2) (i1 <> i2)

instance Monoid DecisionStat where
  mempty = DecisionStat mempty mempty mempty mempty mempty

newtype Bagged k = Bagged {getBag :: M.Map k Int} deriving (ToJSON, Show, Eq)

singletonbag :: Ord k => k -> Bagged k
singletonbag k = Bagged (M.singleton k 1)

instance ToJSON DecisionStat where
  toJSON (DecisionStat chp cgold cwin cgwin citems) =
    object
      [ ("hp", enc fromIntegral chp),
        ("gold", enc fromIntegral cgold),
        ("winning", enc fromIntegral cwin),
        ("goldwin", enc fromGW cgwin),
        ("items", enc getInventory citems)
      ]
    where
      enc :: (k1 -> Word64) -> Bagged k1 -> Value
      enc f = toJSON . M.fromList . map (first f) . M.toList . getBag

instance FromJSON DecisionStat where
  parseJSON = withObject "DecisionStat" $ \o ->
    DecisionStat
      <$> mk fromIntegral "hp" o
      <*> mk fromIntegral "gold" o
      <*> mk fromIntegral "winning" o
      <*> mk toGW "goldwin" o
      <*> mk Inventory "items" o
    where
      mk :: Ord a => (Word64 -> a) -> Text -> Object -> A.Parser (Bagged a)
      mk f t o = Bagged . M.fromList . map (first f) . M.toList <$> o .: t

instance Ord k => Semigroup (Bagged k) where
  Bagged a <> Bagged b = Bagged (M.unionWith (+) a b)

instance Ord k => Monoid (Bagged k) where
  mempty = Bagged mempty

tohisto :: Ord k => M.Map ChapterId (Bagged k) -> M.Map k (M.Map ChapterId Int)
tohisto r = M.fromListWith (M.unionWith (+)) $ do
  (cid, Bagged bg) <- M.toList r
  (bv, bn) <- M.toList bg
  pure (bv, M.singleton cid bn)

csvlines :: CharacterConstant -> Maybe FightDetails -> [(CharacterVariable, ChapterId)] -> String
csvlines ccst mfd entries = unlines (headers : map csvline entries)
  where
    allchapters = S.fromList (map snd entries)
    chapterCorr = M.fromList (zip (S.toList allchapters) [0 :: Int ..])
    allitems = S.toList (foldMap (S.fromList . map fst . items . view equipment . fst) entries)
    csvline :: (CharacterVariable, ChapterId) -> String
    csvline (cv, i) =
      intercalate
        "\t"
        ( show (M.findWithDefault (-1) i chapterCorr) :
          show (cv ^. curendurance . to getEndurance) :
          show (itemCount Meal inv + itemCount Laumspur inv) :
          map (show . (`itemCount` inv)) allitems
            ++ maybe [] (map (show . fromRational @Double) . fightstats) mfd
        )
      where
        inv = cv ^. equipment
        fightstats fd = [winchance ccst cv fd, expectedEndurance ccst cv fd]
    headers = intercalate "\t" ("chapter" : "endurance" : "allmeals" : map show allitems ++ maybe [] (const ["winchance", "expectedendurance"]) mfd)