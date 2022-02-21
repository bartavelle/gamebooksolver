{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This file is used for file generation/interop
module LoneWolf.Data where

import Codec.Serialise (Serialise)
import Control.Lens
import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Word (Word64)
import GHC.Generics (Generic)
import LoneWolf.Chapter (ChapterId, FightDetails)
import LoneWolf.Character
import LoneWolf.Combat (expectedEndurance, winchance)
import LoneWolf.Rules (NextStep (NewChapter), getMaxHp)
import Numeric (readDec, readHex)
import Options.Applicative
import SimpleSolver (ChoppedSolution, choppedScore)
import Text.Read (readMaybe)

newtype ERatio = ERatio {getERatio :: Rational}

instance FromJSON ERatio where
  parseJSON = withObject "Ratio" $ \m -> haskellratio m <|> radixed m
    where
      haskellratio m = do
        n <- m .: "numerator"
        d <- m .: "denominator"
        pure (ERatio (n % d))
      radixed m = do
        radix <- m .: "radix"
        let readfunction = case (radix :: Int) of
              10 -> readDec
              16 -> readHex
              _ -> error ("invalid base " ++ show radix)
        let rd s = case readfunction (T.unpack s) of
              [(n, "")] -> n
              _ -> error ("Can't parse value " ++ show s)
        vl <- m .: "value"
        let (n, d) = case T.splitOn "/" vl of
              [x] -> (rd x, 1)
              [n', d'] -> (rd n', rd d')
              _ -> error ("invalid value " ++ show vl)
        pure (ERatio (n % d))

instance ToJSON ERatio where
  toJSON (ERatio r) = object [("radix", toJSON @Int 10), ("value", toJSON (show (numerator r) ++ "/" ++ show (denominator r)))]

data Multistat = Multistat
  { _mbook :: Book,
    _msdisciplines :: [Discipline],
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
  { _cvitems :: Maybe [(Item, Int)],
    _cvgold :: Int,
    _cvflags :: [Flag]
  }
  deriving (Generic, Show, Eq)

instance Serialise CVarState

data ChapterAggreg g = ChapterAggreg
  { _citems :: M.Map Inventory g,
    _cflags :: M.Map Flags g,
    _cendurance :: M.Map Endurance g,
    _ctransitions :: M.Map ChapterId g,
    _cscore :: g
  }
  deriving (Generic, Show, Eq, Functor)

data DecisionStats g = DecisionStats
  { _dbookid :: Book,
    _dres :: M.Map ChapterId (ChapterAggreg g),
    _dsttmap :: M.Map ChapterId Word64
  }
  deriving (Generic, Show, Eq, Functor)

instance FromJSON g => FromJSON (ChapterAggreg g) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance FromJSON g => FromJSON (DecisionStats g) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance Semigroup g => Semigroup (ChapterAggreg g) where
  ChapterAggreg i1 f1 e1 t1 s1 <> ChapterAggreg i2 f2 e2 t2 s2 =
    ChapterAggreg
      (M.unionWith (<>) i1 i2)
      (M.unionWith (<>) f1 f2)
      (M.unionWith (<>) e1 e2)
      (M.unionWith (<>) t1 t2)
      (s1 <> s2)

emptyAggreg :: Ord g => g -> ChapterAggreg g
emptyAggreg = ChapterAggreg mempty mempty mempty mempty

mkchar :: Bool -> CharacterConstant -> CVarState -> CharacterVariable
mkchar autoweapon cst (CVarState sitems gld flgs) = chr
  where
    chr_beforeflags = mkCharacter (_maxendurance cst) (inventoryFromList allitems)
    chr_afterflags = foldl' (\c f -> c & LoneWolf.Character.flag f .~ True) chr_beforeflags flgs
    chr = chr_afterflags & curendurance .~ getMaxHp cst chr_afterflags
    allitems_pre = fromMaybe (defaultItems (_bookid cst)) sitems ++ [(Gold, gld)]
    allitems
      | autoweapon && notElem (Weapon Sommerswerd) (map fst allitems_pre) =
        case specialties of
          Nothing -> allitems_pre
          Just w -> (Weapon w, 1) : filter (not . isWeapon . fst) allitems_pre
      | otherwise = allitems_pre
    specialties = cst ^? discipline . traverse . _WeaponSkill
    isWeapon i = case i of
      Weapon _ -> True
      _ -> False

instance ToJSON CVarState

instance FromJSON CVarState where
  parseJSON = withObject "CVarState" $ \o ->
    CVarState
      <$> (o .: "_cvitems" <|> o .: "_items")
      <*> (o .: "_cvgold" <|> o .: "_gold")
      <*> (o .: "_cvflags" <|> o .: "flags")

defaultItems :: Book -> [(Item, Int)]
defaultItems Book01 = [(Weapon ShortSword, 1), (Shield, 1)]
defaultItems Book02 = [(Weapon ShortSword, 1), (sealHammerdalVol2, 1), (Shield, 1)]
defaultItems Book03 = [(Weapon Sommerswerd, 1), (Laumspur, 1), (Meal, 1)]
defaultItems Book04 = [(Weapon Sommerswerd, 1), (Laumspur, 1), (Meal, 4), (Shield, 1)] -- non obvious which is best
defaultItems Book05 = [(Weapon Sommerswerd, 1), (Laumspur, 1), (Shield, 1), (Meal, 2)]

defaultCVarState :: CVarState
defaultCVarState = CVarState Nothing 15 []

pbook :: Parser Book
pbook = option (eitherReader bookreader) (long "book" <> help "Book")
  where
    bookreader s = case map toLower s of
      "1" -> Right Book01
      "01" -> Right Book01
      "book1" -> Right Book01
      "book01" -> Right Book01
      "2" -> Right Book02
      "02" -> Right Book02
      "book2" -> Right Book02
      "book02" -> Right Book02
      "3" -> Right Book03
      "03" -> Right Book03
      "book3" -> Right Book03
      "book03" -> Right Book03
      "4" -> Right Book04
      "04" -> Right Book04
      "book4" -> Right Book04
      "book04" -> Right Book04
      "5" -> Right Book05
      "05" -> Right Book05
      "book5" -> Right Book05
      "book05" -> Right Book05
      _ -> Left ("Unrecognized book " ++ s)

cvariable :: Parser CVarState
cvariable =
  CVarState
    <$> fmap (cdefaultItems . M.toList . M.fromListWith (+) . map (,1)) (many (option (eitherReader readItem) (long "item" <> short 'i' <> help "Starting items (default items if empty)")))
    <*> option auto (long "gold" <> help "Starting gold" <> value 15)
    <*> many (option auto (long "flag" <> help "Starting flags"))
  where
    cdefaultItems [] = Nothing
    cdefaultItems itms = Just itms

readItem :: String -> Either String Item
readItem s = case readMaybe s of
  Just x -> pure x
  Nothing -> case s of
    "ropeB04" -> pure ropeB04
    "FireSphereB04" -> pure fireSphereB04
    "ropeB05" -> pure ropeB05
    "FireSphereB05" -> pure fireSphereB05
    "prismB05" -> pure prismB05
    "Sommerswerd" -> pure $ Weapon Sommerswerd
    "onyxmedallion5" -> pure onyxMedallion
    _ -> case readMaybe s of
      Just w -> pure (Weapon w)
      _ -> Left ("Unknown item: " ++ s)

eqcvarstate :: Book -> CVarState -> CVarState -> Bool
eqcvarstate book (CVarState i1 g1 f1) (CVarState i2 g2 f2) = g1 == g2 && f1 == f2 && M.fromListWith (+) itms1 == M.fromListWith (+) itms2
  where
    itms1 = fromMaybe (defaultItems book) i1
    itms2 = fromMaybe (defaultItems book) i2

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
      (MindShield, "MS"),
      (MindBlast, "MB"),
      (AnimalKinship, "AK"),
      (MindOverMatter, "MO"),
      (WeaponSkill Dagger, "DA"),
      (WeaponSkill Spear, "SP"),
      (WeaponSkill Mace, "MA"),
      (WeaponSkill Warhammer, "WH"),
      (WeaponSkill Sword, "SW"),
      (WeaponSkill Axe, "AX"),
      (WeaponSkill Quarterstaff, "QS"),
      (WeaponSkill ShortSword, "SS")
    ]

rdiscnames :: M.Map String Discipline
rdiscnames = M.fromList $ map swap $ M.toList discnames

soldumpSummary :: SolutionDump -> Multistat
soldumpSummary (SolutionDump (SolDesc _ ccst cvar) sol) =
  let nstates = length sol
      firstChapter ns = case ns of
        NewChapter 1 _ -> True
        _ -> False
      (_, csol) = case filter (firstChapter . fst) sol of
        x : _ -> x
        _ -> error "More than one first chapter?"
      score = choppedScore csol
      book = _bookid ccst
   in Multistat book (_discipline ccst) cvar [MultistatEntry (_maxendurance ccst) (_combatSkill ccst) (fromRational score) score nstates]

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
      [ ("hp", enc (fromIntegral @Endurance @Word64) chp),
        ("gold", enc id cgold),
        ("winning", enc (fromIntegral @Endurance @Word64) cwin),
        ("goldwin", enc fromGW cgwin),
        ("items", enc items citems)
      ]
    where
      enc :: (ToJSONKey x, Ord x) => (k1 -> x) -> Bagged k1 -> Value
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