{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module LoneWolf.Data where

import Control.Lens (makeLenses)
import Data.Aeson
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import LoneWolf.Character
import Options.Applicative

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
  deriving (Generic, Show)

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

cvariable :: Parser CVarState
cvariable =
  CVarState
    <$> fmap (cdefaultItems . M.toList . M.fromListWith (+) . map (,1)) (many (option auto (long "item" <> short 'i' <> help "Starting items (default items if empty)")))
    <*> option auto (long "gold" <> help "Starting gold" <> value 15)
    <*> option auto (long "meals" <> help "Starting meals" <> value 0)
  where
    cdefaultItems [] = defaultItems
    cdefaultItems itms = itms

eqcvarstate :: CVarState -> CVarState -> Bool
eqcvarstate (CVarState i1 g1 m1) (CVarState i2 g2 m2) = g1 == g2 && m1 == m2 && M.fromList i1 == M.fromListWith (+) i2

makeLenses ''Multistat
makeLenses ''MultistatEntry
makeLenses ''CVarState
