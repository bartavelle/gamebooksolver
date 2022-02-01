{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Control.Monad (forM_, guard)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.List (isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Debug.Trace (traceM)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Character
import LoneWolf.Data
import Lucid
import Lucid.Base (TermRaw)
import Options.Applicative
import System.Directory (getDirectoryContents)
import Text.Printf (printf)

data Opts = Opts Book Mode

data Mode
  = ChapterStats

mode :: Parser Mode
mode =
  subparser
    ( command "chapterstats" (info (pure ChapterStats) (progDesc "Stats for chapter"))
    )

options :: Parser Opts
options = Opts <$> pbook <*> mode

programOpts :: ParserInfo Opts
programOpts =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Generate figures for the blogposts"
    )

showDisc :: Discipline -> String
showDisc d =
  case d of
    WeaponSkill w -> show w
    _ -> show d

disciplines :: [Discipline]
disciplines = [Camouflage, Hunting, SixthSense, Tracking, Healing, WeaponSkill ShortSword, WeaponSkill Spear, MindShield, MindBlast, AnimalKinship, MindOverMatter]

dpairs :: [(Discipline, Discipline)]
dpairs = disciplines >>= \d1 -> disciplines >>= \d2 -> (d1, d2) <$ guard (d1 < d2)

type SttsG r = (Book, M.Map ChapterId (r, r), M.Map ChapterId (M.Map ChapterId r), M.Map ChapterId Word64)

type Stts = SttsG Rational

data Stats = Stats
  { _fp :: FilePath,
    _sbook :: Book,
    _sdisciplines :: [Discipline],
    _svariable :: CVarState,
    _sentry :: MultistatEntry,
    _scores :: M.Map ChapterId (Rational, Rational),
    _transitions :: M.Map ChapterId (M.Map ChapterId Rational),
    _states :: M.Map ChapterId Word64
  }
  deriving (Show)

loadContent :: FilePath -> IO (Either String (FilePath, SttsG ERatio, Multistat))
loadContent jotpath = do
  let basepath = reverse (drop 4 (reverse jotpath))
  a1 <- eitherDecodeFileStrict jotpath
  a2 <- eitherDecodeFileStrict (basepath ++ ".json")
  pure ((,,) basepath <$> a1 <*> a2)

loadData :: Book -> IO [Stats]
loadData book = do
  let bookdir = case book of
        Book01 -> "data/B01/"
        Book02 -> "data/B02/"
        Book03 -> "data/B03/"
        Book04 -> "data/B04/"
        Book05 -> "data/B05/"
  allfiles <- map (bookdir <>) . filter (isSuffixOf ".jot") <$> getDirectoryContents bookdir
  allcontent <- mapM loadContent allfiles
  mapM_ traceM (allcontent ^.. traverse . _Left)
  let convert :: (FilePath, SttsG ERatio, Multistat) -> Stats
      convert (fp, (bk, scores, transitions, sttmap), ms) =
        let Multistat _ discs varstt [entry] = ms
         in Stats fp bk discs varstt entry (fmap (bimap getERatio getERatio) scores) (fmap (fmap getERatio) transitions) sttmap
  pure $! (allcontent ^.. traverse . _Right . to convert)

percent :: Double -> String
percent = printf "%.3f%%" . (* 100)

rpercent :: Rational -> String
rpercent = printf "%.3f%%" . (* 100) . fromRational @Double

rowStyleRG :: TermRaw Text arg => Double -> arg
rowStyleRG d =
  let color = truncate (d * 255.0) :: Int
   in style_ (fromString (printf "background-color: #%02x%02x00; color: #000;" (255 - color) color))

rowStyleGreen :: TermRaw Text arg => Double -> arg
rowStyleGreen d =
  let color = truncate (d * 255.0) :: Int
      textcolor :: String
      textcolor =
        if d > 0.4
          then "#000"
          else "#fff"
   in style_ (fromString (printf "background-color: #00%02x00; color: %s;" color textcolor))

heatmap :: (forall arg. TermRaw Text arg => Double -> arg) -> Maybe String -> [xs] -> [ys] -> (xs -> String) -> (ys -> String) -> (ys -> xs -> Maybe (Html (), Double)) -> Html ()
heatmap rowStyle mname xs ys showX showY scorer = table_ [class_ "pure-table"] $ do
  thead_ $
    tr_ $ do
      th_ (maybe "" fromString mname)
      mapM_ (th_ . div_ . span_ . fromString . showX) xs
  tbody_ $
    forM_ ys $ \d1 -> tr_ $ do
      th_ (fromString (showY d1))
      forM_ xs $ \d2 ->
        case scorer d1 d2 of
          Nothing -> td_ "?"
          Just (txt, score) -> td_ [rowStyle score] $ do
            strong_ txt

skemap :: (Endurance -> CombatSkill -> Maybe (Html (), Double)) -> Html ()
skemap = heatmap rowStyleGreen Nothing [10 .. 19] [20 .. 29] (show . getCombatSkill) (show . getEndurance)

disciplineMap :: (forall arg. TermRaw Text arg => Double -> arg) -> (Discipline -> Discipline -> Maybe (Html (), Double)) -> Html ()
disciplineMap rowStyle scorer =
  let sdiscs = sortOn (\d -> fmap (negate . snd) (scorer d d)) disciplines
   in heatmap rowStyle Nothing sdiscs sdiscs showDisc showDisc scorer

data P x = P x x deriving (Show, Eq, Ord, Functor, Foldable)

instance Traversable P where
  traverse f (P a b) = P <$> f a <*> f b

rebagWithItems :: (Ord a) => (Inventory -> a) -> Bagged Inventory -> Bagged a
rebagWithItems selector = Bagged . M.fromListWith (+) . map (first selector) . M.toList . getBag

main :: IO ()
main = do
  Opts book mde <- execParser programOpts
  dt <- loadData book
  case mde of
    ChapterStats -> case book of
      Book05 -> print (b05stats dt)
      _ -> error ("unsupported book stats for " ++ show book)

winrate :: Stats -> Rational
winrate = _mratio . _sentry

visitrate :: ChapterId -> Stats -> Rational
visitrate cid stts = M.findWithDefault 0 cid (fmap fst (_scores stts))

linkrate :: ChapterId -> ChapterId -> Stats -> Rational
linkrate src dst stts = fromMaybe 0 $ do
  tmap <- M.lookup src (_transitions stts)
  let allprobs = sum tmap
  guard (allprobs > 0)
  (/ allprobs) <$> M.lookup dst tmap

normalDiscs :: S.Set Discipline
normalDiscs = S.fromList [Camouflage, Hunting, SixthSense, Tracking, Healing, MindShield, MindBlast, AnimalKinship, MindOverMatter]

missingdiscs :: Stats -> S.Set Discipline
missingdiscs stats =
  let discs = S.fromList (_sdisciplines stats)
   in S.difference normalDiscs discs

b05stats :: [Stats] -> Html ()
b05stats astts = heatmap rowStyleGreen (Just "Missing disc") (map fst cols) (map _fp ordered) id (\n -> mdiscname (mpo M.! n)) getentry
  where
    b c = if c then (fromString "yes", 1) else (fromString "no", 0)
    r c = if c == 0 then (fromString "-", 0) else (fromString (rpercent c), fromRational c)
    bl = b . (== 1)
    cols =
      [ ("Win rate", r . winrate),
        ("Sommerswerd", b . not . hasitem (Weapon Sword)),
        ("Silver Helm", b . hasitem SilverHelm),
        ("Fought Elix", b . hasflag FoughtElix),
        ("Imprisonment", r . visitrate 69),
        ("Offer Oede", r . visitrate 344),
        ("Sober", r . linkrate 302 283),
        ("Surrender", bl . linkrate 1 176),
        ("Blowpipe & steel dart", r . linkrate 313 325),
        ("Visualization", \e -> (a_ [href_ (T.pack ("/home/bartavelle/gits/misc/gamebooksolver/" ++ _fp e ++ ".svg"))] (fromString "link"), 1))
      ]
    hasitem i = maybe False (elem i . map fst) . _cvitems . _svariable
    hasflag f = elem f . _cvflags . _svariable
    cmap = M.fromList cols
    ordered :: [Stats]
    ordered = sortOn (negate . winrate) astts
    mdiscname = unwords . map show . S.toList . missingdiscs
    mpo = M.fromList [(_fp e, e) | e <- ordered]
    getentry :: String -> String -> Maybe (Html (), Double)
    getentry entry col =
      let Just e = M.lookup entry mpo
          fn = case M.lookup col cmap of
            Just f -> f
            _ -> error col
       in Just (fn e)