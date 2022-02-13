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
import Data.Bits.Lens (bitAt)
import Data.List (intercalate, isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceM)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Character
import LoneWolf.Data
import Lucid
import Lucid.Base (TermRaw)
import Options.Applicative
  ( Parser,
    ParserInfo,
    command,
    execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    subparser,
    (<**>),
  )
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

data Stats = Stats
  { _fp :: FilePath,
    _sdisciplines :: [Discipline],
    _svariable :: CVarState,
    _sentry :: MultistatEntry,
    _sdecisions :: DecisionStats Rational
  }
  deriving (Show)

loadContent :: FilePath -> IO (Either String (FilePath, DecisionStats ERatio, Multistat))
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
  let convert :: (FilePath, DecisionStats ERatio, Multistat) -> Stats
      convert (fp, stts, ms) =
        let Multistat _ discs varstt [entry] = ms
         in Stats fp discs varstt entry (fmap getERatio stts)

  pure $! (allcontent ^.. traverse . _Right . to convert)

groupWinstates :: [(CharacterVariable, ERatio)] -> M.Map (M.Map Item Int, S.Set Flag) Rational
groupWinstates = M.fromListWith (+) . map (bimap extractCV getERatio)
  where
    extractCV cv = (M.fromList (items (_cequipment cv)), S.fromList (allFlags cv))

percent :: Double -> String
percent = printf "%.2f%%" . (* 100)

rpercent :: Rational -> String
rpercent = printf "%.2f%%" . (* 100) . fromRational @Double

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
heatmap rowStyle mname xs ys showX showY = heatmapH rowStyle (fmap fromString mname) xs ys (fromString . showX) (\n -> ([], fromString (showY n)))

heatmapH :: (forall arg. TermRaw Text arg => Double -> arg) -> Maybe (Html ()) -> [xs] -> [ys] -> (xs -> Html ()) -> (ys -> ([Attribute], Html ())) -> (ys -> xs -> Maybe (Html (), Double)) -> Html ()
heatmapH rowStyle mname xs ys showX showY scorer = table_ [class_ "pure-table"] $ do
  thead_ $
    tr_ $ do
      th_ (fromMaybe "" mname)
      mapM_ (th_ . div_ . span_ . showX) xs
  tbody_ $
    forM_ ys $ \d1 -> tr_ $ do
      uncurry th_ (showY d1)
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
      Book03 -> print (b03stats dt)
      Book04 -> print (b04stats dt)
      Book05 -> print (b05stats dt)
      _ -> error ("unsupported book stats for " ++ show book)

winrate :: Stats -> Rational
winrate = _mratio . _sentry

visitrate :: ChapterId -> Stats -> Rational
visitrate cid stts = M.findWithDefault 0 cid (fmap _cscore (_dres (_sdecisions stts)))

mvisitrate :: [ChapterId] -> Stats -> (Html (), Double)
mvisitrate cids stts = (fromString (intercalate " / " (map rpercent rates)), fromRational (sum rates / fromIntegral (length cids)))
  where
    rates = map (`visitrate` stts) cids

linkrate :: ChapterId -> ChapterId -> Stats -> Rational
linkrate src dst stts = fromMaybe 0 $ do
  tmap <- M.lookup src (fmap _ctransitions (_dres (_sdecisions stts)))
  let allprobs = sum tmap
  guard (allprobs > 0)
  (/ allprobs) <$> M.lookup dst tmap

normalDiscs :: S.Set Discipline
normalDiscs = S.fromList [Camouflage, Hunting, SixthSense, Tracking, Healing, MindShield, MindBlast, AnimalKinship, MindOverMatter]

missingdiscs :: Stats -> S.Set Discipline
missingdiscs stats =
  let discs = S.fromList (_sdisciplines stats)
   in S.difference normalDiscs discs

mdiscname :: Stats -> String
mdiscname = intercalate " / " . map show . S.toList . missingdiscs

hasitem :: Item -> Stats -> Bool
hasitem i = maybe False (elem i . map fst) . _cvitems . _svariable

hasflag :: Flag -> Stats -> Bool
hasflag f = elem f . _cvflags . _svariable

finalChapter :: Stats -> ChapterId
finalChapter stts = case _dbookid (_sdecisions stts) of
  Book05 -> 400
  _ -> 350

finalStat :: Stats -> ChapterAggreg Rational
finalStat stts = case M.lookup (finalChapter stts) (_dres (_sdecisions stts)) of
  Nothing -> error ("no final chapter in " ++ show (_fp stts))
  Just x -> x

finalItems :: Stats -> M.Map Inventory Rational
finalItems = _citems . finalStat

finalFlags :: Stats -> M.Map Flags Rational
finalFlags = _cflags . finalStat

finalFlag :: Flag -> Stats -> Rational
finalFlag f stts = sum (M.filterWithKey (const . view (bitAt (fromEnum f))) wstates) / rawrate
  where
    wstates = finalFlags stts
    rawrate = sum wstates

finalItem :: Item -> Stats -> Rational
finalItem i stts = rate / rawrate
  where
    wstates = finalItems stts
    rawrate = sum wstates
    rate = sum $ do
      (inv, p) <- M.toList wstates
      let cnt = itemCount i inv
      pure (fromIntegral cnt * p)

humanNumber :: Double -> String
humanNumber = go units
  where
    go [] _ = error "should not happen"
    go [lastunit] n = printf "%.2f" n ++ lastunit
    go (u : us) n
      | n > 1000 = go us (n / 1000)
      | otherwise = printf "%.2f" n ++ u
    units :: [String]
    units = ["", "K", "M", "G"]

blogpostStats :: [Stats] -> [(String, Stats -> (Html (), Double))] -> Html ()
blogpostStats astts cols = heatmapH rowStyleGreen (Just ("Missing disc" *> br_ [] *> "#states")) (map fst cols) (map _fp ordered) fromString colshow getentry
  where
    maxstates = maximum (map (_states . _sentry) astts)
    colshow n =
      let stt = mpo M.! n
          nstates = _states (_sentry stt)
          ratio = fromIntegral nstates / fromIntegral maxstates
       in ( [rowStyleRG (1 - ratio)],
            do
              a_ [href_ (T.pack ("/images/lonewolf/" ++ drop 5 (_fp stt) ++ ".svg"))] (fromString (mdiscname stt))
              br_ []
              fromString (fromString (humanNumber (fromIntegral nstates)) <> " states")
          )
    mpo = M.fromList [(_fp x, x) | x <- ordered]
    cmap = M.fromList cols
    ordered = sortOn (negate . winrate) astts
    getentry entry col =
      let Just e = M.lookup entry mpo
          fn = case M.lookup col cmap of
            Just f -> f
            _ -> error col
       in Just (fn e)

fmtb :: Bool -> (Html (), Double)
fmtb c = if c then (fromString "yes", 1) else (fromString "no", 0)

fmtr :: Rational -> (Html (), Double)
fmtr c = if c == 0 then (fromString "-", 0) else (fromString (rpercent c), fromRational c)

fmtbl :: Rational -> (Html (), Double)
fmtbl = fmtb . (== 1)

fmtq :: Rational -> Rational -> (Html (), Double)
fmtq mx q = (fromString (printf "%.2f" (fromRational @Double q)), fromRational (q / mx))

getallflags :: Flags -> [Flag]
getallflags flgs = filter (\f -> view (bitAt (fromEnum f)) flgs) [minBound .. maxBound]

finalStateRecap :: [Stats] -> Html ()
finalStateRecap = mapM_ showi
  where
    regroupFlags :: Stats -> M.Map Flag Rational
    regroupFlags = M.fromListWith (+) . concatMap expandFlgs . M.toList . finalFlags
      where
        expandFlgs (flgs, p) = do
          f <- getallflags flgs
          pure (f, p)
    regroupItems :: Stats -> M.Map Item Rational
    regroupItems = M.fromListWith (+) . concatMap expandItms . M.toList . finalItems
      where
        expandItms (itms, p) = do
          (i, q) <- items itms
          pure (i, fromIntegral q * p)
    showi :: Stats -> Html ()
    showi stt = do
      let rawrate = sum (finalFlags stt)
      h2_ $ fromString (_fp stt ++ " end states")
      ul_ $ do
        forM_ (M.toList (regroupItems stt)) $ \(itm, p) -> li_ (fromString (showItem Book04 itm ++ " - " ++ rpercent (p / rawrate)))
        forM_ (M.toList (regroupFlags stt)) $ \(flg, p) -> li_ (fromString (show flg ++ " - " ++ rpercent (p / rawrate)))

finalStateRecap' :: Book -> [Stats] -> Html ()
finalStateRecap' bk astts = blogpostStats astts cols
  where
    allitems = foldMap (S.fromList . map fst . concatMap items . M.keys . finalItems) astts
    allflags :: S.Set Flag
    allflags = foldMap (S.fromList . concatMap getallflags . M.keys . finalFlags) astts
    itemcols = [(showItem bk i, fmtq (iq i) . finalItem i) | i <- S.toList allitems]
    flagcols = [(show f, fmtr . finalFlag f) | f <- S.toList allflags]
    cols = itemcols ++ flagcols
    iq Meal = 6
    iq Gold = 50
    iq Laumspur = 5
    iq _ = 1

b03stats :: [Stats] -> Html ()
b03stats astts = do
  let cols =
        [ ("Win rate", fmtr . winrate),
          ("Raw rate", fmtr . sum . _cendurance . finalStat),
          ("SS", fmtb . not . hasitem (Weapon Sword)),
          ("End SH", fmtr . finalFlag HelmetIsSilver),
          ("Baknar oil", fmtr . finalFlag baknarOilB03),
          ("End +4", fmtr . finalItem StrengthPotion4)
        ]
  blogpostStats astts cols
  finalStateRecap' Book03 astts

b04stats :: [Stats] -> Html ()
b04stats astts = do
  let cols =
        [ ("Win rate", fmtr . winrate),
          ("Raw rate", fmtr . sum . _cendurance . finalStat),
          ("SS", fmtb . not . hasitem (Weapon Sword)),
          ("SH", fmtb . hasflag HelmetIsSilver),
          ("+4", fmtb . hasitem StrengthPotion4),
          ("Fought Elix", fmtr . finalFlag FoughtElix),
          ("Laumspur collect", mvisitrate [12, 268, 302]),
          ("End with +2 Strength Potion", fmtr . finalItem StrengthPotion),
          ("End with +4 Strength Potion", fmtr . finalItem StrengthPotion4)
        ]
  blogpostStats astts cols
  finalStateRecap' Book04 astts

b05stats :: [Stats] -> Html ()
b05stats astts = do
  let cols =
        [ ("SS", fmtb . not . hasitem (Weapon Sword)),
          ("SH", fmtb . hasflag HelmetIsSilver),
          ("+4", fmtb . hasitem StrengthPotion4),
          ("Win rate", fmtr . winrate),
          ("Previously fought the Elix", fmtb . hasflag FoughtElix),
          ("Imprisoned", fmtr . visitrate 69),
          ("Offer Oede", fmtr . visitrate 344),
          ("Prism route", fmtr . finalItem prismB05),
          ("Sash route", fmtr . finalItem sashB05),
          ("Fight Dhorgaan", fmtr . visitrate 253),
          ("End with Sommerswerd", fmtr . finalItem (Weapon Sommerswerd))
        ]
  blogpostStats astts cols
  finalStateRecap' Book05 astts