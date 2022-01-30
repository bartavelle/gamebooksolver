{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad (forM_, guard)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.List (isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.String (fromString)
import Data.Text (Text)
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
  = Generalstats CVarState
  | Heatmap CVarState
  | Histo FilePath

mode :: Parser Mode
mode =
  subparser
    ( command "general" (info (Generalstats <$> cvariable) (progDesc "General stats"))
        <> command "heatmap" (info (Heatmap <$> cvariable) (progDesc "Skill/endurance heatmap"))
        <> command "histo" (info (Histo <$> strArgument (metavar "PATH")) (progDesc "Histograms from stats"))
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

loadContent :: FilePath -> IO (Either String Multistat)
loadContent = eitherDecodeFileStrict

loadData :: Book -> CVarState -> IO [Multistat]
loadData book cvar = do
  let bookdir = case book of
        Book01 -> "data/B01/"
        Book02 -> "data/B02/"
  allfiles <- map (bookdir <>) . filter (isSuffixOf ".json") <$> getDirectoryContents bookdir
  allcontent <- mapM loadContent allfiles
  mapM_ traceM (allcontent ^.. traverse . _Left)
  let keepitems m@(Multistat bk _ rcvar _) = (m & variable .~ rcvar) <$ guard (bk == book && eqcvarstate book rcvar cvar)
  pure $! ((allcontent ^.. traverse . _Right) >>= keepitems)

type MSMap = M.Map (Book, Discipline, Discipline) (M.Map (Endurance, CombatSkill) (Rational, Int))

percent :: Double -> String
percent = printf "%.2f%%" . (* 100)

msmap :: [Multistat] -> MSMap
msmap lst = M.fromListWith M.union $ do
  Multistat book discs _ entries <- lst
  let emap = M.fromList $ do
        MultistatEntry e c _ score stts <- entries
        pure ((e, c), (score, stts))
      (k1, k2) = case discs of
        [d1, d2] -> (d1, d2)
        [d1] -> (d1, d1)
        _ -> error ("Invalid amount of disciplines: " ++ show discs)
  mkey <- [(book, k1, k2), (book, k2, k1)]
  pure (mkey, emap)

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

heatmap :: (forall arg. TermRaw Text arg => Double -> arg) -> [xs] -> [ys] -> (xs -> String) -> (ys -> String) -> (ys -> xs -> Maybe (String, Double)) -> Html ()
heatmap rowStyle xs ys showX showY scorer = table_ [class_ "pure-table"] $ do
  thead_ $
    tr_ $ do
      th_ ""
      mapM_ (th_ . div_ . span_ . fromString . showX) xs
  tbody_ $
    forM_ ys $ \d1 -> tr_ $ do
      th_ (fromString (showY d1))
      forM_ xs $ \d2 ->
        case scorer d1 d2 of
          Nothing -> td_ "?"
          Just (txt, score) -> td_ [rowStyle score] $ do
            strong_ (toHtml txt)

skemap :: (Endurance -> CombatSkill -> Maybe (String, Double)) -> Html ()
skemap = heatmap rowStyleGreen [10 .. 19] [20 .. 29] (show . getCombatSkill) (show . getEndurance)

disciplineMap :: (forall arg. TermRaw Text arg => Double -> arg) -> (Discipline -> Discipline -> Maybe (String, Double)) -> Html ()
disciplineMap rowStyle scorer =
  let sdiscs = sortOn (\d -> fmap (negate . snd) (scorer d d)) disciplines
   in heatmap rowStyle sdiscs sdiscs showDisc showDisc scorer

stateTable :: Book -> MSMap -> Html ()
stateTable book cmsmap = disciplineMap rowStyleRG scorer
  where
    scorer d1 d2 = do
      let rd1 = min d1 d2
          rd2 = max d1 d2
      stts <- M.lookup (book, rd1, rd2) maxmap
      pure (show stts, sratio stts)
    maxmap = fmap extractMax cmsmap
    extractMax mp = maximum $ do
      (_, (_, stts)) <- M.toList mp
      pure stts
    minstate = minimum maxmap
    maxstate = maximum maxmap
    sratio :: Int -> Double
    sratio st = fromIntegral (maxstate - st) / fromIntegral (maxstate - minstate)

scoreTableAt :: (Endurance, CombatSkill) -> Book -> MSMap -> Html ()
scoreTableAt stats book cmsmap = disciplineMap rowStyleGreen scorer
  where
    maxmap = M.mapWithKey extractMax cmsmap
    extractMax k mp = case M.lookup stats mp of
      Nothing -> error ("uncalculated for key " ++ show k)
      Just (r, _) -> fromRational r :: Double
    scorer d1 d2 = do
      let rd1 = min d1 d2
          rd2 = max d1 d2
      score <- M.lookup (book, rd1, rd2) maxmap
      pure (percent score, score)


post1 :: Book -> CVarState -> IO ()
post1 book cstt = do
  content <- loadData book cstt
  let mcontent = msmap content
  print (stateTable book mcontent)
  print (scoreTableAt (29,19) book mcontent)
  print (scoreTableAt (25,15) book mcontent)
  print (scoreTableAt (20,10) book mcontent)

post2 :: Book -> CVarState -> IO ()
post2 book cstt = do
  content <- loadData book cstt
  let mcontent = msmap content
      best = mcontent M.! (book, MindBlast, AnimalKinship)
  print (skemap (\e s -> fmap (\(score, _) -> (percent (fromRational score), fromRational score)) (M.lookup (e, s) best)))

data P x = P x x deriving (Show, Eq, Ord, Functor, Foldable)

instance Traversable P where
  traverse f (P a b) = P <$> f a <*> f b

rebagWithItems :: (Ord a) => (Inventory -> a) -> Bagged Inventory -> Bagged a
rebagWithItems selector = Bagged . M.fromListWith (+) . map (first selector) . M.toList . getBag

histo :: FilePath -> IO ()
histo fp = do
  r <- either (error . show) id <$> eitherDecodeFileStrict fp :: IO (M.Map ChapterId DecisionStat)
  print r

main :: IO ()
main = do
  Opts book mde <- execParser programOpts
  case mde of
    Generalstats cv -> post1 book cv
    Heatmap cv -> post2 book cv
    Histo fp -> histo fp
