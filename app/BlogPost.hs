{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Main (main, flagAt) where

import Control.Applicative (many)
import Control.Concurrent.ParallelIO (parallel)
import Control.Lens hiding (argument)
import Control.Monad (forM_, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bits.Lens (bitAt)
import Data.List (intercalate, isSuffixOf, maximumBy, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Ratio (denominator, numerator)
import qualified Data.Set as S
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import Debug.Trace (traceM)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Character
import LoneWolf.Data
import LoneWolf.Various (showFlag)
import Lucid
import Lucid.Base (TermRaw)
import Options.Applicative
  ( Alternative ((<|>)),
    Parser,
    ParserInfo,
    argument,
    auto,
    command,
    execParser,
    flag',
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    strOption,
    subparser,
    value,
    (<**>),
  )
import qualified Options.Applicative as O
import System.Console.Haskeline
import System.Directory (getDirectoryContents)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Opts = Opts Book Mode String

data Mode
  = ChapterStats
  | Console [CQuery] [Int] Bool
  | Repl

data CQuery
  = ItemAt Item
  | FlagAt Flag
  | Passage
  | Winrate
  | Rawwinrate
  | Weapons

mode :: Parser Mode
mode =
  subparser
    (command "chapterstats" (info (pure ChapterStats) (progDesc "Stats for chapter")))
    <|> subparser (command "console" (info pConsole (progDesc "Item at")))
    <|> subparser (command "repl" (info (pure Repl) (progDesc "repl")))

pConsole :: Parser Mode
pConsole = Console <$> many pQuery <*> many (argument auto (metavar "CHAPTER")) <*> O.switch (long "details")

pQuery :: Parser CQuery
pQuery = i <|> f <|> p <|> w <|> r <|> ws
  where
    i = ItemAt <$> option auto (long "item")
    f = FlagAt <$> option auto (long "flag")
    p = flag' Passage (long "passage")
    w = flag' Winrate (long "win" <> help "Win rate (including next book stats)")
    r = flag' Rawwinrate (long "raw" <> help "Raw win rate (only this book)")
    ws = flag' Rawwinrate (long "weapons" <> help "Amount of weapons")

options :: Parser Opts
options =
  Opts
    <$> pbook
    <*> mode
    <*> strOption (long "imgsuffix" <> value "")

programOpts :: ParserInfo Opts
programOpts =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Generate figures for the blogposts"
    )

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
      mf path = do
        r <- eitherDecodeFileStrict path
        pure $ case r of
          Right x -> Right x
          Left rr -> Left (path ++ ": " ++ rr)
  a1 <- mf jotpath
  a2 <- mf (basepath ++ ".json")
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
  allcontent <- parallel (map loadContent allfiles)
  mapM_ traceM (allcontent ^.. traverse . _Left)
  let convert :: (FilePath, DecisionStats ERatio, Multistat) -> Stats
      convert (fp, stts, ms) =
        let Multistat _ discs varstt [entry] = ms
         in Stats fp discs varstt entry (fmap getERatio stts)

  pure $! (allcontent ^.. traverse . _Right . to convert)

rpercent :: Rational -> String
rpercent = rpercentg "%.3f%%"

rpercentg :: String -> Rational -> String
rpercentg fmt v =
  let pct = v * 100
   in if denominator pct == 1
        then printf "%d%%" (numerator pct)
        else printf fmt (fromRational @Double pct)

rpercent1 :: Rational -> String
rpercent1 = rpercentg "%.1f%%"

rowStyleRG :: (TermRaw Text arg) => Double -> arg
rowStyleRG d =
  let color = truncate (d * 255.0) :: Int
   in style_ (fromString (printf "background-color: #%02x%02x00; color: #000;" (255 - color) color))

optColors :: [Text]
optColors = ["#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8", "#a00000", "#aaffc3", "#808000", "#ffd8b1", "#808080", "#ffffff"]

colorStyle :: (TermRaw Text arg) => T.Text -> T.Text -> arg
colorStyle color bg = style_ ("background-color: " <> bg <> "; color: " <> color <> ";")

rowStyleGreen :: (TermRaw Text arg) => Double -> arg
rowStyleGreen d =
  let color = truncate (d * 255.0) :: Int
      textcolor :: String
      textcolor =
        if d > 0.4
          then "#000"
          else "#fff"
   in style_ (fromString (printf "background-color: #00%02x00; color: %s;" color textcolor))

heatmapH :: Maybe Text -> (forall arg. (TermRaw Text arg) => Double -> arg) -> Maybe (Html ()) -> [xs] -> [ys] -> (xs -> Html ()) -> (ys -> ([Attribute], Html ())) -> (ys -> xs -> Maybe (Html (), Double)) -> Html ()
heatmapH thstyle rowStyle mname xs ys showX showY scorer = table_ [class_ "pure-table"] $ do
  thead_ $
    tr_ $ do
      let mth cnt = case thstyle of
            Just st -> th_ [class_ st] cnt
            Nothing -> th_ cnt
      th_ (fromMaybe "" mname)
      mapM_ (mth . div_ . span_ . showX) xs
  tbody_ $
    forM_ ys $ \d1 -> tr_ $ do
      uncurry th_ (showY d1)
      forM_ xs $ \d2 ->
        case scorer d1 d2 of
          Nothing -> td_ "?"
          Just (txt, score) -> td_ [rowStyle score] $ do
            strong_ txt

data P x = P x x deriving (Show, Eq, Ord, Functor, Foldable)

instance Traversable P where
  traverse f (P a b) = P <$> f a <*> f b

winrate :: Stats -> Rational
winrate = getERatio . _mratio . _sentry

visitrate :: ChapterId -> Stats -> Rational
visitrate cid stts = M.findWithDefault 0 cid (fmap _cscore (_dres (_sdecisions stts)))

mvisitrate :: [ChapterId] -> Stats -> (Html (), Double)
mvisitrate cids stts = (fromString (intercalate "/" (map rpercent rates)), fromRational (sum rates / fromIntegral (length cids)))
  where
    rates = map (`visitrate` stts) cids

normalDiscs :: S.Set Discipline
normalDiscs = S.fromList [Camouflage, Hunting, SixthSense, Tracking, Healing, MindShield, MindBlast, AnimalKinship, MindOverMatter, WeaponSkill Sword]

mdiscname :: Stats -> String
mdiscname st = if S.size actual > 6
                  then "missing: " ++ sd missing
                  else sd actual
  where
    sd = intercalate "/" . map showDisc . S.toList
    missing = normalDiscs `S.difference` actual
    actual = S.fromList (_sdisciplines st)
    showDisc = \case
      Camouflage -> "CA"
      Hunting -> "HU"
      SixthSense -> "6S"
      Tracking -> "TR"
      Healing -> "HL"
      WeaponSkill Sword -> "W(SW)"
      MindShield -> "MS"
      MindBlast -> "MB"
      AnimalKinship -> "AK"
      MindOverMatter -> "MM"
      WeaponSkill x -> show x

hasitem :: Item -> Stats -> Bool
hasitem i = (> 0) . M.findWithDefault 0 i . _cvitems . _svariable

hasflag :: Flag -> Stats -> Bool
hasflag f = elem f . _cvflags . _svariable

finalChapter :: Stats -> ChapterId
finalChapter stts = case _dbookid (_sdecisions stts) of
  Book05 -> 400
  _ -> 350

finalStat :: Stats -> ChapterAggreg Rational
finalStat stts = statsAt (finalChapter stts) stts

statsAt :: ChapterId -> Stats -> ChapterAggreg Rational
statsAt cid stts = case M.lookup cid (_dres (_sdecisions stts)) of
  Nothing -> emptyAggreg 0
  Just x -> x

itemsAt :: ChapterId -> Stats -> M.Map Inventory Rational
itemsAt cid = _citems . statsAt cid

flagsAt :: ChapterId -> Stats -> M.Map Flags Rational
flagsAt cid = _cflags . statsAt cid

finalItems :: Stats -> M.Map Inventory Rational
finalItems = _citems . finalStat

finalFlags :: Stats -> M.Map Flags Rational
finalFlags = _cflags . finalStat

finalFlag :: Flag -> Stats -> Rational
finalFlag f stts = sum (M.filterWithKey (const . view (bitAt (fromEnum f))) wstates) / rawrate
  where
    wstates = finalFlags stts
    rawrate = sum wstates

itemAt :: ChapterId -> Item -> Stats -> Rational
itemAt cid i stts = if rawrate > 0 then rate / rawrate else rate
  where
    wstates = itemsAt cid stts
    rawrate = sum wstates
    rate = sum $ do
      (inv, p) <- M.toList wstates
      let cnt = itemCount i inv
      pure (fromIntegral cnt * p)

itemAtDetails :: ChapterId -> Item -> Stats -> [(Rational, Int)]
itemAtDetails cid i stts = sort $ map swap $ M.toList $ M.fromListWith (+) $ do
  (inv, p) <- M.toList wstates
  let amnt = itemCount i inv
  guard (amnt > 0)
  pure (amnt, adjust p)
  where
    wstates = itemsAt cid stts
    rawrate = sum wstates
    adjust x = if rawrate > 0 then x / rawrate else x

finalItemDetails :: Item -> Stats -> [(Rational, Int)]
finalItemDetails i stts = itemAtDetails (finalChapter stts) i stts

flagAt :: ChapterId -> Flag -> Stats -> Rational
flagAt cid fl stts = if rawrate > 0 then rate / rawrate else rate
  where
    wstates = flagsAt cid stts
    rawrate = sum wstates
    rate = sum $ do
      (flgs, p) <- M.toList wstates
      pure (if flgs ^. bitAt (fromEnum fl) then p else 0)

finalItem :: Item -> Stats -> Rational
finalItem i stts = if rawrate > 0 then rate / rawrate else rate
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

blogpostStatsDG :: String -> [Stats] -> [(String, Stats -> (Html (), Double))] -> Html ()
blogpostStatsDG imgsuffix astts rawcols = heatmapH Nothing rowStyleGreen (Just "Disciplines") (map fst cols) (map _fp ordered) fromString colshow getentry
  where
    cols = ("states", \stt -> let st = _states (_sentry stt) in (fromString (humanNumber (fromIntegral st)), 1 - fromIntegral (st - minstates) / fromIntegral (maxstates - minstates))) : rawcols
    maxstates = maximum (map (_states . _sentry) astts)
    minstates = minimum (map (_states . _sentry) astts)
    colshow :: String -> ([Attribute], Html ())
    colshow n =
      let stt = case M.lookup n mpo of
            Just x -> x
            Nothing -> error "a"
          mdisc = mdiscname stt
       in ( [colorStyle "#000" (M.findWithDefault (error (show (mdisc, ordered_discs))) mdisc ordered_discs)],
            fromString mdisc
              <> a_ [href_ (T.pack ("/images/lonewolf" ++ imgsuffix ++ "/" ++ drop 5 (_fp stt) ++ ".svg"))] "🗺️"
          )
    mpo = M.fromList [(_fp x, x) | x <- ordered]
    cmap = M.fromList cols
    max_score_by_discipline = M.fromListWith max $ do
      st <- astts
      pure (mdiscname st, winrate st)
    ordered_discs = M.fromList $ zip (M.keys max_score_by_discipline) (cycle optColors)
    getmaxscore d = M.findWithDefault 0 (mdiscname d) max_score_by_discipline
    ordered = sortOn (\x -> (negate (getmaxscore x), mdiscname x, negate (winrate x), _fp x)) astts
    getentry entry col =
      let e = M.findWithDefault (error "c") entry mpo
          fn = case M.lookup col cmap of
            Just f -> f
            _ -> error col
       in Just (fn e)

blogpostStats :: Maybe Text -> String -> [Stats] -> [(String, Stats -> (Html (), Double))] -> Html ()
blogpostStats mthstyle imgsuffix astts cols = heatmapH mthstyle rowStyleGreen (Just ("Missing disc" *> br_ [] *> "#states")) (map fst cols) (map _fp ordered) fromString colshow getentry
  where
    maxstates = maximum (map (_states . _sentry) astts)
    colshow n =
      let stt = M.findWithDefault (error "d") n mpo
          nstates = _states (_sentry stt)
          ratio = fromIntegral nstates / fromIntegral maxstates
       in ( [rowStyleRG (1 - ratio)],
            do
              a_ [href_ (T.pack ("/images/lonewolf" ++ imgsuffix ++ "/" ++ drop 5 (_fp stt) ++ ".svg"))] (fromString (mdiscname stt))
              br_ []
              fromString (fromString (humanNumber (fromIntegral nstates)) <> " states")
          )
    mpo = M.fromList [(_fp x, x) | x <- ordered]
    cmap = M.fromList cols
    ordered = sortOn (\x -> (negate (winrate x), _fp x)) astts
    getentry entry col =
      let e = M.findWithDefault (error "d") entry mpo
          fn = case M.lookup col cmap of
            Just f -> f
            _ -> error col
       in Just (fn e)

fmtb :: Bool -> (Html (), Double)
fmtb c = if c then (fromString "yes", 1) else (fromString "no", 0)

fmtr :: Rational -> (Html (), Double)
fmtr 0 = (fromString "-", 0)
fmtr 1 = (fromString "yes", 1)
fmtr c = (fromString (rpercent c), fromRational c)

fmtr1 :: Rational -> (Html (), Double)
fmtr1 0 = (fromString "-", 0)
fmtr1 1 = (fromString "yes", 1)
fmtr1 c = (fromString (rpercent1 c), fromRational c)

showDetails :: [(Rational, Int)] -> (Html (), Double)
showDetails lst = (html, sum (map (\(r, amnt) -> fromRational (r * fromIntegral amnt)) lst))
  where
    html = table_ [class_ "pure-table"] $ forM_ lst $ \(r, amnt) -> do
      tr_ $ do
        td_ (fromString (show amnt))
        td_ (fromString (rpercent r))

fmtbl :: Rational -> (Html (), Double)
fmtbl = fmtb . (== 1)

fmtq :: Rational -> Rational -> (Html (), Double)
fmtq mx q =
  let col =
        if denominator q == 1
          then show (numerator q)
          else printf "%.2f" (fromRational @Double q)
   in (fromString col, min 1 (fromRational (q / mx)))

fmtqi :: Rational -> Rational -> (Html (), Double)
fmtqi mx q = (fromString (printf "%d" (truncate @Rational @Int q)), min 1 (fromRational (q / mx)))

getallflags :: Flags -> [Flag]
getallflags flgs = filter (\f -> view (bitAt (fromEnum f)) flgs) [minBound .. maxBound]

finalStateRecap' :: String -> Book -> [Stats] -> Html ()
finalStateRecap' imgsuffix bk astts = blogpostStats (Just "vertical") imgsuffix astts cols
  where
    allitems :: S.Set Item
    allitems = foldMap (S.fromList . map fst . concatMap items . M.keys . finalItems) astts `S.difference` ignoreditems
    allflags :: S.Set Flag
    allflags = foldMap (S.fromList . concatMap getallflags . M.keys . finalFlags) astts `S.difference` ignoredflags
    itemcols = [(showItem bk i, fmtq (iq i) . finalItem i) | i <- S.toList allitems]
    flagcols = [(showFlag bk f, fmtr1 . finalFlag f) | f <- S.toList allflags]
    cols = itemcols ++ flagcols
    iq Meal = 6
    iq Gold = 50
    iq Laumspur = 5
    iq _ = 1
    (ignoredflags, ignoreditems) = case bk of
      Book05 -> (S.fromList [], S.fromList [Backpack, Shield, BodyArmor])
      _ -> (S.empty, S.empty)

erawrate :: Stats -> Rational
erawrate = sum . _cendurance . finalStat

summary :: String -> Book -> [Stats] -> [(String, Stats -> (Html (), Double))] -> Html ()
summary imgsuffix book astts cols = do
  let bydesc = M.fromListWith (++) [(mdiscname stt, [stt]) | stt <- astts]
      best = fmap (maximumBy (comparing erawrate)) bydesc
  h2_ "Best raw rates"
  blogpostStatsDG imgsuffix (M.elems best) cols
  h2_ "Recap"
  blogpostStatsDG imgsuffix astts cols
  h2_ "End state details"
  finalStateRecap' imgsuffix book astts

b01stats :: String -> [Stats] -> Html ()
b01stats imgsuffix astts = do
  let cols =
        [ ("Win rate", fmtr . winrate),
          ("Raw rate", fmtr . erawrate),
          ("Start", sitem),
          ("S money", fmtq 25 . itemAt 1 Gold),
          ("E money", fmtq 25 . finalItem Gold)
        ]
      sitem c = case (hasitem (Weapon Sword) c, hasitem Helmet c, hasitem Meal c, hasitem BodyArmor c, hasitem Potion4Hp c) of
        (True, False, False, False, False) -> (fromString "sword", 1)
        (False, True, False, False, False) -> (fromString "helmet", 1)
        (False, False, True, False, False) -> (fromString "meals", 1)
        (False, False, False, True, False) -> (fromString "armor", 1)
        (False, False, False, False, True) -> (fromString "potion", 1)
        (False, False, False, False, False) -> (fromString "none", 0.5)
        x -> (fromString (show x), 0)
  summary imgsuffix Book01 astts cols

b02stats :: String -> [Stats] -> Html ()
b02stats imgsuffix astts = do
  let cols =
        [ ("Win rate", fmtr . winrate),
          ("Raw rate", fmtr . erawrate),
          ("S money", fmtq 30 . itemAt 1 Gold),
          ( "Start protection",
            \stt -> case (hasitem BodyArmor stt, hasitem Shield stt) of
              (True, False) -> (fromString "body armor", 1)
              (False, True) -> (fromString "shield", 1)
              (True, True) -> (fromString "both", 0)
              (False, False) -> (fromString "none", 0)
          )
        ]
  summary imgsuffix Book02 astts cols

b03stats :: String -> [Stats] -> Html ()
b03stats imgsuffix astts = do
  let cols =
        [ ("Win rate", fmtr . winrate),
          ("Raw rate", fmtr . erawrate),
          ("DM", (\c -> if c then (fromString "LM", 1) else (fromString "2M", 1)) . hasitem Laumspur),
          ("Armor", fmtb . hasitem BodyArmor),
          ("GotA", fmtb . hasflag Knowledge01),
          ("Corridor (206)", fmtr . visitrate 206),
          ("Passage (6)", fmtr . visitrate 6),
          ("End SH", fmtr . finalItem silverHelmet),
          ("End +4", fmtr . finalItem StrengthPotion4),
          ("Triangle", fmtr . finalItem blueStoneTriangleB03)
        ]
  let fights =
        map
          (\(n, c) -> (n ++ " (" ++ show c ++ ")", fmtr . visitrate c))
          [ ("Baknar", 103),
            ("IB", 158),
            ("IB", 241),
            ("IB", 68),
            ("IB", 14),
            ("Frostwyrm", 265),
            ("Kalkoth", 147),
            ("IB", 161),
            ("IB", 260),
            ("IB", 296)
          ]
      bydesc = M.fromListWith (++) [(mdiscname stt, [stt]) | stt <- astts]
      best = fmap (maximumBy (comparing erawrate)) bydesc
  h2_ "Best raw rates"
  blogpostStatsDG imgsuffix (M.elems best) cols
  h2_ "Fights"
  "Note: IB stands for Ice Barbarian"
  blogpostStatsDG imgsuffix (M.elems best) fights
  h2_ "Recap"
  blogpostStatsDG imgsuffix astts cols
  h2_ "All fights"
  "Note: IB stands for Ice Barbarian"
  blogpostStatsDG imgsuffix astts fights
  h2_ "End state details"
  finalStateRecap' imgsuffix Book03 astts

b04stats :: String -> [Stats] -> Html ()
b04stats imgsuffix astts = do
  let cols =
        [ ("Win rate", fmtr . winrate),
          ("Raw rate", fmtr . erawrate),
          ("SS", fmtb . hasitem (Weapon Sommerswerd)),
          ("SH", fmtb . hasitem silverHelmet),
          ("Fought Elix", fmtr . finalFlag FoughtElix),
          ("Laumspur collect", mvisitrate [12, 268, 302]),
          ("Kept +2str", fmtr . finalItem StrengthPotion),
          ("kept +4str", keptp4),
          ("Final gold", showDetails . finalItemDetails Gold)
        ]
      keptp4 s =
        let started = hasitem StrengthPotion4 s
            finish = finalItem StrengthPotion4 s
         in case (started, finish) of
              (True, 1.0) -> fmtb True
              (False, 0.0) -> ("n/a", 0.8)
              (True, _) -> ("used a bit", fromRational @Double finish)
              (False, _) -> ("impossible", fromRational @Double finish)
  summary imgsuffix Book04 astts cols

b05stats :: String -> [Stats] -> Html ()
b05stats imgsuffix astts = do
  let cols =
        [ ("SS", fmtb . not . hasitem (Weapon Sword)),
          ("SH", fmtb . hasitem silverHelmet),
          ("+2", fmtb . hasitem StrengthPotion),
          ("+4", fmtb . hasitem StrengthPotion4),
          ("BA", fmtb . hasitem BodyArmor),
          ("EX", fmtb . hasflag FoughtElix),
          ("Win rate", fmtr . winrate),
          ("Imprisoned", fmtr . visitrate 69),
          ("Offer Oede", fmtr . visitrate 344),
          ("Limbdeath", fmtr . visitrate 81),
          ("Prism", fmtr . finalItem prismB05),
          ("c27 +2HP", fmtb . (> 0) . itemAt 160 Potion2Hp),
          ("c27 +LS", fmtb . (> 1) . itemAt 160 Laumspur),
          ("c27 +6HP", fmtb . (> 0) . itemAt 160 Potion6Hp),
          ("Dhorgaan", fmtr . visitrate 253),
          ("Keep SS", fmtr . finalItem (Weapon Sommerswerd))
        ]
  summary imgsuffix Book05 astts cols

showlineforcol :: [CQuery] -> Bool -> [Stats] -> ChapterId -> String
showlineforcol lst detailled dt cid =
  let mkcol :: Stats -> CQuery -> Rational
      mkcol s = \case
        ItemAt i -> itemAt cid i s
        FlagAt f -> flagAt cid f s
        Passage -> visitrate cid s
        Winrate -> winrate s
        Rawwinrate -> erawrate s
        Weapons -> sum (map (\w -> itemAt cid (Weapon w) s) [minBound .. maxBound])
      lns :: M.Map [Rational] [Stats]
      lns = M.fromListWith (++) (map mkline dt)
      mkline :: Stats -> ([Rational], [Stats])
      mkline st = (map (mkcol st) lst, [st])

      merge_discs stts = foldl1 S.intersection (map (S.fromList . _sdisciplines) stts)
      all_common_discs = merge_discs dt

      merge_items stts = foldl1 S.intersection (map (M.keysSet . _cvitems . _svariable) stts)
      all_common_items = merge_items dt

      merge_flags stts = foldl1 S.intersection (map (S.fromList . _cvflags . _svariable) stts)
      all_common_flags = merge_flags dt

      showline :: ([Rational], [Stats]) -> String
      showline (cols, stts) = intercalate "\t" (map (\n -> if n == 0 then "ZERO   " else printf "%.5f" (fromRational @Double n)) cols ++ details)
        where
          common_discs = merge_discs stts `S.difference` all_common_discs
          common_items = merge_items stts `S.difference` all_common_items
          common_flags = merge_flags stts `S.difference` all_common_flags
          details
            | detailled = if length lns == 1 then ["ALL"] else sort (map (drop 9 . _fp) stts)
            | otherwise =
                show (length stts)
                  : map show (S.toList common_discs)
                  ++ map show (S.toList common_items)
                  ++ map show (S.toList common_flags)
   in unlines (map showline (reverse $ M.toList lns))

main :: IO ()
main = do
  Opts book mde imgsuffix <- execParser programOpts
  dt <- loadData book
  case mde of
    ChapterStats -> case book of
      Book01 -> print (b01stats imgsuffix dt)
      Book02 -> print (b02stats imgsuffix dt)
      Book03 -> print (b03stats imgsuffix dt)
      Book04 -> print (b04stats imgsuffix dt)
      Book05 -> print (b05stats imgsuffix dt)
    Console lst cids detailled -> do
      forM_ cids $ \cid -> do
        putStrLn ("chapter " <> show cid)
        putStrLn (showlineforcol lst detailled dt cid)
    Repl ->
      let loop = do
            minput <- getInputLine "% "
            case fmap words minput of
              Just ["quit"] -> pure ()
              Just [n] | Just cid <- readMaybe n -> liftIO (putStrLn (showlineforcol [Passage] False dt cid)) >> loop
              Just ["p", n] | Just cid <- readMaybe n -> liftIO (putStrLn (showlineforcol [Passage] True dt cid)) >> loop
              Just ["i", itm, n] | Just cid <- readMaybe n, Right ritm <- readItem itm -> liftIO (putStrLn (showlineforcol [ItemAt ritm] False dt cid)) >> loop
              Just ["i", 'S' : num, n] | Just cid <- readMaybe n, Just sitm <- readMaybe num -> liftIO (putStrLn (showlineforcol [ItemAt (GenSpecial (GenCounter sitm))] False dt cid)) >> loop
              Just ["i", 'G' : num, n] | Just cid <- readMaybe n, Just sitm <- readMaybe num -> liftIO (putStrLn (showlineforcol [ItemAt (GenBackpack (GenCounter sitm))] False dt cid)) >> loop
              Just ["ip", itm, n] | Just cid <- readMaybe n, Right ritm <- readItem itm -> liftIO (putStrLn (showlineforcol [ItemAt ritm] True dt cid)) >> loop
              Just ["ip", 'S' : num, n] | Just cid <- readMaybe n, Just sitm <- readMaybe num -> liftIO (putStrLn (showlineforcol [ItemAt (GenSpecial (GenCounter sitm))] True dt cid)) >> loop
              Just ["ip", 'G' : num, n] | Just cid <- readMaybe n, Just sitm <- readMaybe num -> liftIO (putStrLn (showlineforcol [ItemAt (GenBackpack (GenCounter sitm))] True dt cid)) >> loop
              Just ["f", sflg, n] | Just cid <- readMaybe n, Just flg <- readMaybe sflg -> liftIO (putStrLn (showlineforcol [FlagAt flg] False dt cid)) >> loop
              Just ["fp", sflg, n] | Just cid <- readMaybe n, Just flg <- readMaybe sflg -> liftIO (putStrLn (showlineforcol [FlagAt flg] True dt cid)) >> loop
              Just ["wpns", n] | Just cid <- readMaybe n -> liftIO (putStrLn (showlineforcol [Weapons] False dt cid)) >> loop
              Just ["pwpns", n] | Just cid <- readMaybe n -> liftIO (putStrLn (showlineforcol [Weapons] True dt cid)) >> loop
              _ -> loop
       in runInputT defaultSettings loop
