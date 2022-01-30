{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Codec.Compression.Zstd.Lazy (compress, decompress)
import qualified Codec.Serialise as S
import Control.Lens hiding (argument)
import Control.Monad (forM_, guard, when)
import Data.Aeson (decode, eitherDecodeFileStrict, encode, encodeFile)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Data.Lens (biplate)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Word (Word64, Word8)
import Debug.Trace (traceM)
import qualified LoneWolf.Book01
import qualified LoneWolf.Book02
import qualified LoneWolf.Book03
import qualified LoneWolf.Book04
import qualified LoneWolf.Book05
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Combat (expectedEndurance, winchance)
import LoneWolf.Data
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Solve (orderChapters, solveLWs, step)
import LoneWolf.StateSelector (Log (..), Selector (..), parseSelector, selectChar)
import LoneWolf.Various (getDestinations)
import Options.Applicative
import SimpleSolver
import Solver (Probably)
import System.Directory (getDirectoryContents)
import Text.Printf (printf)
import Text.Read (readMaybe)

pchapters :: Book -> [(ChapterId, Chapter)]
pchapters book = case book of
  Book01 -> LoneWolf.Book01.pchapters
  Book02 -> LoneWolf.Book02.pchapters
  Book03 -> LoneWolf.Book03.pchapters
  Book04 -> LoneWolf.Book04.pchapters
  Book05 -> LoneWolf.Book05.pchapters

getBoundary :: Book -> Maybe (S.Set Item, S.Set Flag)
getBoundary b = case b of
  Book02 -> Just (S.fromList [Weapon Sommerswerd], S.empty)
  Book03 -> Just (S.fromList [Weapon Sommerswerd, SilverHelm], S.empty)
  Book04 -> Just (S.fromList [Weapon Sommerswerd, SilverHelm], S.singleton FoughtElix)
  _ -> Nothing

simpleSol :: M.Map (S.Set Item, S.Set Flag) Rational -> Book -> CharacterConstant -> CharacterVariable -> [Int] -> (Rational, [(NextStep, Solution NextStep String)])
simpleSol endscores book ccst cvar target =
  let (solution, smap) = solveLWs scorer book target (pchapters book) ccst cvar
      scorer = case getBoundary book of
        Nothing -> \_ _ -> 1
        Just (sitms, sflgs) -> \itms flgs ->
          let ritms = itms `S.intersection` sitms
              rflgs = flgs `S.intersection` sflgs
           in case M.lookup (ritms, rflgs) endscores of
                Nothing -> error ("unknown score combination: " ++ show (ritms, rflgs) ++ " keys are " ++ show (M.keys endscores))
                Just r -> r
   in (_score solution, smap)

data Opts = Opts
  { _optDebug :: Bool,
    _optResultDir :: Maybe FilePath,
    _optCommand :: Command
  }

data DumpMode = Json | Other
  deriving (Eq)

data Command
  = ExploreFile FilePath (Maybe (Log Selector))
  | DecisionGraph DumpMode FilePath
  | SolDump DumpMode SolDesc (Maybe FilePath) Bool
  | ExtractFile FilePath (Log Selector)
  | DecisionTracker FilePath FightDetails (Log Selector)
  | DecodeItems Book Word64
  | ShowStates FilePath (Log Selector)
  | Dot FilePath
  | DumpBook Book

options :: Parser Opts
options =
  Opts
    <$> switch (long "debug" <> help "Verbose execution")
    <*> optional (strOption (long "results" <> help "Path where results .json files are stored"))
    <*> scommand

fightdetails :: Parser FightDetails
fightdetails =
  FightDetails
    <$> strOption (long "opponent" <> metavar "NAME" <> help "Opponent name" <> showDefault <> value "dummy")
    <*> fmap CombatSkill (option auto (long "oskill" <> help "Opponent combat skill" <> value 10 <> showDefault))
    <*> fmap Endurance (option auto (long "oendurance" <> help "Opponent endurance" <> value 15 <> showDefault))
    <*> pure []

cconstant :: Parser CharacterConstant
cconstant =
  CharacterConstant
    <$> fmap Endurance (option auto (long "endurance" <> short 'e' <> help "Max endurance" <> value 25 <> showDefault))
    <*> fmap CombatSkill (option auto (long "skill" <> short 's' <> help "Combat skill" <> value 15 <> showDefault))
    <*> disciplines
    <*> pbook

disciplines :: Parser [Discipline]
disciplines = many (option (maybeReader decodedisc) (long "discipline" <> short 'd' <> help "Disciplines"))
  where
    decodedisc s = M.lookup s rdiscnames <|> readMaybe s

dumpmode :: Parser DumpMode
dumpmode = option (maybeReader validator) (long "mode" <> help "dump mode (json, cbor)" <> value Other)
  where
    validator "json" = Just Json
    validator "cbor" = Just Other
    validator "other" = Just Other
    validator _ = Nothing

soldesc :: Parser SolDesc
soldesc =
  SolDesc
    <$> many (option auto (long "stopat" <> metavar "CHAPTER" <> help "Stop at these chapters"))
    <*> cconstant
    <*> cvariable

charfilter :: Parser (Log Selector)
charfilter = option (eitherReader parseSelector) (long "filter" <> help "Character filter")

scommand :: Parser Command
scommand =
  subparser
    ( command
        "decisiongraph"
        ( info
            (DecisionGraph <$> dumpmode <*> strArgument (metavar "PATH" <> help "saved cbor game"))
            (progDesc "Generate a graphviz graph of decisions")
        )
        <> command
          "explorefrom"
          ( info
              (ExploreFile <$> strArgument (metavar "PATH" <> help "saved cbor game") <*> optional charfilter)
              (progDesc "Explore the solution from a saved file")
          )
        <> command
          "soldump"
          ( info
              ( SolDump
                  <$> dumpmode
                    <*> soldesc
                    <*> optional (strArgument (metavar "PATH" <> help "Dumped file path"))
                    <*> switch (long "autoweapon" <> help "Automatically set the weapon to the specialty, unless it is the Sommerswerd")
              )
              (progDesc "Dump a solution")
          )
        <> command "showstates" (info (ShowStates <$> strArgument (metavar "PATH" <> help "Dumped file path") <*> charfilter) (progDesc "Show states"))
        <> command "extract" (info (ExtractFile <$> strArgument (metavar "PATH" <> help "saved cbor game") <*> charfilter) (progDesc "Extract solution data from a file"))
        <> command "decisiontracker" (info (DecisionTracker <$> strArgument (metavar "PATH" <> help "saved cbor game") <*> fightdetails <*> charfilter) (progDesc "Displays stats about how a decision is taken"))
        <> command "decodeitems" (info (DecodeItems <$> pbook <*> argument auto (metavar "ITEMS" <> help "numerical representation of items")) (progDesc "Decode numerical inventory"))
        <> command "dot" (info (Dot <$> strArgument (help "path to the json dump")) (progDesc "Generate a dot file from the json dump"))
        <> command "dumpbook" (info (DumpBook <$> pbook) (progDesc "Dump a book as JSON"))
    )

programOpts :: ParserInfo Opts
programOpts =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Solve and explore book02 solutions"
    )

todot ::
  Book ->
  M.Map ChapterId (M.Map ChapterId Rational) ->
  M.Map ChapterId (Rational, Rational) ->
  M.Map ChapterId Word64 ->
  IO ()
todot book soltransitions scores sttmap = do
  let chapters = pchapters book
      alltransitions :: M.Map ChapterId (M.Map ChapterId [String])
      alltransitions = M.fromList (fmap (fmap mkdestmap) chapters)
      mkdestmap = M.fromList . getDestinations book . _pchoice
      getedgedesc src dst = maybe [] (map show) (M.lookup src alltransitions >>= M.lookup dst)
      showEdge mweight src dst = "  A" ++ show src ++ " -> A" ++ show dst ++ " [" ++ unwords [label, color] ++ "];"
        where
          color = "color=" ++ if isJust mweight then "black" else "darkgrey"
          label = "label=\"" ++ filter (/= '"') (unwords (maybe "" percent mweight : getedgedesc src dst)) ++ "\""
  putStrLn "digraph g {"
  forM_ chapters $ \(cid, chapter) ->
    let cdesc = _pchoice chapter
        combat = has (outcomePlate . biplate . _Fight) cdesc || has (biplate . _EvadeFight) cdesc
        eating = preview (outcomePlate . biplate . _MustEat) cdesc
        solscore = M.lookup cid scores
        (sscore, expendurance) = maybe ("na", Nothing) (\(sc, e) -> (percent sc, Just (fromRational @Double (if sc > 0 then e / sc else e)))) solscore
        nodestyle
          | eating == Just NoHunt = "color=yellow style=filled"
          | eating == Just Hunt = "color=yellow"
          | combat = "color=red"
          | has (biplate . _GameLost) cdesc = "style=filled fontcolor=white"
          | has _Special cdesc = "shape=square color=blue"
          | has _Just solscore = "color=grey"
          | otherwise = ""
        hp = case expendurance of
          Nothing -> ""
          Just e -> printf " [e=%.2f]" e
        nstates = printf " [stts=%d]" (M.findWithDefault 0 cid sttmap)
        nodecolor
          | has (biplate . _GameLost) cdesc = "black"
          | otherwise = case expendurance of
            Nothing -> "white"
            Just e -> printf "#%02x%02x%02x" (truncate @Double @Word8 (255 * (20 - e) / 20)) (255 :: Word8) (255 :: Word8)
     in putStrLn ("  \"A" ++ show cid ++ "\" [style=filled fillcolor=\"" ++ nodecolor ++ "\"label=\"" ++ show cid ++ " " ++ sscore ++ hp ++ nstates ++ "\" " ++ nodestyle ++ "];")
  let srcdst s = S.fromList $ do
        (src, dsts) <- M.toList s
        dst <- M.keys dsts
        pure (src, dst)
      missingtransitions = S.difference (srcdst alltransitions) (srcdst soltransitions)
  forM_ missingtransitions $ \(src, dst) -> putStrLn (showEdge Nothing src dst)
  forM_ (M.toList soltransitions) $ \(src, dsts) -> do
    let ttl = sum dsts
        dsts' = if ttl > 0 then fmap (/ ttl) dsts else dsts
    forM_ (M.toList dsts') $ \(dst, weight) -> putStrLn (showEdge (Just weight) src dst)
  putStrLn "}"

getSol :: M.Map (S.Set Item, S.Set Flag) Rational -> Bool -> SolDesc -> (Rational, [(NextStep, Solution NextStep String)])
getSol scoremap autoweapon (SolDesc fchapters ccst ccvar) =
  let cvar = mkchar autoweapon ccst ccvar
      finalchapters = case fchapters of
        [] -> [if _bookid ccst == Book05 then 400 else 350]
        fc -> fc
   in simpleSol scoremap (_bookid ccst) ccst cvar finalchapters

shortNS :: CharacterConstant -> NextStep -> String
shortNS ccst ns =
  case ns of
    HasLost _ -> "lost"
    HasWon _ -> "won"
    NewChapter cid cv -> "ch:" ++ show cid ++ " " ++ shortState ccst cv

shortState :: CharacterConstant -> CharacterVariable -> String
shortState ccst s = "e:" ++ printf "%2d" (s ^. curendurance . to getEndurance) ++ " " ++ itemlist (items (s ^. equipment)) ++ " " ++ show (allFlags s) ++ mstored
  where
    mstored = if null sitems then "" else " stored:" ++ itemlist sitems
    sitems = items (s ^. prevequipment)
    itemlist = intercalate "/" . map singleitem
    singleitem (i, c) =
      itm ++ (if c == 1 then "" else "(" ++ show c ++ ")")
      where
        itm = case (i, M.lookup (_bookid ccst) itemIds >>= M.lookup i) of
          (_, Just d) -> d
          (Weapon x, _) -> show x
          _ -> show i

percent :: Rational -> String
percent = printf "%.3f%%" . fromRational @Double . (* 100)

selector :: (a -> String) -> [a] -> IO a
selector disp choices = do
  forM_ (zip [0 :: Int ..] choices) $ \(choiceid, choice) -> do
    putStrLn (printf "%02d" choiceid ++ " - " ++ disp choice)
  case choices of
    [] -> error "no choices"
    [x] -> pure x
    _ -> do
      idx <- read <$> getLine
      if idx >= 0 && idx < length choices
        then pure (choices !! idx)
        else do
          putStrLn "ERROR"
          selector disp choices

exploreChopped :: HM.HashMap NextStep (ChoppedSolution NextStep) -> CharacterConstant -> NextStep -> IO ()
exploreChopped mp cc = go
  where
    bk = _bookid cc
    book = IM.fromList (pchapters bk)
    order = orderChapters bk book
    stepper = step order book cc
    go ns = do
      let nstates = stepper ns
          advance :: Rational -> Probably NextStep -> IO ()
          advance score outcomes = do
            putStrLn (percent score ++ " - " ++ shortNS cc ns)
            -- selection
            (_, ch) <- selector (\(dsc, os) -> (if os == outcomes then " *** " else "  -  ") ++ dsc) nstates
            -- outcome
            (cns, _) <- selector (\(sns, p) -> percent p ++ " - " ++ shortNS cc sns) ch
            go cns

      case HM.lookup ns mp of
        Nothing -> error ("not found " ++ show ns)
        Just sl ->
          case sl of
            CLeaf sc -> putStrLn ("Win: " ++ percent sc)
            CLeafLost -> putStrLn "Lost!"
            CNode sc os -> advance sc (map (first (fromMaybe (HasLost 0))) os)
            CJump sc ns' -> advance sc [(ns', 1)]

ctarget :: NextStep -> ChoppedSolution NextStep -> Maybe (CharacterVariable, ChapterId)
ctarget (NewChapter _ cv) (CNode _ ((Just (NewChapter tgt _), _) : _)) = Just (cv, tgt)
ctarget (NewChapter _ cv) (CJump _ (NewChapter tgt _)) = Just (cv, tgt)
ctarget _ _ = Nothing

getStats :: FightDetails -> CharacterConstant -> [(NextStep, ChoppedSolution NextStep)] -> M.Map ChapterId DecisionStat
getStats fdetails ccst = foldl' mkhistogram mempty . map extract
  where
    add cv tgt =
      ( tgt,
        DecisionStat
          (singletonbag (cv ^. curendurance))
          (singletonbag gld)
          (singletonbag een)
          (singletonbag (GoldWin gld wc))
          (singletonbag (cv ^. equipment))
      )
      where
        wc = truncate (winchance ccst cv fdetails * 10)
        een = truncate (expectedEndurance ccst cv fdetails)
        gld = cv ^. equipment . gold
    extract :: (NextStep, ChoppedSolution NextStep) -> Maybe (ChapterId, DecisionStat)
    extract (ns, cs) = fmap (uncurry add) (ctarget ns cs)

mkhistogram :: M.Map ChapterId DecisionStat -> Maybe (ChapterId, DecisionStat) -> M.Map ChapterId DecisionStat
mkhistogram curmp Nothing = curmp
mkhistogram mp (Just (cid, ds)) = M.unionWith (<>) (M.singleton cid ds) mp

nsChapter :: NextStep -> Maybe ChapterId
nsChapter ns = case ns of
  HasLost cid -> Just cid
  HasWon _ -> Nothing
  NewChapter cid _ -> Just cid

countstates :: [(NextStep, a)] -> M.Map ChapterId Word64
countstates = M.fromListWith (+) . mapMaybe (fmap (,1) . extractCid . fst)
  where
    extractCid ns = case ns of
      NewChapter tgt _ -> Just tgt
      HasLost tgt -> Just tgt
      HasWon _ -> Nothing

mksol :: NextStep -> HM.HashMap NextStep (ChoppedSolution NextStep) -> (M.Map ChapterId (M.Map ChapterId Rational), M.Map ChapterId (Rational, Rational))
mksol ini sttmap = go M.empty M.empty [(1, ini)]
  where
    go ::
      M.Map ChapterId (M.Map ChapterId Rational) ->
      M.Map ChapterId (Rational, Rational) ->
      [(Rational, NextStep)] ->
      (M.Map ChapterId (M.Map ChapterId Rational), M.Map ChapterId (Rational, Rational))
    go !linkmap !scoremap !todo =
      case todo of
        [] -> (linkmap, scoremap)
        (p, s) : ss ->
          let ee = case s of
                HasLost _ -> 0
                HasWon _ -> 0
                NewChapter _ stt -> fromIntegral (stt ^. curendurance)
           in case (nsChapter s, HM.lookup s sttmap) of
                (Just src, Just sol) ->
                  case sol of
                    CLeafLost -> go linkmap scoremap ss
                    CLeaf score -> go linkmap (updateScore (p * score, p * ee) src) ss
                    CJump _ stt -> go (updateChoice src [(p, stt)]) (updateScore (p, p * ee) src) ((p, stt) : ss)
                    CNode _ pms ->
                      let pms' = mapMaybe (\(mstt, sc) -> (p * sc,) <$> mstt) pms
                       in go (updateChoice src pms') (updateScore (p, p * ee) src) (pms' ++ ss)
                _ -> go linkmap scoremap ss
      where
        updateChoice :: ChapterId -> [(Rational, NextStep)] -> M.Map ChapterId (M.Map ChapterId Rational)
        updateChoice src = foldl' updateChoice' linkmap
          where
            updateChoice' !curmap (p1, dstt) = case nsChapter dstt of
              Nothing -> curmap
              Just dst -> M.insertWith (M.unionWith (+)) src (M.singleton dst p1) curmap

        updateScore :: (Rational, Rational) -> ChapterId -> M.Map ChapterId (Rational, Rational)
        updateScore score cid = M.insertWith (\(s1, e1) (s2, e2) -> (s1 + s2, e1 + e2)) cid score scoremap

loadResults :: Maybe FilePath -> CharacterConstant -> IO (M.Map (S.Set Item, S.Set Flag) Rational)
loadResults Nothing _ = pure M.empty
loadResults (Just pth) ccst = do
  allfiles <- map ((pth <> "/") <>) . filter (isSuffixOf ".json") <$> getDirectoryContents pth
  let bk = _bookid ccst
      (fitms, fflgs) = case getBoundary bk of
        Nothing -> error ("No boundary for book " ++ show bk)
        Just s -> s
      loadContent p = do
        r <- eitherDecodeFileStrict p
        case r of
          Left rr -> error rr
          Right x -> pure (p, x)
      curdiscs = S.fromList (_discipline ccst)
  allcontent <- traverse loadContent allfiles
  let lst = do
        (_, Multistat nbk ndiscs cvar entries) <- allcontent
        guard (succ bk == nbk)
        let diff = S.fromList ndiscs `S.difference` curdiscs
        guard (length diff <= 1)
        let startitems = S.fromList (map fst (fromMaybe (defaultItems bk) (_cvitems cvar)))
            startflags = S.fromList (_cvflags cvar)
        MultistatEntry en sk _ s _ <- entries
        guard (en == _maxendurance ccst && sk == _combatSkill ccst)
        pure ((startitems `S.intersection` fitms, startflags `S.intersection` fflgs), s)
  pure (M.fromListWith max lst)

main :: IO ()
main = do
  Opts _ resdir cmd <- execParser programOpts
  case cmd of
    DumpBook bk -> BS8.putStrLn (encode (pchapters bk))
    SolDump dmode sd mtarget autoweapon -> do
      res <- loadResults resdir (_ccst sd)
      let (_, solmap) = getSol res autoweapon sd
          dmap = SolutionDump sd (map (fmap chopSolution) solmap)
          todump = case dmode of
            Json -> encode dmap
            Other -> compress 3 (S.serialise dmap)
      case mtarget of
        Just pth -> do
          BSL.writeFile pth todump
          when (dmode == Other) $ encodeFile (pth ++ ".json") (soldumpSummary dmap)
        Nothing -> BSL.putStr todump
    DecisionGraph dmode pth -> do
      cnt <- decompress <$> BSL.readFile pth
      let SolutionDump (SolDesc _ cc cv) res = S.deserialise cnt
          book = _bookid cc
          (transitions, scores) = mksol (NewChapter 1 (mkchar False cc cv)) (HM.fromList res)
          sttmap = countstates res
      traceM ("Loaded " ++ show (length res) ++ " states, computing decision graph")
      case dmode of
        Other -> todot book transitions scores sttmap
        Json -> BS8.putStrLn (encode (book, scores, transitions, sttmap))
    ExploreFile pth mselector -> do
      cnt <- decompress <$> BSL.readFile pth
      let SolutionDump (SolDesc _ cc _) res = S.deserialise cnt
      print cc
      let startStates = filter (selectChar check) (map fst res)
          check = fromMaybe (P (InChapter 1)) mselector
      case startStates of
        [] -> putStrLn ("Expression " ++ show mselector ++ " did not select anything")
        sstate : _ -> exploreChopped (HM.fromList res) cc sstate
    ShowStates pth selc -> do
      cnt <- decompress <$> BSL.readFile pth
      let SolutionDump (SolDesc _ cc _) res = S.deserialise cnt
      print cc
      let startStates = filter (selectChar selc . fst) res
      forM_ startStates $ \(k, v) -> do
        let (score, subs) = case v of
              CLeafLost -> (0, [])
              CLeaf sc -> (sc, [])
              CJump sc nxt -> (sc, [(1, nxt)])
              CNode sc nxt -> (sc, mapMaybe (\(mns, p) -> (p,) <$> mns) nxt)
        putStrLn (percent score ++ " - " ++ shortNS cc k)
        forM_ subs $ \(p, nstt) -> putStrLn ("  " ++ percent p ++ " - " ++ shortNS cc nstt)
    ExtractFile pth sel -> do
      cnt <- decompress <$> BSL.readFile pth
      let SolutionDump _ res = S.deserialise cnt
          startStates = filter (selectChar sel . fst) res
      BSL.putStr (encode startStates)
    DecisionTracker pth fdetails sel -> do
      cnt <- decompress <$> BSL.readFile pth
      let SolutionDump (SolDesc _ ccst _) res = S.deserialise cnt
          startStates = filter (selectChar sel . fst) res
          stats = getStats fdetails ccst startStates
      BSL.putStr (encode stats)
    DecodeItems book itms ->
      let inv = Inventory itms
       in mapM_ (\(i, q) -> putStrLn (show q ++ " " ++ showItem book i)) (items inv)
    Dot pth -> do
      Just (book, scores_, transitions_, sttmap) <- decode <$> BS8.readFile pth
      let scores = fmap (bimap getERatio getERatio) scores_
          transitions = fmap (fmap getERatio) transitions_
      todot book transitions scores sttmap