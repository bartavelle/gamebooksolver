{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Codec.Compression.Zstd.Lazy (compress, decompress)
import qualified Codec.Serialise as S
import Control.Lens hiding (argument)
import Control.Monad (forM_, guard, when)
import Data.Aeson (eitherDecodeFileStrict, encode, encodeFile)
import Data.Bifunctor (first)
import Data.Bits.Lens (bitAt)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Data.Lens (biplate)
import Data.GraphViz (toDot)
import Data.GraphViz.Attributes (Shape (Record), color, filled, fontColor, shape, style, toLabel)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (Black, Blue, DarkGray, Gray, Red, White, Yellow))
import Data.GraphViz.Attributes.Complete (Attribute (RankDir, URL), RankDir (FromLeft, FromTop), RecordField (FieldLabel, FlipFields))
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Types.Canonical
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Word (Word64)
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
  ( Alternative (many, (<|>)),
    Parser,
    ParserInfo,
    argument,
    auto,
    command,
    eitherReader,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    progDesc,
    short,
    showDefault,
    strArgument,
    strOption,
    subparser,
    switch,
    value,
    (<**>),
  )
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
  Book01 -> Just (S.singleton Gold, S.fromList [PermanentSkillReduction, PermanentSkillReduction2])
  Book02 -> Just (S.empty, S.fromList [PermanentSkillReduction, PermanentSkillReduction2])
  Book03 -> Just (S.fromList [Weapon Sommerswerd, SilverHelm], S.fromList [PermanentSkillReduction, PermanentSkillReduction2])
  Book04 -> Just (S.fromList [Weapon Sommerswerd, SilverHelm], S.fromList [FoughtElix, PermanentSkillReduction, PermanentSkillReduction2])
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

data DotInfo = DExpectedAmount Item | DHasItem Item | DHasFlag Flag

bookDinfo :: Book -> [DotInfo]
bookDinfo b = case b of
  Book05 -> [DHasFlag LimbDeath]
  _ -> []

mkdot :: DecisionStats Rational -> DotGraph ChapterId
mkdot (DecisionStats book res sttmap) =
  DotGraph
    { strictGraph = False,
      directedGraph = True,
      graphID = Just (Str "G"),
      graphStatements = DotStmts {attrStmts = [], subGraphs = [], nodeStmts = nodes, edgeStmts = edges}
    }
  where
    chapters = pchapters book
    soltransitions = fmap _ctransitions res
    alltransitions :: M.Map ChapterId (M.Map ChapterId [String])
    alltransitions = M.fromList (fmap (fmap mkdestmap) chapters)
    mkdestmap = M.fromList . getDestinations book . _pchoice
    getedgedesc src dst = maybe [] (map show) (M.lookup src alltransitions >>= M.lookup dst)
    urlpart = case book of
      Book01 -> "01fftd"
      Book02 -> "02fotw"
      Book03 -> "03tcok"
      Book04 -> "04tcod"
      Book05 -> "05sots"
    srcdst s = S.fromList $ do
      (src, dsts) <- M.toList s
      dst <- M.keys dsts
      pure (src, dst)
    missingtransitions = S.difference (srcdst alltransitions) (srcdst soltransitions)
    nodes = do
      (cid, chapter) <- chapters
      let nodestyle
            | eating == Just NoHunt = [style filled, color Yellow]
            | eating == Just Hunt = [color Yellow]
            | combat = [color Red]
            | has (biplate . _GameLost) cdesc = [style filled, fontColor White]
            | has _Special cdesc = [color Blue]
            | has _Just chapterstats = [color Gray]
            | otherwise = [style filled]
          cdesc = _pchoice chapter
          combat = has (outcomePlate . biplate . _Fight) cdesc || has (biplate . _EvadeFight) cdesc
          eating = preview (outcomePlate . biplate . _MustEat) cdesc
          chapterstats = M.lookup cid res
          ctitle = _title chapter
          url = "https://www.projectaon.org/en/xhtml/lw/" ++ urlpart ++ "/sect" ++ ctitle ++ ".htm"
          scorelabels =
            case chapterstats of
              Just stts ->
                let ttl = sum (_cendurance stts)
                 in FlipFields
                      [ FieldLabel (T.pack (printf "%dstts" (M.findWithDefault 0 cid sttmap))),
                        FieldLabel (T.pack (percent (LoneWolf.Data._cscore stts))),
                        FieldLabel $
                          if ttl > 0
                            then T.pack (printf "%.2f hp" (fromRational @Double (sum [fromIntegral e * p / ttl | (e, p) <- M.toList (_cendurance stts)])))
                            else ""
                      ]
              Nothing -> FieldLabel (T.pack (printf "%d states" (M.findWithDefault 0 cid sttmap)))
          showDinfo di = case di of
            DHasFlag flg -> do
              stts <- chapterstats
              let amount = sum (M.filterWithKey (const . view (bitAt (fromEnum flg))) (_cflags stts))
              if amount > 0
                then Just (FlipFields [FieldLabel (T.pack (show flg)), FieldLabel (T.pack (percent amount))])
                else Nothing
            _ -> error "unhandled showDinfo"
          nodelabel = FieldLabel (T.pack ctitle) : scorelabels : mapMaybe showDinfo (bookDinfo book)
      pure (DotNode cid (shape Record : toLabel [FlipFields nodelabel] : URL (T.pack url) : nodestyle))
    showEdge mweight src dst = DotEdge src dst [color clr, toLabel lbl]
      where
        clr = if isJust mweight then Black else DarkGray
        lbl = unwords (maybe "" percent mweight : getedgedesc src dst)
    edges =
      map (uncurry (showEdge Nothing)) (S.toList missingtransitions) ++ do
        (src, dsts) <- M.toList soltransitions
        let ttl = sum dsts
            dsts' = if ttl > 0 then fmap (/ ttl) dsts else dsts
        (dst, weight) <- M.toList dsts'
        pure (showEdge (Just weight) src dst)

todot :: DecisionStats Rational -> IO ()
todot = T.putStrLn . renderDot . toDot . mkdot

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
    singleitem (i, c) = showItem (_bookid ccst) i ++ (if c == 1 then "" else "(" ++ show c ++ ")")

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
      rdstats <- do
        r <- eitherDecodeFileStrict pth
        case r of
          Left rr -> error rr
          Right x -> pure x
      let dstats = fmap getERatio rdstats
      todot dstats