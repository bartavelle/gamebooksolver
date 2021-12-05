{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Codec.Compression.Zstd.Lazy (compress, decompress)
import qualified Codec.Serialise as S
import Control.Lens (to, (^.))
import Control.Monad (forM_, guard, when)
import Data.Aeson (encode, encodeFile)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import LoneWolf.Book02 (pchapters)
import LoneWolf.Chapter (ChapterId, FightDetails (..))
import LoneWolf.Character
import LoneWolf.Combat (expectedEndurance, winchance)
import LoneWolf.Data
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Solve (orderChapters, solveLWs, step)
import LoneWolf.StateSelector (Log (..), Selector (..), parseSelector, selectChar)
import Options.Applicative
import SimpleSolver
import Solver (Probably)
import Text.Printf (printf)
import Text.Read (readMaybe)

simpleSol :: CharacterConstant -> CharacterVariable -> [Int] -> (Rational, HM.HashMap NextStep (Solution NextStep String))
simpleSol ccst cvar target =
  let (solution, smap) = solveLWs target pchapters ccst cvar
   in (_score solution, smap)

data Opts = Opts
  { _optDebug :: Bool,
    _optCommand :: Command
  }

data DumpMode = Json | CBor
  deriving (Eq)

data Command
  = Explore SolDesc (Maybe (Log Selector))
  | ExploreFile FilePath (Maybe (Log Selector))
  | ShowStates SolDesc ChapterId
  | SolDump DumpMode SolDesc (Maybe FilePath)
  | MultiStats [Discipline] CVarState
  | ExtractFile FilePath (Log Selector)
  | DecisionTracker FilePath FightDetails (Log Selector)

options :: Parser Opts
options = Opts <$> switch (long "debug" <> help "Verbose execution") <*> scommand

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

disciplines :: Parser [Discipline]
disciplines = many (option (maybeReader decodedisc) (long "discipline" <> short 'd' <> help "Disciplines"))
  where
    decodedisc s = M.lookup s rdiscnames <|> readMaybe s

dumpmode :: Parser DumpMode
dumpmode = option (maybeReader validator) (long "mode" <> help "dump mode (json, cbor)" <> value CBor)
  where
    validator "json" = Just Json
    validator "cbor" = Just CBor
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
        "explore"
        ( info
            (Explore <$> soldesc <*> optional charfilter)
            (progDesc "Explore the solution")
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
              ( SolDump <$> dumpmode <*> soldesc <*> optional (strArgument (metavar "PATH" <> help "Dumped file path"))
              )
              (progDesc "Dump a solution")
          )
        <> command
          "multistats"
          ( info
              ( MultiStats
                  <$> disciplines
                  <*> cvariable
              )
              (progDesc "Multi stats")
          )
        <> command "showstates" (info (ShowStates <$> soldesc <*> argument auto (metavar "CHAPTERID" <> help "Chapter with states to explore")) (progDesc "Show states"))
        <> command "extract" (info (ExtractFile <$> strArgument (metavar "PATH" <> help "saved cbor game") <*> charfilter) (progDesc "Extract solution data from a file"))
        <> command "decisiontracker" (info (DecisionTracker <$> strArgument (metavar "PATH" <> help "saved cbor game") <*> fightdetails <*> charfilter) (progDesc "Displays stats about how a decision is taken"))
    )

programOpts :: ParserInfo Opts
programOpts =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Solve and explore book02 solutions"
    )

exploreSolution :: Solution NextStep String -> IO ()
exploreSolution sol =
  case sol of
    LeafLost -> putStrLn "LOST"
    Leaf score stt -> do
      putStrLn "DONE"
      print score
      print stt
    Node dsc stt _ outcome -> do
      print stt
      putStrLn dsc
      case outcome of
        [] -> putStrLn "no outcomes"
        [(nsol, _)] -> do
          putStrLn "Single outcome"
          putStrLn ""
          exploreSolution nsol
        _ -> do
          let showSubsol ss =
                case ss of
                  LeafLost -> "LOST"
                  Leaf _ _ -> "FINISH"
                  Node dsc' _ sc' _ -> dsc' ++ " -> sc=" ++ show (fromRational sc' :: Double)
          forM_ (zip [0 :: Int ..] outcome) $ \(idx, (subsol, pb)) -> putStrLn (show idx ++ " " ++ show (fromRational pb :: Double) ++ " " ++ showSubsol subsol)
          idx <- read <$> getLine
          exploreSolution (fst (outcome !! idx))

getSol :: SolDesc -> (Rational, HM.HashMap NextStep (Solution NextStep String))
getSol (SolDesc fchapters ccst ccvar) =
  let cvar = mkchar ccst ccvar
      finalchapters = case fchapters of
        [] -> [350]
        fc -> fc
   in simpleSol ccst cvar finalchapters

shortSolstate :: Solution NextStep String -> String
shortSolstate s = case s of
  LeafLost -> "!lost"
  Leaf sc es -> "win " ++ printf "%.03f" (fromRational sc :: Double) ++ " -> " ++ shortNS es
  Node _ stt _ _ -> shortNS stt

shortNS :: NextStep -> String
shortNS ns =
  case ns of
    HasLost _ -> "lost"
    HasWon _ -> "won"
    NewChapter cid cv _ -> "ch:" ++ show cid ++ " " ++ shortState cv

shortState :: CharacterVariable -> String
shortState s = "e:" ++ printf "%2d" (s ^. curendurance . to getEndurance) ++ " " ++ itemlist (items (s ^. equipment))
  where
    itemlist = intercalate "/" . map singleitem
    singleitem (i, c) =
      itm ++ (if c == 1 then "" else "(" ++ show c ++ ")")
      where
        itm = case i of
          Weapon x -> show x
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
    book = IM.fromList pchapters
    order = orderChapters book
    stepper = step order book cc
    go ns = do
      let nstates = stepper ns
          advance :: Rational -> Probably NextStep -> IO ()
          advance score outcomes = do
            putStrLn (percent score ++ " - " ++ shortNS ns)
            -- selection
            (_, ch) <- selector (\(desc, os) -> (if os == outcomes then " *** " else "  -  ") ++ desc) nstates
            -- outcome
            (cns, _) <- selector (\(sns, p) -> percent p ++ " - " ++ shortNS sns) ch
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
ctarget (NewChapter _ cv _) (CNode _ ((Just (NewChapter tgt _ _), _) : _)) = Just (cv, tgt)
ctarget (NewChapter _ cv _) (CJump _ (NewChapter tgt _ _)) = Just (cv, tgt)
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

main :: IO ()
main = do
  Opts debug cmd <- execParser programOpts
  case cmd of
    SolDump dmode sd mtarget -> do
      let (_, solmap) = getSol sd
          dmap = SolutionDump sd (HM.toList (fmap chopSolution solmap))
          todump = case dmode of
            Json -> encode dmap
            CBor -> compress 3 (S.serialise dmap)
      case mtarget of
        Just pth -> do
          BSL.writeFile pth todump
          when (dmode == CBor) $ encodeFile (pth ++ ".json") (soldumpSummary dmap)
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
    Explore sd mselector -> do
      let (_, solmap) = getSol sd
          startStates = filter (selectChar check) (HM.keys solmap)
          check = fromMaybe (P (InChapter 1)) mselector
      case startStates of
        [] -> putStrLn ("Expression " ++ show mselector ++ " did not select anything")
        sstate : _ -> case HM.lookup sstate solmap of
          Nothing -> putStrLn "No solution?!?"
          Just sol -> exploreSolution sol
    MultiStats discs scvariable ->
      let entries = do
            e <- if AnimalKinship `elem` discs then [20 .. 29] else [20, 25, 29]
            s <- if AnimalKinship `elem` discs then [10 .. 19] else [10, 15, 19]
            let cst = CharacterConstant e s discs
                (score, solmap) = getSol (SolDesc [350] cst scvariable)
            pure (MultistatEntry e s (fromRational score) score (HM.size solmap))
       in do
            when debug (forM_ entries $ \(MultistatEntry e s c _ _) -> print (e, s, c))
            BSL.putStr (encode (Multistat discs scvariable entries))
    ShowStates sd chapterid -> do
      let (_, solmap) = getSol sd
          filterState (c, v) =
            case c of
              NewChapter cid cv _ -> (cv, v) <$ guard (cid == chapterid)
              _ -> Nothing
          startStates = mapMaybe filterState (HM.toList solmap)
      forM_ startStates $ \(k, v) -> do
        let l2 = case v of
              Node desc _ score outcomes -> printf "%.03f " (fromRational score :: Double) ++ targetstate ++ " " ++ desc
                where
                  targetstate = case outcomes of
                    [] -> "[]"
                    [(x, _)] -> shortSolstate x
                    _ -> "[" ++ show (length outcomes) ++ "]"
              _ -> shortSolstate v
        putStrLn (shortState k)
        putStrLn (" -> " ++ l2)
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
