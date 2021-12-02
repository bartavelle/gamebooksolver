module Main (main) where

import Control.Lens (to, (^.))
import Control.Monad (forM_, guard, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import LoneWolf.Book02 (pchapters)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Character
import LoneWolf.Data
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Solve (solveLWs)
import LoneWolf.StateSelector (Log, Selector, selectChar)
import Options.Applicative
import SimpleSolver
import Text.Printf (printf)

simpleSol :: CharacterConstant -> CharacterVariable -> [Int] -> (Rational, HM.HashMap NextStep (Solution NextStep String))
simpleSol ccst cvar target =
  let (solution, smap) = solveLWs target pchapters ccst cvar
   in (_score solution, smap)

data Opts = Opts
  { _optDebug :: Bool,
    _optCommand :: Command
  }

data Command
  = Explore SolDesc (Maybe ChapterId) (Maybe (Log Selector))
  | ShowStates SolDesc ChapterId
  | JsonDump SolDesc (Maybe FilePath)
  | Standard SolDesc
  | MultiStats [Discipline] CVarState

data SolDesc = SolDesc
  { _finalchapters :: [ChapterId],
    _ccst :: CharacterConstant,
    _cvar :: CVarState
  }

options :: Parser Opts
options = Opts <$> switch (long "debug" <> help "Verbose execution") <*> scommand

cconstant :: Parser CharacterConstant
cconstant =
  CharacterConstant
    <$> fmap Endurance (option auto (long "endurance" <> short 'e' <> help "Max endurance" <> value 25))
    <*> fmap CombatSkill (option auto (long "skill" <> short 's' <> help "Combat skill" <> value 15))
    <*> disciplines

disciplines :: Parser [Discipline]
disciplines = many (option auto (long "discipline" <> short 'd' <> help "Disciplines"))

soldesc :: Parser SolDesc
soldesc =
  SolDesc
    <$> many (option auto (long "stopat" <> metavar "CHAPTER" <> help "Stop at these chapters"))
    <*> cconstant
    <*> cvariable

scommand :: Parser Command
scommand =
  subparser
    ( command
        "explore"
        ( info
            ( Explore
                <$> soldesc
                  <*> optional (argument auto (metavar "CHAPTERID" <> help "Start at this chapter"))
                  <*> optional (option auto (long "filter" <> help "Character filter"))
            )
            (progDesc "Explore the solution")
        )
        <> command
          "jsondump"
          ( info
              ( JsonDump <$> soldesc <*> optional (strArgument (metavar "PATH" <> help "Dumped file path"))
              )
              (progDesc "Dump as JSON")
          )
        <> command "standard" (info (Standard <$> soldesc) (progDesc "Just display statistics"))
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

showRecap :: Foldable t => SolDesc -> Rational -> t a -> IO ()
showRecap (SolDesc _ ccst cvar) score solmap = do
  putStrLn ("CONSTANT: " ++ show ccst)
  putStrLn ("VARIABLE: " ++ show (mkchar ccst cvar))
  putStrLn ("STATES:   " ++ show (length solmap))
  putStrLn ("WIN:      " ++ show (fromRational score :: Double))

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
    NewChapter _ cv _ -> shortState cv

shortState :: CharacterVariable -> String
shortState s = "e:" ++ printf "%02d" (s ^. curendurance . to getEndurance) ++ " " ++ itemlist (items (s ^. equipment))
  where
    itemlist = intercalate "/" . map singleitem
    singleitem (i, c) =
      itm ++ (if c == 1 then "" else "(" ++ show c ++ ")")
      where
        itm = case i of
          Weapon x -> show x
          _ -> show i

main :: IO ()
main = do
  Opts debug cmd <- execParser programOpts
  case cmd of
    Standard sd -> do
      let (r, solmap) = getSol sd
      showRecap sd r solmap
    JsonDump sd mtarget -> do
      let (_, solmap) = getSol sd
          encoded = encode $ HM.toList $ fmap chopSolution solmap
      case mtarget of
        Just pth -> BSL.writeFile pth encoded
        Nothing -> BSL.putStr encoded
    Explore sd mcid mselector -> do
      let (_, solmap) = getSol sd
          chapterid = fromMaybe 1 mcid
          startStates = filter startChapter (HM.keys solmap)
          check = maybe (const True) selectChar mselector
          startChapter c =
            case c of
              NewChapter cid cvar' _ -> cid == chapterid && check cvar'
              _ -> False
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