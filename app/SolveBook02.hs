module Main (main) where

import Control.Monad (forM_)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import LoneWolf.Book02 (pchapters)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Character
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Solve (solveLWs)
import LoneWolf.StateSelector (Log, Selector, selectChar)
import Options.Applicative
import SimpleSolver

simpleSol :: CharacterConstant -> CharacterVariable -> [Int] -> (Rational, HM.HashMap NextStep (Solution NextStep String))
simpleSol ccst cvar target =
  let (solution, smap) = solveLWs target pchapters ccst cvar
   in (_score solution, smap)

data Opts
  = Explore SolDesc (Maybe ChapterId) (Maybe (Log Selector))
  | JsonDump SolDesc (Maybe FilePath)
  | Standard SolDesc
  | MultiStats [Discipline] CVarState

data SolDesc = SolDesc
  { _finalchapters :: [ChapterId],
    _ccst :: CharacterConstant,
    _cvar :: CVarState
  }

data CVarState = CVarState
  { _items :: [Item],
    _gold :: Int,
    _meals :: Int
  }

mkchar :: CharacterConstant -> CVarState -> CharacterVariable
mkchar cst (CVarState sitems gld cmeals) = mkCharacter (_maxendurance cst) (inventoryFromList allitems)
  where
    numbereditems = M.toList $
      M.fromListWith (+) $ do
        i <- sitems
        pure (i, 1)
    citems = if null sitems then [(Weapon ShortSword, 1), (SealHammerdalVol2, 1)] else numbereditems
    allitems = citems ++ [(Gold, gld), (Meal, cmeals)]

cconstant :: Parser CharacterConstant
cconstant =
  CharacterConstant
    <$> fmap Endurance (option auto (long "endurance" <> short 'e' <> help "Max endurance" <> value 25))
    <*> fmap CombatSkill (option auto (long "skill" <> short 's' <> help "Combat skill" <> value 15))
    <*> disciplines

disciplines :: Parser [Discipline]
disciplines = many (option auto (long "discipline" <> short 'd' <> help "Disciplines"))

cvariable :: Parser CVarState
cvariable =
  CVarState
    <$> many (option auto (long "item" <> short 'i' <> help "Starting items (default items if empty)"))
    <*> option auto (long "gold" <> help "Starting gold" <> value 15)
    <*> option auto (long "meals" <> help "Starting meals" <> value 2)

soldesc :: Parser SolDesc
soldesc =
  SolDesc
    <$> many (option auto (long "stopat" <> metavar "CHAPTER" <> help "Stop at these chapters"))
    <*> cconstant
    <*> cvariable

options :: Parser Opts
options =
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

main :: IO ()
main = do
  cmd <- execParser programOpts
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
    MultiStats discs variable ->
      forM_ [20 .. 29] $ \e -> forM_ [10 .. 19] $ \s -> do
        let cst = CharacterConstant e s discs
            (score, _) = getSol (SolDesc [350] cst variable)
        print (e, s, fromRational score :: Double)