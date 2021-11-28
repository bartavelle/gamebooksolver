module Main (main) where

import Control.Monad
import qualified Data.HashMap.Strict as HM
import LoneWolf.Book02 (pchapters)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Solve
import LoneWolf.StateSelector
import Options.Applicative
import SimpleSolver

simpleSol :: [Int] -> (Rational, HM.HashMap NextStep (Solution NextStep String))
simpleSol target =
  let (solution, smap) = solveLWs target pchapters startConstant startVariable
   in (_score solution, smap)

data Opts = Opts
  { _finalchapters :: [ChapterId],
    _exploreAt :: Maybe ChapterId,
    _cfilter :: Maybe (Log Selector)
  }

options :: Parser Opts
options =
  Opts
    <$> many (argument auto (metavar "CHAPTER" <> help "Stop at these chapters"))
    <*> optional (option auto (long "explore" <> help "Start exploring at this chapter"))
    <*> optional (option auto (long "filter" <> help "Character filter"))

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

main :: IO ()
main = do
  putStrLn "Solving for the following initial state:"
  print startConstant
  print startVariable
  opts <- execParser programOpts
  let checkChapters cs
        | null cs = [39]
        | otherwise = cs
  let (score, solmap) = simpleSol (checkChapters (_finalchapters opts))
  putStrLn ("Winning probability: " ++ show (fromRational score :: Double) ++ " [" ++ show score ++ "]")
  putStrLn ("solution map size: " ++ show (length solmap))
  forM_ (_exploreAt opts) $ \chapterid ->
    let startStates = filter startChapter (HM.keys solmap)
        check = case (_cfilter opts) of
          Nothing -> const True
          Just flt -> selectChar flt
        startChapter c =
          case c of
            NewChapter cid cvar _ -> cid == chapterid && check cvar
            _ -> False
     in case startStates of
          [] -> putStrLn ("Expression " ++ show (_cfilter opts) ++ " did not select anything")
          sstate : _ -> case HM.lookup sstate solmap of
            Nothing -> putStrLn "No solution?!?"
            Just sol -> exploreSolution sol