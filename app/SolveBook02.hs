module Main (main) where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import LoneWolf.Book02 (pchapters)
import LoneWolf.Rules (NextStep (..))
import LoneWolf.Solve
import qualified SimpleSolver as S
import Solver
import System.Environment (getArgs)
import Text.Read (readMaybe)

defaultSol :: [Int] -> (Rational, SolMap NextStep)
defaultSol target =
  let solution = solveLW target pchapters startConstant startVariable
   in (getCertain (_score solution), toSolMap (HasLost 0) solution)

simpleSol :: [Int] -> (Rational, SolMap NextStep)
simpleSol target =
  let solution = solveLWs target pchapters startConstant startVariable
   in (S._score solution, S.toSolMap (HasLost 0) solution)

main :: IO ()
main = do
  putStrLn "Solving for the following initial state:"
  print startConstant
  print startVariable
  args <- getArgs
  let (score, solmap) = case args of
        ("simple" : xs) -> simpleSol (checkChapters $ mapMaybe readMaybe xs)
        _ -> defaultSol (checkChapters $ mapMaybe readMaybe args)
      checkChapters cs
        | null cs = [39]
        | otherwise = cs
  putStrLn ("Winning probability: " ++ show (fromRational score :: Double) ++ " [" ++ show score ++ "]")
  putStrLn ("solution map size: " ++ show (length solmap))
  mapM_ print (M.toList solmap)