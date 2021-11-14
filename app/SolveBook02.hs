module Main (main) where

import Data.Maybe (mapMaybe)
import LoneWolf.Book02
import LoneWolf.Rules (NextStep)
import LoneWolf.Solve
import qualified SimpleSolver as S
import Solver
import System.Environment
import Text.Read (readMaybe)

defaultSol :: [Int] -> (Rational, [(NextStep, Proba)])
defaultSol target =
  let solution = solveLW target pchapters startConstant startVariable
   in (getCertain (_score solution), winStates solution)

simpleSol :: [Int] -> (Rational, [(NextStep, Proba)])
simpleSol target =
  let solution = solveLWs target pchapters startConstant startVariable
   in (S._score solution, S.winStates solution)

main :: IO ()
main = do
  putStrLn "Solving for the following initial state:"
  print startConstant
  print startVariable
  args <- getArgs
  let (score, wstates) = case args of
        ("simple" : xs) -> simpleSol (checkChapters $ mapMaybe readMaybe xs)
        _ -> defaultSol (checkChapters $ mapMaybe readMaybe args)
      checkChapters cs
        | null cs = [39]
        | otherwise = cs
  putStrLn ("Winning probability: " ++ show (fromRational score :: Double) ++ " [" ++ show score ++ "]")

--   let showWinState (st, p) = printf "%.4f %s\n" (fromRational p :: Double) (show st)
--   mapM_ showWinState (sortBy (flip (comparing snd)) wstates)
