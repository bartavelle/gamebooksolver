module Main (main) where

import Control.Lens
import Data.Data.Lens
import Data.List
import Data.Maybe (mapMaybe)
import Data.Ord
import LoneWolf.Book02
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Rules (NextStep)
import LoneWolf.Solve
import qualified SimpleSolver as S
import Solver
import System.Environment
import Text.Printf
import Text.Read (readMaybe)

pchapters :: [(ChapterId, Chapter)]
pchapters = map patch chapters
  where
    patch (200, Chapter t d _) = (200, Chapter t d (NoDecision (Goto 158)))
    patch (314, Chapter t d dc) = (314, Chapter t d (limitMoneyAt 12 dc))
    patch (33, Chapter t d dc) = (33, Chapter t d (limitMoneyAt 12 dc))
    patch (150, Chapter t d _) = (150, Chapter t d (NoDecision (Simple [MustEat Hunt] condhammer))) -- patch, better way
    patch x = x
    condhammer =
      Conditionally
        [ (HasItem SealHammerdalVol2 1, Goto 15),
          (Always True, Goto 244)
        ]
    limitMoneyAt maxmoney dc =
      dc & biplate %~ \o ->
        Conditionally
          [ (HasItem Gold maxmoney, Simple [LoseItemKind [PouchSlot], GainItem Gold maxmoney] o),
            (Always True, o)
          ]

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
