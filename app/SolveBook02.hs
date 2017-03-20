module Main where

import LoneWolf.Solve
import LoneWolf.Book02
import Solver

import Text.Printf
import Data.List
import Data.Ord

main :: IO ()
main = do
    putStrLn "solving..."
    let solution = solveLW chapters startConstant startVariable
    print (fromRational (_score solution) :: Double)
    let wstates = winStates solution
        showWinState (st, p) = printf "%.4f %s\n" (fromRational p :: Double) (show st)
    mapM_ showWinState (sortBy (flip (comparing snd)) wstates)
