module Main where

import LoneWolf.Solve
import LoneWolf.Book02
import Solver

main :: IO ()
main = print (getSolScore $ solveLW chapters startConstant startVariable)
