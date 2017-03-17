module Main (main) where

import LoneWolf.Chapter
import LoneWolf.Book02

import Control.Lens
import Data.Data.Lens
import Control.Monad
import Data.List

getDestinations :: Decision -> [ChapterId]
getDestinations d =
    case d of
        Decisions lst -> concatMap (getDestinations . snd) lst
        CanTake _ _ d' -> getDestinations d'
        Cansell _ _ d' -> getDestinations d'
        Canbuy _ _ d' -> getDestinations d'
        Conditional _ d' -> getDestinations d'
        EvadeFight _ cid _ co -> cid : getDestinationsO co
        AfterCombat d' -> getDestinations d'
        Special s -> case s of
            Cartwheel -> [169,186]
            Portholes -> [197]
        NoDecision co -> getDestinationsO co
  where
    getDestinationsO co = case co of
                              Simple _ co' -> getDestinationsO co'
                              Fight _ co' -> getDestinationsO co'
                              Randomly lst -> concatMap (getDestinationsO . snd) lst
                              Conditionally lst -> concatMap (getDestinationsO . snd) lst
                              Goto c -> [c]
                              GameLost -> []
                              GameWon -> []

main :: IO ()
main = do
    putStrLn "digraph G {"
    putStrLn "rankdir=LR;"
    forM_ chapters $ \(_, Chapter title _ desc) -> do
        let hasCombat = has (outcomePlate . biplate . _Fight) desc || has (biplate . _EvadeFight) desc
            eating = preview (outcomePlate . biplate . _MustEat) desc
            style | eating == Just NoHunt = "[color=yellow style=filled]"
                  | eating == Just Hunt = "[color=yellow]"
                  | hasCombat = "[color=red]"
                  | otherwise = ""
        putStrLn (show title ++ " " ++ style ++ ";")
        forM_ (nub $ getDestinations desc) $ \dest -> do
            putStrLn (show title ++ " -> " ++ show (show dest))
    putStrLn "}"
