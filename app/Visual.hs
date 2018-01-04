module Main (main) where

import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Book02
import LoneWolf.Simplify
import Simplifier

import Control.Lens
import Data.Data.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import System.Environment (getArgs)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

getDestinations :: Decision -> [(ChapterId, [String])]
getDestinations d =
    case d of
        Decisions lst -> concatMap (getDestinations . snd) lst
        CanTake i c d' -> append (takeDesc i c) $ getDestinations d'
        Cansell _ _ d' -> append "sell" $ getDestinations d'
        Canbuy _ _ d' -> append "buy" $ getDestinations d'
        Conditional c d' -> append (condDesc c) $ getDestinations d'
        EvadeFight _ cid _ co -> (cid, ["evade"]) : append "fight" (getDestinationsO co)
        AfterCombat d' -> getDestinations d'
        Special s -> case s of
            Cartwheel -> [(169,["lost"]),(186,["won"])]
            Portholes -> [(197,[])]
        NoDecision co -> getDestinationsO co
  where
    itemDesc i c = case i of
                       Gold -> show c ++ "$"
                       Weapon _ -> "w"
                       TicketVol2 -> "TKT"
                       Meal ->show c ++ "M"
                       _ -> show i
    takeDesc i c = '+' : itemDesc i c
    condDesc c = case c of
                     Always True -> ""
                     Always False -> "never"
                     HasItem i cnt -> '?' : itemDesc i cnt
                     Not co -> '!' : condDesc co
                     COr a b -> condDesc a ++ "||" ++ condDesc b
                     CAnd a b -> condDesc a ++ "&&" ++ condDesc b
                     HasDiscipline di -> '?' : case di of
                                                  SixthSense -> "6th"
                                                  Hunting -> "hunt"
                                                  MindOverMatter -> "MoM"
                                                  Healing -> "heal"
                                                  Tracking -> "trk"
                                                  MindBlast -> "mb"
                                                  Camouflage -> "camo"
                                                  AnimalKinship -> "ak"
                                                  _ -> show di
    append s = map (fmap (s:))
    sdesc s = case s of
                  LoseItemKind k | length k == 4 -> Just "LOSEALL"
                  LoseItem i c -> Just ('-' : itemDesc i c)
                  MustEat _ -> Just "eat"
                  HealPlayer h -> Just ('+' : show (getEndurance h) ++ "HP")
                  DamagePlayer h -> Just ('-' : show (getEndurance h) ++ "HP")
                  FullHeal -> Just "+100%HP"
                  HalfHeal -> Just "+50%HP"
                  _ -> Just (show s)
    getDestinationsO co = case co of
                              Simple s co' -> map (fmap (mapMaybe sdesc s ++)) (getDestinationsO co')
                              Fight _ co' -> getDestinationsO co'
                              Randomly lst -> concatMap (\(p, o) -> append ("r(" ++ show (numerator p) ++ "/" ++ show (denominator p) ++ ")") (getDestinationsO o)) lst
                              Conditionally lst -> concatMap (\(c,o) -> append (condDesc c) $ getDestinationsO o) lst
                              Goto c -> [(c, [])]
                              GameLost -> []
                              GameWon -> []

main :: IO ()
main = do
    disciplines <- map read <$> getArgs
    putStrLn "digraph G {"
    putStrLn "rankdir=LR;"
    let cconstant = CharacterConstant 25 25 disciplines
        (priority, _, childsComplete) = forSimplification cconstant (IM.fromList chapters)
        badEdges = uselessEdges priority childsComplete 1 350
    forM_ (simplify cconstant chapters) $ \(cnumber, Chapter ctitle _ cdesc) -> do
        let hasCombat = has (outcomePlate . biplate . _Fight) cdesc || has (biplate . _EvadeFight) cdesc
            eating = preview (outcomePlate . biplate . _MustEat) cdesc
            style | eating == Just NoHunt = "color=yellow style=filled"
                  | eating == Just Hunt = "color=yellow"
                  | hasCombat = "color=red"
                  | has (biplate . _GameLost) cdesc = "fillcolor=black style=filled fontcolor=white"
                  | has _Special cdesc = "shape=square color=blue"
                  | otherwise = ""
            label | has (biplate . _GameLost) cdesc = ctitle ++ " :("
                  | otherwise = ctitle
        putStrLn (show ctitle ++ " [" ++ style ++ " label=" ++ show label ++ " URL=\"https://www.projectaon.org/en/xhtml/lw/02fotw/sect" ++ ctitle ++ ".htm\"];")
        forM_ (nub $ getDestinations cdesc) $ \(dest, comment) ->
            putStrLn (show ctitle ++ " -> " ++ show (show dest) ++ " [label=" ++ show (unwords (nub comment)) ++ " color=" ++ (if (cnumber, dest) `S.member` badEdges then "red" else "black") ++ "]")
    putStrLn "}"
