{-# LANGUAGE TemplateHaskell #-}
module LoneWolf.CombatChart where

import LoneWolf.Character
import Control.Lens

data Hits = Kill | Damage Endurance deriving (Show, Eq)

makePrisms ''Hits

-- | (Opponent, LoneWolf)
hits :: CombatSkill -> [(Hits,Hits)]
hits ratio = hits' nratio
    where
        nratio | ratio < -10 = -6
               | ratio >  10 =  6
               | otherwise   = ratio `div` 2
        hits' (-6) = [ ( Damage 0, Kill   ), (  Damage 0,  Kill   ), (  Damage 0,  Damage 8), ( Damage 0,  Damage 8), ( Damage 1,  Damage 7), ( Damage 2,  Damage 6), ( Damage 3,  Damage 5), ( Damage 4,  Damage 4), ( Damage 5,  Damage 3), ( Damage 6,  Damage 0) ]
        hits' (-5) = [ ( Damage 0, Kill   ), (  Damage 0,  Damage 8), ( Damage 0,  Damage 7), ( Damage 1,  Damage 7), ( Damage 2,  Damage 6), ( Damage 3,  Damage 6), ( Damage 4,  Damage 5), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 0) ]
        hits' (-4) = [ ( Damage 0, Damage 8), ( Damage 0,  Damage 7), ( Damage 1,  Damage 6), ( Damage 2,  Damage 6), ( Damage 3,  Damage 5), ( Damage 4,  Damage 5), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 2), ( Damage 8,  Damage 0) ]
        hits' (-3) = [ ( Damage 0, Damage 6), ( Damage 1,  Damage 6), ( Damage 2,  Damage 5), ( Damage 3,  Damage 5), ( Damage 4,  Damage 4), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 2), ( Damage 8,  Damage 0), ( Damage 9,  Damage 0) ]
        hits' (-2) = [ ( Damage 1, Damage 6), ( Damage 2,  Damage 5), ( Damage 3,  Damage 5), ( Damage 4,  Damage 4), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 2), ( Damage 8,  Damage 1), ( Damage 9,  Damage 0), ( Damage 10, Damage 0) ]
        hits' (-1) = [ ( Damage 2, Damage 5), ( Damage 3,  Damage 5), ( Damage 4,  Damage 4), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 2), ( Damage 8,  Damage 2), ( Damage 9,  Damage 1), ( Damage 10, Damage 0), ( Damage 11, Damage 0) ]
        hits'   0  = [ ( Damage 3, Damage 5), ( Damage 4,  Damage 4), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 2), ( Damage 8,  Damage 2), ( Damage 10, Damage 1), ( Damage 10, Damage 0), ( Damage 11, Damage 0), ( Damage 12, Damage 0) ]
        hits'   1  = [ ( Damage 4, Damage 5), ( Damage 5,  Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 3), ( Damage 8,  Damage 2), ( Damage 9,  Damage 2), ( Damage 11, Damage 1), ( Damage 11, Damage 0), ( Damage 12, Damage 0), ( Damage 14, Damage 0) ]
        hits'   2  = [ ( Damage 5, Damage 4), ( Damage 6,  Damage 3), ( Damage 7,  Damage 3), ( Damage 8,  Damage 2), ( Damage 9,  Damage 2), ( Damage 10, Damage 2), ( Damage 12, Damage 1), ( Damage 12, Damage 0), ( Damage 14, Damage 0), ( Damage 16, Damage 0) ]
        hits'   3  = [ ( Damage 6, Damage 4), ( Damage 7,  Damage 3), ( Damage 8,  Damage 3), ( Damage 9,  Damage 2), ( Damage 10, Damage 2), ( Damage 11, Damage 1), ( Damage 14, Damage 0), ( Damage 14, Damage 0), ( Damage 16, Damage 0), ( Damage 18, Damage 0) ]
        hits'   4  = [ ( Damage 7, Damage 4), ( Damage 8,  Damage 3), ( Damage 9,  Damage 2), ( Damage 10, Damage 2), ( Damage 11, Damage 2), ( Damage 12, Damage 1), ( Damage 14, Damage 0), ( Damage 16, Damage 0), ( Damage 18, Damage 0), ( Kill   ,   Damage 0) ]
        hits'   5  = [ ( Damage 8, Damage 3), ( Damage 9,  Damage 3), ( Damage 10, Damage 2), ( Damage 11, Damage 2), ( Damage 12, Damage 2), ( Damage 14, Damage 1), ( Damage 16, Damage 0), ( Damage 18, Damage 0), ( Kill   ,   Damage 0), ( Kill   ,   Damage 0) ]
        hits'   6  = [ ( Damage 9, Damage 3), ( Damage 10, Damage 2), ( Damage 11, Damage 2), ( Damage 12, Damage 2), ( Damage 14, Damage 1), ( Damage 16, Damage 1), ( Damage 18, Damage 0), ( Kill   ,   Damage 0), ( Kill   ,   Damage 0), ( Kill   ,   Damage 0) ]

        hits'   a = error ("hits " ++ show a)
