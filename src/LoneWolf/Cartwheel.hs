module LoneWolf.Cartwheel (allmatrices, solveFor, showMatrix, graph) where

import Linear.Matrix
import Control.Lens
import qualified Data.IntMap as IM
import Data.List
import Text.Printf

type Matrix a = IM.IntMap (IM.IntMap a)

solveFor :: Int -> Int -> [(Int, Rational)]
solveFor target startmoney = IM.toList (fmap toRational (IM.filter (/= 0) (idouble !* IM.singleton startmoney 1)))
  where
    idouble = IM.findWithDefault (approx target) target allmatrices

emptyMatrix :: Num a => Int -> Matrix a
emptyMatrix n = IM.fromListWith (IM.unionWith (+)) $ do
    x <- [0 .. n + 6]
    y <- [0 .. n + 6]
    return (x, IM.singleton y 0)

transition :: Fractional a => Int -> Matrix a
transition n = foldl' ins (emptyMatrix n) (transitions ++ zero ++ fixedWon)
  where
   ins :: IM.IntMap (IM.IntMap a) -> (Int, Int, a) -> IM.IntMap (IM.IntMap a)
   ins mtx (x,y,r) = mtx & ix y . ix x .~ r
   zero = [(0,0,1)]
   fixedWon = do
       x <- [n .. n + 6]
       return (x,x,1)
   transitions = do
    x <- [1 .. n - 1]
    [ (x, max 0 (x - 1), 7 / 10), (x, x + 7, 1 / 10), (x, x + 4, 2 / 10) ]

approx :: Fractional a => Int -> Matrix a
approx n = iterate (\a -> a !*! a) (transition n) !! 20

allmatrices :: IM.IntMap (Matrix Double)
allmatrices = IM.fromList [ (n, approx n) | n <- [21..50] ]

-- | Only used for the blog post
showMatrix :: Matrix Double -> String
showMatrix mtx = unlines $ map (\line -> "<tr>" ++ showCell line ++ "</tr>") (IM.elems mtx)
  where
   showCell line = mconcat $ do
       (_, v) <- IM.toList line
       return ("<td>" ++ (if v == 0 then "" else take 4 (printf "%f" v)) ++ "</td>")

-- | Only used for the blog post
graph :: String
graph = unlines $ do
    (a, (p1, p2)) <- z
    return (unwords [show a, show p1, show p2])
  where
    z = zip p22 p40 & map (\( (a,p1), (_,p2) )  -> (a, (p1, p2) ) )
    p22 = (approx 22 :: Matrix Double) & IM.findWithDefault mempty 0 & IM.toList
    p40 = (approx 50 :: Matrix Double) & IM.findWithDefault mempty 0 & IM.toList


