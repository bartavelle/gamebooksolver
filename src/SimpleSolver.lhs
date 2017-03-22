> {-# LANGUAGE RankNTypes #-}
> module SimpleSolver where

> import Data.Ord (comparing)
> import Data.List
> import Solver hiding (solve, Solution(..), getSolScore, winStates)
> import qualified Data.MemoCombinators as Memo
> import Control.Parallel.Strategies

> data Solution state description = Node { _desc :: description
>                                        , _stt  :: state
>                                        , _score :: Rational
>                                        , _outcome :: Probably (Solution state description)
>                                        }
>                                 | LeafLost
>                                 | Leaf Rational state
>                                 deriving (Show, Eq)

> getSolScore :: Solution state description -> Rational
> getSolScore s = case s of
>                  LeafLost     -> 0
>                  Leaf x _     -> x
>                  Node _ _ x _ -> x

> winStates :: Ord state => Solution state description -> Probably state
> winStates s = case s of
>   LeafLost      -> []
>   Leaf _ st     -> certain st
>   Node _ _ _ ps -> regroup $ concat $ parMap rseq (\(o,p) -> fmap (*p) <$> winStates o) ps

> solve :: Memo.Memo state
>       ->  (state -> [Choice state description]) -- the choice function
>       -> (state -> Score)
>       -> state
>       -> Solution state description
> solve memo getChoice score = go
>  where
>   go = memo solve'
>   solve' stt =
>     case score stt of
>         Known x -> Leaf x stt
>         Unknown -> if null choices
>                     then LeafLost
>                     else maximumBy (comparing getSolScore) scored
>     where
>       choices = getChoice stt
>       scored = parMap rseq scoreTree choices
>       scoreTree (cdesc, pstates) =
>           let ptrees = map (\(o,p) -> (go o, p)) pstates
>           in  Node cdesc stt (sum (map (\(o, p) -> p * getSolScore o) ptrees)) ptrees

