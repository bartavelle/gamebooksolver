---
title: Solving a single player game!
date: 2017-03-11
series: Game book solver
---

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Solver where

> import qualified Data.Discrimination.Grouping as D
> import Data.Ord (comparing)
> import Data.List

> type Proba = Rational
> type Probably a = [(a, Proba)]
> type Choice state description = [(description, Probably state)]
> type Solver state description = state -> (description, Rational, Probably state)

> data Solution state description = Node { _desc :: description
>                                        , _stt  :: state
>                                        , _score :: Rational
>                                        , _outcome :: Probably (Solution state description)
>                                        }
>                                 | LeafLost
>                                 | LeafWin Rational state
>                                 deriving (Show, Eq)

> data Score = Lose | Win Rational | Unknown

> certain :: a -> Probably a
> certain a = [(a,1)]

> regroup :: D.Grouping a => Probably a -> Probably a
> regroup = map (\( (a,s): as ) -> (a, s + sum (map snd as)) ) . D.groupWith fst

> getSolScore :: Solution state description -> Rational
> getSolScore s = case s of
>                  LeafLost -> 0
>                  LeafWin x _ -> x
>                  Node _ _ x _ -> x

> solve ::  (state -> Choice state description) -- the choice function
>        -> (state -> Score)
>        -> state
>        -> Solution state description
> solve getChoice score stt =
>   case score stt of
>       Lose -> LeafLost
>       Win x -> LeafWin x stt
>       Unknown -> if null choices
>                   then LeafLost
>                   else maximumBy (comparing getSolScore) scored
>   where
>     choices = getChoice stt
>     scored = do
>       (cdesc, pstates) <- choices
>       let ptrees = do
>               (o, p) <- pstates
>               return (solve getChoice score o, p)
>       return (Node cdesc stt (sum (map (\(o, p) -> p * getSolScore o) ptrees)) ptrees)

