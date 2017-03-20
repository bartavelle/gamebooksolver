---
title: Solving a single player game!
date: 2017-03-11
series: Game book solver
---

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Solver where

> import qualified Data.MemoCombinators as Memo
> import Data.Ord (comparing)
> import Data.List
> import Data.Hashable
> import qualified Data.HashMap.Strict as HM

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

> regroup :: (Eq a, Hashable a) => Probably a -> Probably a
> regroup = HM.toList . HM.fromListWith (+)

> winStates :: (Eq state, Hashable state) => Solution state description -> Probably state
> winStates s = case s of
>   LeafLost -> []
>   LeafWin _ st -> certain st
>   Node _ _ _ ps -> regroup $ do
>       (o, p) <- ps
>       fmap (*p) <$> winStates o

> getSolScore :: Solution state description -> Rational
> getSolScore s = case s of
>                  LeafLost -> 0
>                  LeafWin x _ -> x
>                  Node _ _ x _ -> x

> solve ::  Memo.Memo state
>        -> (state -> Choice state description) -- the choice function
>        -> (state -> Score)
>        -> state
>        -> Solution state description
> solve memo getChoice score = go
>   where
>     go = memo solve'
>     solve' stt =
>       case score stt of
>           Lose -> LeafLost
>           Win x -> LeafWin x stt
>           Unknown -> if null choices
>                       then LeafLost
>                       else maximumBy (comparing getSolScore) scored
>       where
>         choices = getChoice stt
>         scored = do
>           (cdesc, pstates) <- choices
>           let ptrees = map (\(o, p) -> (go o, p)) pstates
>           return (Node cdesc stt (sum (map (\(o, p) -> p * getSolScore o) ptrees)) ptrees)

