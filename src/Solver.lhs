---
title: Solving a single player game!
date: 2017-03-11
series: Game book solver
---

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE BangPatterns #-}
> module Solver where

> import qualified Data.MemoCombinators as Memo
> import Data.Ord (comparing)
> import Data.List
> import qualified Data.Map.Strict as M
> import Control.Parallel.Strategies
> import Data.Function (on)
> import GHC.Generics

> type Proba = Rational
> type Probably a = [(a, Proba)]
> type Choice state description = (description, Probably state)
> type Solver state description = state -> (description, Rational, Probably state)

> data Solution state description = Node { _desc    :: !description
>                                        , _stt     :: !state
>                                        , _score   :: !PScore
>                                        , _outcome :: !(Probably (Solution state description))
>                                        }
>                                 | LeafLost
>                                 | Leaf !Rational !state
>                                 deriving (Show, Eq, Generic)

> data PScore = Approximate { _proba :: !Proba, _pscore :: !Rational, _next :: PScore }
>             | Certain !Rational
>             deriving (Show, Eq, Generic)

> scoreCompare :: Rational -> PScore -> PScore -> Ordering
> scoreCompare _ (Certain sa) (Certain sb) = compare sa sb
> scoreCompare smax na@(Certain sa) (Approximate pb sb nb)
>   | sa < sb = LT
>   | sa > sb / pb = GT
>   | otherwise = scoreCompare smax na nb
> scoreCompare smax (Approximate pa sa na) nb@(Certain sb)
>   | sb < sa = GT
>   | sb > sa / pa = LT
>   | otherwise = scoreCompare smax na nb
> scoreCompare smax ca@(Approximate pa sa na) cb@(Approximate pb sb nb)
>   | sa / pa > sb + (1 - pb) * smax = GT
>   | sb / pb > sa + (1 - pa) * smax = LT
>   | pa > pb   = scoreCompare smax ca nb
>   | otherwise = scoreCompare smax na cb

> getCertain :: PScore -> Rational
> getCertain ps = case ps of
>                   Certain x -> x
>                   Approximate _ _ n -> getCertain n

> data Score = Known !Rational | Unknown

> certain :: a -> Probably a
> certain a = [(a,1)]

> regroup :: Ord a => Probably a -> Probably a
> regroup = M.toList . M.fromListWith (+)

> winStates :: Ord state => Solution state description -> Probably state
> winStates s = case s of
>   LeafLost -> []
>   Leaf _ st -> certain st
>   Node _ _ _ ps -> regroup $ concat $ parMap rseq (\(o,p) -> fmap (*p) <$> winStates o) ps

> getSolScore :: Solution state description -> Rational
> getSolScore s = case s of
>                  LeafLost     -> 0
>                  Leaf x _     -> x
>                  Node _ _ x _ -> getCertain x

> solve :: Memo.Memo state
>       -> Rational -- max score
>       -> (state -> [Choice state description])
>       -> (state -> Score)
>       -> state
>       -> Solution state description
> solve memo maxScore getChoice score = go
>   where
>     go = memo solve'
>     solve' stt =
>       case score stt of
>           Known x -> Leaf x stt
>           Unknown -> if null choices
>                       then LeafLost
>                       else maximumBy (scoreCompare maxScore `on` _score) scored
>       where
>         choices = getChoice stt
>         scored = parMap rseq scoreTree choices
>         scoreTree (cdesc, pstates) = let ptrees = map (\(o, p) -> (go o, p)) pstates
>                                      in Node cdesc stt (mkPScore ptrees) ptrees

This works on the assumption the states are grouped!

> mkPScore :: [(Solution state description, Proba)] -> PScore
> mkPScore = go 0 0 . sortBy (flip (comparing snd))
>   where
>     go curproba curscore lst =
>       case lst of
>         [] -> Certain curscore
>         ((st, p) : xs) ->
>           let !nproba = curproba + p
>               !nscore = curscore + (getSolScore st) * p
>           in  if nproba == 1
>                   then Certain nscore
>                   else Approximate nproba nscore (go nproba nscore xs)

