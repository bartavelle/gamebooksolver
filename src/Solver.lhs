---
title: Solving a single player game!
date: 2017-03-11
series: Game book solver
---

> module Solver where

> import qualified Data.Discrimination.Grouping as D

> type Proba = Rational
> type Probably a = [(a, Proba)]

> certain :: a -> Probably a
> certain a = [(a,1)]

> regroup :: D.Grouping a => Probably a -> Probably a
> regroup = map (\( (a,s): as ) -> (a, s + sum (map snd as)) ) . D.groupWith fst

