---
title: Solving a single player game!
date: 2017-03-11
series: Game book solver
---

> module Solver where

> type Proba = Rational
> type Probably a = [(a, Proba)]

> certain :: a -> Probably a
> certain a = [(a,1)]

