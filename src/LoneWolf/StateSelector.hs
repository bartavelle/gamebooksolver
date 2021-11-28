{-# LANGUAGE GADTs #-}

module LoneWolf.StateSelector where

import Control.Lens
import LoneWolf.Character

data Log a = P a | And [Log a] | Or [Log a] | Not (Log a)
  deriving (Show, Read)

matcher :: (a -> Bool) -> Log a -> Bool
matcher p l =
  case l of
    P x -> p x
    And lst -> all (matcher p) lst
    Or lst -> any (matcher p) lst
    Not x -> not (matcher p x)

data Selector
  = HasItem Item
  | WithHp Ordering Endurance
  deriving (Show, Read)

selectChar :: Log Selector -> CharacterVariable -> Bool
selectChar sel cvar = matcher m sel
  where
    inv = cvar ^. equipment
    end = cvar ^. curendurance
    m (HasItem i) = hasItem i inv
    m (WithHp ord target) = compare end target == ord