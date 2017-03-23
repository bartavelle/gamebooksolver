module LoneWolf.Simplify (simplify) where

import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Rules

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Lens

simplify :: [(ChapterId, Chapter)] -> [(ChapterId, Chapter)]
simplify = IM.toList . simplify' 1 . IM.fromList

data LinkType = Neutral
              | Good (M.Map Item Int)
              | Bad
              | Both
              deriving (Show, Eq, Ord)

instance Monoid LinkType where
    mempty = Neutral
    Neutral `mappend` b = b
    Good a `mappend` Good b = Good (M.unionWith (+) a b)
    a `mappend` Neutral = a
    a `mappend` b = if a == b
                        then a
                        else Both

getLinks :: CharacterConstant -> CharacterVariable -> Decision -> IM.IntMap LinkType
getLinks cconstant cvariable d = case d of
    Decisions decisions       -> IM.unionsWith (<>) (map (getLinks cconstant cvariable . snd) decisions)
    CanTake i n d'            -> fmap (Good (M.singleton i n) <>) (getLinks cconstant (cvariable & equipment %~ addItem i n) d')
    Canbuy _ _ d'             -> fmap (Good mempty <>) (getLinks cconstant cvariable d')
    Cansell _ _ d'            -> fmap (Good mempty <>) (getLinks cconstant cvariable d')
    Special Cartwheel         -> IM.singleton 136 Both
    Special Portholes         -> IM.singleton 197 Neutral -- no game
    EvadeFight _ evadeid _ co -> IM.insertWith (<>) evadeid Bad (fmap (Bad <>) (getLinksO cconstant cvariable co))
    AfterCombat d'            -> getLinks cconstant cvariable d'
    NoDecision o              -> getLinksO cconstant cvariable o
    Conditional c d'          -> if check cconstant cvariable c
                            then getLinks cconstant cvariable d'
                            else mempty

getLinksO :: CharacterConstant -> CharacterVariable -> ChapterOutcome -> IM.IntMap LinkType
getLinksO cconstant cvariable co = case co of
    Fight _ o         -> fmap (Bad <>) (go o)
    Randomly lst      -> IM.unionsWith (<>) (map (go . snd) lst)
    Conditionally lst -> getLinksO cconstant cvariable (uCheck cconstant cvariable lst)
    Goto x            -> IM.singleton x Neutral
    GameLost          -> IM.singleton 1000 Bad
    GameWon           -> IM.singleton 350 (Good mempty)
    Simple effects o  -> fmap (foldMap getEffectType effects <>) (getLinksO cconstant cvariable o)
  where
   go = getLinksO cconstant cvariable

getEffectType :: SimpleOutcome -> LinkType
getEffectType so = case so of
    DamagePlayer _ -> Bad
    HealPlayer _   -> Good mempty
    HalfHeal       -> Good mempty
    FullHeal       -> Good mempty
    LoseItemKind _ -> Bad
    LoseItem _ _   -> Bad
    MustEat _      -> Bad
    GainItem i n   -> Good (M.singleton i n)

simplify' :: ChapterId -> IM.IntMap Chapter -> IM.IntMap Chapter
simplify' = undefined getLinks
