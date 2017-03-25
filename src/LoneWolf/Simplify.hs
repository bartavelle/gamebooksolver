module LoneWolf.Simplify (simplify) where

import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.Rules

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import Control.Lens

newtype ChapterSummary = ChapterSummary { _getSummary :: M.Map (S.Set ChapterId) (LinkType, ItemList) }
                         deriving (Show, Eq)

newtype ItemList = ItemList { _getItemList :: M.Map Item Int }
                   deriving (Show, Eq)

instance Monoid ItemList where
    mempty = ItemList mempty
    mappend (ItemList a) (ItemList b) = ItemList (M.unionWith (+) a b)

instance Monoid ChapterSummary where
    mempty = ChapterSummary mempty
    mappend (ChapterSummary a) (ChapterSummary b) = ChapterSummary (M.unionWith (<>) a b)

getSummary :: Lens' ChapterSummary (M.Map (S.Set ChapterId) (LinkType, ItemList))
getSummary f (ChapterSummary s) = ChapterSummary <$> f s

simplify :: [(ChapterId, Chapter)] -> [(ChapterId, Chapter)]
simplify = IM.toList . simplify' 1 . IM.fromList

data LinkType = Neutral
              | Good
              | Bad
              | Both
              deriving (Show, Eq, Ord)

instance Monoid LinkType where
    mempty = Neutral
    Neutral `mappend` b = b
    a `mappend` Neutral = a
    a `mappend` b = if a == b
                        then a
                        else Both

alt :: ChapterSummary -> ChapterSummary -> ChapterSummary
alt (ChapterSummary a) (ChapterSummary b) = ChapterSummary $ M.fromListWith (<>) $ do
   (ca, (la, ia)) <- M.toList a
   (cb, (lb, ib)) <- M.toList b
   return (ca <> cb, (la <> lb, ialt ia ib))

ialt :: ItemList -> ItemList -> ItemList
ialt (ItemList a) (ItemList b) = ItemList (M.intersectionWith min a b)

itemlist :: Item -> Int -> ItemList
itemlist i n = ItemList (M.singleton i n)

getLinks :: CharacterConstant -> CharacterVariable -> Decision -> ChapterSummary
getLinks cconstant cvariable d = case d of
    Decisions decisions       -> foldMap (getLinks cconstant cvariable . snd) decisions
    CanTake i n d'            -> getLinks cconstant (cvariable & equipment %~ addItem i n) d' & getSummary . traverse <>~ (Good, itemlist i n)
    Canbuy _ _ d'             -> getLinks cconstant cvariable d' & getSummary . traverse . _1 <>~ Good
    Cansell _ _ d'            -> getLinks cconstant cvariable d' & getSummary . traverse . _1 <>~ Good
    Special Cartwheel         -> ChapterSummary (M.singleton (S.singleton 136) (Both, mempty))
    Special Portholes         -> ChapterSummary (M.singleton (S.singleton 197) (Neutral, mempty)) -- no game
    EvadeFight _ evadeid _ co -> getLinksO cconstant cvariable co & getSummary . traverse . _1 <>~ Bad & getSummary %~ M.insertWith (<>) (S.singleton evadeid) (Bad, mempty)
    AfterCombat d'            -> getLinks cconstant cvariable d'
    NoDecision o              -> getLinksO cconstant cvariable o
    Conditional c d'          -> if check cconstant cvariable c
                                     then getLinks cconstant cvariable d'
                                     else mempty

getLinksO :: CharacterConstant -> CharacterVariable -> ChapterOutcome -> ChapterSummary
getLinksO cconstant cvariable co = case co of
    Fight _ o         -> go o & getSummary . traverse . _1 <>~ Bad
    Randomly lst      -> foldl1 alt (map (go . snd) lst)
    Conditionally lst -> getLinksO cconstant cvariable (uCheck cconstant cvariable lst)
    Goto x            -> ChapterSummary $ M.singleton (S.singleton x) (Neutral, mempty)
    GameLost          -> ChapterSummary $ M.singleton mempty (Bad, mempty)
    GameWon           -> ChapterSummary $ M.singleton mempty (Good, mempty)
    Simple effects o  -> getLinksO cconstant cvariable o & getSummary . traverse <>~ foldMap getEffectType effects
  where
   go = getLinksO cconstant cvariable

getEffectType :: SimpleOutcome -> (LinkType, ItemList)
getEffectType so = case so of
    DamagePlayer _ -> (Bad, mempty)
    HealPlayer _   -> (Good, mempty)
    HalfHeal       -> (Good, mempty)
    FullHeal       -> (Good, mempty)
    LoseItemKind _ -> (Bad, mempty)
    LoseItem _ _   -> (Bad, mempty)
    MustEat _      -> (Bad, mempty)
    GainItem i n   -> (Good, itemlist i n)

simplify' :: ChapterId -> IM.IntMap Chapter -> IM.IntMap Chapter
simplify' = undefined getLinks
