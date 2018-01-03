module LoneWolf.Simplify (simplify, path, getLinks, forSimplification) where

import LoneWolf.Chapter
import LoneWolf.Character
import Simplifier

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Graph as G
import Control.Monad.State
import Data.Monoid
import Control.Lens

import Debug.Trace

newtype ChapterSummary
  = ChapterSummary
  { _getSummary :: M.Map (S.Set ChapterId) (LinkType, ItemList)
  } deriving (Show, Eq)

newtype ItemList
  = ItemList
  { _getItemList :: M.Map Item Int
  } deriving (Show, Eq)

instance Monoid ItemList where
    mempty = ItemList mempty
    mappend (ItemList a) (ItemList b) = ItemList (M.unionWith (+) a b)

instance Monoid ChapterSummary where
    mempty = ChapterSummary mempty
    mappend (ChapterSummary a) (ChapterSummary b) = ChapterSummary (M.unionWith (<>) a b)

getSummary :: Lens' ChapterSummary (M.Map (S.Set ChapterId) (LinkType, ItemList))
getSummary f (ChapterSummary s) = ChapterSummary <$> f s

getItemList :: Lens' ItemList (M.Map Item Int)
getItemList f (ItemList lst) = ItemList <$> f lst

simplify :: CharacterConstant -> [(ChapterId, Chapter)] -> [(ChapterId, Chapter)]
simplify cconstant = IM.toList . simplify' cconstant . dropUnreachable cconstant . IM.fromList

path :: CharacterConstant -> IM.IntMap Chapter -> ChapterId -> ChapterId -> Bool
path cconstant chapters start end = evalState (go start) mempty
  where
    go :: ChapterId -> State (S.Set ChapterId) Bool
    go curnode
      | curnode == end = return True
      | otherwise = do
          visited <- use (contains curnode)
          if visited
            then return False
            else case IM.lookup curnode chapters of
              Nothing -> return False
              Just chapter -> do
                contains curnode .= True
                let targets = getCChildren cconstant (_pchoice chapter)
                    search True _ = return True
                    search False t = go t
                foldM search False targets

getCChildren :: CharacterConstant -> Decision -> S.Set ChapterId
getCChildren cconstant = mconcat . M.keys . _getSummary . getLinks cconstant

dropUnreachable :: CharacterConstant -> IM.IntMap Chapter -> IM.IntMap Chapter
dropUnreachable cconstant chapters = fmap pruneBadChoices chapters
  where
    pruneBadChoices = pchoice . _Decisions %~ filter (reachablechoice . snd)
    reachablechoice decision =
      let targets = getCChildren cconstant decision
      in  any (\i -> path cconstant chapters i 350) targets

data ConditionIs = Possible
                 | Impossible
                 | Unknown
                 deriving (Show, Eq, Ord)

instance Monoid ConditionIs where
    mempty = Unknown
    mappend a b | a == b = a
                | otherwise = Unknown

alt :: ChapterSummary -> ChapterSummary -> ChapterSummary
alt (ChapterSummary a) (ChapterSummary b) = ChapterSummary $ M.fromListWith (<>) $ do
   (ca, (la, ia)) <- M.toList a
   (cb, (lb, ib)) <- M.toList b
   return (ca <> cb, (la <> lb, ialt ia ib))

ialt :: ItemList -> ItemList -> ItemList
ialt (ItemList a) (ItemList b) = ItemList (M.intersectionWith min a b)

itemlist :: Item -> Int -> ItemList
itemlist i n = ItemList (M.singleton i n)

getLinks :: CharacterConstant -> Decision -> ChapterSummary
getLinks cconstant d = case d of
    Decisions decisions       -> foldMap (getLinks cconstant . snd) decisions
    CanTake i n d'            -> getLinks cconstant d' & getSummary . traverse <>~ (Good, itemlist i n)
    Canbuy _ _ d'             -> getLinks cconstant d' & getSummary . traverse . _1 <>~ Good
    Cansell _ _ d'            -> getLinks cconstant d' & getSummary . traverse . _1 <>~ Good
    Special Cartwheel         -> ChapterSummary (M.singleton (S.singleton 136) (Both, mempty))
    Special Portholes         -> ChapterSummary (M.singleton (S.singleton 197) (Neutral, mempty)) -- no game
    EvadeFight _ evadeid _ co -> getLinksO cconstant co & getSummary . traverse . _1 <>~ Bad & getSummary %~ M.insertWith (<>) (S.singleton evadeid) (Bad, mempty)
    AfterCombat d'            -> getLinks cconstant d'
    NoDecision o              -> getLinksO cconstant o
    Conditional c d'          -> case pcheck cconstant Nothing c of
                                   Impossible -> mempty
                                   _ -> getLinks cconstant d'

pcheck :: CharacterConstant -> Maybe ItemList -> BoolCond -> ConditionIs
pcheck cconstant mitems cond
  = case cond of
      Always True -> Possible
      Always False -> Impossible
      HasDiscipline d -> if d `elem` cconstant ^. discipline
                           then Possible
                           else Impossible
      Not c -> case pcheck cconstant mitems c of
                 Possible -> Impossible
                 Impossible -> Possible
                 Unknown -> Unknown
      COr c1 c2 -> case (pcheck cconstant mitems c1, pcheck cconstant mitems c2) of
                     (Unknown, _) -> Unknown
                     (_, Unknown) -> Unknown
                     (Possible, _) -> Possible
                     (_, Possible) -> Possible
                     (Impossible, Impossible) -> Impossible
      CAnd c1 c2 -> case (pcheck cconstant mitems c1, pcheck cconstant mitems c2) of
                     (Impossible, _) -> Impossible
                     (_, Impossible) -> Impossible
                     (Unknown, _) -> Unknown
                     (_, Unknown) -> Unknown
                     (Possible, Possible) -> Possible
      HasItem i n -> case mitems ^? _Just . getItemList . ix i of
                       Nothing -> Unknown
                       Just k -> if k >= n
                                   then Possible
                                   else Impossible

getLinksO :: CharacterConstant -> ChapterOutcome -> ChapterSummary
getLinksO cconstant co = case co of
    Fight _ o         -> go o & getSummary . traverse . _1 <>~ Bad
    Randomly lst      -> foldl1 alt (map (go . snd) lst)
    Conditionally lst -> foldMap (go . snd) (filter ( (/= Impossible) . pcheck cconstant Nothing . fst) lst )
    Goto x            -> ChapterSummary $ M.singleton (S.singleton x) (Neutral, mempty)
    GameLost          -> ChapterSummary $ M.singleton mempty (Bad, mempty)
    GameWon           -> ChapterSummary $ M.singleton mempty (Good, mempty)
    Simple effects o  -> getLinksO cconstant o & getSummary . traverse <>~ foldMap getEffectType effects
  where
   go = getLinksO cconstant

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

forSimplification :: CharacterConstant
                  -> IM.IntMap Chapter
                  -> (ChapterId -> Int, ChapterId -> S.Set ChapterId)
forSimplification cconstant chapters = (priority, childs)
  where
    childsMap = childMap cconstant chapters
    childs k = IM.findWithDefault mempty k childsMap
    priority k = IM.findWithDefault 999 k priomap
    priomap = IM.fromList (zip tsorted [0 :: Int ..])
    tsorted = G.topSort graph
    graph = G.buildG (1, 350) $ do
      (p, cs) <- IM.toList childsMap
      c <- S.toList cs
      return (p, c)

simplify' :: CharacterConstant -> IM.IntMap Chapter -> IM.IntMap Chapter
simplify' cconstant chapters = case bubbleTree priority childs 1 350 of
                                 Nothing -> chapters
                                 Just f -> trace (T.drawTree $ fmap show f) chapters
  where
    (priority, childs) = forSimplification cconstant chapters

childMap :: CharacterConstant -> IM.IntMap Chapter -> IM.IntMap (S.Set ChapterId)
childMap cconstant = fmap (getCChildren cconstant . _pchoice)

