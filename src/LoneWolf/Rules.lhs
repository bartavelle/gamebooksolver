> {-# LANGUAGE DeriveGeneric #-}
> module LoneWolf.Rules where

> import LoneWolf.Character
> import LoneWolf.Chapter
>
> import qualified Data.Discrimination.Grouping as D
> import Control.Lens
> import GHC.Generics

> data NextStep = NewChapter ChapterId CharacterVariable
>               | HasLost
>               | HasWon CharacterVariable
>               deriving (Show, Eq, Generic)

> type Probably a = [(a, Proba)]

> instance D.Grouping NextStep where

> certain :: a -> Probably a
> certain a = [(a,1)]

> check :: CharacterConstant -> CharacterVariable -> BoolCond -> Bool
> check = undefined

> regroup :: D.Grouping a => Probably a -> Probably a
> regroup = map (\( (a,s): as ) -> (a, s + sum (map snd as)) ) . D.groupWith fst

> update :: CharacterConstant -> CharacterVariable -> ChapterOutcome -> Probably NextStep
> update cconstant cvariable outcome =
>   case outcome of
>     Goto cid -> certain (NewChapter cid cvariable)
>     GameLost -> certain HasLost
>     GameWon  -> certain (HasWon cvariable)
>     DamagePlayer dmg nxt ->
>       update cconstant (cvariable & curendurance -~ dmg) nxt
>     HealPlayer heal nxt ->
>       let newvariable = cvariable & curendurance %~ \hp -> max maxhp (hp + heal)
>       in  update cconstant newvariable nxt
>     FullHeal nxt ->
>       update cconstant (cvariable & curendurance .~ (cconstant ^. maxendurance)) nxt
>     HalfHeal nxt -> let newvariable = cvariable & curendurance %~ \hp -> (hp + maxhp) `div` 2
>                     in  update cconstant newvariable nxt
>     GainItem item count nxt ->
>       let newvariable = cvariable & equipment %~ addItem item count
>       in  update cconstant newvariable nxt
>     LoseItem item lost nxt ->
>       let newvariable = cvariable & equipment %~ delItem item lost
>       in  update cconstant newvariable nxt
>     LoseItemKind slots nxt ->
>       let newvariable = cvariable & equipment %~ delItemSlot slots
>       in  update cconstant newvariable nxt
>     MustEat canhunt nxt
>       | canhunt == Hunt && Hunting `elem` _discipline cconstant
>               -> update cconstant cvariable nxt
>       | hasItem Laumspur eqp && (maxhp - curhp >= 3) -> laumspur
>       | hasItem Meal eqp -> update cconstant cvariable (LoseItem Meal 1 nxt)
>       | hasItem Laumspur eqp -> laumspur
>       | otherwise -> update cconstant cvariable (DamagePlayer 3 nxt)
>       where laumspur = update cconstant cvariable (HealPlayer 3 (LoseItem Laumspur 1 nxt))
>             eqp = cvariable ^. equipment
>     Conditionally conditions -> uCheck conditions
>     Randomly rands -> regroup $ do
>       (p, o) <- rands
>       fmap (*p) <$> update cconstant cvariable o
>     Fight fd nxt -> regroup $ do
>       (charendurance, p) <- fight cconstant cvariable fd
>       case fd ^? fightMod . traverse . _FakeFight of
>          Nothing -> if charendurance <= 0
>                       then [(HasLost, p)]
>                       else fmap (*p) <$> update cconstant (cvariable & curendurance .~ charendurance) nxt
>          Just cid -> if charendurance <= 0
>                        then [(NewChapter cid cvariable, p)]
>                        else fmap (*p) <$> update cconstant cvariable nxt
>  where
>    curhp = cvariable ^. curendurance
>    maxhp = cconstant ^. maxendurance
>    uCheck conds = case conds of
>                     [] -> error "Can't happen"
>                     [(_, lst)] -> update cconstant cvariable lst
>                     ((c,o):cs) -> if check cconstant cvariable c
>                                     then update cconstant cvariable o
>                                     else uCheck cs

> fight :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably Endurance
> fight = undefined
