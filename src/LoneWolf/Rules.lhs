> {-# LANGUAGE DeriveGeneric #-}
> module LoneWolf.Rules where

> import LoneWolf.Character
> import LoneWolf.Chapter
> import LoneWolf.CombatChart
> import Solver
>
> import qualified Data.Discrimination.Grouping as D
> import Control.Lens
> import Data.Maybe
> import GHC.Generics

> data NextStep = NewChapter ChapterId CharacterVariable
>               | HasLost
>               | HasWon CharacterVariable
>               deriving (Show, Eq, Generic)

> instance D.Grouping NextStep where

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
>                        else map (fmap (*p)) $ update cconstant cvariable $
>                               case fd ^? fightMod . traverse . _Evaded . _2 of
>                                   Just evasionDestination -> Goto evasionDestination
>                                   Nothing -> nxt
>  where
>    curhp = cvariable ^. curendurance
>    maxhp = getMaxHp cconstant cvariable
>    uCheck conds = case conds of
>                     [] -> error "Can't happen"
>                     [(_, lst)] -> update cconstant cvariable lst
>                     ((c,o):cs) -> if check cconstant cvariable c
>                                     then update cconstant cvariable o
>                                     else uCheck cs

> getMaxHp :: CharacterConstant -> CharacterVariable -> Endurance
> getMaxHp cconstant cvariable = cconstant ^. maxendurance + if hasItem ChainMail (cvariable ^. equipment) then 4 else 0

> fight :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably Endurance
> fight cconstant cvariable fdetails = regroup $ do
>   ((hpLW, hpOpponent), p) <- fightRound cconstant cvariable fdetails
>   let evaded = fdetails ^? fightMod . traverse . _Evaded . _1 == Just 0
>       outcome
>         | hpLW <= 0 = return (-1, p)
>         | hpOpponent <= 0 || evaded = return (hpLW, p)
>         | otherwise = let nvariable = cvariable & curendurance .~ hpLW
>                           ndetails = fdetails & fendurance .~ hpOpponent
>                                               & fightMod %~ mapMaybe decrementTimed
>                       in  fmap (*p) <$> fight cconstant nvariable ndetails
>   outcome

> fightRound :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably (Endurance, Endurance)
> fightRound cconstant cvariable fdetails = regroup $ do
>   let ratio = getRatio cconstant cvariable fdetails
>       modifiers = map getTimed (fdetails ^. fightMod)
>   (odmgOpponent, odmgLoneWolf) <- hits ratio
>   let dmgLoneWolf | PlayerInvulnerable `elem` modifiers = Damage 0
>                   | EnemyMindblast `elem` modifiers && hasn't (discipline . traverse . _MindShield) cconstant = odmgLoneWolf & _Damage +~ 2
>                   | otherwise = odmgLoneWolf
>       dmgOpponent | DoubleDamage `elem` modifiers = odmgOpponent &_Damage *~ 2
>                   | hasItem (Weapon Sommerswerd) (cvariable ^. equipment) && Undead `elem` modifiers = odmgOpponent &_Damage *~ 2
>                   | otherwise = odmgOpponent
>       changeHp dmg curhp = case dmg of
>                               Kill -> -1
>                               Damage x -> curhp - x
>   return ( (changeHp dmgLoneWolf (cvariable ^. curendurance), changeHp dmgOpponent (fdetails ^. fendurance) ), 1 / 10 )

> decrementTimed :: FightModifier -> Maybe FightModifier
> decrementTimed m = case m of
>                   Timed n x -> if n > 1 then Just (Timed (n - 1) x) else Nothing
>                   Evaded n cid -> Just (Evaded (n - 1) cid)
>                   _ -> Just m

> getTimed :: FightModifier -> FightModifier
> getTimed m = case m of
>                   Timed _ x -> x
>                   _ -> m

> getRatio :: CharacterConstant -> CharacterVariable -> FightDetails -> CombatSkill
> getRatio cconstant cvariable fdetails =
>          baseSkill + weaponModifier + mindblastBonus + combatBonus + shieldBonus
>            - fdetails ^. fcombatSkill
>  where
>     baseSkill = cconstant ^. combatSkill
>     combatBonus = sumOf (traverse . _CombatBonus) modifiers
>     modifiers = map getTimed (fdetails ^. fightMod)
>     weapons = getWeapons (cvariable ^. equipment)
>     disciplines = cconstant ^. discipline
>     weaponModifier | BareHanded `elem` modifiers = -4
>                    | null weapons = -4
>                    | Sommerswerd `elem` weapons = if any ((`elem` disciplines) . WeaponSkill) [ShortSword, BroadSword, Sword]
>                                                       then 12
>                                                       else 10
>                    | any ((`elem` disciplines) . WeaponSkill) weapons = 2
>                    | otherwise = 0
>     mindblastBonus = if MindBlast `elem` disciplines && not (MindblastImmune `elem` modifiers)
>                          then 2
>                          else 0
>     shieldBonus = if hasItem Shield (cvariable ^. equipment) then 2 else 0

