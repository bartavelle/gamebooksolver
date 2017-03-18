> {-# LANGUAGE RankNTypes #-}
> module LoneWolf.Combat (fight, getRatio) where

> import LoneWolf.Character
> import LoneWolf.Chapter
> import LoneWolf.CombatChart
> import Solver
>
> import qualified Data.MemoCombinators as Memo
> import Control.Lens
> import Data.Maybe

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
>                               Kill -> 0
>                               Damage x -> curhp - x
>   return ( (changeHp dmgLoneWolf (cvariable ^. curendurance), changeHp dmgOpponent (fdetails ^. fendurance) ), 1 / 10 )

> decrementTimed :: FightModifier -> Maybe FightModifier
> decrementTimed m = case m of
>                   Timed n x -> if n > 1 then Just (Timed (n - 1) x) else Nothing
>                   _ -> Just m

> getTimed :: FightModifier -> FightModifier
> getTimed m = case m of
>                   Timed _ x -> x
>                   _ -> m

> data FightType = Vanilla
>                | GodMode
>                | Mindblasted
>                deriving (Show, Eq, Enum, Bounded)

> fight :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably Endurance
> fight cconstant cvariable fdetails
>   | has (fightMod . traverse . _Evaded) fdetails =
>       (\(a,p) -> (fst a, p)) <$> fightRound cconstant cvariable fdetails
>   | has (fightMod . traverse . _Timed) fdetails = regroup $ do
>       ((hpLW, hpOpponent), p) <- fightRound cconstant cvariable fdetails
>       let outcome
>             | hpLW <= 0 = return (0, p)
>             | hpOpponent <= 0 = return (hpLW, p)
>             | otherwise = let nvariable = cvariable & curendurance .~ hpLW
>                               ndetails = fdetails & fendurance .~ hpOpponent
>                                                   & fightMod %~ mapMaybe decrementTimed
>                           in  fmap (*p) <$> fight cconstant nvariable ndetails
>       outcome
>   | otherwise = regroup $ do
>       let ratio = getRatio cconstant cvariable fdetails
>           modifiers = fdetails ^. fightMod
>           ohp = if DoubleDamage `elem` modifiers || (hasItem (Weapon Sommerswerd) (cvariable ^. equipment) && Undead `elem` modifiers)
>                   then (fdetails ^. fendurance + 1) `div` 2
>                   else fdetails ^. fendurance
>           ftype | PlayerInvulnerable `elem` modifiers = GodMode
>                 | EnemyMindblast `elem` modifiers && hasn't (discipline . traverse . _MindShield) cconstant = Mindblasted
>                 | otherwise = Vanilla
>       ((php, _), p) <- fightSimpleM ratio ftype (cvariable ^. curendurance) ohp
>       return (max 0 php, p)

> memo4 :: Memo.Memo a -> Memo.Memo b -> Memo.Memo c -> Memo.Memo d -> (a -> b -> c -> d -> r) -> (a -> b -> c -> d -> r)
> memo4 a b c d = a . (Memo.memo3 b c d .)

> fightSimpleM :: CombatSkill -- ratio
>              -> FightType
>              -> Endurance -- player hp
>              -> Endurance -- opponent hp
>              -> Probably (Endurance, Endurance)
> fightSimpleM = memo4 Memo.integral Memo.enum Memo.integral Memo.integral fightSimple

> fightSimple :: CombatSkill -- ratio
>             -> FightType
>             -> Endurance -- player hp
>             -> Endurance -- opponent hp
>             -> Probably (Endurance, Endurance)
> fightSimple ratio ftype php ohp
>   | php <= 0 || ohp <= 0 = certain (max 0 php, max 0 ohp)
>   | otherwise = regroup $ do
>       (odmgOpponent, odmgLoneWolf) <- hits ratio
>       let dmgLoneWolf = case (ftype, odmgLoneWolf) of
>                           (GodMode, _)            -> php
>                           (_, Kill)               -> 0
>                           (Vanilla, Damage x)     -> php - x
>                           (Mindblasted, Damage x) -> php - x - 2
>           dmgOpponent = case odmgOpponent of
>                           Kill -> 0
>                           Damage x -> ohp - x
>       fmap (/10) <$> fightSimpleM ratio ftype dmgLoneWolf dmgOpponent

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

