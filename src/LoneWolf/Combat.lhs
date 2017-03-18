> module LoneWolf.Combat (fight) where

> import LoneWolf.Character
> import LoneWolf.Chapter
> import LoneWolf.CombatChart
> import Solver
>
> import Control.Lens
> import Data.Maybe

> fight :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably Endurance
> fight cconstant cvariable fdetails = regroup $ do
>   ((hpLW, hpOpponent), p) <- fightRound cconstant cvariable fdetails
>   let evaded = has (fightMod . traverse . _Evaded) fdetails
>       outcome
>         | hpLW <= 0 = return (-1, p)
>         | hpOpponent <= 0 || evaded = return (hpLW, p)
>         | otherwise = let nvariable = cvariable & curendurance .~ hpLW
>                           ndetails = fdetails & fendurance .~ hpOpponent
>                                               & fightMod %~ mapMaybe decrementTimed
>                       in  fmap (*p) <$> fight cconstant nvariable ndetails
>   outcome

> data FightType = Vanilla
>                | GodMode
>                | Mindblasted
>                deriving (Show, Eq)


> fightSimple :: CombatSkill -- ratio
>             -> FightType
>             -> Endurance -- player hp
>             -> Endurance -- opponent hp
>             -> Probably (Endurance, Endurance)
> fightSimple ratio ftype php ohp
>   | php <= 0 || ohp <= 0 = certain (php, ohp)
>   | otherwise = regroup $ do
>       (odmgOpponent, odmgLoneWolf) <- hits ratio
>       let dmgLoneWolf = case (ftype, odmgLoneWolf) of
>                           (GodMode, _)            -> php
>                           (_, Kill)               -> -1
>                           (Vanilla, Damage x)     -> php - x
>                           (Mindblasted, Damage x) -> php - x - 2
>           dmgOpponent = case odmgOpponent of
>                           Kill -> -1
>                           Damage x -> ohp - x
>       fightSimple ratio ftype dmgLoneWolf dmgOpponent

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

