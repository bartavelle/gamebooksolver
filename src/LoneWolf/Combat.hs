{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module LoneWolf.Combat (fight, getRatio, Escaped (..), winchance, expectedEndurance, fightRound, rustResult) where

import Control.Lens
import Data.Maybe (mapMaybe)
import qualified Data.MemoCombinators as Memo
import Data.Monoid (Sum (Sum, getSum))
import Data.Ratio (denominator, numerator)
import qualified Data.Set as S
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.CombatChart (hits)
import Solver (Probably, certain, regroup)

data CombatInfo = CombatInfo
  { _skilldiff :: CombatSkill,
    _specialization :: Maybe Weapon,
    _weapons :: [Weapon],
    _modifiers :: [FightModifier],
    _discs :: [Discipline],
    _cflags :: [Flag],
    _shield :: Bool,
    _silverhelm :: Bool,
    _lwendurance :: Endurance,
    _opendurance :: Endurance
  }

decrementTimed :: FightModifier -> Maybe FightModifier
decrementTimed m = case m of
  Timed n x ->
    if n > 1
      then Just (Timed (n - 1) x)
      else case x of
        Evaded _ -> Just x
        OnNotYetWon _ -> Just x
        ForceEMindblast -> Just x
        _ -> Nothing
  _ -> Just m

getTimed :: FightModifier -> FightModifier
getTimed m = case m of
  Timed _ x -> x
  _ -> m

data FightType
  = Vanilla
  | GodMode
  | Mindblasted
  deriving (Show, Eq, Enum, Bounded)

data Escaped a = HasEscaped Int a | NotEscaped a | LateWin Int a | Lost ChapterId | Stopped ChapterId a
  deriving (Functor, Eq, Ord, Show)

winchance :: CharacterConstant -> CharacterVariable -> FightDetails -> Rational
winchance cc cv fd = sum $ do
  (end, p) <- fight cc cv fd
  pure $ case end of
    NotEscaped hp -> if hp > 0 then p else 0
    HasEscaped _ _ -> p
    LateWin _ _ -> p
    Stopped _ hp -> if hp > 0 then p else 0
    Lost _ -> 0

expectedEndurance :: CharacterConstant -> CharacterVariable -> FightDetails -> Rational
expectedEndurance cc cv fd = sum $ do
  (end, p) <- fight cc cv fd
  pure $ case end of
    HasEscaped _ hp -> fromIntegral hp * p
    LateWin _ hp -> fromIntegral hp * p
    NotEscaped hp -> fromIntegral hp * p
    Stopped _ hp -> fromIntegral hp * p
    Lost _ -> 0

fightVanillaM :: CombatSkill -> Endurance -> Endurance -> Probably (Endurance, Endurance)
fightVanillaM = Memo.memo3 Memo.bits Memo.bits Memo.bits fightVanilla

fightVanilla :: CombatSkill -> Endurance -> Endurance -> Probably (Endurance, Endurance)
fightVanilla ratio php ohp
  | php <= 0 || ohp <= 0 = certain (max 0 php, max 0 ohp)
  | otherwise = regroup $ do
    (odmg, pdmg) <- hits ratio
    fmap (/ 10) <$> fightVanillaM ratio (php - pdmg) (ohp - odmg)

fightMindBlastedM :: CombatSkill -> Endurance -> Endurance -> Probably (Endurance, Endurance)
fightMindBlastedM = Memo.memo3 Memo.integral Memo.integral Memo.integral fightMindBlasted

fightMindBlasted :: CombatSkill -> Endurance -> Endurance -> Probably (Endurance, Endurance)
fightMindBlasted ratio php ohp
  | php <= 0 || ohp <= 0 = certain (max 0 php, max 0 ohp)
  | otherwise = regroup $ do
    (odmg, pdmg) <- hits ratio
    fmap (/ 10) <$> fightMindBlastedM ratio (php - pdmg - 2) (ohp - odmg)

cmodifiers :: Lens' CombatInfo [FightModifier]
cmodifiers f cinfo = (\m' -> cinfo {_modifiers = m'}) <$> f (_modifiers cinfo)

lwendurance :: Lens' CombatInfo Endurance
lwendurance f cinfo = (\m' -> cinfo {_lwendurance = m'}) <$> f (_lwendurance cinfo)

opendurance :: Lens' CombatInfo Endurance
opendurance f cinfo = (\m' -> cinfo {_opendurance = m'}) <$> f (_opendurance cinfo)

relevantDiscs :: S.Set Discipline
relevantDiscs = S.fromList [MindBlast, MindShield]

relevantFlags :: S.Set Flag
relevantFlags = S.fromList [LimbDeath, PermanentSkillReduction, PermanentSkillReduction2, StrengthPotionActive, PotentStrengthPotionActive]

mkCombatInfo :: CharacterConstant -> CharacterVariable -> FightDetails -> CombatInfo
mkCombatInfo cconstant cvariable fdetails = CombatInfo skill specialization weapons modifiers discs cflags shield silverhelm nlwendurance nopendurance
  where
    skill = cconstant ^. combatSkill - fdetails ^. fcombatSkill
    specialization = cconstant ^? discipline . traverse . _WeaponSkill
    weapons = getWeapons (cvariable ^. equipment)
    modifiers = S.toList $ S.fromList (fdetails ^. fightMod)
    discs = S.toList $ S.fromList (cconstant ^. discipline) `S.intersection` relevantDiscs
    cflags = S.toList $ S.fromList (allFlags cvariable) `S.intersection` relevantFlags
    shield = hasItem Shield (cvariable ^. equipment)
    silverhelm = hasItem Helmet (cvariable ^. equipment) && hasFlag HelmetIsSilver cvariable
    nlwendurance = cvariable ^. curendurance
    nopendurance = fdetails ^. fendurance

getRatio' :: CombatInfo -> CombatSkill
getRatio' cinfo =
  _skilldiff cinfo
    + weaponModifier
    + mindblastBonus
    + combatBonus
    + shieldBonus
    + onflag PermanentSkillReduction (-1)
    + onflag PermanentSkillReduction2 (-2)
    + onflag StrengthPotionActive 2
    + onflag PotentStrengthPotionActive 4
    + onflag LimbDeath (-3)
    + helmBonus
  where
    flgs = _cflags cinfo
    onflag f r = if f `elem` flgs then r else 0
    combatBonus = sumOf (traverse . _CombatBonus) modifiers
    modifiers = map getTimed (_modifiers cinfo)
    weapons = _weapons cinfo
    disciplines = _discs cinfo
    wskill = _specialization cinfo
    weaponModifier
      | BareHanded `elem` modifiers = -4
      | null weapons = -4
      | Sommerswerd `elem` weapons =
        if wskill `elem` [Just ShortSword, Just BroadSword, Just Sword]
          then 10
          else 8
      | MagicSpear `elem` weapons =
        if wskill == Just Spear
          then 2
          else 0
      | Just sk <- wskill, sk `elem` weapons = 2
      | otherwise = 0
    mindblastBonus =
      if MindBlast `elem` disciplines && notElem MindblastImmune modifiers
        then 2
        else 0
    shieldBonus = if _shield cinfo && LimbDeath `notElem` flgs then 2 else 0
    helmBonus = if _silverhelm cinfo then 2 else 0

fightRound' :: CombatInfo -> Probably (Endurance, Endurance)
fightRound' cinfo = regroup $ do
  let ratio = getRatio' cinfo
      modifiers = map getTimed (_modifiers cinfo)
      dpr = getSum (cinfo ^. cmodifiers . folded . _DPR . to Sum)
  (rdmgOpponent, odmgLoneWolf) <- hits ratio
  let odmgOpponent = rdmgOpponent + dpr
  let dmgLoneWolf
        | PlayerInvulnerable `elem` modifiers = 0
        | (EnemyMindblast `elem` modifiers && MindShield `notElem` _discs cinfo) || ForceEMindblast `elem` modifiers = odmgLoneWolf + 2
        | otherwise = odmgLoneWolf
      dmgOpponent
        | EnemyInvulnerable `elem` modifiers = 0
        | DoubleDamage `elem` modifiers = odmgOpponent * 2
        | Sommerswerd `elem` _weapons cinfo && Undead `elem` modifiers = odmgOpponent * 2
        | otherwise = odmgOpponent
      changeHp dmg curhp = max 0 (curhp - dmg)
  return ((changeHp dmgLoneWolf (_lwendurance cinfo), changeHp dmgOpponent (_opendurance cinfo)), 1 / 10)

fightM :: CombatInfo -> Probably (Escaped Endurance)
fightM = fight'

fight' :: CombatInfo -> Probably (Escaped Endurance)
fight' cinfo
  | Just ecid <- cinfo ^? cmodifiers . folded . _StopFight = certain (Stopped ecid (cinfo ^. lwendurance))
  | Just ecid <- cinfo ^? cmodifiers . folded . _Evaded = regroup $ do
    ((hpLW, _), p) <- fightRound' cinfo
    pure $
      if hpLW <= 0
        then (lost, p)
        else (HasEscaped ecid hpLW, p)
  | Just instakill <- preview (cmodifiers . folded . _Poisonous) cinfo = regroup $ do
    let prevlwhp = _lwendurance cinfo
        prevophp = _opendurance cinfo
    -- bullshit fight :( there can be cases where nothing happens, so it's looping :(
    let roundresults = regroup $ do
          ((lwhp, ophp), p) <- fightRound' cinfo
          if lwhp < prevlwhp
            then [((True, 1), p * instakill), ((False, ophp), p * (1 - instakill))]
            else [((False, ophp), p)]

        useful = filter ((/= (False, prevophp)) . fst) roundresults
        usefulproba = sum (map snd useful)
    ((instadeath, ophp), rp) <- useful
    let p = rp / usefulproba
    let outcome
          | instadeath = pure (lost, p)
          | ophp <= 0 = pure (NotEscaped prevlwhp, p)
          | otherwise =
            let ncinfo = cinfo & opendurance .~ ophp & cmodifiers %~ mapMaybe decrementTimed
             in fmap (* p) <$> fightM ncinfo
    outcome
  -- we can't run the optimized combat if there are still timed effects, or DPR effects
  | has (cmodifiers . folded . _Timed) cinfo || has (cmodifiers . folded . _DPR) cinfo = regroup $ do
    ((hpLW, hpOpponent), p) <- fightRound' cinfo
    let outcome
          | hpLW <= 0 = return (lost, p)
          | hpOpponent <= 0 = return (NotEscaped hpLW, p)
          | otherwise =
            let ncinfo = cinfo & lwendurance .~ hpLW & opendurance .~ hpOpponent & cmodifiers %~ mapMaybe decrementTimed
             in fmap (* p) <$> fightM ncinfo
    outcome
  | otherwise = regroup $ do
    let ratio = getRatio' cinfo
        modifiers = cinfo ^. cmodifiers
        ohp =
          if DoubleDamage `elem` modifiers || (Sommerswerd `elem` _weapons cinfo && Undead `elem` modifiers)
            then (cinfo ^. opendurance + 1) `div` 2
            else cinfo ^. opendurance
        ftype
          | PlayerInvulnerable `elem` modifiers = error "PlayerInvulnerable is only a timed effect"
          | (EnemyMindblast `elem` modifiers && MindShield `notElem` _discs cinfo) || ForceEMindblast `elem` modifiers = fightMindBlastedM
          | otherwise = fightVanillaM
    ((php, _), p) <- ftype ratio (cinfo ^. lwendurance) ohp
    let constr = maybe NotEscaped LateWin (cinfo ^? cmodifiers . folded . _OnNotYetWon)
    pure $
      if php <= 0
        then (lost, p)
        else (constr (max 0 php), p)
  where
    lost = case cinfo ^? cmodifiers . folded . _OnLose of
      Nothing -> NotEscaped 0
      Just tid -> Lost tid

fight :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably (Escaped Endurance)
fight cconstant cvariable fdetails = fightM (mkCombatInfo cconstant cvariable fdetails)

getRatio :: CharacterConstant -> CharacterVariable -> FightDetails -> CombatSkill
getRatio cconstant cvariable fdetails = getRatio' (mkCombatInfo cconstant cvariable fdetails)

fightRound :: CharacterConstant -> CharacterVariable -> FightDetails -> Probably (Endurance, Endurance)
fightRound cconstant cvariable fdetails = fightRound' (mkCombatInfo cconstant cvariable fdetails)

rustResult :: Probably (Escaped Endurance) -> [String]
rustResult = map format
  where
    format (c, r) =
      "Proba {v: " ++ showC c
        ++ ", p: Rational::from(("
        ++ show (numerator r)
        ++ ", "
        ++ show (denominator r)
        ++ "))},"
    showC (NotEscaped (Endurance e)) = "Escaped::Std(Endurance(" ++ show e ++ "))"
    showC (HasEscaped d (Endurance e)) = "Escaped::Escaped(" ++ show d ++ ", Endurance(" ++ show e ++ "))"
    showC x = error (show x)