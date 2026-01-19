{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use camelCase" -}
module LoneWolf.Rocq (rocqChapter) where

import Data.String (IsString (fromString))
import LoneWolf.Chapter (BoolCond (..), Chapter, ChapterG (Chapter), ChapterId, ChapterOutcomeG (..), DecisionG (..), FightDetailsG (FightDetails), FightModifierG (..), SimpleOutcome (..))
import LoneWolf.Character (Flag (..), GenCounter (..), Item (..), Slot (..), Endurance (Endurance), CombatSkill (CombatSkill))
import Prettyprinter
import Prettyprinter.Util
import Data.Ratio (numerator, denominator)

rocqChapter :: [(ChapterId, Chapter)] -> IO ()
rocqChapter chapters = putDocW 160 (headers <> render_chapters chapters <> ".")
  where
    headers =
      vsep
        [ "Require Import chapters.",
          "From mathcomp Require Import ssralg rat ssrint.",
          "Require Import Stdlib.Lists.List.",
          "Import ListNotations."
        ]
        <> line
        <> "Definition chapters := "

rocqlist :: (x -> Doc a) -> [x] -> Doc a
rocqlist f = brackets . hsep . punctuate ";" . map f

render_chapters :: [(ChapterId, Chapter)] -> Doc a
render_chapters = brackets . nest 2 . vsep . punctuate ";" . map render_cid_chapter

render_cid_chapter :: (ChapterId, Chapter) -> Doc a
render_cid_chapter (cid, Chapter _ _ c) = parens (viaShow cid <> comma <> render_dec c)

render_dec :: DecisionG Rational -> Doc a
render_dec = \case
  Decisions decs -> "Decisions" <+> rocqlist (render_dec . snd) decs
  RetrieveEquipment d -> "retrieve_equipment" <+> parens (render_dec d)
  CanTake i q n -> "can_take" <+> show_item i <+> viaShow q <+> parens (render_dec n)
  Canbuy i p n -> "can_buy" <+> show_item i <+> viaShow p <+> parens (render_dec n)
  Cansell i p n -> "can_sell" <+> show_item i <+> viaShow p <+> parens (render_dec n)
  Conditional c d -> "conditional" <+> parens (render_cond c) <+> parens (render_dec d)
  Special s -> parens ("special" <+> viaShow s)
  NoDecision co -> "none" <+> parens (render_co co)
  EvadeFight r c d co -> "evade_fight" <+> viaShow r <+> viaShow c <+> parens (fight_details d) <+> parens (render_co co)
  AfterCombat d -> "after_combat" <+> parens (render_dec d)
  RemoveItemFrom s i d -> "remove_item_from" <+> show_slot s <+> viaShow i <+> parens (render_dec d)

show_slot :: Slot -> Doc a
show_slot = \case
  WeaponSlot -> "SWeapon"
  BackpackSlot -> "SBackpack"
  SpecialSlot -> "SSpecial"
  PouchSlot -> "SPouch"

render_cond :: BoolCond -> Doc a
render_cond = \case
  HasDiscipline d -> "HasDiscipline" <+> parens (viaShow d)
  Not c -> "Not" <+> parens (render_cond c)
  COr a b -> "COr" <+> parens (render_cond a) <+> parens (render_cond b)
  CAnd a b -> "CAnd" <+> parens (render_cond a) <+> parens (render_cond b)
  HasItem i n -> "HasItem" <+> show_item i <+> viaShow n
  Always b -> "Always" <+> if b then "true" else "false"
  HasEndurance (Endurance e) -> "HasEndurance" <+> viaShow e
  HasFlag f -> "HasFlag" <+> show_flag f
  HasLevel l -> "HasLevel" <+> viaShow l

show_item :: Item -> Doc a
show_item = \case
  Weapon w -> parens ("Weapon" <+> viaShow w)
  GenSpecial (GenCounter c) -> parens ("GenSpecial" <+> fromString ('S' : show c))
  GenBackpack (GenCounter c) -> parens ("GenBackpack" <+> fromString ('S' : show c))
  x -> viaShow x

render_co :: ChapterOutcomeG Rational -> Doc a
render_co = \case
  Fight fd co -> "Fight" <+> parens (fight_details fd) <+> parens (render_co co)
  OneRound fd co1 co2 co3 -> "OneRound" <+> parens (fight_details fd) <+> parens (render_co co1) <+> parens (render_co co2) <+> parens (render_co co3)
  Randomly lst -> "Randomly" <+> rocqlist (\(p, co) -> parens (show_proba p <+> comma <+> render_co co)) lst
  LoseItemFrom s q co -> "LoseItemFrom" <+> show_slot s <+> viaShow q <+> parens (render_co co)
  Conditionally lst -> "Conditionally" <+> rocqlist (\(c, x) -> parens (render_cond c <+> comma <+> render_co x)) lst
  Simple sos co -> "Simple" <+> rocqlist simple_outcome sos <+> parens (render_co co)
  Goto cid -> "Goto" <+> viaShow cid
  GameLost -> "GameLost"
  GameWon -> "GameWon"

simple_outcome :: SimpleOutcome -> Doc a
simple_outcome = \case
  GainItem i q -> "GainItem" <+> show_item i <+> viaShow q
  LoseItem i q -> "LoseItem" <+> show_item i <+> viaShow q
  LoseItemKind sls -> "LoseItemKind" <+> rocqlist show_slot sls
  SetFlag f -> "SetFlag" <+> show_flag f
  ClearFlag f -> "ClearFlag" <+> show_flag f
  DamagePlayer (Endurance e) -> "DamagePlayer" <+> viaShow e
  HealPlayer (Endurance e) -> "HealPlayer" <+> viaShow e
  x -> viaShow x

show_flag :: Flag -> Doc a
show_flag = viaShow

fight_details :: FightDetailsG Rational -> Doc a
fight_details (FightDetails _  sk (Endurance en) mds) = "Details" <+> show_skill sk <+> viaShow en <+> rocqlist fight_mod mds

fight_mod :: FightModifierG Rational -> Doc a
fight_mod = \case
  Timed n fm -> "Timed" <+> viaShow n <+> parens (fight_mod fm)
  Poisonous p -> "Poisonous" <+> show_proba p
  CombatBonus sk -> "CombatBonus" <+> show_skill sk
  DPR (Endurance e) -> "Dpr" <+> viaShow e
  x -> viaShow x

show_skill :: CombatSkill -> Doc a
show_skill (CombatSkill sk)= parens (viaShow sk) <> "%Z"

show_proba :: Rational -> Doc a
show_proba r = parens (viaShow (numerator r) <+> "/" <+> viaShow (denominator r)) <> "%R"
