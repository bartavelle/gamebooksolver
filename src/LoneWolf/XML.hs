{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LoneWolf.XML where

import Control.Lens hiding (children)
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.XML.Gen
import LoneWolf.XML.XML03 (book03gen, extraChapters03)
import LoneWolf.XML.XML04
import LoneWolf.XML.XML05 (book05gen, extraChapters05)
import Text.XML.Expat.Lens.Generic (children, named, parameterized, (./))
import Text.XML.Expat.Tree (UNode, defaultParseOptions, parse)

parseChapter :: Book -> UNode String -> Maybe (ChapterId, Chapter)
parseChapter b =
  case b of
    Book01 -> parseChapterBookGen b book01gen
    Book02 -> error "Book 2 has been manally converted, please do not edit again"
    Book03 -> parseChapterBookGen b book03gen
    Book04 -> parseChapterBookGen b book04gen
    Book05 -> parseChapterBookGen b book05gen

extraChapters :: Book -> [(ChapterId, Chapter)]
extraChapters b =
  case b of
    Book03 -> extraChapters03
    Book04 -> extraChapters04
    Book05 -> extraChapters05
    _ -> []

loadXML :: Book -> FilePath -> IO String
loadXML book xmlpath =
  unlines
    . intersperse "    , "
    . map mkChapterModule
    . (++ extraChapters book)
    . mapMaybe (parseChapter book)
    . toListOf getChapters
    . fst
    . parse defaultParseOptions
    <$> L.readFile xmlpath
  where
    mkChapterModule (cid, Chapter ttl cdesc pch) =
      "  (" ++ show cid ++ ", Chapter " ++ unwords [show ttl, show cdesc, "\n       ", "(", show pch, ")"] ++ ")"
    getChapters = children . traverse . parameterized "id" "title" ./ named "data" ./ parameterized "class" "numbered" ./ named "data" ./ named "section" . parameterized "class" "numbered"

-- todo verify that we are unarmed at chapter 260
book01gen :: ChapterId -> [AC] -> Decision -> Maybe Decision
book01gen cid ac computedDecision =
  case cid of
    12 ->
      Just
        ( Decisions
            [ ("If you have 10 Gold Crowns and wish to pay him, turn to 262.", Conditional (HasItem Gold 10) (NoDecision (Simple [LoseItem Gold 10] (Goto 262)))),
              ("If you do not have enough Gold Crowns or do not wish to pay him, turn to 247.", NoDecision (Goto 247))
            ]
        )
    15 -> Just (CanTake (Weapon Sword) 1 computedDecision)
    17 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [CombatBonus (-1)])
    20 -> Just (CanTake Backpack 1 (CanTake Meal 2 (CanTake (Weapon Dagger) 1 computedDecision)))
    21 -> Just (NoDecision (Randomly [(13 % 20, Goto 189), (63 % 200, GameLost), (7 % 200, Goto 312)]))
    29 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [EnemyMindblast])
    33 -> Just (CanTake Gold 3 computedDecision)
    34 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [EnemyMindblast])
    36 -> Just (NoDecision (Randomly [(1 % 2, Simple [DamagePlayer 2] (Goto 140)), (1 % 2, Goto 323)]))
    37 ->
      Just
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Camouflage, turn to 282.", Conditional (HasDiscipline Camouflage) (NoDecision (Simple [MustEat Hunt] (Goto 282)))),
              ("If you wish to approach them and tell your story, turn to 289.", NoDecision (Simple [MustEat Hunt] (Goto 289)))
            ]
        )
    43 -> Just (computedDecision & _EvadeFight . _1 .~ 3)
    46 ->
      Just
        ( Decisions
            [ ("If you have the Kai Discipline of Sixth Sense, turn to 296.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 296))),
              ( "otherwise",
                Conditional
                  (Not (HasDiscipline SixthSense))
                  ( Decisions
                      [ ("If you accept the offer, turn to 246.", Conditional (HasItem Gold 2) (NoDecision (Simple [LoseItem Gold 2] (Goto 246)))),
                        ("If you refuse and try to ride around the lake, turn to 90.", NoDecision (Goto 90))
                      ]
                  )
              )
            ]
        )
    55 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [CombatBonus 4])
    62 -> takeItems [(Backpack, 1), (Gold, 28), (Weapon Sword, 1)] computedDecision
    76 -> Just (NoDecision (Simple [DamagePlayer 2, GainItem vordakGem 1] (Goto 118)))
    94 -> takeItems [(Gold, 16)] computedDecision
    112 ->
      let [Combat g1 s1 h1 fm1, Combat g2 s2 h2 fm2, Jmp jd1 jt1, Jmp jd2 jt2] = ac
          fght n = Fight (FightDetails g1 (CombatSkill s1) (Endurance (fromIntegral h1)) fm1) (Fight (FightDetails g2 (CombatSkill s2) (Endurance (fromIntegral h2)) fm2) (Goto n))
       in Just (Decisions [(jd1, NoDecision (fght jt1)), (jd2, NoDecision (fght jt2))])
    113 -> takeItems [(Laumspur, 3)] computedDecision
    119 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    124 -> takeItems [(Gold, 15), (silverKey, 1)] computedDecision
    130 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    133 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [MindblastImmune])
    136 ->
      Just
        ( computedDecision
            & _NoDecision . _Fight . _1 . fightMod .~ [CombatBonus 1]
            & _NoDecision . _Fight . _2 . _Fight . _1 . fightMod .~ [CombatBonus 1]
        )
    137 -> Just computedDecision -- the gems are useless
    144 -> Just (Decisions chapter144)
    146 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    147 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    148 -> takeItems [(Weapon Warhammer, 1)] computedDecision
    158 -> Just (NoDecision (Simple [DamagePlayer 6] (Randomly [(6 % 10, Goto 106), (4 % 10, Simple [DamagePlayer 4] (Goto 106))])))
    161 -> takeItems [(goldenKey, 1)] computedDecision
    162 -> Just (NoDecision (Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1] (Conditionally [(HasDiscipline MindOverMatter, Goto 258), (Always True, Goto 127)])))
    164 -> takeItems [(StrengthPotion, 1)] computedDecision
    166 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    168 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    170 ->
      Just
        ( computedDecision & _NoDecision %~ \fight ->
            Conditionally
              [ (HasItem torch 1, fight & _Fight . _1 . fightMod .~ [MindblastImmune]),
                (Always True, fight & _Fight . _1 . fightMod .~ [CombatBonus (-3), MindblastImmune])
              ]
        )
    173 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasItem silverKey 1, Goto 158),
                  (Always True, Goto 259)
                ]
            )
        )
    174 -> Just (NoDecision (Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1] (Goto 190)))
    184 -> takeItems [(Gold, 40), (Meal, 4), (Weapon Sword, 1)] (NoDecision (Simple [MustEat Hunt] (Goto 64)))
    188 ->
      Just
        ( NoDecision
            ( Randomly
                [ (7 % 10, Simple [LoseItemKind [BackpackSlot], LoseItem Backpack 1] (Goto 303)),
                  (3 % 10, Simple [DamagePlayer 3] (Goto 303))
                ]
            )
        )
    197 -> takeItems [(Gold, 6), (Weapon ShortSword, 1)] computedDecision
    199 -> takeItems [(Meal, 1)] computedDecision
    203 -> Just (NoDecision (Simple [DamagePlayer 10] (Conditionally [(HasEndurance 10, Goto 80), (Always True, Goto 344)])))
    205 -> Just (computedDecision & _Outcome %~ Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1])
    208 ->
      let [Combat g1 s1 h1 _, Jmp jd1 jt1, Jmp jd2 jt2] = ac
          fght n = Fight (FightDetails g1 (CombatSkill s1) (Endurance (fromIntegral h1)) []) (Goto n)
       in Just (Decisions [(jd1, NoDecision (fght jt1)), (jd2, NoDecision (fght jt2))])
    212 -> Just (computedDecision & _NoDecision %~ Simple [FullHeal])
    227 ->
      let [Combat g1 s1 h1 _, Jmp _ jt1, Jmp _ jt2] = ac
       in Just (NoDecision (Fight (FightDetails g1 (CombatSkill s1) (Endurance (fromIntegral h1)) [OnDamage jt1]) (Goto jt2)))
    229 ->
      let [Combat g1 s1 h1 _, Jmp jd1 jt1, Jmp jd2 jt2] = ac
          fght n = Fight (FightDetails g1 (CombatSkill s1) (Endurance (fromIntegral h1)) [CombatBonus (-1)]) (Goto n)
       in Just (Decisions [(jd1, NoDecision (fght jt1)), (jd2, NoDecision (fght jt2))])
    231 -> Just (EvadeFight 2 7 (FightDetails "Robber" 13 20 [Timed 4 (OnNotYetWon 203)]) (Goto 94))
    235 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    236 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 6, SetFlag PermanentSkillReduction, LoseItem vordakGem 99])
    243 -> takeItems [(Weapon Mace, 1)] computedDecision
    255 -> takeItems [(Weapon Sword, 1)] (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune])
    258 -> Just (computedDecision & _Outcome %~ Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1])
    263 -> takeItems [(Gold, 3)] computedDecision
    269 -> takeItems [(Gold, 10)] computedDecision
    276 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    277 -> Just (computedDecision & _Outcome %~ Simple [LoseItemKind [WeaponSlot]]) -- TODO, if you have 2 weapons, only one of them is broken
    283 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline MindShield, Fight (FightDetails "Vordak" 17 25 [Timed 1 (CombatBonus 2)]) (Goto 123)),
                  (Always True, Fight (FightDetails "Vordak" 19 25 [Timed 1 (CombatBonus 4)]) (Goto 123))
                ]
            )
        )
    290 -> takeItems [(Weapon Quarterstaff, 1)] computedDecision
    291 -> takeItems [(Gold, 6), (Weapon Dagger, 1), (Weapon Spear, 1)] computedDecision
    304 -> Just (NoDecision (Simple [DamagePlayer 2, GainItem vordakGem 1] (Goto 2)))
    305 -> takeItems [(Weapon Spear, 1)] computedDecision
    307 -> takeItems [(Meal, 1)] computedDecision -- TODO, replace a weapon with the warhammer
    308 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    313 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    315 -> takeItems [(Gold, 6)] computedDecision
    319 -> takeItems [(Gold, 20), (Weapon Dagger, 1)] computedDecision
    320 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    339 -> Just (EvadeFight 0 7 (FightDetails "Robber" 13 20 [Timed 4 (OnNotYetWon 203)]) (Goto 94))
    342 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [EnemyMindblast, MindblastImmune])
    343 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    347 -> takeItems [(Weapon ShortSword, 1), (torch, 1)] computedDecision
    349 -> takeItems [(crystalStarPendantB01, 1)] computedDecision
    288 -> Just (NoDecision (Conditionally [(HasFlag ReceivedCrystalStarPendant, Goto 399), (Always True, Goto 294)]))
    350 -> Just (NoDecision GameWon)
    _ -> Just computedDecision
  where
    chapter144 = c144loseBackpack ++ c144loseWeapon ++ c144nothingtolose
    backpackitems = filter (\itm -> itemSlot itm == BackpackSlot) [minBound .. maxBound]
    c144loseWeapon = do
      wpn <- [minBound .. maxBound]
      pure ("Losing " ++ show wpn, Conditional (HasItem (Weapon wpn) 1) (computedDecision & _Outcome %~ Simple [LoseItem (Weapon wpn) 1, DamagePlayer 2]))
    c144loseBackpack = do
      itm <- backpackitems
      pure ("Losing " ++ show itm, Conditional (CAnd (HasItem itm 1) (Not (HasItem Backpack 1))) (computedDecision & _Outcome %~ Simple [LoseItem itm 1, DamagePlayer 2]))
    c144nothingtolose = [("Nothing to lose", Conditional ntl (computedDecision & _Outcome %~ Simple [DamagePlayer 2]))]
      where
        ntl = foldr CAnd (Not (HasItem Backpack 1)) [Not (HasItem itm 1) | itm <- backpackitems ++ map Weapon [minBound .. maxBound]]
