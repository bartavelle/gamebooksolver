module LoneWolf.XML.XML04 where

import Control.Lens hiding (children)
import Data.Ratio ((%))
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.XML.Gen

book04gen :: ChapterId -> [AC] -> Decision -> Maybe Decision
book04gen cid _ computedDecision =
  case cid of
    2 -> takeItems [(Backpack, 1), (Gold, 12), (Meal, 2), (Weapon Sword, 1), (Weapon Mace, 1), (Weapon Dagger, 1), (Weapon Warhammer, 1)] computedDecision
    3 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    10 -> takeItems [(onyxMedallion, 1)] computedDecision
    -- do not include the rope as it is unused
    12 -> takeItems [(Backpack, 1), (Laumspur, 1), (Weapon Sword, 1), (Weapon Spear, 1)] computedDecision
    19 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    22 -> Just (LoseItemFrom BackpackSlot 1 (NoDecision (Goto 157)))
    24 ->
      Just
        ( Decisions
            [ ("If you have the Kai Discipline of Healing, you can help this wounded soldier by turning to 238.", Conditional (HasDiscipline Healing) (NoDecision (Goto 238))),
              ("If you wish to continue up the stairs to the watchtower roof, turn to 223.", NoDecision (Goto 223)),
              ("If you wish to pick up the wounded archer's bow and man his position at the arrow slit, turn to 207.", NoDecision (Goto 207))
            ]
        )
    26 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune])
    35 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (HasDiscipline Hunting) (HasLevel Guardian), Randomly [(1 % 2, Goto 147), (1 % 2, Goto 231)]),
                  (Always True, Randomly [(8 % 10, Goto 147), (2 % 10, Goto 231)])
                ]
            )
        )
    40 ->
      Just
        ( Decisions
            [ ("If you have the Kai Discipline of Tracking, and if you have reached the Kai rank of Aspirant or higher, turn to 349.", Conditional (CAnd (HasDiscipline Tracking) (HasLevel Aspirant)) (NoDecision (Goto 349))),
              ("otherwise", Conditional (Not (HasDiscipline Tracking)) (Decisions [("If you wish to follow the wagon track into the west tunnel, turn to 55.", NoDecision (Goto 55)), ("If you wish to climb the stairs to the balcony and explore the passage, turn to 291.", NoDecision (Goto 291))]))
            ]
        )
    43 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (COr (HasDiscipline MindOverMatter) anyweaponskill) (COr (HasDiscipline Hunting) (HasDiscipline SixthSense)), Randomly [(4 % 10, Goto 262), (6 % 10, Goto 111)]),
                  (COr (HasDiscipline MindOverMatter) anyweaponskill, Randomly [(6 % 10, Goto 262), (4 % 10, Goto 111)]),
                  (COr (HasDiscipline Hunting) (HasDiscipline SixthSense), Randomly [(5 % 10, Goto 262), (5 % 10, Goto 111)]),
                  (Always True, Randomly [(7 % 10, Goto 262), (3 % 10, Goto 111)])
                ]
            )
        )
    44 -> takeItems [(Gold, 12), (Meal, 2)] computedDecision
    52 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    54 ->
      Just
        ( Decisions
            [ ("If you have the Kai Discipline of Healing and you have reached the Kai rank of Warmarn, turn to 4.", Conditional (CAnd (HasDiscipline Healing) (HasLevel Warmarn)) (NoDecision (Goto 4))),
              ("otherwise", Conditional (Not (HasDiscipline Healing)) (Decisions [("If you do not have this skill, or if you have yet to reach the rank of Kai Warmarn, you can risk walking through the fungi by turning to 65.", NoDecision (Goto 65)), ("Alternatively, if you wish, you may eat some of the fungi by turning to 201.", NoDecision (Goto 201))]))
            ]
        )
    57 -> Just (NoDecision (Conditionally [(CAnd (HasItem (Weapon Sword) 1) (HasFlag captainDvalSword), Goto 327), (Always True, Goto 289)]))
    62 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 3 (CombatBonus (-2))])
    67 ->
      Just
        ( NoDecision
            ( Simple
                [DamagePlayer 1]
                ( Conditionally
                    [ (HasItem (Weapon Sommerswerd) 1, Goto 292),
                      (COr (HasDiscipline Hunting) (HasDiscipline MindOverMatter), Randomly [(3 % 10, Goto 242), (4 % 10, Goto 263), (3 % 10, Goto 278)]),
                      (Always True, Randomly [(5 % 10, Goto 242), (4 % 10, Goto 263), (1 % 10, Goto 278)])
                    ]
                )
            )
        )
    70 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasItem onyxMedallion 1, Goto 305),
                  (CAnd (HasDiscipline Camouflage) (HasLevel Guardian), Goto 49),
                  (Always True, Goto 159)
                ]
            )
        )
    74 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 1])
    75 ->
      Just
        ( NoDecision
            ( Simple
                [DamagePlayer 2]
                ( Conditionally
                    [ (HasDiscipline Camouflage, Randomly [(3 % 10, Goto 192), (7 % 10, Goto 16)]),
                      (Always True, Randomly [(6 % 10, Goto 192), (4 % 10, Goto 16)])
                    ]
                )
            )
        )
    77 -> Just (EvadeFight 2 98 (FightDetails "Vassaginian Captain" 22 28 [MindblastImmune, EnemyMindblast]) (Goto 10))
    78 -> Just (CanTake flaskHolyWaterB04 1 (NoDecision (Simple [MustEat Hunt] (Goto 233))))
    79 -> takeItems [(tinderBoxB04, 1), (torchB04, 5)] computedDecision
    84 -> takeItems [(scrollB04, 1)] computedDecision
    88 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune])
    94 -> Just (NoDecision (Simple [LoseItemKind [BackpackSlot]] (Goto 219)))
    102 -> takeItems [(Gold, 12), (Meal, 2)] computedDecision
    103 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    109 -> takeItems [(Gold, 3), (Weapon Dagger, 1), (Weapon Sword, 1)] computedDecision
    112 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasEndurance 21, Randomly [(2 % 10, Goto 42), (8 % 10, Goto 303)]),
                  (HasEndurance 10, Randomly [(5 % 10, Goto 42), (5 % 10, Goto 303)]),
                  (Always True, Randomly [(8 % 10, Goto 42), (2 % 10, Goto 303)])
                ]
            )
        )
    113 ->
      Just
        ( Decisions
            [ ("If you have reached the Kai rank of Warmarn, turn to 166.", Conditional (HasLevel Warmarn) (NoDecision (Goto 166))),
              ( "Otherwise.",
                Conditional
                  (Not (HasLevel Warmarn))
                  ( Decisions
                      [ ("If you have yet to reach this level of Kai training, you can launch a surprise attack on the bandits and turn to 14.", NoDecision (Goto 14)),
                        ("If you would rather try to sneak across the river under the cover of the many large boulders that divert the watercourse, turn to 316.", NoDecision (Goto 316)),
                        ("If you would rather avoid crossing here and head back the way you have just come, turn to 232.", NoDecision (Goto 232))
                      ]
                  )
              )
            ]
        )
    133 -> Just (EvadeFight 0 307 (FightDetails "Bandit Patrol" 18 35 [OnDamage 17]) (Goto 265))
    117 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (CAnd (HasItem tinderBoxB04 1) (HasItem torchB04 1)) (HasItem fireSphereB04 1), Goto 22),
                  (COr (HasDiscipline SixthSense) (HasDiscipline Tracking), Randomly [(4 % 10, Goto 99), (6 % 10, Goto 256)]),
                  (Always True, Randomly [(7 % 10, Goto 99), (3 % 10, Goto 256)])
                ]
            )
        )
    122 ->
      Just
        ( NoDecision
            ( Simple
                [DamagePlayer 1]
                ( Conditionally
                    [ (HasDiscipline MindBlast, Fight (FightDetails "Barraka" 25 29 [MindblastImmune]) (Goto 350)),
                      (Always True, Fight (FightDetails "Barraka" 25 29 [MindblastImmune, CombatBonus (-4)]) (Goto 350))
                    ]
                )
            )
        )
    123 -> takeItems [(tinderBoxB04, 1), (torchB04, 5)] computedDecision
    126 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    127 -> Just (computedDecision & _Outcome %~ Simple [SetFlag FoughtElix])
    128 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(2 % 10, Goto 103), (8 % 10, Goto 98)]),
                  (Always True, Randomly [(5 % 10, Goto 103), (5 % 10, Goto 98)])
                ]
            )
        )
    129 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    139 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    141 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    143 -> Just (NoDecision (Fight (FightDetails "Tunnel Guard 1" 16 22 []) (Goto 360)))
    147 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 1 PlayerInvulnerable])
    152 -> takeItems [(Gold, 6), (Weapon Sword, 1), (Meal, 2), (brassKeyB04, 1)] computedDecision
    153 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 2 (CombatBonus (-4))])
    157 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 1])
    158 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3, LoseItemKind [BackpackSlot]])
    159 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    165 -> Just (Decisions [("accept the meal", NoDecision (Goto 319)), ("eat your own", NoDecision (Simple [MustEat NoHunt] (Goto 13)))])
    -- pick is useless from here
    167 -> takeItems [(Backpack, 1), (Meal, 2 {-,  (pickAB04, 1), (pickBB04, 1) -})] computedDecision
    173 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasItem (Weapon Sommerswerd) 1, Goto 275),
                  (CAnd (HasItem pickAB04 1) (HasItem pickBB04 1), Randomly [(1 % 2, Goto 143), (1 % 2, Goto 179)]),
                  (Always True, Randomly [(7 % 10, Goto 143), (3 % 10, Goto 179)])
                ]
            )
        )
    176 -> Just (NoDecision (Simple [DamagePlayer 3] (Fight (FightDetails "Bandit Warrior" 17 25 []) (Goto 7))))
    182 ->
      Just
        ( Decisions
            [ ( "Chase",
                NoDecision
                  ( Conditionally
                      [ (COr (HasDiscipline Tracking) (HasDiscipline Hunting), Goto 332),
                        (Always True, Goto 58)
                      ]
                  )
              ),
              ("If you decide to let him go, return to your men by turning to 165.", NoDecision (Goto 165))
            ]
        )
    183 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Camouflage, Randomly [(3 % 10, Goto 198), (7 % 10, Goto 338)]),
                  (Always True, Randomly [(7 % 10, Goto 198), (3 % 10, Goto 338)])
                ]
            )
        )
    185 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    188 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    193 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 2 (StopFight 311)])
    194 -> Just (NoDecision (Randomly [(1 % 10, Fight (FightDetails "Giant Meresquid" 16 37 [Timed n ForceEMindblast]) (Goto 32)) | n <- [1 .. 10]]))
    200 ->
      let cnd = COr (HasDiscipline Hunting) (HasFlag visitedGornCove)
       in Just
            ( Decisions
                [ ("If you possess the Kai Discipline of Hunting or if you have ever visited Gorn Cove, turn to 45.", Conditional cnd (NoDecision (Goto 45))),
                  ( "otherwise",
                    Conditional
                      (Not cnd)
                      ( Decisions
                          [ ("If you wish to fight the bandits, turn to 133.", NoDecision (Goto 133)),
                            ("If you wish to evade them, you must run across the open plain towards Ruanon-eight hundred yards away. Turn to 307.", NoDecision (Goto 307))
                          ]
                      )
                  )
                ]
            )
    204 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    207 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (anyweaponskill, Randomly [(3 % 10, Goto 336), (7 % 10, Goto 218)]),
                  (Always True, Randomly [(5 % 10, Goto 336), (5 % 10, Goto 218)])
                ]
            )
        )
    209 -> Just (NoDecision (Conditionally [(HasLevel Aspirant, Goto 111), (Always True, Goto 43)]))
    213 -> takeItems [(tinderBoxB04, 1), (torchB04, 1), (pickAB04, 1), (pickBB04, 1), (Weapon Axe, 1)] computedDecision
    -- 222 -> Just (CanTake (Weapon Sword) 1 (NoDecision (Simple [SetFlag captainDvalSword] (Goto 165))))
    222 -> Just (CanTake (Weapon Sword) 1 (NoDecision (Goto 165)))
    230 -> takeItems [(Gold, 9), (Meal, 2), (Weapon Sword, 1), (Weapon Dagger, 1)] computedDecision
    231 -> takeItems [(Gold, 3), (Meal, 1), (Weapon Sword, 1)] computedDecision
    234 -> Just (NoDecision (Goto 194)) -- same chapter
    236 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    246 ->
      Just
        ( NoDecision
            ( Simple
                [DamagePlayer 8]
                ( Conditionally
                    [ (HasItem (Weapon Sommerswerd) 1, Goto 34),
                      (Always True, Goto 85)
                    ]
                )
            )
        )
    247 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    249 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (anyweaponskill, Randomly [(2 % 10, Goto 153), (4 % 10, Goto 323), (4 % 10, Goto 39)]),
                  (Always True, Randomly [(4 % 10, Goto 153), (4 % 10, Goto 323), (2 % 10, Goto 39)])
                ]
            )
        )
    253 -> Just (NoDecision (Conditionally [(HasLevel Warmarn, Goto 72), (Always True, Goto 135)]))
    258 ->
      Just
        ( Decisions
            [ ("If you possess an Onyx Medallion, turn to 305.", Conditional (HasItem (GenSpecial (GenCounter 11)) 1) (NoDecision (Goto 305))),
              ( "otherwise",
                Conditional
                  (Not (HasItem (GenSpecial (GenCounter 11)) 1))
                  ( Decisions
                      [ ("If you have the Kai Discipline of Camouflage and you have reached the Kai Rank of Guardian or higher, turn to 49.", Conditional (CAnd (HasDiscipline Camouflage) (HasLevel Guardian)) (NoDecision (Goto 49))),
                        ("otherwise", Conditional (Not (HasDiscipline Camouflage)) (Decisions [("If you do not have this Special Item or this Discipline and Kai Rank, dive into the tall crops and hide by turning to 159.", NoDecision (Goto 159))]))
                      ]
                  )
              )
            ]
        )
    263 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    268 -> takeItems [(Gold, 4), (Meal, 2), (Weapon Spear, 1), (Weapon BroadSword, 1), (ironKeyB04, 1), (brassKeyB04, 1), (Laumspur, 1)] computedDecision
    269 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    270 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasItem tinderBoxB04 1) (HasItem torchB04 1), Goto 29),
                  (HasItem fireSphereB04 1, Goto 168),
                  (Always True, Goto 361)
                ]
            )
        )
    271 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(5 % 10, Goto 9), (5 % 10, Goto 104)]),
                  (Always True, Randomly [(7 % 10, Goto 9), (3 % 10, Goto 104)])
                ]
            )
        )
    272 -> Just (LoseItemFrom WeaponSlot 1 (NoDecision (Simple [DamagePlayer 5] (Goto 79))))
    275 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    279 -> Just (NoDecision (Conditionally [(CAnd (HasItem (Weapon Sword) 1) (HasFlag captainDvalSword), Goto 327), (Always True, Goto 289)]))
    280 -> Just (NoDecision (Goto 231)) -- same chapter
    283 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    284 -> Just (NoDecision (Simple [DamagePlayer 3] (Goto 362)))
    289 ->
      Just
        ( Decisions
            [ ("If you have reached the Kai rank of Warmarn (you possess 8 Kai Disciplines), turn to 255.", Conditional (HasLevel Warmarn) (NoDecision (Goto 255))),
              ("If you wish to help the captain, turn to 5.", Conditional (Not (HasLevel Warmarn)) (NoDecision (Goto 5))),
              ("If you wish to rally the defenders before the enemy reach the barricade, turn to 86.", Conditional (Not (HasLevel Warmarn)) (NoDecision (Goto 86)))
            ]
        )
    302 -> takeItems [(Laumspur, 2), (StrengthPotion, 1), (flaskHolyWaterB04, 1)] computedDecision
    303 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    309 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Camouflage, Randomly [(4 % 10, Goto 138), (6 % 10, Goto 244)]),
                  (Always True, Randomly [(8 % 10, Goto 138), (2 % 10, Goto 244)])
                ]
            )
        )
    319 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [CombatBonus (-2)])
    322 -> takeItems [(pickAB04, 1), (pickBB04, 1)] (computedDecision & _Outcome %~ Simple [HealPlayer 2])
    324 -> Just (computedDecision & _Outcome %~ Simple [MustEat Hunt])
    325 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune])
    327 -> Just (computedDecision & _Outcome %~ Simple [LoseItem (Weapon Sword) 1, ClearFlag captainDvalSword])
    340 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    341 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    342 -> Just (computedDecision & _Outcome %~ Simple [LoseItemKind [BackpackSlot]])
    343 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasEndurance 20, Randomly [(4 % 10, Goto 194), (6 % 10, Goto 61)]),
                  (HasEndurance 12, Randomly [(7 % 10, Goto 194), (3 % 10, Goto 61)]),
                  (Always True, Randomly [(9 % 10, Goto 194), (1 % 10, Goto 61)])
                ]
            )
        )
    344 -> Just (NoDecision (Conditionally [(HasLevel Aspirant, Goto 111), (Always True, Goto 43)]))
    350 -> Just (NoDecision GameWon)
    _ -> Just computedDecision

extraChapters04 :: [(ChapterId, Chapter)]
extraChapters04 =
  [ ( 360,
      Chapter
        "143b"
        "Second fight"
        ( Decisions
            [ ("evade", NoDecision (Goto 87)),
              ("fight", NoDecision (Fight (FightDetails "Tunnel Guard 2" 15 21 []) (Goto 230)))
            ]
        )
    ),
    ( 361,
      Chapter
        "270b"
        "Choice"
        ( Decisions
            [ ("way forward", NoDecision (Goto 246)),
              ("crypt door", NoDecision (Goto 183))
            ]
        )
    ),
    ( 362,
      Chapter
        "284b"
        "Choice"
        ( EvadeFight 0 89 (FightDetails "Bandit Warrior" 16 25 []) (Goto 7)
        )
    )
  ]