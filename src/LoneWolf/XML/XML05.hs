module LoneWolf.XML.XML05 where

import Control.Lens hiding (children)
import Data.Ratio ((%))
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.XML.Gen

book05gen :: ChapterId -> [AC] -> Decision -> Maybe Decision
book05gen cid _ computedDecision =
  case cid of
    2 -> takeItems [(oedeHerb, 1)] (computedDecision & _Outcome %~ Simple [ClearFlag LimbDeath]) -- TODO, make sure that oede herb can be consumed
    3 ->
      takeItems
        [(StrengthPotion, 1), (Gold, 4), (Weapon Dagger, 1), (Weapon Sword, 1), (blowpipeSleepDart, 1)]
        ( computedDecision & _Outcome %~ Simple [SetFlag bronzeDoorSecretB05]
        )
    4 -> takeItems [(Weapon Sword, 1)] (computedDecision & _Outcome . _Fight . _1 . fightMod %~ (Timed 1 EnemyInvulnerable :))
    8 -> Just (NoDecision (Simple [DamagePlayer 2] (Randomly [(1 % 2, Goto 67), (1 % 2, Goto 76)])))
    11 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (HasDiscipline Camouflage) (HasDiscipline Hunting), Randomly [(1 % 2, Goto 167), (1 % 2, Goto 190)]),
                  (Always True, Randomly [(3 % 10, Goto 167), (7 % 10, Goto 190)])
                ]
            )
        )
    12 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune, CombatBonus (-2)])
    14 -> Just (RetrieveEquipment computedDecision)
    15 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasLevel Guardian, Randomly [(9 % 10, Goto 151), (1 % 10, Goto 175)]),
                  (Always True, Randomly [(8 % 10, Goto 151), (2 % 10, Goto 175)])
                ]
            )
        )
    19 ->
      Just
        ( Decisions
            [ ( "If you wish to give the guards what they demand, erase any two Backpack Items (except Meals) from your Action Chart. You may now pass through the gate and enter the palace gardens.",
                RemoveItemFrom BackpackSlot 2 (NoDecision (Goto 137))
              ),
              ( "If you do not or cannot give the guards two Backpack Items, you must leave the gate and search for some other means of entering the palace.",
                NoDecision (Goto 49)
              )
            ]
        )
    20 ->
      Just
        ( Decisions
            [ ("jump into the sea", NoDecision (Goto 142)),
              ("surrender", NoDecision (Goto 176)),
              ("fight", NoDecision (Fight (FightDetails "Horseman" 21 28 [OnLose 161, Timed 3 (StopFight 82)]) (Goto 125)))
            ]
        )
    22 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 8])
    23 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasDiscipline Hunting) (HasFlag needOedeB05), Randomly [(1 % 10, Goto 77), (7 % 10, Goto 192), (2 % 10, Goto 114)]),
                  (HasDiscipline Hunting, Randomly [(1 % 2, Goto 192), (1 % 2, Goto 114)]),
                  (HasFlag needOedeB05, Randomly [(3 % 10, Goto 77), (7 % 10, Goto 192)]),
                  (Always True, Randomly [(7 % 10, Goto 77), (3 % 10, Goto 114)])
                ]
            )
        )
    27 -> Just (Canbuy Potion6Hp 7 (Canbuy Laumspur 5 (Canbuy Potion2Hp 3 computedDecision)))
    30 ->
      Just
        ( Decisions
            [ ("If you have reached the Kai rank of Guardian or higher, turn to 62.", Conditional (HasLevel Guardian) (NoDecision (Goto 62))),
              ( "Otherwise",
                Conditional
                  (Not (HasLevel Guardian))
                  ( Decisions
                      [ ("If you wish to hide outside on the narrow ledge that runs round the palace wall, turn to 152.", NoDecision (Goto 152)),
                        ("If you wish to attack the guards as they march past, turn to 124.", NoDecision (Goto 124))
                      ]
                  )
              )
            ]
        )
    31 -> Just (NoDecision (Conditionally [(HasItem fireSphereB05 1, Goto 143), (Always True, Goto 183)]))
    34 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    35 -> takeItems [(Weapon Mace, 1), (copperKeyB05, 1)] (computedDecision & _Outcome %~ Simple [SetFlag jewelledMaceB05])
    38 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    40 -> Just (LoseItemFrom BackpackSlot 1 (NoDecision (Simple [DamagePlayer 2, LoseItemKind [PouchSlot]] (Goto 17))))
    48 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasLevel Guardian, Randomly [(2 % 10, Goto 34), (8 % 10, Goto 80)]),
                  (Always True, Randomly [(5 % 10, Goto 34), (5 % 10, Goto 80)])
                ]
            )
        )
    49 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasLevel Guardian) (HasDiscipline Hunting), Randomly [(2 % 10, Goto 106), (8 % 10, Goto 189)]),
                  (HasLevel Guardian, Randomly [(3 % 10, Goto 106), (7 % 10, Goto 189)]),
                  (HasDiscipline Hunting, Randomly [(5 % 10, Goto 106), (5 % 10, Goto 189)]),
                  (Always True, Randomly [(6 % 10, Goto 106), (4 % 10, Goto 189)])
                ]
            )
        )
    50 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    51 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    52 -> takeItems [(Gold, 4), (gaolerKeyB05, 1), (Weapon Dagger, 1), (Weapon Sword, 1)] computedDecision
    54 ->
      Just
        ( Decisions
            [ ("If you need the Oede herb, turn to 68.", Conditional (HasFlag needOedeB05) (NoDecision (Goto 68))),
              ("If you do not need this herb, but still decide to enter the shop, turn to 154.", Conditional (Not (HasFlag needOedeB05)) (NoDecision (Goto 154))),
              ("If you wish to continue along in a new alleyway that heads off towards the Grand Palace, turn to 179.", Conditional (Not (HasFlag needOedeB05)) (NoDecision (Goto 179)))
            ]
        )
    55 ->
      Just
        ( Decisions
            [ ("If you need the Oede herb", Conditional (HasFlag needOedeB05) (NoDecision (Goto 68))),
              ( "otherwise",
                Conditional
                  (Not (HasFlag needOedeB05))
                  ( Decisions
                      [ ("If you do not need this herb, but still decide to enter the shop", NoDecision (Goto 154)),
                        ("If you wish to continue along in a new alleyway that heads off towards the Grand Palace", NoDecision (Goto 179))
                      ]
                  )
              )
            ]
        )
    56 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (foldl1 COr [HasDiscipline (WeaponSkill w) | w <- [minBound .. maxBound]], Randomly [(2 % 10, Goto 7), (8 % 10, Goto 28)]),
                  (Always True, Randomly [(4 % 10, Goto 7), (6 % 10, Goto 28)])
                ]
            )
        )
    57 ->
      let NoDecision fght = computedDecision
       in Just
            ( NoDecision
                ( Conditionally
                    [ (HasFlag FoughtElix, fght & _Fight . _1 . fightMod .~ [CombatBonus 2]),
                      (Always True, fght)
                    ]
                )
            )
    58 ->
      Just
        ( Decisions
            [ ("Know combination", Conditional (HasFlag bronzeDoorSecretB05) (NoDecision (Goto 67))),
              ( "otherwise",
                Conditional
                  (Not (HasFlag bronzeDoorSecretB05))
                  ( Decisions
                      [ ("guess", NoDecision (Goto 98)),
                        ("give up", NoDecision (Goto 156))
                      ]
                  )
              )
            ]
        )
    63 -> Just (computedDecision & _Outcome %~ Simple [SetFlag Poisonned2])
    64 ->
      let NoDecision fght = computedDecision
       in Just
            ( NoDecision
                ( Conditionally
                    [ (HasDiscipline MindBlast, fght & _Fight . _1 . fightMod .~ [CombatBonus 2]),
                      (Always True, fght)
                    ]
                )
            )
    67 -> takeItems [(Weapon Quarterstaff, 1)] computedDecision
    69 -> Just (computedDecision & _Outcome %~ Simple [StoreEquipment])
    72 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    81 -> Just (computedDecision & _Outcome %~ Simple [SetFlag needOedeB05])
    83 -> Just (computedDecision & _Outcome %~ Simple [SetFlag bronzeDoorSecretB05])
    86 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    93 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 1])
    100 -> takeItems [(copperKeyB05, 1), (prismB05, 1)] computedDecision
    101 ->
      takeItems
        [(Gold, 4), (Weapon Dagger, 1), (Weapon Sword, 1), (StrengthPotion, 1), (blowpipeSleepDart, 1)]
        (computedDecision & _Outcome %~ Simple [SetFlag bronzeDoorSecretB05])
    102 -> takeItems [(Weapon Sword, 1), (Weapon Dagger, 1), (Weapon Warhammer, 1), (Gold, 6), (gaolerKeyB05, 1)] computedDecision
    103 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 2])
    106 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 3 (CombatBonus (-2))])
    108 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    110 ->
      let NoDecision fght = computedDecision
       in Just
            ( NoDecision
                ( Conditionally
                    [ (HasDiscipline MindBlast, fght & _Fight . _1 . fightMod .~ [CombatBonus 2]),
                      (Always True, fght)
                    ]
                )
            )
    111 -> takeItems [(copperKeyB05, 1), (Gold, 3)] computedDecision
    118 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasDiscipline Hunting) (HasLevel Warmarn), Randomly [(1 % 10, Goto 89), (9 % 10, Goto 21)]),
                  (HasLevel Warmarn, Randomly [(2 % 10, Goto 89), (8 % 10, Goto 21)]),
                  (HasDiscipline Hunting, Randomly [(3 % 10, Goto 89), (7 % 10, Goto 21)]),
                  (Always True, Randomly [(4 % 10, Goto 89), (6 % 10, Goto 21)])
                ]
            )
        )
    119 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 3 PlayerInvulnerable])
    125 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(2 % 10, Goto 50), (8 % 10, Goto 191)]),
                  (Always True, Randomly [(4 % 10, Goto 50), (6 % 10, Goto 191)])
                ]
            )
        )
    127 -> Just (Special B05S127)
    130 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 1])
    131 -> takeItems [(Potion2Hp, 1), (Meal, 3), (prismB05, 1), (Laumspur, 1), (Weapon Dagger, 1)] computedDecision
    135 -> Just (NoDecision (Fight (FightDetails "Sharnazim Warrior" 17 22 [CombatBonus (-2), OnLose 161]) (Goto 130)))
    137 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    146 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (HasDiscipline Camouflage) (HasDiscipline Hunting), Randomly [(1 % 2, Goto 44), (1 % 2, Goto 190)]),
                  (Always True, Randomly [(3 % 10, Goto 44), (7 % 10, Goto 190)])
                ]
            )
        )
    152 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasDiscipline Hunting) (HasLevel Savant), Randomly [(1 % 2, Goto 38), (1 % 2, Goto 87)]),
                  (COr (HasLevel Savant) (HasDiscipline Hunting), Randomly [(1 % 10, Goto 5), (6 % 10, Goto 38), (3 % 10, Goto 87)]),
                  (Always True, Randomly [(3 % 10, Goto 5), (6 % 10, Goto 38), (1 % 10, Goto 87)])
                ]
            )
        )
    154 ->
      Just
        ( Canbuy
            Laumspur
            5
            ( Canbuy
                StrengthPotion
                4
                ( Canbuy
                    Potion2Hp
                    3
                    ( Canbuy tinctureGraveweed 1 (NoDecision (Goto 179))
                    )
                )
            )
        )
    159 ->
      let NoDecision (Fight fd gt) = computedDecision
       in Just (NoDecision (Simple [DamagePlayer 2] (Fight (fd & fightMod .~ [Timed 3 (CombatBonus (-2))]) gt)))
    161 -> Just (NoDecision (Simple [HalfHeal] (Goto 69)))
    162 ->
      let NoDecision fight = computedDecision
       in Just
            ( NoDecision
                ( Conditionally
                    [ (HasFlag needOedeB05, Randomly [(1 % 10, Simple [DamagePlayer x] (Goto 114)) | x <- [2 .. 11]]),
                      (Always True, Simple [DamagePlayer 1] fight)
                    ]
                )
            )
    163 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    169 -> Just (Canbuy sashB05 2 computedDecision)
    178 ->
      let fght n = NoDecision (Fight (FightDetails "Armoury Guard" 16 22 []) (Goto n))
       in Just (Decisions [("search body", fght 52), ("ignore body", fght 140)])
    180 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline MindBlast, Goto 45),
                  (COr (HasDiscipline SixthSense) (HasDiscipline Hunting), Randomly [(3 % 10, Goto 120), (7 % 10, Goto 193)]),
                  (Always True, Randomly [(6 % 10, Goto 120), (4 % 10, Goto 193)])
                ]
            )
        )
    182 -> takeItems [(Weapon Spear, 1), (Weapon BroadSword, 1)] computedDecision
    183 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    188 -> Just (NoDecision (Conditionally [(HasItem sashB05 1, Goto 172), (Always True, Goto 72)]))
    190 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [CombatBonus (-2)])
    192 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    194 ->
      let NoDecision (Fight fds gt) = computedDecision
       in Just
            ( NoDecision
                ( Conditionally
                    [ (HasDiscipline MindShield, Fight fds gt),
                      (Always True, Fight (fds & fightMod .~ [CombatBonus (-3)]) gt)
                    ]
                )
            )
    198 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(4 % 10, Goto 25), (6 % 10, Goto 141)]),
                  (Always True, Randomly [(7 % 10, Goto 25), (3 % 10, Goto 141)])
                ]
            )
        )
    207 -> takeItems [(brassWhistleB05, 1)] computedDecision
    208 -> Just (computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasItem oedeHerb 1))
    211 -> Just (Canbuy Potion4Hp 5 (computedDecision & _Outcome %~ Simple [HealPlayer 2]))
    221 -> Just (NoDecision (Conditionally [(HasFlag ReceivedCrystalStarPendant, Goto 336), (Always True, Goto 275)]))
    224 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline AnimalKinship, Goto 308),
                  (HasItem onyxMedallion 1, Goto 319),
                  (HasLevel Aspirant, Randomly [(2 % 10, Goto 370), (4 % 10, Goto 240), (4 % 10, Goto 287)]),
                  (Always True, Randomly [(3 % 10, Goto 370), (4 % 10, Goto 240), (2 % 10, Goto 287), (1 % 10, Goto 257)])
                ]
            )
        )
    226 -> Just (Decisions [("Attack the guards", NoDecision (Goto 401)), ("evade before they have a change to strike", NoDecision (Goto 209))])
    229 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline SixthSense, Randomly [(4 % 10, Goto 385), (6 % 10, Goto 251)]),
                  (Always True, Randomly [(7 % 10, Goto 385), (3 % 10, Goto 251)])
                ]
            )
        )
    237 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 1])
    238 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    239 ->
      let tdiscs = foldl1 CAnd [HasDiscipline d | d <- [Hunting, Tracking, Camouflage]]
       in Just
            ( NoDecision
                ( Conditionally
                    [ (HasItem tinctureGraveweed 1, Goto 260),
                      (CAnd (HasLevel Guardian) tdiscs, Goto 303),
                      (tdiscs, Randomly [(3 % 10, Goto 324), (7 % 10, Goto 303)]),
                      (HasLevel Guardian, Randomly [(2 % 10, Goto 324), (8 % 10, Goto 303)]),
                      (Always True, Randomly [(5 % 10, Goto 324), (5 % 10, Goto 303)])
                    ]
                )
            )
    240 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [DoubleDamage])
    242 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasDiscipline Camouflage) (HasDiscipline MindShield), Randomly [(2 % 10, Goto 262), (8 % 10, Goto 378)]),
                  (HasDiscipline Camouflage, Randomly [(1 % 2, Goto 262), (1 % 2, Goto 378)]),
                  (HasDiscipline MindShield, Randomly [(4 % 10, Goto 262), (6 % 10, Goto 378)]),
                  (Always True, Randomly [(7 % 10, Goto 262), (3 % 10, Goto 378)])
                ]
            )
        )
    248 -> Just (computedDecision & _Decisions . ix 0 . _2 .~ Conditional (HasItem Gold 5) (NoDecision (Simple [LoseItem Gold 5] (Goto 328))))
    251 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 6])
    253 ->
      let NoDecision fight = computedDecision
          bonus3 = fight & _Fight . _1 . fightMod .~ [CombatBonus 3]
          bonus5 = fight & _Fight . _1 . fightMod .~ [CombatBonus 5]
       in Just
            ( NoDecision
                ( Conditionally
                    [ ( CAnd (HasItem (Weapon Mace) 1) (HasFlag jewelledMaceB05),
                        Conditionally $
                          [ (HasItem (Weapon Sommerswerd) 1, fight), -- if we have the sommerswerd, then we use it no matter what
                            (HasDiscipline (WeaponSkill Mace), bonus5) -- if we have weaponskill mace, favor the mace
                          ]
                            -- if we would use another weapon, factor in the other weapon bonus
                            ++ [(CAnd (HasDiscipline (WeaponSkill w)) (HasItem (Weapon w) 1), bonus3) | w <- [minBound .. maxBound], w /= Mace && w /= Sommerswerd]
                            -- or else, use the mace
                            ++ [(Always True, bonus5)]
                      ),
                      (Always True, fight)
                    ]
                )
            )
    254 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    255 -> do
      takng <- takeItems [(blackCubeB05, 1)] computedDecision
      pure $ Decisions [("take cube", takng), ("don't", computedDecision)]
    257 -> Nothing
    264 -> Just (computedDecision & _Outcome %~ \o -> Conditionally [(HasDiscipline MindBlast, o), (Always True, Simple [DamagePlayer 2] o)])
    265 -> Just (computedDecision & _Decisions . ix 0 . _2 .~ Conditional (HasItem Gold 1) (NoDecision (Simple [LoseItem Gold 1] (Goto 397))))
    270 -> Just (LoseItemFrom BackpackSlot 2 (NoDecision (Goto 241))) -- TODO: a bit buggy, should lose weapon + special item if no backpack items
    273 -> Just (NoDecision (Simple [DamagePlayer 1] (Goto 402)))
    276 -> Just (computedDecision & _Decisions . ix 0 . _2 .~ Conditional (HasItem Gold 5) (NoDecision (Simple [LoseItem Gold 5] (Goto 326))))
    278 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    280 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 1 PlayerInvulnerable])
    281 -> Just (CanTake (Weapon Mace) 1 (NoDecision (Simple [SetFlag jewelledMaceB05] (Goto 241))))
    282 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline MindOverMatter, Goto 295),
                  (CAnd (HasLevel Warmarn) (COr (HasDiscipline Camouflage) (HasDiscipline Hunting)), Randomly [(5 % 10, Goto 389), (5 % 10, Goto 236)]),
                  (COr (HasDiscipline Camouflage) (HasDiscipline Hunting), Randomly [(3 % 10, Goto 357), (5 % 10, Goto 389), (2 % 10, Goto 236)]),
                  (HasLevel Warmarn, Randomly [(2 % 10, Goto 357), (5 % 10, Goto 389), (3 % 10, Goto 236)]),
                  (Always True, Randomly [(5 % 10, Goto 357), (5 % 10, Goto 389), (0 % 10, Goto 236)])
                ]
            )
        )
    284 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    285 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    287 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    288 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasFlag ReceivedCrystalStarPendant, Goto 399),
                  (Always True, Goto 294)
                ]
            )
        )
    290 -> do
      takng <- takeItems [(blackCubeB05, 1)] computedDecision
      pure $ Decisions [("take cube", takng), ("don't", computedDecision)]
    297 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    299 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [EnemyMindblast, MindblastImmune])
    301 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasLevel Guardian) (HasDiscipline Hunting), Randomly [(8 % 10, Goto 363), (2 % 10, Goto 259)]),
                  (HasLevel Guardian, Randomly [(7 % 10, Goto 363), (3 % 10, Goto 259)]),
                  (HasDiscipline Hunting, Randomly [(6 % 10, Goto 363), (4 % 10, Goto 259)]),
                  (Always True, Randomly [(5 % 10, Goto 363), (5 % 10, Goto 259)])
                ]
            )
        )
    302 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 3])
    310 -> takeItems [(copperKeyB05, 1)] computedDecision
    312 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (HasDiscipline Hunting) (HasDiscipline SixthSense), Goto 210),
                  (Always True, Randomly [(1 % 2, Goto 354), (4 % 10, Goto 371), (1 % 10, Goto 232)])
                ]
            )
        )
    316 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [Timed 3 (CombatBonus (-2))])
    320 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    325 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (foldl1 COr [HasDiscipline (WeaponSkill w) | w <- [minBound .. maxBound]], Randomly [(2 % 10, Goto 384), (8 % 10, Goto 398)]),
                  (Always True, Randomly [(4 % 10, Goto 384), (6 % 10, Goto 398)])
                ]
            )
        )
    331 -> Just (NoDecision (Goto 373))
    341 -> takeItems [(copperKeyB05, 1), (Weapon BroadSword, 1)] computedDecision
    344 ->
      Just
        ( Decisions
            [ ("If you possess some Oede herb and wish to give it to the poor vaxeler, turn to 321.", Conditional (HasItem oedeHerb 1) (NoDecision (Simple [LoseItem oedeHerb 1] (Goto 321)))),
              ("If you do not have any Oede herb, or if you do not wish to give it to this unfortunate wretch, flee the cave and turn to 270.", NoDecision (Goto 270))
            ]
        )
    350 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3, LoseItem (Weapon Sommerswerd) 1])
    353 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [EnemyMindblast])
    354 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    355 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod %~ (++ [EnemyMindblast, MindblastImmune]))
    357 -> Just (Special B05S357)
    359 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 2])
    360 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(9 % 100, Goto 334), (11 % 20, Goto 226), (9 % 25, Goto 297)]),
                  (Always True, Randomly [(1 % 10, Goto 334), (9 % 20, Goto 226), (9 % 20, Goto 297)])
                ]
            )
        )
    362 -> Just (computedDecision & _Decisions . ix 0 . _2 .~ Conditional (HasItem Gold 1) (NoDecision (Simple [LoseItem Gold 1] (Goto 237))))
    368 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    369 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 6])
    370 -> Just (NoDecision (Simple [DamagePlayer 3] (Goto 403)))
    371 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 4])
    372 ->
      Just
        ( NoDecision
            ( Simple
                [DamagePlayer 1]
                ( Conditionally
                    [ (HasDiscipline MindOverMatter, Goto 269),
                      (HasLevel Aspirant, Randomly [(2 % 10, Goto 366), (8 % 10, Goto 277)]),
                      (Always True, Randomly [(4 % 10, Goto 366), (6 % 10, Goto 277)])
                    ]
                )
            )
        )
    375 -> Just (computedDecision & _NoDecision . _Fight . _1 . fightMod .~ [MindblastImmune])
    379 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 6])
    380 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    381 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(2 % 10, Goto 368), (8 % 10, Goto 252)]),
                  (Always True, Randomly [(1 % 2, Goto 368), (1 % 2, Goto 252)])
                ]
            )
        )
    382 -> Just (NoDecision (Conditionally [(HasFlag ReceivedCrystalStarPendant, Goto 399), (Always True, Goto 294)]))
    385 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 12])
    387 -> Just (EvadeFight 0 205 (FightDetails "Drakkarim" 17 35 []) (Goto 341))
    388 -> Just (Canbuy (Weapon Sword) 5 (Canbuy (Weapon BroadSword) 9 (Canbuy (Weapon Dagger) 3 computedDecision)))
    389 -> Just (NoDecision (Fight (FightDetails "Sentry" 15 23 []) (Goto 404)))
    391 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasLevel Warmarn, Goto 242),
                  (Always True, Goto 222)
                ]
            )
        )
    392 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasLevel Savant) (HasEndurance 26), Randomly [(2 % 10, Goto 364), (8 % 10, Goto 218)]),
                  (CAnd (HasLevel Savant) (HasEndurance 15), Randomly [(4 % 10, Goto 364), (6 % 10, Goto 218)]),
                  (HasLevel Savant, Randomly [(6 % 10, Goto 364), (4 % 10, Goto 218)]),
                  (HasEndurance 26, Randomly [(5 % 10, Goto 364), (5 % 10, Goto 218)]),
                  (HasEndurance 15, Randomly [(7 % 10, Goto 364), (3 % 10, Goto 218)]),
                  (Always True, Randomly [(9 % 10, Goto 364), (1 % 10, Goto 218)])
                ]
            )
        )
    393 -> Just (NoDecision (Conditionally [(HasDiscipline MindShield, Goto 405), (Always True, Simple [DamagePlayer 2] (Goto 405))]))
    400 -> Just (NoDecision GameWon)
    _ -> Just computedDecision

extraChapters05 :: [(ChapterId, Chapter)]
extraChapters05 =
  [ ( 401, -- shadow of 334, but with the effect from chapter 226
      Chapter
        "334"
        "You are in combat with two grim-faced warriors. They block the stair and you must fight both of them as one enemy."
        (EvadeFight 0 209 (FightDetails "Tower Guards" 17 32 [Timed 2 PlayerInvulnerable]) (Goto 310))
    ),
    ( 402, -- duplicated from 273 because the player is damaged before the fight
      Chapter
        "273"
        "..."
        (EvadeFight 0 238 (FightDetails "Drakkarim" 18 35 []) (Goto 345))
    ),
    ( 403, -- duplicated from 370 because the player is damaged before the fight
      Chapter
        "370"
        "..."
        (NoDecision (Fight (FightDetails "Itikar" 17 30 [DoubleDamage]) (Goto 217)))
    ),
    ( 404, -- choices from 389
      Chapter
        "404"
        "..."
        ( Decisions
            [ ("If you win the combat and wish to search the sentry's body", NoDecision (Goto 207)),
              ("If you wish to ignore the body and hurry into the Itikar's pen", NoDecision (Goto 224))
            ]
        )
    ),
    ( 405, -- duplicated from 393 because the player is damaged before the fight
      Chapter
        "393"
        "..."
        (EvadeFight 3 228 (FightDetails "Drakkar" 16 25 [Timed 2 (CombatBonus (-2))]) (Goto 255))
    )
  ]