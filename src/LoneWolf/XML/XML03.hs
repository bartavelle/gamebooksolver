module LoneWolf.XML.XML03 where

import Control.Lens hiding (children)
import Data.Ratio ((%))
import LoneWolf.Chapter
import LoneWolf.Character
import LoneWolf.XML.Gen

book03gen :: ChapterId -> [AC] -> Decision -> Maybe Decision
book03gen cid _ computedDecision =
  case cid of
    4 -> takeItems [(Weapon Sword, 1), (blueStoneDiscB03, 1)] computedDecision
    8 -> Just (computedDecision & _Outcome %~ Simple [SetFlag baknarOilB03])
    10 ->
      let breakGW = Simple [DamagePlayer 1] (Goto 368)
          getGW = Simple [GainItem graveWeedB03 1] (Goto 368)
          nxt =
            Decisions
              [ ( "try graveweed",
                  Conditional
                    (HasFlag visitedGraveyardAncientsB03)
                    ( NoDecision
                        ( Conditionally
                            [ (COr (HasDiscipline MindOverMatter) (HasDiscipline MindBlast), Randomly [(2 % 10, breakGW), (8 % 10, getGW)]),
                              (Always True, Randomly [(1 % 2, breakGW), (1 % 2, getGW)])
                            ]
                        )
                    )
                ),
                ("don't try graveweed", NoDecision (Goto 368))
              ]
       in takeItems [(Backpack, 1)] nxt
    12 -> takeItems [(Meal, 3), (ropeB03, 1)] (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    15 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline MindOverMatter, Goto 185),
                  (COr (HasItem (Weapon Dagger) 1) (HasItem (Weapon Sword) 1), Goto 86),
                  (Always True, Goto 323)
                ]
            )
        )
    16 -> Just (LoseItemFrom BackpackSlot 2 (NoDecision (Goto 63)))
    17 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3, LoseItemKind [WeaponSlot]])
    21 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    25 -> takeItems [(blueStoneTriangleB03, 1)] computedDecision
    26 -> takeItems [(Weapon Dagger, 1), (Weapon Sword, 1), (Weapon Mace, 1)] computedDecision
    27 -> Just (computedDecision & _Outcome %~ \o -> Conditionally [(HasFlag baknarOilB03, o), (Always True, Simple [DamagePlayer 2] o)])
    29 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasItem (Weapon Sommerswerd) 1, Goto 43),
                  (HasDiscipline MindOverMatter, Goto 121),
                  (Always True, Randomly [(5 % 10, Goto 226), (4 % 10, Goto 266), (1 % 10, Goto 312)])
                ]
            )
        )
    32 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasFlag killKL11, Goto 364),
                  (Always True, Fight (FightDetails "Kalkoth 1" 11 35 [OnDamage 66]) (Goto 364))
                ]
            )
        )
    33 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    36 -> Just (NoDecision (Conditionally [(CAnd (HasDiscipline SixthSense) (HasLevel Guardian), Goto 341), (HasDiscipline SixthSense, Goto 264), (Always True, Goto 124)]))
    38 -> takeItems [(Backpack, 1), (ropeB03, 1)] computedDecision
    43 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    49 -> Just (computedDecision & _Outcome %~ Simple [LoseItemKind [BackpackSlot], LoseItem Backpack 1])
    50 -> Just (NoDecision (Conditionally [(HasItem glowingCrystalB03 1, Goto 139), (Always True, Goto 189)]))
    54 -> Nothing
    55 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 5, SetFlag PermanentSkillReduction2])
    59 -> takeItems [(blueStoneTriangleB03, 1)] computedDecision
    61 -> Just (NoDecision GameLost)
    62 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 3])
    67 -> Just (Decisions [("use rope", Conditional (HasItem ropeB03 1) (NoDecision (Goto 328))), ("don't use rope", NoDecision (Goto 166))])
    70 -> Just (NoDecision (Conditionally [(HasFlag baknarOilB03, Goto 209), (Always True, Goto 339)]))
    77 -> Nothing
    79 -> Just (computedDecision & _Outcome %~ Simple [LoseItem graveWeedB03 1, HealPlayer 6])
    83 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune, EnemyMindblast])
    84 -> takeItems [(blueStoneTriangleB03, 1)] computedDecision
    86 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (anyweaponskill, Randomly [(1 % 2, Goto 47), (1 % 2, Goto 194)]),
                  (Always True, Randomly [(8 % 10, Goto 47), (2 % 10, Goto 194)])
                ]
            )
        )
    88 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Poisonous (1 % 10)])
    90 -> Nothing
    91 -> Just (computedDecision & _Outcome %~ Simple [SetFlag baknarOilB03])
    92 -> Just (Decisions [("use rope", Conditional (HasItem ropeB03 1) (NoDecision (Goto 133))), ("don't use rope", NoDecision (Goto 297))])
    94 -> Just (NoDecision (Randomly [(7 % 10, Simple [DamagePlayer 3] (Goto 176)), (3 % 10, GameLost)]))
    96 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasFlag baknarOilB03, Randomly [(7 % 10, Goto 59), (3 % 10, Goto 214)]),
                  (Always True, Randomly [(9 % 10, Goto 59), (1 % 10, Goto 214)])
                ]
            )
        )
    99 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune, EnemyMindblast, Undead, NoPotion])
    102 -> takeItems [(effigyB03, 1)] computedDecision
    106 -> Just (EvadeFight 0 145 (FightDetails "Ice Barbarians" 19 36 [MindblastImmune]) (Goto 338))
    107 ->
      Just
        ( Decisions
            [ ("If you have smeared Baknar oil into your skin, turn to 202.", Conditional (HasFlag baknarOilB03) (NoDecision (Goto 202))),
              ( "otherwise",
                Conditional
                  (Not (HasFlag baknarOilB03))
                  ( Decisions
                      [ ("If you have not coated yourself with Baknar oil, you can avoid the Kalkoth by retracing your steps back to the other tunnel. Turn to 284.", NoDecision (Goto 284)),
                        ("If you wish to attack the squabbling creatures, turn to 138.", NoDecision (Goto 138))
                      ]
                  )
              )
            ]
        )
    108 -> Just (EvadeFight 1 366 (FightDetails "Ice Barbarian" 16 24 []) (Goto 282))
    119 -> takeItems [(Meal, 5), (ropeB03, 1)] computedDecision
    121 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    137 ->
      let fht mods = Fight (FightDetails "Ice barbarian + Doomwolf" 30 30 mods) (Goto 28)
       in Just (NoDecision (Conditionally [(HasDiscipline MindBlast, fht [CombatBonus (-1)]), (Always True, fht [])]))
    138 -> Just (NoDecision (Simple [SetFlag killKL8] (Goto 362)))
    140 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    146 -> Just (NoDecision (Goto 29)) -- same chapter
    149 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (COr (HasDiscipline Tracking) (HasDiscipline Hunting)) (HasDiscipline SixthSense), Randomly [(3 % 10, Goto 286), (7 % 10, Goto 333)]),
                  (Always True, Randomly [(5 % 10, Goto 286), (5 % 10, Goto 333)])
                ]
            )
        )
    150 ->
      let choices = Conditionally [(HasItem (Weapon Sommerswerd) 1, Goto 120), (HasItem fireSphereB03 1, Goto 310), (Always True, Goto 367)]
       in Just (NoDecision (Conditionally [(HasFlag baknarOilB03, choices), (Always True, Simple [DamagePlayer 2] choices)]))
    151 -> Just (NoDecision (Conditionally [(HasFlag baknarOilB03, Goto 209), (Always True, Goto 339)]))
    152 ->
      Just
        ( Decisions
            [ ( "throw " ++ show n ++ " gold",
                Conditional
                  (HasItem Gold n)
                  (NoDecision (Simple [LoseItem Gold n] (Randomly [(pb, Goto 319), (1 - pb, Goto 181)])))
              )
              | n <- [1 .. 10],
                let pb = fromIntegral n / 10
            ]
        )
    155 ->
      Just
        ( NoDecision
            ( Simple
                [MustEat NoHunt]
                ( Conditionally
                    [ (HasEndurance 21, Randomly [(2 % 10, Goto 248), (8 % 10, Goto 191)]),
                      (HasEndurance 10, Randomly [(3 % 10, Goto 248), (7 % 10, Goto 191)]),
                      (Always True, Randomly [(5 % 10, Goto 248), (5 % 10, Goto 191)])
                    ]
                )
            )
        )
    156 -> Nothing
    157 -> Just (computedDecision & _Outcome %~ Simple [LoseItem gallowBrushB03 1, HealPlayer 6])
    161 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune])
    164 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Undead])
    170 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 6])
    171 -> Nothing
    177 -> Nothing
    180 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [DPR 3])
    181 -> Just (Decisions [("attack", NoDecision (Goto 208)), ("go back", NoDecision (Goto 189))])
    183 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(2 % 10, Goto 89), (8 % 10, Goto 215)]),
                  (Always True, Randomly [(4 % 10, Goto 89), (6 % 10, Goto 215)])
                ]
            )
        )
    185 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline SixthSense, Randomly [(3 % 10, Goto 22), (7 % 10, Goto 326)]),
                  (Always True, Randomly [(5 % 10, Goto 22), (5 % 10, Goto 326)])
                ]
            )
        )
    192 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    196 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2, LoseItemKind [WeaponSlot]])
    206 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    209 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    210 ->
      Just
        ( Decisions
            [ ("If you have the Kai Discipline of Sixth Sense, turn to 316.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 316))),
              ("If you wish to take the Gold Bracelet, turn to 236.", NoDecision (Goto 236)),
              ("If you do not wish to take the Bracelet, make your way quickly along the corridor towards a distant junction by turning to 215.", NoDecision (Goto 215))
            ]
        )
    211 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasEndurance 10, Randomly [(4 % 10, Goto 95), (6 % 10, Goto 196)]),
                  (Always True, Randomly [(7 % 10, Goto 95), (3 % 10, Goto 196)])
                ]
            )
        )
    214 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    217 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 10])
    218 -> takeItems [(diamondB03, 1)] computedDecision
    223 -> takeItems [(Meal, 5), (ropeB03, 1)] computedDecision
    225 -> Nothing
    226 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    233 -> Nothing
    235 -> Just (computedDecision & _Outcome %~ Simple [ClearFlag killKL8, ClearFlag killKL10, ClearFlag killKL11])
    237 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt])
    241 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [Timed 2 PlayerInvulnerable])
    250 -> Nothing
    251 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 2])
    258 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (HasDiscipline SixthSense) (HasDiscipline Hunting), Randomly [(1 % 10, Simple [DamagePlayer (max 0 n)] (Goto 63)) | n <- [-1 .. 8]]),
                  (Always True, Randomly [(1 % 10, Simple [DamagePlayer n] (Goto 63)) | n <- [1 .. 10]])
                ]
            )
        )
    262 ->
      Just
        ( Decisions
            [ ("If you possess the Kai Discipline of Sixth Sense, turn to 71.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 71))),
              ( "otherwise",
                Conditional
                  (Not (HasDiscipline SixthSense))
                  ( NoDecision (Randomly [(7 % 10, Goto 320), (3 % 10, Goto 140)])
                  )
              )
            ]
        )
    263 -> Just (EvadeFight 0 277 (FightDetails "Kalkoth 1" 11 35 [OnDamage 66]) (Simple [SetFlag killKL11] (Goto 360)))
    265 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune])
    267 -> Just (computedDecision & _Outcome %~ Simple [LoseItem glowingCrystalB03 1])
    268 -> Nothing
    272 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasItem (Weapon Sommerswerd) 1, Goto 213),
                  (Always True, Randomly [(4 % 10, Goto 143), (6 % 10, Goto 58)])
                ]
            )
        )
    277 -> Just (computedDecision & _Decisions . ix 0 . _2 %~ Conditional anyweapon)
    280 -> takeItems [(ornateSilverKeyB03, 1)] (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    282 -> takeItems [(Weapon Spear, 1), (blueStoneDiscB03, 1)] computedDecision
    283 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(2 % 10, Goto 53), (3 % 10, Goto 16), (5 % 10, Goto 113)]),
                  (Always True, Randomly [(5 % 10, Goto 53), (3 % 10, Goto 16), (2 % 10, Goto 113)])
                ]
            )
        )
    284 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (CAnd (HasDiscipline Hunting) (HasEndurance 8), Randomly [(2 % 10, Goto 94), (8 % 10, Goto 176)]),
                  (HasDiscipline Hunting, Randomly [(4 % 10, Goto 94), (6 % 10, Goto 176)]),
                  (HasEndurance 8, Randomly [(4 % 10, Goto 94), (6 % 10, Goto 176)]),
                  (Always True, Randomly [(6 % 10, Goto 94), (4 % 10, Goto 176)])
                ]
            )
        )
    289 -> Nothing
    290 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (HasDiscipline SixthSense) (HasLevel Aspirant), Goto 341),
                  (HasDiscipline SixthSense, Goto 124),
                  (Always True, Goto 264)
                ]
            )
        )
    294 -> Just (computedDecision & _Outcome %~ Simple [MustEat NoHunt, MustEat NoHunt])
    295 -> takeItems [(fireSphereB03, 1)] computedDecision
    298 -> takeItems [(blueStoneTriangleB03, 1)] computedDecision
    299 -> Just (computedDecision & _Outcome %~ \o -> Conditionally [(HasDiscipline MindOverMatter, o), (Always True, Simple [DamagePlayer 2] o)])
    302 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(0 % 10, Goto 37), (6 % 10, Goto 193), (4 % 10, Goto 243)]),
                  (Always True, Randomly [(2 % 10, Goto 37), (6 % 10, Goto 193), (2 % 10, Goto 243)])
                ]
            )
        )
    303 -> Just (computedDecision & _Outcome %~ Simple [LoseItem ornateSilverKeyB03 1])
    304 -> Just (computedDecision & _Outcome . _Fight . _1 . fightMod .~ [MindblastImmune, EnemyMindblast, Undead])
    308 -> takeItems [(SilverHelm, 1)] computedDecision
    309 -> takeItems [(blueStoneTriangleB03, 1), (fireSphereB03, 1)] computedDecision
    311 -> Nothing
    321 -> takeItems [(blueStoneTriangleB03, 1)] computedDecision
    323 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (COr (COr (HasDiscipline Hunting) (HasDiscipline Tracking)) (HasDiscipline SixthSense), Randomly [(2 % 10, Goto 76), (8 % 10, Goto 2)]),
                  (Always True, Randomly [(5 % 10, Goto 76), (5 % 10, Goto 2)])
                ]
            )
        )
    326 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 1])
    328 -> Just (computedDecision & _Outcome %~ Simple [DamagePlayer 6])
    329 ->
      Just
        ( Decisions
            [ ("If you have smeared Baknar oil into your skin, turn to 202.", Conditional (HasFlag baknarOilB03) (NoDecision (Goto 202))),
              ("If you have not applied Baknar oil to your skin, you can avoid the Kalkoth by retracing your route back to the entrance of the other tunnel, by turning to 284.", NoDecision (Goto 284)),
              ("If you wish to attack the creatures, you can turn to 138.", NoDecision (Goto 138))
            ]
        )
    330 ->
      Just
        ( Decisions
            [ ("If you have a Rope, you can climb down and attempt to capture him by turning to 100.", Conditional (HasItem ropeB03 1) (NoDecision (Goto 100))),
              ("If you do not have a Rope, you can descend the spiral staircase by turning to 148.", Conditional (Not (HasItem ropeB03 1)) (NoDecision (Goto 148))),
              ("If you prefer, you can investigate the door at the end of the balcony by turning to 61.", NoDecision (Goto 61))
            ]
        )
    331 -> Just (computedDecision & _Outcome %~ \o -> Conditionally [(HasDiscipline Healing, o), (Always True, Simple [DamagePlayer 6] o)])
    334 -> takeItems [(glowingCrystalB03, 1)] computedDecision
    340 -> Just (computedDecision & _Outcome %~ Simple [HealPlayer 6])
    343 ->
      Just
        ( NoDecision
            ( Fight
                (FightDetails "Doomwolf 1" 15 24 [])
                ( Fight
                    (FightDetails "Doomwolf 2" 14 23 [])
                    ( Fight
                        (FightDetails "Doomwolf 3" 14 20 [])
                        (Fight (FightDetails "Ice Barbarian" 17 29 [MindblastImmune]) (Goto 28))
                    )
                )
            )
        )
    346 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasDiscipline Hunting, Randomly [(2 % 10, Goto 195), (8 % 10, Goto 232)]),
                  (Always True, Randomly [(4 % 10, Goto 195), (6 % 10, Goto 232)])
                ]
            )
        )
    349 ->
      Just
        ( NoDecision
            ( Conditionally
                [ (HasItem glowingCrystalB03 1, Goto 139),
                  (Always True, Goto 97)
                ]
            )
        )
    350 -> Just (NoDecision GameWon)
    _ -> Just computedDecision

extraChapters03 :: [(ChapterId, Chapter)]
extraChapters03 =
  [ (360, Chapter "263b" "fight 2" (EvadeFight 0 277 (FightDetails "Kalkoth 2" 10 32 [OnDamage 66]) (Simple [SetFlag killKL10] (Goto 361)))),
    (361, Chapter "263b" "fight 3" (EvadeFight 0 277 (FightDetails "Kalkoth 3" 8 30 [OnDamage 66]) (Simple [SetFlag killKL8] (Goto 25)))),
    (362, Chapter "138b" "fight 1" (EvadeFight 0 277 (FightDetails "Kalkoth 1" 11 35 [OnDamage 66]) (Simple [SetFlag killKL11] (Goto 363)))),
    (363, Chapter "138b" "fight 2" (EvadeFight 0 277 (FightDetails "Kalkoth 2" 10 32 [OnDamage 66]) (Simple [SetFlag killKL10] (Goto 25)))),
    (364, Chapter "032b" "fight 2" (NoDecision (Conditionally [(HasFlag killKL10, Goto 365), (Always True, Fight (FightDetails "Kalkoth 2" 10 32 [OnDamage 66]) (Goto 365))]))),
    (365, Chapter "032b" "fight 3" (NoDecision (Conditionally [(HasFlag killKL8, Goto 25), (Always True, Fight (FightDetails "Kalkoth 3" 8 30 [OnDamage 66]) (Goto 25))]))),
    (366, Chapter "108b" "escaping" (Decisions [("south", NoDecision (Goto 330)), ("north", NoDecision (Goto 198))])),
    ( 367,
      Chapter
        "150b"
        "choice"
        ( Decisions
            [ ("If you wish to attack the swirling cyclone with a weapon, turn to 18.", NoDecision (Goto 18)),
              ("If you wish to try to escape into the distant archway, turn to 211.", NoDecision (Goto 211))
            ]
        )
    ),
    (368, takepotion "red" (HasDiscipline Healing) Potion5Hp 1 369),
    (369, takepotion "orange" (COr (HasDiscipline Healing) (HasDiscipline SixthSense)) StrengthPotion 2 370),
    (370, takepotion "green" (COr (HasDiscipline AnimalKinship) (HasLevel Aspirant)) gallowBrushB03 1 126)
  ]
  where
    takepotion nm cnd i q nxt =
      Chapter
        "10b"
        ("try to take " ++ nm ++ " potion")
        ( Decisions
            [ ("try to take the " ++ nm ++ " potion", Conditional cnd (CanTake i q (NoDecision (Goto nxt)))),
              ("don't try", Conditional (Not cnd) (NoDecision (Goto nxt)))
            ]
        )