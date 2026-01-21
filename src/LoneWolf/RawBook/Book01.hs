module LoneWolf.RawBook.Book01 (chapters) where

import Data.Ratio ((%))
import LoneWolf.Chapter
import LoneWolf.Character

chapters :: [(ChapterId, Chapter)]
chapters =
  [ ( 1,
      Chapter
        "1"
        "You must make haste for you sense it is not safe to linger by the smoking remains of the ruined monastery. The black-winged beasts could return at any moment. You must set out for the Sommlending capital of Holmgard and tell the King the terrible news of the massacre: that the whole \233lite of Kai warriors, save yourself, have been slaughtered. Without the Kai Lords to lead her armies, Sommerlund will be at the mercy of their ancient enemy, the Darklords.\nFighting back tears, you bid farewell to your dead kinsmen. Silently, you promise that their deaths will be avenged. You turn away from the ruins and carefully descend the steep track.\nAt the foot of the hill, the path splits into two directions, both leading into a large wood.\n"
        ( Decisions
            [ ("If you wish to use your Kai Discipline of Sixth Sense, turn to 141.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 141))),
              ("If you wish to take the right path into the wood, turn to 85.", NoDecision (Goto 85)),
              ("If you wish to follow the left track, turn to 275.", NoDecision (Goto 275))
            ]
        )
    ),
    ( 2,
      Chapter
        "2"
        "As you dash through the thickening trees, the shouts of the Giaks begin to fade behind you. You have nearly outdistanced them completely, when you crash headlong into a tangle of low branches.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 343), (1 % 2, Goto 276)]))
    ),
    ( 3,
      Chapter
        "3"
        "Staying close to the officer, you follow him through an arched portal and up a short flight of stairs to a long hall. Soldiers run back and forth bearing orders on ornate scrolls to officers stationed around the city wall.\nA haggard and scar-faced man dressed in the white and purple robes of the King's court approaches you and bids you follow him to the citadel.\n"
        ( Decisions
            [ ("If you wish to follow this man, turn to 196.", NoDecision (Goto 196)),
              ("If you wish to decline his offer and return to the crowded streets, turn to 144.", NoDecision (Goto 144))
            ]
        )
    ),
    ( 4,
      Chapter
        "4"
        "It is a small one-man canoe in very poor condition. The wood has split and warped, and the craft appears to be leaking in several places. You quickly patch up the worst of the holes with some clay and bail out the water. This seems to stop the leaking for the moment. Stowing your equipment at the bow, you set off downstream, using a piece of driftwood as a paddle.\nAfter a short while, you hear the sound of horses galloping towards you along the left bank.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Sixth Sense, turn to 218.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 218))),
              ("If you wish to hide in the bottom of the canoe, turn to 75.", NoDecision (Goto 75)),
              ("If you wish to try to attract their attention, turn to 175.", NoDecision (Goto 175))
            ]
        )
    ),
    ( 5,
      Chapter
        "5"
        "After about an hour of walking, the track slowly bears round to the east. You reach a shallow ford where a fast-flowing brook runs on a steep rocky course towards the south. Just beyond the ford is a junction where the track meets a wider path running north to south. Realizing that the north path will take you away from the capital, you turn right at the junction and head south.\n"
        (NoDecision (Goto 111))
    ),
    ( 6,
      Chapter
        "6"
        "In the distance you can hear the sound of horses galloping nearer. You crouch behind a tree and wait as the riders come closer. They are the cavalry of the King's Guard wearing the white uniforms of His Majesty's army.\n"
        ( Decisions
            [ ("If you wish to call them, turn to 183.", NoDecision (Goto 183)),
              ("If you wish to let them pass and then continue on your way through the forest, turn to 200.", NoDecision (Goto 200))
            ]
        )
    ),
    ( 7,
      Chapter
        "7"
        "For what seems an eternity, the rush of the crowd carries you along like a leaf on a fast-flowing stream. You desperately fight to stay on your feet, but you feel weak and dizzy from your ordeal, and your legs are as heavy as lead. Suddenly, you catch a glimpse of a long, narrow stone stairway that leads up to the roof of an inn.\nGathering the last reserves of your strength, you dive for the stairs and climb slowly up to the top. From here you can see the magnificent view of the rooftops and spires of Holmgard, with the high stone walls of the citadel gleaming in the sun.\nThe houses and buildings of the capital are built very close to each other, and it is possible to jump from one roof to the next. In fact many of the citizens of Holmgard used to use the \"Roofways\" (as they are known) when the heavy autumn rains made the unpaved parts of the streets too muddy for walking. But after many accidents, a royal decree forbade their use.\nAfter careful thought, you decide to use the \"Roofways\", as they are your only chance of reaching the King. You have hopped, skipped, and jumped across several streets and you are only one street away from the citadel when you come to the end of a row of rooftops.\nThe jump to the next row is much further than anything you have tried before, and your stomach begins to feel as if it were full of butterflies. Determined to reach the citadel, you turn and take a long run-up to the jump. With blood pounding in your ears, you sprint to the edge of the roof and leap into space, your eyes fixed on the opposite rooftop.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(3 % 10, Goto 108), (7 % 10, Goto 25)]))
    ),
    ( 8,
      Chapter
        "8"
        "Your Kai Sixth Sense warns there is a fierce battle raging in the south. Your common sense tells you that the south is also the quickest route to the capital.\n"
        (NoDecision (Goto 70))
    ),
    ( 9,
      Chapter
        "9"
        "You cannot move: you are being held rigid by some powerful force. Your eyes are drawn towards the mouth of the skeleton. From deep in the earth you hear a low humming, like the sound of millions of angry bees. A dull red glow appears in the empty eye sockets of the dead King and the humming increases until your ears are filled with the deafening roar. You are in the presence of an ancient evil, far older and stronger than the Darklords themselves.\n"
        ( Decisions
            [ ("If you possess a Vordak Gem, turn to 236.", Conditional (HasItem (GenSpecial (GenCounter 0)) 1) (NoDecision (Goto 236))),
              ("otherwise", Conditional (Not (HasItem (GenSpecial (GenCounter 0)) 1)) (Decisions [("If you do not, turn to 292.", NoDecision (Goto 292))]))
            ]
        )
    ),
    ( 10,
      Chapter
        "10"
        "You are sweating and your legs ache. In the middle distance you can see a group of cottages.\n"
        ( Decisions
            [ ("If you wish to enter a cottage and rest for a while, turn to 115.", NoDecision (Goto 115)),
              ("If you wish to press on, turn to 83.", NoDecision (Goto 83))
            ]
        )
    ),
    ( 11,
      Chapter
        "11"
        "You quickly dodge into the doorway of a stable and hide your surgeon's cloak in the straw, for it would be better to be seen as a Kai Lord than as a charlatan.\nWithout wasting a second, you set off towards the Great Hall on the other side of the courtyard.\n"
        (NoDecision (Goto 139))
    ),
    ( 12,
      Chapter
        "12"
        "The bodyguard looks at you with great suspicion and then slams the door shut. You can hear the sound of voices inside the caravan. Suddenly the door swings open and the face of a wealthy merchant appears.\nHe demands 10 Gold Crowns as payment for the ride.\n"
        ( Decisions
            [ ("If you have 10 Gold Crowns and wish to pay him, turn to 262.", Conditional (HasItem Gold 10) (NoDecision (Simple [LoseItem Gold 10] (Goto 262)))),
              ("If you do not have enough Gold Crowns or do not wish to pay him, turn to 247.", NoDecision (Goto 247))
            ]
        )
    ),
    ( 13,
      Chapter
        "13"
        "The path soon ends at a large clearing. In the centre of the clearing is a tree much taller and wider than any others you have seen in the forest.\nLooking up through the massive branches you can see a large treehouse some twenty-five to thirty feet above the ground. There is no ladder, but the gnarled bark of the tree offers many footholds.\n"
        ( Decisions
            [ ("If you wish to climb the tree and search the treehouse, turn to 307.", NoDecision (Goto 307)),
              ("If you would rather press on, turn to 213.", NoDecision (Goto 213))
            ]
        )
    ),
    ( 14,
      Chapter
        "14"
        "You reach the top of a small wooded hill on which several large boulders form a rough circle. Suddenly you hear a loud growl from behind a rock to your left.\n"
        ( Decisions
            [ ("If you wish to draw your weapon and prepare to fight, turn to 43.", NoDecision (Goto 43)),
              ("If you would rather take evasive action by running as fast as you can over the hill, turn to 106.", NoDecision (Goto 106))
            ]
        )
    ),
    ( 15,
      Chapter
        "15"
        "You pass through a long, dark tunnel of overhanging branches that eventually opens out into a large clearing. On a stone plinth in the centre of the clearing is a Sword, sheathed in a black leather scabbard. A handwritten note has been tied to the hilt, but it is in a language which is foreign to you.\nYou may take the Sword if you wish, and note it on your Action Chart.\nThere are three exits from the clearing.\n"
        ( CanTake
            (Weapon Sword)
            1
            ( Decisions
                [ ("If you decide to go east, turn to 207.", NoDecision (Goto 207)),
                  ("If you decide to go west, turn to 201.", NoDecision (Goto 201)),
                  ("If you decide to go south, turn to 35.", NoDecision (Goto 35))
                ]
            )
        )
    ),
    ( 16,
      Chapter
        "16"
        "You manage to free a horse from the straps securing it to the caravan. It is frightened by the scent of the approaching Doomwolves, and the cries of their evil riders-the Giaks.\nPreparing your weapon, you spur your skittish horse towards the oncoming beasts. They are less than fifty yards away and they are lowering their lances at you as they get nearer and nearer.\n"
        (NoDecision (Goto 192))
    ),
    ( 17,
      Chapter
        "17"
        "You raise your weapon to strike at the beast as its razor-fanged mouth snaps shut just inches from your head. Buffeted by the beating of its wings you find it difficult to stand.\nDeduct 1 point from your COMBAT SKILL and fight the Kraan.\nIf you kill the creature, you quickly descend the far side of the hill to avoid the Giaks.\nPick a number from the Random Number Table.\n"
        ( NoDecision
            ( Fight
                ( FightDetails
                    { _opponent = "Kraan",
                      _fcombatSkill = CombatSkill {getCombatSkill = 16},
                      _fendurance = Endurance {getEndurance = 24},
                      _fightMod = [CombatBonus (CombatSkill {getCombatSkill = -1})]
                    }
                )
                (Randomly [(1 % 10, Goto 53), (1 % 5, Goto 274), (7 % 10, Goto 316)])
            )
        )
    ),
    ( 18,
      Chapter
        "18"
        "You are awoken by the sound of troops in the distance. Across the lake you see the black-cloaked figures of Drakkarim and a pack of Doomwolves and their riders. A Kraan appears from above the trees and lands on the roof of the small wooden shack.\nIt is ridden by a creature dressed in red. The Kraan takes off and begins to fly across the lake to where you are hidden.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Camouflage, turn to 114.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 114))),
              ("If you wish to ride deeper in the forest, turn to 239.", NoDecision (Goto 239)),
              ("If you wish to fight the creature, turn to 29.", NoDecision (Goto 29))
            ]
        )
    ),
    ( 19,
      Chapter
        "19"
        "Just ahead through the tall trees you can see clumps of dark-red gallowbrush, a thorny briar with sharp crimson barbs. The common name for this forest weed is \"Sleeptooth\", for the thorns are very sharp and can make you feel weak and sleepy if they scratch your skin.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Tracking, turn to 69.", Conditional (HasDiscipline Tracking) (NoDecision (Goto 69))),
              ( "otherwise",
                Conditional
                  (Not (HasDiscipline Tracking))
                  ( Decisions
                      [ ("You can avoid the Sleeptooth by returning to the track. Turn to 272.", NoDecision (Goto 272)),
                        ("Or you can push on through the briars, deeper into the forest, by turning to 119.", NoDecision (Goto 119))
                      ]
                  )
              )
            ]
        )
    ),
    ( 20,
      Chapter
        "20"
        "It seems that whoever lived here left in a great hurry-and they must have left quite recently. A half-eaten meal still remains on the table, and a mug of dark jala is still warm to the touch.\nSearching a chest and small wardrobe, you find a Backpack, food (enough for two Meals), and a Dagger.\nIf you wish to take these items, remember to mark them on your Action Chart. You continue your mission.\n"
        (CanTake Backpack 1 (CanTake Meal 2 (CanTake (Weapon Dagger) 1 (NoDecision (Goto 273)))))
    ),
    ( 21,
      Chapter
        "21"
        "You have ridden about two miles into the tangle of trees when the ground becomes very marshy.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(13 % 20, Goto 189), (63 % 200, GameLost), (7 % 200, Goto 312)]))
    ),
    ( 22,
      Chapter
        "22"
        "Knocking aside the leader, you sprint off along the highway. Then behind you the ominous *click* of a crossbow being cocked sends a shiver down your spine.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 181), (1 % 2, Goto 145)]))
    ),
    ( 23,
      Chapter
        "23"
        "The corridor soon widens into a large hall. At the far end, a stone staircase leads up to a huge door. Two black candles on either side of the stone steps dimly illuminate the chamber. You notice that no wax has melted, and as you get nearer you can feel that they give off no heat. Ancient engravings cover the stone surfaces of the walls.\nAnxious to leave this evil tomb, you examine the door for a latch. An ornate pin appears to lock the door, but there is also a keyhole in the lockplate.\n"
        ( Decisions
            [ ("If you have a Golden Key and wish to use it, turn to 326.", Conditional (HasItem (GenSpecial (GenCounter 1)) 1) (NoDecision (Goto 326))),
              ("If you have the Kai Discipline of Mind Over Matter, turn to 151.", Conditional (HasDiscipline MindOverMatter) (NoDecision (Goto 151))),
              ("If you wish to remove the pin, turn to 337.", NoDecision (Goto 337))
            ]
        )
    ),
    ( 24,
      Chapter
        "24"
        "The merchant shouts to the driver of the caravan to jump. \"We're under attack!\" he cries, disappearing through a circular window.\n"
        ( Decisions
            [ ("If you decide to jump after him, turn to 234.", NoDecision (Goto 234)),
              ("If you decide to run through the caravan and grab the reins of the horse team, turn to 184.", NoDecision (Goto 184))
            ]
        )
    ),
    ( 25,
      Chapter
        "25"
        "You land with such a crash on the opposite roof, that the wind is knocked out of you and you lie flat on your back with your head in a spin.\nIt takes a minute or so for you to realize that you've made it and are perfectly safe. When you are sure you are all right, you jump up and let out a shout for joy at your skill and daring.\nQuickly you find a way across the roof and climb down a long drainpipe to the street below. You see the large iron doors of the citadel open, and a wagon drawn by two large horses tries to leave. The horses are frightened by the noisy crowd and they both rear up, causing the wagon to smash a front wheel against the door. In the confusion, you see a chance to enter and quickly slip inside just as the guards slam the doors shut.\n"
        (NoDecision (Goto 139))
    ),
    ( 26,
      Chapter
        "26"
        "Cautiously, you move along the corridor until you come to a sharp eastward turn. A strange greenish light can be seen in the distance.\n"
        ( Decisions
            [ ("If you wish to continue, turn to 249.", NoDecision (Goto 249)),
              ("If you wish to go back and try the southern route, turn to 100.", NoDecision (Goto 100))
            ]
        )
    ),
    ( 27,
      Chapter
        "27"
        "You walk along this path for over an hour, carefully watching the sky above you in case the Kraan attack again. Up ahead, a large tree has fallen across the path. As you approach, you can hear voices coming from the other side of the massive trunk.\n"
        ( Decisions
            [ ("If you choose to attack, turn to 250.", NoDecision (Goto 250)),
              ("If you choose to listen to what the voices say, turn to 52.", NoDecision (Goto 52))
            ]
        )
    ),
    ( 28,
      Chapter
        "28"
        "After a few hundred yards, the path joins another one running north to south.\n"
        ( Decisions
            [ ("If you wish to go northwards, turn to 130.", NoDecision (Goto 130)),
              ("If you wish to head south, turn to 147.", NoDecision (Goto 147))
            ]
        )
    ),
    ( 29,
      Chapter
        "29"
        "You stride out to the water's edge and prepare yourself for combat. The Kraan and its rider spot you and begin to speed across the lake barely inches above the surface.\nThe rider lets out a scream that freezes your blood. He is a Vordak, a fierce lieutenant of the Darklords.\nHe is upon you and you must fight him. Deduct 2 points from your COMBAT SKILL unless you have the Kai Discipline of Mindshield, for the creature is attacking you with its Mindforce as well as with a huge black mace.\n"
        ( NoDecision
            ( Fight
                ( FightDetails
                    { _opponent = "Vordak",
                      _fcombatSkill = CombatSkill {getCombatSkill = 17},
                      _fendurance = Endurance {getEndurance = 25},
                      _fightMod = [EnemyMindblast]
                    }
                )
                (Goto 270)
            )
        )
    ),
    ( 30,
      Chapter
        "30"
        "The people look tired and hungry. They have come many miles from their burning city. Suddenly, you hear the beat of huge wings coming from the north.\n\"Kraan, Kraan! Hide yourselves!\" the cry goes up all along the road.\nJust in front of you, a wagon carrying small children breaks down, its right wheel jammed in a furrow. The children scream in panic.\n"
        ( Decisions
            [ ("If you wish to help the children, turn to 194.", NoDecision (Goto 194)),
              ("If you'd rather run for the cover of the trees, turn to 261.", NoDecision (Goto 261))
            ]
        )
    ),
    ( 31,
      Chapter
        "31"
        "You try to comfort the injured man as best you can, but his wounds are serious and he is soon unconscious again. Covering him with his cape, you turn and press deeper into the forest.\n"
        (NoDecision (Goto 264))
    ),
    ( 32,
      Chapter
        "32"
        "You have ridden about three miles when, in the distance, you spot the unmistakable silhouette of five large Doomwolves. Riding on their backs are Giaks. They seem to be going on ahead to where the path leads down into an open meadow. Suddenly, one of the Giaks leaves the others and begins to ride back along the path towards you.\n"
        ( Decisions
            [ ("If you wish to hide in the undergrowth and let him pass, turn to 176.", NoDecision (Goto 176)),
              ("If you wish to fight him, turn to 340.", NoDecision (Goto 340))
            ]
        )
    ),
    ( 33,
      Chapter
        "33"
        "The floor of the cave is quite dry and dusty. As you explore deeper in the half-light, you detect the stale odour of rotting flesh. Littering a crevice are the bones, fur, and teeth of several small animals. You notice a small cloth bag among these remains which you open to discover 3 Gold Crowns. Pocketing these coins, you leave what appears to be the lair of a mountain cat and carefully descend the hill.\n"
        (CanTake Gold 3 (NoDecision (Goto 248)))
    ),
    ( 34,
      Chapter
        "34"
        "Without warning, a terrible apparition in red robes swoops down from the sky on the back of a Kraan. Its cry freezes your blood. The beast is a Vordak, a fierce lieutenant of the Darklords.\nHe is above you and you must fight him.\nDeduct 2 points from your COMBAT SKILL unless you have the Kai Discipline of Mindshield, for the creature is attacking you with the power of its Mindforce as well as with a huge black mace.\n"
        ( NoDecision
            ( Fight
                ( FightDetails
                    { _opponent = "Vordak",
                      _fcombatSkill = CombatSkill {getCombatSkill = 17},
                      _fendurance = Endurance {getEndurance = 25},
                      _fightMod = [EnemyMindblast]
                    }
                )
                (Goto 328)
            )
        )
    ),
    ( 35,
      Chapter
        "35"
        "The forest is becoming denser, and the path more tangled with thorny briars. Almost completely hidden by the undergrowth, you notice another path branching off towards the east. Your current route seems to be coming to a prickly end, so you decide to follow the new path eastwards.\n"
        (NoDecision (Goto 207))
    ),
    ( 36,
      Chapter
        "36"
        "The old watchtower ladder is rotten and several rungs break as you climb. Pick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 140)), (1 % 2, Goto 323)]))
    ),
    ( 37,
      Chapter
        "37"
        "You are feeling tired and hungry and you must stop to eat. After your Meal, you retrace your steps back to the citadel and begin to walk around the high, indomitable stone wall.\nYou discover another entrance on the eastern side, guarded as before by two armoured soldiers.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Camouflage, turn to 282.", Conditional (HasDiscipline Camouflage) (NoDecision (Simple [MustEat Hunt] (Goto 282)))),
              ("If you wish to approach them and tell your story, turn to 289.", NoDecision (Simple [MustEat Hunt] (Goto 289)))
            ]
        )
    ),
    ( 38,
      Chapter
        "38"
        "For half an hour or more you press on through the forest, through the rich vegetation and ferns. You happen upon a small clear stream where you stop for a few minutes to wash your face and drink of the cold, fresh water.\nFeeling revitalized, you cross the stream and press on. You soon notice the smell of wood smoke which seems to be drifting towards you from the north.\n"
        ( Decisions
            [ ("If you wish to investigate the smell of wood smoke, turn to 128.", NoDecision (Goto 128)),
              ("If you would rather avoid the source of this smoke, turn to 347.", NoDecision (Goto 347))
            ]
        )
    ),
    ( 39,
      Chapter
        "39"
        "After a few seconds, two small furry faces nervously appear over the top of the trunk. They say they are Kakarmi and tell you that the Kraan are everywhere. To the west lie the remains of their village but little is left of it now. They are trying to find the rest of their tribe who took to the forest when the \"Black-wings\" attacked. They point behind them-east along the path-and tell you that the trail appears to be a dead end, but that if you continue through the undergrowth for a few yards more, you will find a watchtower where the path splits into three directions. Take the east path. This leads to the King's highway between the capital city-Holmgard-and the northern port of Toran.\nYou thank the Kakarmi, and bid them farewell.\n"
        (NoDecision (Goto 228))
    ),
    ( 40,
      Chapter
        "40"
        "Keeping a careful watch on the huts for any sign of the enemy, you make your way around the clearing under the cover of the trees and bracken. Rejoining the track, you hurry away from Fogwood.\n"
        (NoDecision (Goto 105))
    ),
    ( 41,
      Chapter
        "41"
        "Three rangers gallop past the river bank, closely followed by the Giaks on their snarling Doomwolves.\nThe bank is steep and you are spotted by the Giak leader who orders five of his troops to open fire at you with their bows. Their black arrows rain down on you.\n"
        ( Decisions
            [ ("If you decide to paddle downstream as fast as you can, turn to 174.", NoDecision (Goto 174)),
              ("If you decide to head for the cover of the trees on the opposite bank, turn to 116.", NoDecision (Goto 116))
            ]
        )
    ),
    ( 42,
      Chapter
        "42"
        "You follow the track for nearly an hour when you come to a crossroads.\n"
        ( Decisions
            [ ("If you wish to continue east, turn to 86.", NoDecision (Goto 86)),
              ("If you would rather head north, turn to 238.", NoDecision (Goto 238)),
              ("If you decide to venture south, turn to 157.", NoDecision (Goto 157)),
              ("Or if you prefer to go west, turn to 147.", NoDecision (Goto 147))
            ]
        )
    ),
    ( 43,
      Chapter
        "43"
        "From behind the rock a huge black bear comes into view. It advances slowly towards you, its mouth open and its face lined in anger and pain.\nYou notice that it is badly wounded and is bleeding from its neck and back. You must fight it.\n"
        ( EvadeFight
            3
            106
            ( FightDetails
                { _opponent = "Black Bear",
                  _fcombatSkill = CombatSkill {getCombatSkill = 16},
                  _fendurance = Endurance {getEndurance = 10},
                  _fightMod = []
                }
            )
            (Goto 195)
        )
    ),
    ( 44,
      Chapter
        "44"
        "Without warning, the old track ends abruptly at the edge of a steep slope. The ground here is very loose and unstable. You lose your footing and fall headlong over the edge.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 277), (1 % 2, Goto 338)]))
    ),
    ( 45,
      Chapter
        "45"
        "These men are not what they seem. The tunic of the leader is genuine but it is heavily bloodstained around the collar, as if its true owner had been murdered. Their weapons are not army issue, but expensive and lavishly decorated like the weapons made by the armourers of Durenor.\nThe leader has a crossbow slung over his pack. An attempt to run would be suicide. You decide that you must fight them or you will surely be murdered as soon as you drop your weapon.\n"
        (NoDecision (Goto 180))
    ),
    ( 46,
      Chapter
        "46"
        "You have covered about two miles when the trees ahead thin out. You can see a small wooden shack on the edge of a lake. A cloaked man approaches you and offers to row you and your horse across the lake for a fee of 2 Gold Crowns.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Sixth Sense, turn to 296.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 296))),
              ("otherwise", Conditional (Not (HasDiscipline SixthSense)) (Decisions [("If you accept the offer, turn to 246.", Conditional (HasItem Gold 2) (NoDecision (Simple [LoseItem Gold 2] (Goto 246)))), ("If you refuse and try to ride around the lake, turn to 90.", NoDecision (Goto 90))]))
            ]
        )
    ),
    ( 47,
      Chapter
        "47"
        "Breathless and sweating, you claw your way towards the summit of the hill. Suddenly, a large winged shadow passes across the hillside. You look up to see a Kraan circling the peak above. Behind you the Giaks are gaining ground.\n"
        ( Decisions
            [ ("Do you stand and fight the Giaks where you are, using the high ground to your advantage? If so, turn to 136.", NoDecision (Goto 136)),
              ("Or do you grit your teeth and press on towards the peak of the hill? Turn to 322.", NoDecision (Goto 322))
            ]
        )
    ),
    ( 48,
      Chapter
        "48"
        "Your Sixth Sense warns you that these troops are not all they seem. You can detect an aura of evil about them. They are in the service of the Darklords.\n"
        (NoDecision (Goto 243))
    ),
    ( 49,
      Chapter
        "49"
        "As you begin to read the inscription, you notice a shadow moving towards you from behind the screen.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 339), (1 % 2, Goto 60)]))
    ),
    ( 50,
      Chapter
        "50"
        "The sound of fighting can be heard in the distance.\n"
        ( Decisions
            [ ("If you wish to continue towards the sound of battle, turn to 97.", NoDecision (Goto 97)),
              ("If you wish to avoid the fighting, change direction and turn to 243.", NoDecision (Goto 243))
            ]
        )
    ),
    ( 51,
      Chapter
        "51"
        "You climb the wooded bank of the river and see the log walls of the fieldworks disappearing into the distance.\nA battle rages about two miles away and the log wall has collapsed in several places where the Darklords are attacking.\nMost of the fieldworks ahead are unmanned, the soldiers having left to supply reinforcements for the raging battle.\n"
        ( Decisions
            [ ("There is a gate in the log wall. If you wish to approach it, turn to 288.", NoDecision (Goto 288)),
              ("If you would prefer to climb over the wall instead, turn to 221.", NoDecision (Goto 221))
            ]
        )
    ),
    ( 52,
      Chapter
        "52"
        "Now that you are closer, you can make out that the voices are not human. The sound is more like a kind of grunting and squeaking.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Animal Kinship, turn to 225.", Conditional (HasDiscipline AnimalKinship) (NoDecision (Goto 225))),
              ("otherwise", Conditional (Not (HasDiscipline AnimalKinship)) (Decisions [("If not, you must climb over the tree and face whatever lurks on the other side. Turn to 250.", NoDecision (Goto 250))]))
            ]
        )
    ),
    ( 53,
      Chapter
        "53"
        "A searing pain tears through your right leg as it is twisted and crushed by the weight of your body. Down and down you tumble, until you finally land in a ditch at the base of the hill with such force that the wind is knocked out of you and you lose consciousness.\nYou are awoken by the sharp pain of something stabbing your chest. It proves to be the tip of a Giak spear. You are greeted by the malicious sneer of its owner as he pins your left arm to the ground. Instinctively you reach for your weapon but it is no longer there.\nDefenceless against the cruel Giaks, the last thing that you see before all light fades is the jagged point of a Giak lance hurtling down towards your throat.\nYour mission ends here.\n"
        (NoDecision GameLost)
    ),
    ( 54,
      Chapter
        "54"
        "It would seem that the heavens have not heard your prayers. A spear whistles past your head and embeds itself in the neck of your galloping horse. With a shriek of pain, the horse topples forward and you both roll in a tangled heap on the highway.\nDazed and pinned down by the weight of the dead body of your horse, the last thing you remember is the sharp penetrating spearheads of the Giak lances.\nYou have failed in your mission.\n"
        (NoDecision GameLost)
    ),
    ( 55,
      Chapter
        "55"
        "Just as the Giak makes his leap, you race forward and strike out with your weapon-knocking the creature away from the young wizard's back.\nYou jump onto the struggling Giak and strike again. Due to the surprise of your attack, add 4 points to your COMBAT SKILL for the duration of this fight but remember to deduct it again as soon as the fight is over.\n"
        ( NoDecision
            ( Fight
                ( FightDetails
                    { _opponent = "Giak",
                      _fcombatSkill = CombatSkill {getCombatSkill = 9},
                      _fendurance = Endurance {getEndurance = 9},
                      _fightMod = [CombatBonus (CombatSkill {getCombatSkill = 4})]
                    }
                )
                (Goto 325)
            )
        )
    ),
    ( 56,
      Chapter
        "56"
        "You hear the scream of a large winged beast above the trees. It is a Kraan, a deadly servant of the Darklords. Quickly you hide beneath the thick fronds of fern until the horrible shrieks have passed away.\n"
        (NoDecision (Goto 222))
    ),
    ( 57,
      Chapter
        "57"
        "The cabin has only one room. In it you see a wooden table and two benches, a large bed made of straw bales lashed together, several bottles of coloured liquids, and an embroidered rug in the centre of the floor.\n"
        ( Decisions
            [ ("If you choose to take a closer look at the bottles, turn to 164.", NoDecision (Goto 164)),
              ("If you choose to pull back the rug, turn to 109.", NoDecision (Goto 109)),
              ("If you choose to leave the room and investigate the stable, turn to 308.", NoDecision (Goto 308))
            ]
        )
    ),
    ( 58,
      Chapter
        "58"
        "Bracing yourself for the run, you head off down the ridge at a steady pace. To the west, the army of the Darklords looks like a giant pot of black ink that has been spilled between the mountains and is spreading into the land below.\nYou have been running for twenty minutes when you catch sight of a pack of Doomwolves lining a shallow ridge to your right.\n"
        ( Decisions
            [ ("If you decide to flatten yourself against the rocks along the side of the road and wait until they pass, turn to 251.", NoDecision (Goto 251)),
              ("If you decide to carry on running, but draw your weapon just in case they attack, turn to 160.", NoDecision (Goto 160))
            ]
        )
    ),
    ( 59,
      Chapter
        "59"
        "Peering into the darkness, you notice that rough stairs have been cut into the earth and that the mouth of the cave is in fact the entrance to a tunnel.\nCarefully descending the slippery stairway, you notice a small silver box on a shelf at the bottom of the staircase.\n"
        ( Decisions
            [ ("If you want to open the silver box, turn to 124.", NoDecision (Goto 124)),
              ("If you wish to return to the surface and press on, turn to 106.", NoDecision (Goto 106)),
              ("If you wish to investigate the tunnel further, turn to 211.", NoDecision (Goto 211))
            ]
        )
    ),
    ( 60,
      Chapter
        "60"
        "The last thing you remember before darkness engulfs you is the flash of a long curved steel knife. You have become yet another victim of the Sage and his robber son-the very one who has just slit your throat!\nYour quest ends here.\n"
        (NoDecision GameLost)
    ),
    ( 61,
      Chapter
        "61"
        "At last you can reach the wooden fieldworks surrounding the outer city. As you race towards a sentry post, you can hear the excited shouts of the guards cheering you on. Thank the gods that they recognize you, for you must appear a ragged and suspicious figure. Your cloak is torn and hangs in tatters, your face is scratched and blood-smeared, and the dust of the Graveyard covers you from head to toe.\nSplashing through a shallow stream, you stagger towards the gate. The full horror of the Graveyard encounter begins to catch up with you. The last thing you recall before exhaustion robs you of consciousness, is falling into the outstretched arms of two soldiers who have run from the fieldworks to help you.\n"
        (NoDecision (Goto 268))
    ),
    ( 62,
      Chapter
        "62"
        "The \"soldiers\" lie dead at your feet. They were bandits who were stealing from the refugees of Toran, and from the abandoned houses and farms in the area.\nSearching their bodies you find 28 Gold Crowns and two Backpacks containing enough food for 3 Meals. They had been armed with a crossbow and three Swords. The crossbow has been damaged in the fight, but the Swords are untouched and you may keep one if you wish.\nYou adjust your equipment, give a cautious glance towards the west, and continue your run towards the outer defences of the capital.\n"
        (CanTake Backpack 1 (CanTake Meal 2 (CanTake Gold 28 (CanTake (Weapon Sword) 1 (NoDecision (Goto 288))))))
    ),
    ( 63,
      Chapter
        "63"
        "The wild old man is screaming at you. He blames you for the war and curses the Kai Lords as agents of the Darklords. He will not listen to reason and you must fight him.\n"
        ( NoDecision
            ( Fight
                ( FightDetails
                    { _opponent = "Madman",
                      _fcombatSkill = CombatSkill {getCombatSkill = 11},
                      _fendurance = Endurance {getEndurance = 10},
                      _fightMod = []
                    }
                )
                (Goto 269)
            )
        )
    ),
    ( 64,
      Chapter
        "64"
        "You are awoken by the cries of a Kraan circling above the caravan. It is early morning and the sky is clear and bright. You can see a pack of Doomwolves less than a quarter of a mile away along the highway ahead. They are preparing to attack. You must act quickly.\n"
        ( Decisions
            [ ("If you decide to gather your equipment and run for the cover of the trees, turn to 188.", NoDecision (Goto 188)),
              ("If you decide to cut free one of the horses and try to break through the attacking Doomwolves to the clear road beyond, then turn to 16.", NoDecision (Goto 16))
            ]
        )
    ),
    ( 65,
      Chapter
        "65"
        "Your senses scream at you that this place is very evil. Leave as quickly as you can.\n"
        (NoDecision (Goto 104))
    ),
    ( 66,
      Chapter
        "66"
        "Startled, you turn around to see a burly sergeant and two soldiers running towards you, their swords drawn as if to strike.\nYou prepare to defend yourself for it looks as if they are about to attack first and ask questions later; but suddenly the sergeant calls his men to a halt. He has recognized your cloak. They put away their weapons and apologize many times for their mistake. The sergeant orders one of the men to fetch the captain of the Guard as he leads you to the doors of the Great Hall.\nYou are greeted by a tall and handsome warrior who listens intently to your story. When you have finished the account of your perilous journey to the capital, you notice a tear in the brave man's eye as he bids you to follow him. You walk through the splendid halls and corridors of the inner Palace. The richness and grandeur are a wonder to behold. You eventually arrive at a large carved door, guarded by two soldiers wearing silver armour.\nYou are about to meet the King.\n"
        (NoDecision (Goto 350))
    ),
    ( 67,
      Chapter
        "67"
        "Your Kai Discipline of Tracking reveals to you fresh paw prints leading off along the south path.\nThey are the prints of a black bear, an animal renowned for its ferocity. You decide the east path would be a much safer route.\n"
        (NoDecision (Goto 252))
    ),
    ( 68,
      Chapter
        "68"
        "After a short walk, you reach a junction where a path crosses your present route heading from west to east.\n"
        ( Decisions
            [ ("If you wish to turn west, go to 130.", NoDecision (Goto 130)),
              ("If you wish to head east, turn to 15.", NoDecision (Goto 15))
            ]
        )
    ),
    ( 69,
      Chapter
        "69"
        "You are very near a friendly village.\n"
        (NoDecision (Goto 272))
    ),
    ( 70,
      Chapter
        "70"
        "You have reached a small bridge. A track follows the stream towards the east. A much narrower path disappears into thick forest towards the south.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Sixth Sense, turn to 8.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 8))),
              ("If you wish to go east, turn to 28.", NoDecision (Goto 28)),
              ("If you wish to go south, turn to 157.", NoDecision (Goto 157))
            ]
        )
    ),
    ( 71,
      Chapter
        "71"
        "You are winded but not hurt. You have fallen fifteen feet or so through the roof of an underground tomb. The walls are sheer and you cannot climb them. An arched tunnel leads out of the tomb towards the east, in front of which lies the sarcophagus of some ancient noble.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Sixth Sense, turn to 65.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 65))),
              ("If you wish to open the sarcophagus to see if it contains any treasure, turn to 242.", NoDecision (Goto 242)),
              ("If you wish to leave via the tunnel, turn to 104.", NoDecision (Goto 104))
            ]
        )
    ),
    ( 72,
      Chapter
        "72"
        "You turn to face a sneering Giak and the razor-fanged jaws of its mount. You must fight them as one enemy.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Giak + Doomwolf", _fcombatSkill = CombatSkill {getCombatSkill = 15}, _fendurance = Endurance {getEndurance = 24}, _fightMod = []}) (Goto 265)))
    ),
    ( 73,
      Chapter
        "73"
        "Pulling your green cloak about you, you blend into the foliage and rocks. Peering carefully up at the track, you are shocked to see that they are not the King's men at all.\nThey are Drakkarim, some of the Darklords' cruellest troops. They must have disguised themselves as soldiers of the King in order to get this far into the forest. Thanking your Kai training for saving your life, you silently slip away from the stream and push on into the forest.\n"
        (NoDecision (Goto 243))
    ),
    ( 74,
      Chapter
        "74"
        "The Kraan and its riders land on the track barely ten feet from where you are hidden.\nThe Giaks leap from the scaly backs of the Kraan and move towards you, their spears raised to strike. You have been seen.\n"
        ( Decisions
            [ ("If you decide to fight them, turn to 138.", NoDecision (Goto 138)),
              ("If you decide to run deeper into the forest without delay, turn to 281.", NoDecision (Goto 281))
            ]
        )
    ),
    ( 75,
      Chapter
        "75"
        "Peering out carefully, you can see three green-clad men on horses racing along the bank. You recognize them as Border Rangers, the regiment of the King's Army that police the western borders. One of them is wounded and is slumped over the neck of his horse.\nClose behind follow a pack of twenty Doomwolves. Their Giak riders are firing arrows at the rangers which fall all around them. One ranger drops from his horse and rolls down the river bank, a black arrow deeply embedded in his right leg.\n"
        ( Decisions
            [ ("If you wish to help the ranger, turn to 260.", NoDecision (Goto 260)),
              ("If you wish to stay hidden and drift downstream, turn to 163.", NoDecision (Goto 163))
            ]
        )
    ),
    ( 76,
      Chapter
        "76"
        "The Gem feels very hot and burns your hand. Lose 2 ENDURANCE points. You quickly grab it with the edge of your cloak and slip this Vordak Gem into your Backpack. A Gem that size must be worth hundreds of Crowns! You smile at your good fortune, mount your horse, and ride off along the south track.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 2}), GainItem (GenSpecial (GenCounter 0)) 1] (Goto 118)))
    ),
    ( 77,
      Chapter
        "77"
        "The Mountain Giaks are unaccustomed to pursuing their prey through forests and you soon outdistance them, until finally the sound of their grunts and curses disappears completely.\nWhen you are satisfied that they have given up the chase, you stop for a few minutes to catch your breath and check your equipment. With the memory of your ruined monastery still blazing in your mind, you gather up your meagre belongings and push on.\n"
        (NoDecision (Goto 19))
    ),
    ( 78,
      Chapter
        "78"
        "As the caravan careers past, you leap for the tailboard and manage to hold fast. Pulling yourself upright, you find that you are standing on the bottom rung of a ladder leading to the rear door of the wagon. Suddenly the top half of the door flies open and you are confronted by the angry face of a bodyguard.\n"
        ( Decisions
            [ ("If you decide to inform him that you are a Kai Lord with an urgent message for the King, turn to 132.", NoDecision (Goto 132)),
              ("If you decide to offer him Gold Crowns for safe passage to the capital, turn to 12.", NoDecision (Goto 12)),
              ("If you decide to attack the guard with your weapon, turn to 220.", NoDecision (Goto 220))
            ]
        )
    ),
    ( 79,
      Chapter
        "79"
        "You come to a small footbridge across a fast-flowing stream. On the other side of the bridge the path turns south. You cross the bridge and follow the path.\n"
        (NoDecision (Goto 204))
    ),
    ( 80,
      Chapter
        "80"
        "You stumble backwards through the front door, clutching your burnt chest with both hands. Smoke is billowing from the shop and you must run-before the Sage or his robber son catch you.\nYou make it back to the main street and lose yourself in the rush of the crowds.\n"
        (NoDecision (Goto 7))
    ),
    ( 81,
      Chapter
        "81"
        "After nearly an hour, the Kraan and their cruel riders vanish towards the west. As the shocked refugees start to emerge from the woods, you can hear the sound of horses in the distance galloping nearer. You stay hidden and wait as the riders come closer. They are the cavalry of the King's Guard wearing the white uniforms of His Majesty's army.\n"
        ( Decisions
            [ ("If you wish to call to them, turn to 183.", NoDecision (Goto 183)),
              ("If you would rather continue along the forest edge towards the south, turn to 200.", NoDecision (Goto 200))
            ]
        )
    ),
    ( 82,
      Chapter
        "82"
        "The giant Gourgaz lies dead at your feet. His evil followers hiss at you and then fall back from the bridge. The Prince's soldiers form a protective wall around you and their dying leader with their shields. Black arrows whistle past your head.\nThe dying Prince looks up into your eyes and says, \"Kai Lord, you must take a message to my father. The enemy is too strong, we cannot hold him. The King must seek that which is in Durenor or all is lost. Take my horse and ride for the capital. May the luck of the gods ride with you.\"\nYou bid a sad farewell to the Prince, mount his white steed, and head south along the forest path. The battle still rages behind you as the Prince's men fight off another assault on the bridge.\n"
        (NoDecision (Goto 235))
    ),
    ( 83,
      Chapter
        "83"
        "You have run about a mile when three soldiers appear from beneath a small footbridge. They demand that you halt and drop your weapons and equipment.\nThey are bloodstained and unshaven. Their leader is wearing the tunic of a soldier of the Toran garrison.\n"
        ( Decisions
            [ ("If you possess the Kai Discipline of Sixth Sense, turn to 45.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 45))),
              ("otherwise", Conditional (Not (HasDiscipline SixthSense)) (Decisions [("If you wish to do as they say, turn to 205.", NoDecision (Goto 205)), ("If you wish to prepare to fight them, turn to 180.", NoDecision (Goto 180)), ("If you demand to know what they want, turn to 232.", NoDecision (Goto 232))]))
            ]
        )
    ),
    ( 84,
      Chapter
        "84"
        "Just as you feel the air beating on your back, you slip free of your horse and roll over-landing with a splash in a muddy ditch by the side of the highway.\nYou are uninjured, and you quickly scramble to your feet and make a dash for the cover of the trees-but with thirty yards left to run, you see the Kraan circling above for another dive.\n"
        (NoDecision (Goto 188))
    ),
    ( 85,
      Chapter
        "85"
        "The path is wide and leads straight into thick undergrowth. The trees are tall here and unusually quiet. You walk for over a mile when suddenly you hear the beating of large wings directly above you. Looking up, you are shocked to see the sinister black outline of a Kraan diving to attack you.\n"
        ( Decisions
            [ ("If you draw your weapon and prepare to fight, turn to 229.", NoDecision (Goto 229)),
              ("If you evade the attack by running south, deeper into the forest, turn to 99.", NoDecision (Goto 99))
            ]
        )
    ),
    ( 86,
      Chapter
        "86"
        "You soon reach another crossroads.\n"
        ( Decisions
            [ ("If you wish to journey east, turn to 6.", NoDecision (Goto 6)),
              ("If you wish to head north, turn to 35.", NoDecision (Goto 35)),
              ("If you prefer to go south, turn to 167.", NoDecision (Goto 167)),
              ("Or if you wish to turn west, turn to 42.", NoDecision (Goto 42))
            ]
        )
    ),
    ( 87,
      Chapter
        "87"
        "Focusing your powers on the lock, you try to visualize the inner mechanism. Gradually its image appears in your mind's eye. It is old and corroded but it still functions. You are in danger of losing your concentration when a subtle *click* confirms that your effort has not been in vain.\nThe pin is an easier task. Slowly it rises out of the lock and falls to the floor. The granite door swings towards you on hidden hinges and the grey half-light of the Graveyard floods into the tomb. The exit is overgrown with graveweed and you suffer many small cuts to your face and hands as you fight your way through to the surface. You are startled by a sudden noise. You turn to see the disembodied head of a corpse laughing at you.\nIn blind panic, you race through the eerie necropolis towards the southern gate.\n"
        (NoDecision (Goto 61))
    ),
    ( 88,
      Chapter
        "88"
        "You cautiously peer around the rock to see a soldier lying on his back. By his side is a Spear and shield. On the shield is the painting of a white pegasus-the Prince of Sommerlund's emblem. He is one of the Prince's soldiers, and he is only just conscious. His uniform is badly torn, and you can see that he has a deep wound in his left arm. As you move nearer, his eyes flicker open. \"Heal me, my lord,\" he begs. \"I can barely feel my arm.\"\n"
        ( Decisions
            [ ("If you possess and wish to use the Kai Discipline of Healing on this man, turn to 216.", Conditional (HasDiscipline Healing) (NoDecision (Goto 216))),
              ("If you do not possess the skill, or if you do not want to use it, then turn to 31.", NoDecision (Goto 31))
            ]
        )
    ),
    ( 89,
      Chapter
        "89"
        "In a cloud of dust and loose rocks you career down the steep hillside. The Kraan is still circling above as if waiting to direct the Giaks after you.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 5, Goto 53), (3 % 10, Goto 274), (1 % 2, Goto 316)]))
    ),
    ( 90,
      Chapter
        "90"
        "Night falls and you are soon engulfed in total darkness. To press on would be useless, for you would be sure to lose your way. Tethering your horse to a tree, you pull your green Kai cloak about you and fall into a restless sleep.\n"
        (NoDecision (Goto 18))
    ),
    ( 91,
      Chapter
        "91"
        "The small shop is dark and musty. Books and bottles of every size and colour fill the many shelves. As you close the door, a small black dog begins to yap at you. A bald man appears from behind a large screen and bids you welcome. He politely enquires as to the nature of your visit and offers you a choice of his wares from the glass counter.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Sixth Sense, turn to 198.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 198))),
              ("otherwise", Conditional (Not (HasDiscipline SixthSense)) (Decisions [("If you wish to look at his wares, turn to 152.", NoDecision (Goto 152)), ("If you would rather decline his offer and return to the street, turn to 7.", NoDecision (Goto 7))]))
            ]
        )
    ),
    ( 92,
      Chapter
        "92"
        "You dive for cover not a moment too soon, for a hail of black arrows scream out of the woods and bombard the area where you were standing seconds before. Pulling your cloak around you to blend into the dense bracken, you dash through the forest and away from the hidden ambushers as fast as possible. This entire area is infested by Giaks and you must escape as quickly as you can. You run without rest for over an hour until you happen to fall upon a straight forest path heading towards the east. You follow the path, taking care to keep watch for signs of the enemy.\n"
        (NoDecision (Goto 13))
    ),
    ( 93,
      Chapter
        "93"
        "You turn and run for the stairs just as a large block falls with a crash behind you. The room you were in has been completely sealed off. As you escape into the daylight, you glimpse behind you the crooked figure of an old druid as he raises his staff. A second later, a bolt of lightning explodes at your feet. You do not stop but run headlong down the hill, cursing the delay but thankful for your Sixth Sense.\n"
        (NoDecision (Goto 106))
    ),
    ( 94,
      Chapter
        "94"
        "The Sage, seeing that you have killed his son, turns and runs from the shop by a back door.\nYou find 12 Gold Crowns in the robber's purse and another 4 Gold Crowns in a wooden box under the counter. Carefully examining the potions and the wand you soon realize that they are all cheap counterfeits. In fact the entire shop is full of imitations. You shake your head and return to the main street.\n"
        (CanTake Gold 16 (NoDecision (Goto 7)))
    ),
    ( 95,
      Chapter
        "95"
        "You soon stumble upon a narrow forest track running from north to south.\n"
        ( Decisions
            [ ("If you wish to set off along the track towards the north, turn to 240.", NoDecision (Goto 240)),
              ("If you wish to go south instead, turn to 5.", NoDecision (Goto 5))
            ]
        )
    ),
    ( 96,
      Chapter
        "96"
        "Holding your breath, you tighten your grip on your weapon and prepare to strike. The tension is unbearable-the Giaks are so close that the foul stench of their unwashed bodies fills your nostrils. You hear them curse in their strange alien tongue and then leave the ledge and start to scramble towards the peak. When you are sure they have gone, you finally exhale and wipe the sweat from your eyes.\n"
        ( Decisions
            [ ("If you wish to explore the cave further, turn to 33.", NoDecision (Goto 33)),
              ("If you wish to leave the cave and descend the hill in case the Giaks return, turn to 248.", NoDecision (Goto 248))
            ]
        )
    ),
    ( 97,
      Chapter
        "97"
        "Ahead of you, you can see a fierce battle raging across a stone bridge. The clash of steel and the cries of men and beasts echo through the forest. In the midst of the fighting, you see Prince Pelathar, the King's son. He is in combat with a large grey Gourgaz who is wielding a black axe above his scaly head. Suddenly, the Prince falls wounded-a black arrow in his side.\n"
        ( Decisions
            [ ("If you wish to defend the fallen Prince, turn to 255.", NoDecision (Goto 255)),
              ("If you wish to run into the forest, turn to 306.", NoDecision (Goto 306))
            ]
        )
    ),
    ( 98,
      Chapter
        "98"
        "The guards seem to believe your story and bow with respect to your rank of Kai Lord. One of them pulls a concealed bell-rope and the huge doors start to swing open. They usher you inside and you hear the doors close behind you.\n"
        (NoDecision (Goto 139))
    ),
    ( 99,
      Chapter
        "99"
        "You dive into the undergrowth just as the beast screams past your head. You quickly look back to see the Kraan turning in the air in preparation for another dive. You scramble to your feet and run deeper into the safety of the forest.\n"
        (NoDecision (Goto 222))
    ),
    ( 100,
      Chapter
        "100"
        "The cold corridor suddenly makes an abrupt turning towards the east. You notice a greenish glow that lights the tunnel in the far distance. As you creep nearer you can see that the corridor opens out into a larger chamber.\nThe strange light seems to emanate from a large bowl resting upon the top of a granite throne. On a plinth in front of the throne stands a statue. It looks like a winged serpent curved in the shape of an \"S\".\n"
        ( Decisions
            [ ("If you wish to sit on the throne, turn to 161.", NoDecision (Goto 161)),
              ("If you wish to examine the statue, turn to 133.", NoDecision (Goto 133)),
              ("If you wish to look for an exit from this chamber, turn to 257.", NoDecision (Goto 257))
            ]
        )
    ),
    ( 101,
      Chapter
        "101"
        "The noise of battle soon fades behind you but the ensuing silence is broken by a voice in your head that accuses you of being a coward, and deserting a fellow human in danger. You try to rid yourself of your nagging conscience by telling yourself that your mission is far more important, and that not only is the life of the young magician in peril but the lives of all your countrymen depend on you reaching the capital alive.\nSuddenly, the sight of a Giak war party in the distance makes you quickly take cover and hide. But it is too late-they have spotted you and you must run as fast as you can.\n"
        (NoDecision (Goto 281))
    ),
    ( 102,
      Chapter
        "102"
        "As you descend the rocky slope towards the Graveyard of the Ancients, you are aware of a strange mist and cloud that swirls all around this grey and forbidding place, blocking the sun and covering the Graveyard in a perpetual gloom. A chill creeps forward to greet your approach.\nWith a feeling of deep dread, you enter the eerie necropolis.\n"
        (NoDecision (Goto 284))
    ),
    ( 103,
      Chapter
        "103"
        "The overgrown path leads to a junction where another track branches off towards the east.\n"
        ( Decisions
            [ ("If you wish to take this path, turn to 13.", NoDecision (Goto 13)),
              ("If you would rather continue towards the northeast, turn to 287.", NoDecision (Goto 287))
            ]
        )
    ),
    ( 104,
      Chapter
        "104"
        "The walls are dank and slimy. The stale air chokes you and cobwebs brush across your face. You can feel panic grip your stomach, as the tunnel gets darker and darker.\nYou reach a junction where the tunnel meets a corridor leading from north to south.\n"
        ( Decisions
            [ ("If you wish to turn north, go to 26.", NoDecision (Goto 26)),
              ("If you wish to go south, turn to 100.", NoDecision (Goto 100))
            ]
        )
    ),
    ( 105,
      Chapter
        "105"
        "In the distance, perched on the branch of an old oak tree is a jet-black raven.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Animal Kinship, you may call to this bird by turning to 298.", Conditional (HasDiscipline AnimalKinship) (NoDecision (Goto 298))),
              ("otherwise", Conditional (Not (HasDiscipline AnimalKinship)) (Decisions [("If you do not possess this skill, or if you do not wish to use it, turn to 335.", NoDecision (Goto 335))]))
            ]
        )
    ),
    ( 106,
      Chapter
        "106"
        "Eventually you come to the edge of a fast-flowing icy stream. The white water cascades over the mossy rocks and disappears towards the east.\n"
        ( Decisions
            [ ("If you wish to follow the stream to the east, turn to 263.", NoDecision (Goto 263)),
              ("If you would rather explore upstream, turn to 334.", NoDecision (Goto 334))
            ]
        )
    ),
    ( 107,
      Chapter
        "107"
        "Running across the room, you lash out at the skulls, smashing them to fragments. You notice that inside each skull is a bubbling grey jelly that seems to writhe and change its shape, sprouting bat-like wings and suckers from its glistening form. In horror and loathing, you race for the exit corridor and escape just as a heavy portcullis falls with a crash, completely sealing off the chamber.\n"
        (NoDecision (Goto 23))
    ),
    ( 108,
      Chapter
        "108"
        "You fly in an arc through the air towards the opposite roof. Everything seems to be happening in slow motion. You see the teeming crowds below in the street, and a nest of callysparrows in the eaves of a roof to your right. You hear their startled cries as you land with a crash on the other side. But it is the last sound that you will ever hear. The tiles splinter and collapse and you fall through the four floors of the \"Green Slipper Inn\" breaking your back in several places.\nYour mission and your life end here.\n"
        (NoDecision GameLost)
    ),
    ( 109,
      Chapter
        "109"
        "The only thing under the carpet is dirt!\n"
        ( Decisions
            [ ("You may take a closer look at the bottles by turning to 164.", NoDecision (Goto 164)),
              ("Or you can leave the room and investigate the stable by turning to 308.", NoDecision (Goto 308))
            ]
        )
    ),
    ( 110,
      Chapter
        "110"
        "You quickly take aim and hurl the rock at the Giak's head as hard as you can, but to your horror the creature ducks and the rock arcs harmlessly over its back. You must act immediately to save the wizard.\n"
        (NoDecision (Goto 55))
    ),
    ( 111,
      Chapter
        "111"
        "Only a few minutes after leaving the junction, you see in the distance a small log cabin and stable. On arrival you check the interior through a side window. The cabin looks deserted.\n"
        ( Decisions
            [ ("If you wish to enter the cabin, turn to 57.", NoDecision (Goto 57)),
              ("If you wish to search the stable, turn to 308.", NoDecision (Goto 308))
            ]
        )
    ),
    ( 112,
      Chapter
        "112"
        "Suddenly, the large rock you are hiding behind is rolled aside and you are faced by two snarling Giaks intent on your death. The cave mouth is a narrow entrance and you can only fight the Giaks one at a time.\n"
        ( Decisions
            [ ( "If you win, you may explore the cave further by turning to 33.",
                NoDecision
                  ( Fight
                      ( FightDetails
                          { _opponent = "Giak 1",
                            _fcombatSkill = CombatSkill {getCombatSkill = 13},
                            _fendurance = Endurance {getEndurance = 10},
                            _fightMod = []
                          }
                      )
                      ( Fight
                          ( FightDetails
                              { _opponent = "Giak 2",
                                _fcombatSkill = CombatSkill {getCombatSkill = 12},
                                _fendurance = Endurance {getEndurance = 10},
                                _fightMod = []
                              }
                          )
                          (Goto 33)
                      )
                  )
              ),
              ( "Or you may leave and descend the hill. Turn to 248.",
                NoDecision
                  ( Fight
                      ( FightDetails
                          { _opponent = "Giak 1",
                            _fcombatSkill = CombatSkill {getCombatSkill = 13},
                            _fendurance = Endurance {getEndurance = 10},
                            _fightMod = []
                          }
                      )
                      ( Fight
                          ( FightDetails
                              { _opponent = "Giak 2",
                                _fcombatSkill = CombatSkill {getCombatSkill = 12},
                                _fendurance = Endurance {getEndurance = 10},
                                _fightMod = []
                              }
                          )
                          (Goto 248)
                      )
                  )
              )
            ]
        )
    ),
    ( 113,
      Chapter
        "113"
        "You have been walking for over half an hour when your eye is caught by some bright red flowers growing near to a mossy bank. You recognize the plants to be Laumspur, a rare and beautiful herb much prized for its healing properties.\nKneeling down, you pick a handful of Laumspur and place it in your Backpack. You may eat this herb to regain lost ENDURANCE points. Each Meal of Laumspur will restore 3 ENDURANCE points, and you have gathered enough for two such Meals. Closing your pack, you continue your mission.\n"
        ( CanTake
            Laumspur
            2
            ( Decisions
                [ ("If you wish to head northeast, turn to 347.", NoDecision (Goto 347)),
                  ("If you wish to head east, turn to 295.", NoDecision (Goto 295))
                ]
            )
        )
    ),
    ( 114,
      Chapter
        "114"
        "You coax the horse to lie down and begin to cover him and yourself with branches and dead leaves. You hear the wings of the Kraan as it passes over the trees. It returns and circles above you, but soon retreats back across the lake.\nYou decide to leave now, in case it returns with some of its friends.\n"
        (NoDecision (Goto 239))
    ),
    ( 115,
      Chapter
        "115"
        "You stumble into the first building and fall to the floor exhausted. You can smell cooked meat. You notice a small cauldron hanging over the embers of a dying fire, and a large oak table that has been set for a meal. Whoever lived here must have left in a great hurry this very morning. There is water in a jug and a loaf of fresh bread on the table.\n"
        ( Decisions
            [ ("If you decide to take a quick Meal, turn to 150.", NoDecision (Goto 150)),
              ("If you decide to search the building, turn to 177.", NoDecision (Goto 177)),
              ("If you would rather leave now and continue your run, turn to 83.", NoDecision (Goto 83))
            ]
        )
    ),
    ( 116,
      Chapter
        "116"
        "As you climb out of the muddy water, black arrows fall all around you. Quickly, you dash for the cover of the trees and wait for the Giaks to leave the opposite bank, before continuing on foot towards the capital.\n"
        (NoDecision (Goto 321))
    ),
    ( 117,
      Chapter
        "117"
        "The man is badly injured and near to death. If you have the Kai Discipline of Healing, you may ease the pain of his wounds but he has been so seriously hurt he is beyond repair by your skills alone. He soon lapses into unconsciousness. You try to make him as comfortable as possible beneath a large forest oak, before leaving and pressing on through the thick woodland towards the northeast.\n"
        (NoDecision (Goto 330))
    ),
    ( 118,
      Chapter
        "118"
        "You spur your horse to a gallop and race down the long straight path. In the far distance you can just make out the silhouette of Holmgard on the horizon, its high walls and tall spires glinting in the morning sun. Your path joins a highway running from north to south. It is the main turnpike road between the northern port of Toran and the capital. You set off towards Holmgard, your eyes peeled for Kraan in the clear morning sky.\n"
        (NoDecision (Goto 224))
    ),
    ( 119,
      Chapter
        "119"
        "The gallowbrush tears your cloak and scratches deep into your arms and legs as you slowly force your way through. Fifteen minutes later you emerge from the briars and stagger onwards between the trees.\nDeduct 2 ENDURANCE points from your current score for the wounds you have sustained.\nYou feel a little dizzy as you push on, and your eyelids seem very heavy. You suddenly find yourself at the edge of a steep wooded slope.\n"
        ( Decisions
            [ ("If you wish to slide down the slope as carefully as you can, turn to 226.", NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 226))),
              ("If you do not feel that you are up to the risk of this tricky descent in your present sleepy state, walk around the edge of the ridge by turning to 38.", NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 38)))
            ]
        )
    ),
    ( 120,
      Chapter
        "120"
        "Behind you can hear the blood-crazy Giaks killing the caravan horses. You risk a quick glance over your shoulder and see a Kraan start to climb high into the air. Will it attack you or is it interested in something else? The dark shadow that is gradually getting larger all around you tells you that you are its intended victim. The Kraan is diving full speed at you!\n"
        ( Decisions
            [ ("If you wait until it is about to strike and then jump from the saddle, turn to 84.", NoDecision (Goto 84)),
              ("If you head as fast as you can for the trees, turn to 171.", NoDecision (Goto 171)),
              ("If you put your head down, pray to the heavens for good luck and gallop on regardless, turn to 54.", NoDecision (Goto 54))
            ]
        )
    ),
    ( 121,
      Chapter
        "121"
        "After a few minutes walking you see a stranger, clad in red, standing in the centre of the track ahead. He has his back towards you, and his head is covered by the hood of his robes. Perched on his outstretched arm is the black raven that you saw earlier.\n"
        ( Decisions
            [ ("If you wish to call the stranger, turn to 342.", NoDecision (Goto 342)),
              ("If you wish to approach the stranger cautiously, turn to 309.", NoDecision (Goto 309)),
              ("If you would rather draw your weapon and attack, turn to 283.", NoDecision (Goto 283))
            ]
        )
    ),
    ( 122,
      Chapter
        "122"
        "Immediately the horse senses your communication. He calms down. You walk towards the beautiful animal and stroke his head reassuringly. You sense that he is frightened and confused. Mounting him, you lead him off to the path and head south once again.\n"
        (NoDecision (Goto 206))
    ),
    ( 123,
      Chapter
        "123"
        "As the creature dies, its body slowly dissolves into a vile green liquid. You notice that all of the grass and the plants beneath the smoking fluid are beginning to shrivel and die. A large valuable looking Gem lies on the ground near to the decaying body.\nFurther along the track you can see a large war party of Giaks running towards you.\n"
        ( Decisions
            [ ("If you wish to take the Gem, turn to 304.", NoDecision (Goto 304)),
              ("If you would rather leave it and run, turn to 2.", NoDecision (Goto 2))
            ]
        )
    ),
    ( 124,
      Chapter
        "124"
        "Inside the box you find 15 Gold Crowns and a Silver Key. If you wish to keep the key, remember to mark it on your Action Chart.\n"
        ( CanTake
            Gold
            15
            ( CanTake
                silverKey
                1
                ( Decisions
                    [ ("You can continue to investigate the tunnel by turning to 211.", NoDecision (Goto 211)),
                      ("Or you may leave and descend the hill by turning to 106.", NoDecision (Goto 106))
                    ]
                )
            )
        )
    ),
    ( 125,
      Chapter
        "125"
        "The path opens out into a large clearing. You notice strange claw prints in the earth. Kraan have landed here. By the number of prints and by the size of the area disturbed, you judge that at least five of the foul creatures landed here in the last twelve hours.\nYou see two exits on the far side of the clearing. One leads west, the other south.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Tracking, turn to 301.", Conditional (HasDiscipline Tracking) (NoDecision (Goto 301))),
              ("otherwise", Conditional (Not (HasDiscipline Tracking)) (Decisions [("If you wish to take the south path, turn to 27.", NoDecision (Goto 27)), ("If you wish to take the west path, turn to 214.", NoDecision (Goto 214))]))
            ]
        )
    ),
    ( 126,
      Chapter
        "126"
        "You ride deeper and deeper into the forest. Silently you thank the Prince for such a fine horse, for although the ground is a tangle of briars and roots, he never once falters. The Doomwolves are soon left far behind and you bring your horse to a halt. The light has faded fast and it is almost night.\n"
        ( Decisions
            [ ("If you wish to press on ahead, turn to 46.", NoDecision (Goto 46)),
              ("If you wish to bear left (the same direction as the path you left far behind) then turn to 143.", NoDecision (Goto 143))
            ]
        )
    ),
    ( 127,
      Chapter
        "127"
        "After an hour of marching, the Drakkarim suddenly halt as a large, grey scaly creature approaches along the track. As the beast draws closer, you can smell its fetid breath on your face. It lets out a roar and grabs your head in its powerful webbed hands. The last thing you hear is the sharp crack of your spine snapping.\nYour quest ends here.\n"
        (NoDecision GameLost)
    ),
    ( 128,
      Chapter
        "128"
        "Carefully parting the dense foliage, you are horrified by the sight that meets you. In a small clearing ahead, three Giaks have tied a man to a wooden stake and are setting fire to a mass of brushwood bundled at his feet. You recognize his tunic as that of a Border Ranger, one of the King's men who police the kingdom near the Durncrag Mountains of the west. He has been badly beaten and is nearly unconscious.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Hunting, turn to 297.", Conditional (HasDiscipline Hunting) (NoDecision (Goto 297))),
              ("otherwise", Conditional (Not (HasDiscipline Hunting)) (Decisions [("If you do not, you must attack the Giaks now in order to save the ranger's life. Turn to 336.", NoDecision (Goto 336))]))
            ]
        )
    ),
    ( 129,
      Chapter
        "129"
        "You reach the main gates of the capital, and stare in awe at the height of the city's walls. Two hundred feet high, the walls of Holmgard have withstood the ravages of both time and the Darklords. You and the officer race through the tunnel of the inner gatehouse, one hundred yards in length, and finally halt outside the doorway of the main watchtower. Great crowds of soldiers and civilians are running to and fro.\n"
        ( Decisions
            [ ("If you wish to continue following the officer, turn to 3.", NoDecision (Goto 3)),
              ("If you feel that you stand a better chance of making your way to the King's citadel on your own, turn to 144.", NoDecision (Goto 144))
            ]
        )
    ),
    ( 130,
      Chapter
        "130"
        "You soon reach a small clearing in the woods. A bench, carved from a fallen tree is set in the centre of the clearing. You are hungry and must now eat a Meal here.\n"
        ( Decisions
            [ ("When you have finished, if you decide to leave the clearing by the south way, turn to 28.", NoDecision (Simple [MustEat Hunt] (Goto 28))),
              ("Or if you prefer the smaller track that leads eastwards into the forest, turn to 201.", NoDecision (Simple [MustEat Hunt] (Goto 201)))
            ]
        )
    ),
    ( 131,
      Chapter
        "131"
        "You have covered about a quarter of a mile when you hear shouting and a noise like thunderclaps ahead. Edging nearer, you soon make out a clearing that you recognize to be the site of the ruins of Raumas, an ancient forest temple.\nA war party of Giaks, some twenty-five to thirty strong, are attacking the ruins from all sides. Many more of the Giaks are dead or dying among the broken pillars of marble, but still they assault whatever is hidden inside. Suddenly, a bolt of blue lightning rips through the front rank of Giaks sending the armour-clad creatures tumbling in all directions. A Giak, taller than the others and dressed from head to foot in black chainmail, curses at his troops as he whips them forward with a barbed flail.\nWith weapon ready, you move to the edge of the clearing, under cover of the thick foliage, and try to catch a glimpse of the defenders. To your amazement, the ruins are being defended by a young man no older than yourself. You recognize his sky-blue robes, embroidered with stars. He is a young theurgist of the Magicians' Guild of Toran: an apprentice in magic.\nFive Giaks charge forward, their spears raised to stab the apprentice as he hurriedly retreats deeper into the ruins. You see him turn and raise his left hand just before a bolt of blue flame shoots from his fingertips into the snarling Giak soldiers. Close to where you are hidden, you see a Giak scuttle past and climb one of the pillars of the temple. He has a long curved dagger in his mouth and he is about to jump on the young wizard standing below.\n"
        ( Decisions
            [ ("If you wish to shout a warning to the wizard, turn to 241.", NoDecision (Goto 241)),
              ("If you wish to run forward and attack the Giak when he jumps, turn to 55.", NoDecision (Goto 55)),
              ("If you wish to pick up a chunk of temple marble and throw it at the Giak's head, turn to 302.", NoDecision (Goto 302)),
              ("Or if you would rather turn and leave the battle area and run back into the woods, turn to 101.", NoDecision (Goto 101))
            ]
        )
    ),
    ( 132,
      Chapter
        "132"
        "The bodyguard looks at you with great suspicion and slams the door. You can hear voices chattering inside the caravan. Suddenly the door swings open and the face of a wealthy merchant appears. He recognizes your Kai cloak and apologizes for his servant's behaviour.\nHe says that they have been attacked several times since they left Toran: by Kraan, by bandits, and by robbers. They thought you may have been a bandit. Inside, the caravan is full of silks and spices. The merchant offers you food which you gratefully accept. After your sumptuous meal, the fatigue of your ordeal finally overcomes you and you slip into a deep sleep.\n"
        (NoDecision (Goto 64))
    ),
    ( 133,
      Chapter
        "133"
        "As you approach the statue, several cracks appear in its stone surface. It suddenly explodes before you as a real Winged Serpent breaks free of its stone mantle and attacks you.\nYou must fight the creature.\n(This creature is immune to Mindblast.)\n"
        (NoDecision (Fight (FightDetails {_opponent = "Winged Serpent", _fcombatSkill = CombatSkill {getCombatSkill = 16}, _fendurance = Endurance {getEndurance = 18}, _fightMod = [MindblastImmune]}) (Goto 266)))
    ),
    ( 134,
      Chapter
        "134"
        "Using your skills, you detect Giak tracks around the perimeter of the clearing. The prints are fresh and you can tell that these cruel minions of the Darklords were in this area less than two hours ago.\n"
        ( Decisions
            [ ("Forewarned by this knowledge, if you decide to investigate the huts, turn to 305.", NoDecision (Goto 305)),
              ("If you would rather avoid the clearing, turn to 40.", NoDecision (Goto 40))
            ]
        )
    ),
    ( 135,
      Chapter
        "135"
        "Peering over the steep undercut of the river bank, you can see a tangle of driftwood along the water's edge. A large tree trunk has grounded on the clay bank next to a small canoe.\n"
        ( Decisions
            [ ("If you wish to use the log to float down the river, turn to 223.", NoDecision (Goto 223)),
              ("If you wish to use the canoe, turn to 4.", NoDecision (Goto 4))
            ]
        )
    ),
    ( 136,
      Chapter
        "136"
        "The Giaks get nearer and then crouch down as if preparing themselves to pounce. You can see the sharp serrated points of their spears and hear their low guttural speech. The larger of the two creatures screams, \"Orgadak taag! Nogjat aga ok!\" and attacks you.\nYou must fight each of the Giaks in turn. Add 1 point to your COMBAT SKILL during this fight, as you have the advantage of the higher ground in your favour.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Giak 1", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 10}, _fightMod = [CombatBonus (CombatSkill {getCombatSkill = 1})]}) (Fight (FightDetails {_opponent = "Giak 2", _fcombatSkill = CombatSkill {getCombatSkill = 12}, _fendurance = Endurance {getEndurance = 10}, _fightMod = [CombatBonus (CombatSkill {getCombatSkill = 1})]}) (Goto 313))))
    ),
    ( 137,
      Chapter
        "137"
        "As the last of the foul creatures die, so the greenish light starts to fade. You notice that in each of the broken skulls lies a Gem. You take these 20 Tomb Guardian Gems before darkness engulfs the chamber. Remember to mark these on your Action Chart as a single Backpack Item.\nYou quickly leave the dead Crypt spawn and press on.\n"
        (NoDecision (Goto 23))
    ),
    ( 138,
      Chapter
        "138"
        "You prepare your weapon and advance to meet the enemy. There are two Mountain Giaks and you must fight them one at a time.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Giak 1", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 10}, _fightMod = []}) (Fight (FightDetails {_opponent = "Giak 2", _fcombatSkill = CombatSkill {getCombatSkill = 12}, _fendurance = Endurance {getEndurance = 10}, _fightMod = []}) (Goto 291))))
    ),
    ( 139,
      Chapter
        "139"
        "The inner courtyard is a bustle of activity. Cavalry scouts are waiting beside their nervous horses for messages from their unit commanders inside the Great Hall. They take orders with great speed to the defenders of the outer fieldworks. No sooner do they gallop off, than other scouts return, many of them breathless and wounded.\nYou have taken less than a dozen steps across the courtyard when you hear a deep voice boom out. \"Stop that man!\"\n"
        (NoDecision (Goto 66))
    ),
    ( 140,
      Chapter
        "140"
        "You are in a clearing where several trees have been cut down to make a rickety watchtower. Below the tower are three paths leading off in different directions.\n"
        ( Decisions
            [ ("If you take the south path, turn to 14.", NoDecision (Goto 14)),
              ("If you take the east path, turn to 252.", NoDecision (Goto 252)),
              ("If you take the southwest path, turn to 215.", NoDecision (Goto 215)),
              ("If you decide to climb the watchtower, turn to 36.", NoDecision (Goto 36))
            ]
        )
    ),
    ( 141,
      Chapter
        "141"
        "Your Sixth Sense has warned you that some of the creatures that attacked the monastery are searching the two paths for any survivors of their raid, but you can avoid both tracks by making your way through the undergrowth of the woods.\n"
        ( Decisions
            [ ("If you wish to head south, turn to 56.", NoDecision (Goto 56)),
              ("Or if you wish to cut through the heavier foliage towards the northeast, turn to 333.", NoDecision (Goto 333))
            ]
        )
    ),
    ( 142,
      Chapter
        "142"
        "You can see the tall grey-white walls and glimmering spires of Holmgard, its banners fluttering from the battlements in the fresh morning breeze. Stretching out towards the west, the River Eledil traces its course from the mountains of the Durncrag Range to the Holmgulf. But from below the mountain peaks you can see a vast black army marching relentlessly on towards the city.\nTo your right you can see the highway heading off over the rolling plain towards Holmgard. At a run you could reach the outer fieldworks of the city defences in an hour, but you would be in the open for most of the time and vulnerable to attack by Kraan. However, ahead of you, a wide and muddy river drifts sluggishly towards the Eledil. You could use the cover of the river banks and swim towards the capital. Or towards your left lies the Graveyard of the Ancients. These tombs and crumbling monuments to a forgotten age would conceal your approach, but it is a forbidden area. Many are the unnamed horrors that lie there in restless sleep, waiting to consume the unwary trespasser.\n"
        ( Decisions
            [ ("If you will try your luck by the highway, turn to 58.", NoDecision (Goto 58)),
              ("If you feel that you stand a better chance of reaching the capital via the river, then turn to 135.", NoDecision (Goto 135)),
              ("Or if you are brave enough to risk the unknown perils of the Graveyard of the Ancients, turn to 102.", NoDecision (Goto 102))
            ]
        )
    ),
    ( 143,
      Chapter
        "143"
        "You soon emerge from the woods onto a main highway. You recognize it as being the main road between the port of Toran in the north and the capital in the south. Spurring your horse on, you estimate you will reach the capital by morning.\n"
        (NoDecision (Goto 149))
    ),
    ( 144,
      Chapter
        "144"
        "You fight your way through the press of bodies along the main street towards the citadel in the distance. City folk are rushing to and fro in the grip of panic, as the cries of Kraan are heard circling high above.\nIn the crush, one item is stolen from your Backpack. If you no longer have a Backpack or if you have no Backpack Items, you lose a Weapon. Remember to take this off your Action Chart.\nA runaway horse and cart career past and knock you into a doorway. You are stunned and you lose 2 ENDURANCE points. As you stagger to your feet, the door bursts open and a decrepit old man attacks you with a meat cleaver. He is quite insane and you must fight him or take evasive action.\n"
        ( Decisions
            [ ( "with backpack",
                Conditional
                  (HasItem Backpack 1)
                  ( Decisions
                      [ ("If you choose to fight, turn to 63.", NoDecision (LoseItemFrom BackpackSlot 1 (Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 63)))),
                        ("If you wish to evade a fight, turn to 217.", NoDecision (LoseItemFrom BackpackSlot 1 (Simple [LoseItem (Weapon Dagger) 1, DamagePlayer (Endurance {getEndurance = 2})] (Goto 217))))
                      ]
                  )
              ),
              ( "without backpack",
                Conditional
                  (Not (HasItem Backpack 1))
                  ( Decisions
                      [ ("If you choose to fight, turn to 63.", NoDecision (LoseItemFrom WeaponSlot 1 (Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 63)))),
                        ("If you wish to evade a fight, turn to 217.", NoDecision (LoseItemFrom WeaponSlot 1 (Simple [LoseItem (Weapon Dagger) 1, DamagePlayer (Endurance {getEndurance = 2})] (Goto 217))))
                      ]
                  )
              )
            ]
        )
    ),
    ( 145,
      Chapter
        "145"
        "You feel as if you have been run down by a cart or wagon. As you fall forward the last thing that you remember before the darkness overcomes you, is the taste of the sandy road and the terrible pain in your back.\n"
        (NoDecision (Goto 165))
    ),
    ( 146,
      Chapter
        "146"
        "You have ridden about a mile when you are knocked from your horse by an arrow grazing your forehead. You lose 3 ENDURANCE points.\nAs you pull yourself to your feet, you see a patrol of Drakkarim emerge from the woods on either side of the road. You have been ambushed and must evade them as quickly as possible by going into the forest.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 3})] (Goto 154)))
    ),
    ( 147,
      Chapter
        "147"
        "After a few minutes walking, you find a mossy hut set back from the path. You are hungry and must eat a Meal here or lose 3 ENDURANCE points. As you eat you notice that the path starts to curve towards the east. \n"
        ( Decisions
            [ ("If you wish to follow it, turn to 42.", NoDecision (Simple [MustEat Hunt] (Goto 42))),
              ("If you wish to return the way you have come, turn to 28.", NoDecision (Simple [MustEat Hunt] (Goto 28)))
            ]
        )
    ),
    ( 148,
      Chapter
        "148"
        "Kicking open the door, you dive into the farmhouse. A Kraan soars overhead, letting out a shriek of victory, a victim hanging in its claws. Getting to your feet, you find yourself alone. But propped against the fireplace is a Warhammer. You may take this Weapon if you wish.\n"
        ( CanTake
            (Weapon Warhammer)
            1
            ( Decisions
                [ ("If you want to stay in the farmhouse, turn to 81.", NoDecision (Goto 81)),
                  ("If you would feel safer in the forest, you can make a dash by turning to 320.", NoDecision (Goto 320)),
                  ("If you wish to search the room further, turn to 199.", NoDecision (Goto 199))
                ]
            )
        )
    ),
    ( 149,
      Chapter
        "149"
        "As you ride along the highway, you notice that light is getting worse. It will soon be completely dark-and impossible to see any dangers that may lurk ahead. You decide to hide and rest at the wood's edge until morning.\nWhen you are satisfied that no one can see you, you pull your warm green cloak about you and drift off into an uneasy sleep.\n"
        (NoDecision (Goto 256))
    ),
    ( 150,
      Chapter
        "150"
        "Although a little overcooked, the food tastes fine (although it is not enough for a whole Meal) and the clear water slakes your thirst. You have spent nearly half an hour resting in this house when you suddenly realize the delay.\n"
        (NoDecision (Goto 83))
    ),
    ( 151,
      Chapter
        "151"
        "If you concentrate on the keyhole, you could move the mechanism of the lock and open it. You can then make the pin levitate and free it from the lockplate, avoiding falling prey to any traps that may be set off as the door unlocks.\n"
        ( Decisions
            [ ("If you wish to use your Kai Discipline of Mind Over Matter to open this lock and levitate the pin, turn to 87.", Conditional (HasDiscipline MindOverMatter) (NoDecision (Goto 87))),
              ("If you wish to remove the pin, turn to 337.", NoDecision (Goto 337))
            ]
        )
    ),
    ( 152,
      Chapter
        "152"
        "The herbalist offers you a selection of special potions. Some increase your strength; some induce invisibility; some give you great powers of stealth; and others give you the power of turning yourself into a gaseous form. The man pulls open the bottom drawer of the counter to reveal a magnificent wand. He says that it is a powerful weapon against all evil creatures, and that it will make you invulnerable in battle. He points to the mystical inscriptions which cover the black staff.\n"
        ( Decisions
            [ ("If you wish to lean over the counter and read the strange inscriptions, turn to 49.", NoDecision (Goto 49)),
              ("If you are more interested in the potions, turn to 231.", NoDecision (Goto 231))
            ]
        )
    ),
    ( 153,
      Chapter
        "153"
        "Before you are the tall grey-white walls and glimmering spires of Holmgard, the city's banners fluttering from the battlements in the fresh morning breeze. Stretching out towards the west, the River Eledil traces its course from the mountains of the Durncrag Range to the Holmgulf. But below the mountain peaks you can see a vast black army marching relentlessly on towards the capital.\nTo your right you can see the highway heading off over the rolling plain towards Holmgard. At a gallop you could make the outer fieldworks of the city's defences in less than an hour, but you would be in the open for most of the time and vulnerable to attack by Kraan. Directly ahead of you, a wide river drifts sluggishly towards the Eledil. If you abandoned your horse, you could swim towards the outer defences under cover of the river banks. Or there is a final alternative. To your left lies the Graveyard of the Ancients. These tombs and crumbling monuments to a forgotten age would conceal your approach but it is a forbidden area. Many are the unnamed horrors that lie there in restless sleep, waiting to consume the unwary trespasser.\n"
        ( Decisions
            [ ("If you will try your luck by the highway, turn to 202.", NoDecision (Goto 202)),
              ("If you feel that you stand a better chance of reaching the capital via the river then turn to 135.", NoDecision (Goto 135)),
              ("Or if you are brave enough to risk the unknown perils of the Graveyard of the Ancients, turn to 329.", NoDecision (Goto 329))
            ]
        )
    ),
    ( 154,
      Chapter
        "154"
        "You are dizzy from your wound and you stumble through the trees like a blind man.\nSuddenly you fall forward as if the ground has been snatched from beneath your feet. You have fallen head-first into a hunting pit. As you look up, you can see four Drakkarim levelling their bows at you, evil sneers spreading simultaneously across their ugly faces.\nAs the world darkens, the last thing you feel is the black shafts of their arrows deep in your chest. You have failed in your mission.\n"
        (NoDecision GameLost)
    ),
    ( 155,
      Chapter
        "155"
        "As you approach, the group of people stop talking. You can see by their expressions that they recognize your green Kai cloak. Slowly, one of the men extends his hand in friendship and says, \"My Lord, we had heard a rumour that the Kai were destroyed. Heaven be praised that it is not so. We feared all was lost.\"\nYou do not tell them of the destruction of the monastery, for they are refugees from Toran and have lost everything they owned. Their only hope now is that the Kai Lords will lead an army to victory. You learn that the northern port was attacked from both air and sea, and that the forces of the Darklords far outnumbered the King's brave garrison. You reassure them that Sommerlund will not fall and wish them luck on their journey ahead.\n"
        (NoDecision (Goto 70))
    ),
    ( 156,
      Chapter
        "156"
        "Black arrows embed themselves in the mud all around you. More Giaks have appeared on the steep slope of the river bank and are firing at you. There is no cover on this side of the river.\n"
        ( Decisions
            [ ("If you wish to dive into the water and swim with the current, turn to 294.", NoDecision (Goto 294)),
              ("If you wish to swim across to the cover of the trees on the other bank, turn to 245.", NoDecision (Goto 245))
            ]
        )
    ),
    ( 157,
      Chapter
        "157"
        "The forest begins to thin out until finally you can make out a road through the trees ahead. The highway is full of people heading south. Many are wheeling their possessions along on handcarts.\n"
        ( Decisions
            [ ("If you wish to join the refugees and perhaps learn more of what has happened in the north, turn to 30.", NoDecision (Goto 30)),
              ("If you would prefer to continue to move south but under cover of the trees, turn to 167.", NoDecision (Goto 167))
            ]
        )
    ),
    ( 158,
      Chapter
        "158"
        "The Key fits and the lock opens. You pull back the door to find yourself face to face with a strange old man. In his right hand is a staff. Suddenly a bolt of lightning shoots from the staff and hits you square in the chest. You lose 6 ENDURANCE points. Gasping with pain, you knock the old man aside and run up the steep staircase towards daylight. You are halfway up the stairs when he fires another bolt at you.\nPick a number from the Random Number Table.\nIf the number is 0-5, the bolt misses you and shatters part of the wall.\nIf the number is 6-9, then you have been hit in the back and lose a further 4 ENDURANCE points.\nIf you survive, you stagger out into the daylight and curse your bad luck. It was only by an unlucky chance you discovered the secret temple of a sect of evil druids. You are very lucky to have escaped with your life. You quickly rejoin the path which now disappears over the hill.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 6})] (Randomly [(3 % 5, Goto 106), (2 % 5, Simple [DamagePlayer (Endurance {getEndurance = 4})] (Goto 106))])))
    ),
    ( 159,
      Chapter
        "159"
        "Your ploy does not work, for the merchant will not allow you to enter his caravan. Suddenly he clicks his fingers and the bodyguard grasps the hilt of his scimitar.\n"
        ( Decisions
            [ ("You must fight him by turning to 191.", NoDecision (Goto 191)),
              ("Or you must jump clear of the speeding caravan. Turn to 234.", NoDecision (Goto 234))
            ]
        )
    ),
    ( 160,
      Chapter
        "160"
        "Pick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 286), (1 % 2, Goto 10)]))
    ),
    ( 161,
      Chapter
        "161"
        "As you sit down, the stone serpent slowly moves forward on its plinth. You suddenly break out in a cold sweat and grasp your weapon with trembling fingers in case it should attack. A red forked tongue appears from the head of this strange statue and dips into the bowl of green light above your head. Slowly the tongue re-emerges holding a Golden Key which, to your surprise, it drops into your lap. A panel in the east wall clicks open to reveal an exit.\nYou take the Key and leave as quickly as possible.\n"
        (CanTake goldenKey 1 (NoDecision (Goto 209)))
    ),
    ( 162,
      Chapter
        "162"
        "As you get nearer to the men, you call to them. As they turn to face you, your skin turns cold and your heart pounds, for they are Drakkarim in disguise. Suddenly they charge at you. Forced to the ground, you are tied up with ropes and dragged behind them along a track. They take your Backpack and Weapons, but do not search your cloak or find your Gold Crowns. They cackle menacingly to themselves, and talk at great length of the tortures that await you at their camp.\n"
        (NoDecision (Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1] (Conditionally [(HasDiscipline MindOverMatter, Goto 258), (Always True, Goto 127)])))
    ),
    ( 163,
      Chapter
        "163"
        "After nearly half an hour you feel the current getting stronger. Looking out across the surface you can see that you are approaching a whirlpool in the middle of a large river bend. You will surely drown if caught in its current, so you quickly swim towards the right hand river bank and continue your mission on foot, carrying all your equipment.\n"
        (NoDecision (Goto 321))
    ),
    ( 164,
      Chapter
        "164"
        "Carefully opening the seals on each of the bottles, you sniff at the contents. They all seem to be different types of wine. Suddenly a smaller bottle tucked behind the others catches your eye. Pulling out the glass stopper, you recognize the smell to be that of Alether, a Potion of Strength, which is orange in colour.\nYou may keep this Potion and swallow it before you fight. It will increase your COMBAT SKILL by 2 points for the duration of your fight. Be sure to mark it down on your Action Chart and to strike it off once you have used it.\n"
        (CanTake StrengthPotion 1 (NoDecision (Goto 308)))
    ),
    ( 165,
      Chapter
        "165"
        "You awake in a fever. Images swim before your eyes and then fade completely. The pain in your back is intense and you cry out for relief. You feel a cool, damp cloth placed on your forehead and glimpse the worried face of a young woman. An old man whispers in her ear and then he disappears from view. The girl kneels at your side and comforts you with words of kindness and reassurance, but the light quickly fades and darkness engulfs you once more.\n"
        (NoDecision (Goto 212))
    ),
    ( 166,
      Chapter
        "166"
        "You are in the presence of a great evil. Your mind is being probed by a powerful and timeless being and you must shield yourself. The struggle has begun and your sanity is at stake. It is a long and torturous ordeal, during which you experience many fantastic and terrible apparitions that tempt and appal you. After this you must lose 4 ENDURANCE points and stagger towards the tunnel.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 4})] (Goto 104)))
    ),
    ( 167,
      Chapter
        "167"
        "You have been travelling for about a mile when you notice two legs sticking out from behind a large boulder.\n"
        ( Decisions
            [ ("If you possess and wish to use the Kai Discipline of Sixth Sense, turn to 178.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 178))),
              ("If you wish to take a closer look, turn to 88.", NoDecision (Goto 88)),
              ("If you would rather avoid meeting their owner and press on into the forest, turn to 264.", NoDecision (Goto 264))
            ]
        )
    ),
    ( 168,
      Chapter
        "168"
        "You pull yourself to the top of the opulent caravan and nestle among the travelling cases and bags. Night will soon engulf the highway. A chill wind blows from the west and you pull your cloak around yourself to keep warm. You listen to the voices below and you can smell the mouthwatering aroma of spiced meat. It reminds you that you are very hungry and must now take a Meal.\nThe fatigue of your ordeal finally catches up with you and you drift off into a restless sleep.\n"
        (NoDecision (Simple [MustEat Hunt] (Goto 64)))
    ),
    ( 169,
      Chapter
        "169"
        "As you pass each skull, it slowly turns, as if watching your every move. You are halfway across the room when you hear the sharp crack of bone splitting. Suddenly you see hideous shapes hatching inside the skulls, and stretching their wings.\nTen slimy winged creatures attack you, and you must fight them as one enemy.\n"
        (EvadeFight 0 23 (FightDetails {_opponent = "Crypt Spawn", _fcombatSkill = CombatSkill {getCombatSkill = 16}, _fendurance = Endurance {getEndurance = 16}, _fightMod = []}) (Goto 137))
    ),
    ( 170,
      Chapter
        "170"
        "The tunnel is dark and the air is much cooler than outside. You carefully advance with one hand on the tunnel wall to aid your sense of direction. You have been in total darkness for three minutes when you detect the foul smell of decay ahead, similar to rotting meat. If you have a Torch and Tinderbox in your Pack, you may light the Torch to see your way ahead.\nSuddenly, something heavy drops from the tunnel ceiling onto your back and you fall to your knees. It is a Burrowcrawler and you must fight it, for it is trying to strangle you with its long slimy tentacles.\nIf you do not have a torch, deduct 3 points from your COMBAT SKILL during this fight. The Burrowcrawler is immune to Mindblast and Animal Kinship.\n"
        ( NoDecision
            ( Conditionally
                [ ( HasItem torch 1,
                    Fight (FightDetails {_opponent = "Burrowcrawler", _fcombatSkill = CombatSkill {getCombatSkill = 17}, _fendurance = Endurance {getEndurance = 7}, _fightMod = [MindblastImmune]}) (Goto 319)
                  ),
                  ( Always True,
                    Fight (FightDetails {_opponent = "Burrowcrawler", _fcombatSkill = CombatSkill {getCombatSkill = 17}, _fendurance = Endurance {getEndurance = 7}, _fightMod = [CombatBonus (CombatSkill {getCombatSkill = -3}), MindblastImmune]}) (Goto 319)
                  )
                ]
            )
        )
    ),
    ( 171,
      Chapter
        "171"
        "You are at the very edge of the wood when your horse rears up in agony. The Kraan has sunk its claws deep into the horse's hind legs and is trying to knock you to the ground with its wings. The ghoulish Giak rider squeals with delight as he stabs at you with his spear. You jump to the ground and dash for the trees, leaving the poor dying horse in the clutches of the Kraan.\n"
        (NoDecision (Goto 303))
    ),
    ( 172,
      Chapter
        "172"
        "Night falls and you are soon engulfed in darkness. To press on any further would be futile, for you would be sure to lose your way. Tethering your horse to a tree, you pull your green Kai cloak about you and fall into a restless sleep.\nYou are awoken by the sound of troops in the distance. Across the lake you see the black shapes of Drakkarim and a pack of Doomwolves. A Kraan appears from above the trees and lands on the roof of the small wooden shack. It is being ridden by a creature dressed in red robes. The Kraan takes off and begins to fly across the lake to where you are hidden.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Camouflage to hide yourself and your horse, turn to 114.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 114))),
              ("If you wish to ride deeper into the forest to escape the Kraan, turn to 239.", NoDecision (Goto 239)),
              ("If you wish to prepare to fight the creature, turn to 29.", NoDecision (Goto 29))
            ]
        )
    ),
    ( 173,
      Chapter
        "173"
        "As you reach the door you hear the crash of a giant stone slab as it falls from the ceiling. Turning around, you see that your exit is now blocked.\n"
        (NoDecision (Conditionally [(HasItem silverKey 1, Goto 158), (Always True, Goto 259)]))
    ),
    ( 174,
      Chapter
        "174"
        "After nearly an hour of drifting downstream, the water current becomes quite strong and you can see that you are being drawn towards a whirlpool near the centre as the river curves round. You know that if you are caught in the swirling water, you stand very little chance of escaping a watery death. You dive into the muddy river and as you begin to swim towards the shore you unfortunately lose your Backpack and Weapons. Without your equipment, you reach the wooded bank.\n"
        (NoDecision (Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1] (Goto 190)))
    ),
    ( 175,
      Chapter
        "175"
        "Waving your arms at the approaching cavalry, you recognize them to be Border Rangers of the King's army, tough woodsmen who police the troubled western frontier of the kingdom. Your relief at seeing them soon fades when you realize that they are fleeing from a pack of Doomwolves with snarling Giak riders. Black arrows are dropping all around the rangers, as the vicious Doomwolves get nearer and nearer.\n"
        ( Decisions
            [ ("If you possess the Kai Discipline of Camouflage, turn to 182.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 182))),
              ("otherwise", Conditional (Not (HasDiscipline Camouflage)) (Decisions [("If you wish to take cover and hide, turn to 41.", NoDecision (Goto 41)), ("If you wish to make for the other bank, turn to 116.", NoDecision (Goto 116))]))
            ]
        )
    ),
    ( 176,
      Chapter
        "176"
        "You hide behind some thick bushes so that the Doomwolf and its rider will not see your white horse. Luckily it works-the beast lopes past and vanishes down the track that you have just come along.\n"
        ( Decisions
            [ ("If you wish to attack the remaining Doomwolves and their riders, turn to 253.", NoDecision (Goto 253)),
              ("If you wish to press on deeper into the forest, turn to 126.", NoDecision (Goto 126))
            ]
        )
    ),
    ( 177,
      Chapter
        "177"
        "You search all of the cupboards in the small cottage but do not find anything of use or value. You decide that you have wasted enough time here and must press on without further delay.\n"
        (NoDecision (Goto 83))
    ),
    ( 178,
      Chapter
        "178"
        "Your skill enables you to recognize the boots and leggings of a King's soldier. You can sense that the man is wounded and in need of help.\n"
        ( Decisions
            [ ("If you wish to aid him, turn to 88.", NoDecision (Goto 88)),
              ("If you would rather leave him here, turn to 264.", NoDecision (Goto 264))
            ]
        )
    ),
    ( 179,
      Chapter
        "179"
        "You have been spotted by the guards who level their crossbows at you.\n"
        ( Decisions
            [ ("If you wish to raise your hands above your head and walk slowly towards them, turn to 318.", NoDecision (Goto 318)),
              ("If you wish to run for cover in the trees, turn to 51.", NoDecision (Goto 51))
            ]
        )
    ),
    ( 180,
      Chapter
        "180"
        "They see you raise your weapon, and they instantly attack you.\nIf you decide to fight them, you must fight them one at a time.\n"
        (EvadeFight 0 22 (FightDetails {_opponent = "Leader", _fcombatSkill = CombatSkill {getCombatSkill = 15}, _fendurance = Endurance {getEndurance = 22}, _fightMod = []}) (Fight (FightDetails {_opponent = "Soldier 1", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 20}, _fightMod = []}) (Fight (FightDetails {_opponent = "Soldier 2", _fcombatSkill = CombatSkill {getCombatSkill = 12}, _fendurance = Endurance {getEndurance = 20}, _fightMod = []}) (Goto 62))))
    ),
    ( 181,
      Chapter
        "181"
        "Instinctively you duck, and dive to avoid the crossbow bolt. The bandit fires and you feel the sleeve of your jacket tear as the missile grazes past your left arm. You thank the gods for your good fortune and sprint on.\nNone of the other bandits have bows and they soon give up the chase. As you sprint off into the distance, you leave them all far behind.\nYou stop just long enough to strap up your wounded arm and then continue along the road towards the outer defences of the capital.\n"
        (NoDecision (Goto 288))
    ),
    ( 182,
      Chapter
        "182"
        "Three rangers gallop past the river bank, closely followed by the Giaks on their snarling mounts-the Doomwolves. But your Camouflage skill has saved you from being spotted. The pack of evil Giaks continue on their chase without even glancing at the river.\n"
        (NoDecision (Goto 174))
    ),
    ( 183,
      Chapter
        "183"
        "The officer orders his men to halt and asks you your business. You tell him who you are, and how the monastery has been destroyed. He is deeply saddened to hear your news. He offers you a horse and asks you to accompany him to Prince Pelathar, the King's son.\n"
        ( Decisions
            [ ("If you accept, turn to 97.", NoDecision (Goto 97)),
              ("If you decide to decline his offer, turn to 200.", NoDecision (Goto 200))
            ]
        )
    ),
    ( 184,
      Chapter
        "184"
        "The caravan is out of control and is bumping wildly through the rough ground that borders the highway. With difficulty you eventually steer the frightened horses back onto the road and halt the caravan.\nA quick search of the interior reveals 40 Gold Crowns, a Sword, and enough Food for 4 Meals. If you wish to keep any of these items, mark them on your Action Chart.\nThe fatigue of your ordeal finally catches up with you. You must eat a Meal after which you fall into a deep sleep.\n"
        (CanTake Gold 40 (CanTake Meal 4 (CanTake (Weapon Sword) 1 (NoDecision (Simple [MustEat Hunt] (Goto 64))))))
    ),
    ( 185,
      Chapter
        "185"
        "You narrow your eyes and scan the trees for some sign of the hidden archer. Your wait is not a long one, for a moment later a sharp pain tears through your chest and you are thrown back by the force of three arrows. Two of the black shafts have sunk deep into your rib cage, and the third has pierced your thigh.\nThe last thing that you see is the canopy of fern trees above and a large green dragonfly as it settles on your belt buckle.\nYour life and your mission end here.\n"
        (NoDecision GameLost)
    ),
    ( 186,
      Chapter
        "186"
        "The Kakarmi disappear into the dense undergrowth and you soon find yourself lost. After nearly two hours of walking you hear the sound of running water. You decide to investigate a little closer.\n"
        (NoDecision (Goto 106))
    ),
    ( 187,
      Chapter
        "187"
        "Two furry faces appear over the top of the trunk. Both pairs of eyes stare at your weapon and the two creatures let out a shriek of fright. Leaping from the trunk, they disappear into the forest.\n"
        ( Decisions
            [ ("If you wish to follow them, turn to 186.", NoDecision (Goto 186)),
              ("If you wish to let them go and continue, turn to 228.", NoDecision (Goto 228))
            ]
        )
    ),
    ( 188,
      Chapter
        "188"
        "You can see the shadow of the Kraan getting larger all around you. It suddenly strikes, pitching you forward onto your face with the power of its attack.\nPick a number from the Random Number Table.\nIf the number you have picked is 0-6, the Kraan has ripped away your Backpack. You have lost the Pack and all the Equipment that was inside it.\nIf the number picked is 7-9, your Backpack is intact but you have been wounded in both arms. Lose 3 ENDURANCE points and run to the trees.\n"
        (NoDecision (Randomly [(7 % 10, Simple [LoseItemKind [BackpackSlot], LoseItem Backpack 1] (Goto 303)), (3 % 10, Simple [DamagePlayer (Endurance {getEndurance = 3})] (Goto 303))]))
    ),
    ( 189,
      Chapter
        "189"
        "You thank your Kai training and your quick thinking, for that bog could have proved as deadly as any Drakkarim or Kraan.\nYou are worried about losing time, and push on further into the trees towards the south. Ahead of you, you see a wide path that also leads south.\n"
        (NoDecision (Goto 118))
    ),
    ( 190,
      Chapter
        "190"
        "You walk for three miles along the water's edge until you chance upon a wrecked river barge. It appears to have served as shelter for someone, as you can see a bed and some cooking utensils through a hole in the deck.\n"
        ( Decisions
            [ ("If you wish to search the barge, turn to 20.", NoDecision (Goto 20)),
              ("If you wish to press on, turn to 273.", NoDecision (Goto 273))
            ]
        )
    ),
    ( 191,
      Chapter
        "191"
        "The bodyguard unsheathes a large scimitar and strikes at your head.\n"
        (EvadeFight 0 234 (FightDetails {_opponent = "Bodyguard", _fcombatSkill = CombatSkill {getCombatSkill = 11}, _fendurance = Endurance {getEndurance = 21}, _fightMod = []}) (Goto 24))
    ),
    ( 192,
      Chapter
        "192"
        "You see the razor-fanged mouth of a Doomwolf and hear the hideous cries of the Giaks. Two of them are coming straight for you. You are saved from certain death when your horse jumps at the approaching beasts, knocking them both to the ground. You lash out at the Giak and open a large wound in his head...and then suddenly, as if by a miracle, you're through and racing on down the highway, clear of the rest of the pack.\nBut a shadow follows you. It is a Kraan and it has started to dive. Its target is you.\n"
        ( Decisions
            [ ("If you veer off the highway towards the cover of the trees, turn to 171.", NoDecision (Goto 171)),
              ("If you press on regardless of the Kraan and gallop flat out down the highway, turn to 120.", NoDecision (Goto 120))
            ]
        )
    ),
    ( 193,
      Chapter
        "193"
        "The beast and its rider lie dead. You notice a Scroll tucked into the Giak's belt. You may take this if you wish, but remember to mark it on your Action Chart. The other Doomwolves are charging along the path towards you.\n"
        ( Decisions
            [ ("If you wish to fight them, turn to 253.", NoDecision (Goto 253)),
              ("If you wish to escape into the woods, turn to 126.", NoDecision (Goto 126))
            ]
        )
    ),
    ( 194,
      Chapter
        "194"
        "You sprint towards the wagon. People are running everywhere in panic as the Kraan make their attack, carrying their poor victims off into the darkening sky. A large Kraan is hovering above the wagon and three snarling Giaks drop from its back onto the startled horses. You must fight them or leave the wagon and run to the safety of a nearby farmhouse.\n"
        ( Decisions
            [ ("If you wish to fight the Giaks, turn to 208.", NoDecision (Goto 208)),
              ("If you wish to run to the farmhouse, turn to 148.", NoDecision (Goto 148))
            ]
        )
    ),
    ( 195,
      Chapter
        "195"
        "Wiping the bear's blood from your weapon, you notice the mouth of a cave hidden behind the rock from which the bear attacked.\n"
        ( Decisions
            [ ("If you wish to investigate this cave further, turn to 59.", NoDecision (Goto 59)),
              ("If you wish to press on, turn to 106.", NoDecision (Goto 106))
            ]
        )
    ),
    ( 196,
      Chapter
        "196"
        "You follow the man into a small library off the main hall. He pushes one of the many books on the shelves which line all four walls, and you hear a metallic *click*. One section of the bookcase slowly slides back to reveal a hidden passage.\n"
        ( Decisions
            [ ("If you wish to follow the man into the passage, turn to 332.", NoDecision (Goto 332)),
              ("If you do not want to enter the dark corridor, leave the guildhall and return to the street. Turn to 144.", NoDecision (Goto 144))
            ]
        )
    ),
    ( 197,
      Chapter
        "197"
        "The Drakkar lies dead at the bottom of the ferry. He has a Short Sword and 6 Gold Crowns which you may keep if you wish. You push the body into the water where it floats for a few seconds before disappearing into the icy depths.\nGrabbing the pole, you steer to the other side of the lake and abandon the ferry.\n"
        (CanTake Gold 6 (CanTake (Weapon ShortSword) 1 (NoDecision (Goto 172))))
    ),
    ( 198,
      Chapter
        "198"
        "You can sense that there is someone else behind the screen. There is a lingering aura of wickedness around this shop. Be on your guard-something is wrong here.\n"
        ( Decisions
            [ ("If you wish to return to the street, turn to 7.", NoDecision (Goto 7)),
              ("If you wish to examine the goods in the glass counter, turn to 152.", NoDecision (Goto 152))
            ]
        )
    ),
    ( 199,
      Chapter
        "199"
        "Most of the cupboards and drawers are empty. Whoever lived here took nearly everything they owned with them, but you do manage to scrape together enough fruit in the cellar for one Meal. You may mark this on your Action Chart.\n"
        (CanTake Meal 1 (NoDecision (Goto 81)))
    ),
    ( 200,
      Chapter
        "200"
        "Night is starting to close in. The shadows of the forest are growing longer and darker. Just as you are about to stop and rest, you see through the trees a line of people moving south along a wide highway. Moving closer, you notice a large merchant's caravan in the centre of the dusty turnpike. It is drawn by six large horses and is moving much faster than any of the other traffic. This could be your chance to reach the capital as quickly as possible.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Camouflage to hide in among the packing cases strapped to the roof, turn to 168.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 168))),
              ("If you wish to jump onto the caravan, turn to 78.", NoDecision (Goto 78))
            ]
        )
    ),
    ( 201,
      Chapter
        "201"
        "You follow the rough track for nearly an hour when you notice ahead of you another wider path branching off towards the south.\n"
        ( Decisions
            [ ("If you wish to turn south along the new path, turn to 238.", NoDecision (Goto 238)),
              ("But if you wish to head east, turn to 15.", NoDecision (Goto 15)),
              ("Or if you wish to go west, turn to 130.", NoDecision (Goto 130))
            ]
        )
    ),
    ( 202,
      Chapter
        "202"
        "Urging your horse forward, you gallop down the long stretch of highway towards the capital. After only a few minutes your horse suddenly slows and finally limps to a halt. You dismount and examine its raised right foreleg. You curse your ill luck, for you see that it has thrown a shoe and injured its hoof quite badly. You will have to leave him here and proceed on foot as quickly as you can.\n"
        (NoDecision (Goto 58))
    ),
    ( 203,
      Chapter
        "203"
        "You suddenly feel a searing pain shoot through your chest as something explodes against you in a shower of red sparks.\nYou lose 10 ENDURANCE points. Through the smoke, the Sage is preparing to throw more explosives at you.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 10})] (Conditionally [(HasEndurance (Endurance {getEndurance = 10}), Goto 80), (Always True, Goto 344)])))
    ),
    ( 204,
      Chapter
        "204"
        "After an hour of walking you arrive at a junction. The path continues south and another path joins it from the west. You realize that the west path will lead you back to the marsh, so you continue southwards.\n"
        (NoDecision (Goto 111))
    ),
    ( 205,
      Chapter
        "205"
        "Their leader picks up your discarded Equipment and ushers you along the road ahead. (You must now erase all Weapons and Backpack Items from your Action Chart.) An evil grin spreads across the face of the other two men, and you suddenly realize that they are not soldiers after all. You make a break for it and run away from there, sprinting towards the distant capital.\nBehind you, the ominous *click* of a crossbow being primed sends a shiver down your spine.\nPick a number from the Random Number Table.\n"
        (NoDecision (Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1] (Randomly [(1 % 2, Goto 181), (1 % 2, Goto 145)])))
    ),
    ( 206,
      Chapter
        "206"
        "The path soon joins a highway where a signpost indicates Toran to the north and Holmgard to the south. You turn south towards the capital.\n"
        (NoDecision (Goto 224))
    ),
    ( 207,
      Chapter
        "207"
        "The track soon reaches a larger road which crosses the stream via a stone bridge. A signpost at the bridge points north to Toran and south to Holmgard. The road itself is jammed with people moving south, some pushing their possessions along on handcarts. You join the refugee column and head towards the capital.\n"
        (NoDecision (Goto 30))
    ),
    ( 208,
      Chapter
        "208"
        "The ghoulish creatures thrust their spears at you and attack. Fight these creatures as a single enemy.\n"
        ( Decisions
            [ ("If you win, you can run to the safety of the farmhouse by turning to 148.", NoDecision (Fight (FightDetails {_opponent = "Giaks", _fcombatSkill = CombatSkill {getCombatSkill = 15}, _fendurance = Endurance {getEndurance = 13}, _fightMod = []}) (Goto 148))),
              ("Or you can return to the woods by turning to 320.", NoDecision (Fight (FightDetails {_opponent = "Giaks", _fcombatSkill = CombatSkill {getCombatSkill = 15}, _fendurance = Endurance {getEndurance = 13}, _fightMod = []}) (Goto 320)))
            ]
        )
    ),
    ( 209,
      Chapter
        "209"
        "You see ahead a corridor sloping upwards, and as you reach the top of this slope, a stone portal slides across to reveal another passage ahead.\nYou step through the opening which then quickly closes with a crunch.\n"
        (NoDecision (Goto 23))
    ),
    ( 210,
      Chapter
        "210"
        "Just inside the door, you are stopped by a journeyman of the Guildhall and asked to explain your intrusion. You calmly inform him of your urgent message for the King, and he hurries you into the Guildmaster's chambers.\nA distinguished old man in deep purple robes turns to greet you and listens to your story. Taking you by the arm, he leads you into an adjoining library and closes the door. Pressing one of the many thousands of books, he releases a secret panel in the wall and beckons you to follow him.\n"
        ( Decisions
            [ ("If you wish to follow him into the dark passage, turn to 332.", NoDecision (Goto 332)),
              ("If you are not completely happy about this man and wish to leave the Guildhall, turn to 37.", NoDecision (Goto 37))
            ]
        )
    ),
    ( 211,
      Chapter
        "211"
        "You walk along a dimly lit corridor which opens out into a large square room, with an oak door in the far wall.\n"
        ( Decisions
            [ ("If you possess the Kai Discipline of Sixth Sense turn to 244.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 244))),
              ("otherwise", Conditional (Not (HasDiscipline SixthSense)) (Decisions [("If you wish to walk across to the door, turn to 173.", NoDecision (Goto 173)), ("If you would prefer to return to the surface and continue your journey, turn to 106.", NoDecision (Goto 106))]))
            ]
        )
    ),
    ( 212,
      Chapter
        "212"
        "When you awake, the pain is but a memory. Restore all lost ENDURANCE points to your original score. A tall man dressed in white robes stands before you, a bowl of herbs in his hands. Placing the leaves into a kettle of boiling water, he then turns to greet you.\n\"You have passed close to death and have seen his face, Kai Lord, but the Grey One has not claimed you for his flock. You are healed in body but I sense that you are wounded in spirit. What is it that troubles you so?\"\nYou recognize the man to be one of the King's senior physicians, for the gold embroidered emblem of a dove upon his sleeve is the sign of his respected vocation. You tell the aged cleric of the events at the monastery and of your perilous journey to the King.\nRaising you gently from the bed by your arm, he bids you follow him. You notice that you are in a lavishly decorated room which leads out through a long corridor lined with tapestries. It slowly dawns on you just where you are.\nThis is the citadel of Holmgard and you are about to meet the King.\n"
        (NoDecision (Simple [FullHeal] (Goto 350)))
    ),
    ( 213,
      Chapter
        "213"
        "You have been trudging through the forest for nearly two hours. The nagging fear that you are lost begins to seem a reality. Apart from the occasional cry of a Kraan in the far distance, you have seen or heard no evidence that the enemy is in this part of the forest. As you descend a rocky hillock, you see something unusual in the tangled woods ahead.\n"
        (NoDecision (Goto 331))
    ),
    ( 214,
      Chapter
        "214"
        "The path gradually narrows until it disappears completely into a mass of dense vegetation. You cannot go any further on this route and therefore you must return to the clearing.\n"
        (NoDecision (Goto 125))
    ),
    ( 215,
      Chapter
        "215"
        "You emerge into a small clearing. In the centre you see the skeletal remains of a large animal. To the south a narrow track leads off into the distance.\n"
        ( Decisions
            [ ("If you wish to examine the skeleton, turn to 346.", NoDecision (Goto 346)),
              ("If you would rather press on, turn to 14.", NoDecision (Goto 14))
            ]
        )
    ),
    ( 216,
      Chapter
        "216"
        "Placing one hand on his forehead and the other on his wounded arm, you feel the warmth of your healing powers leave your body and give strength to the injured man.\nHe tells you his name is Trimis and he is a soldier in Prince Pelathar's army. The Prince and his troops are engaged in battle to the south, where a large force of the Darklords' creatures are attacking the bridge of Alema. During the fight, he had been snatched into the air by a Kraan, and dropped into the forest.\nYou make the soldier as comfortable as possible before continuing on your mission.\n"
        (NoDecision (Goto 264))
    ),
    ( 217,
      Chapter
        "217"
        "You quickly escape from the madman and dodge along a dark alleyway where the houses are small and cramped together. At the very end is a green door with a sign above it that says:\n"
        ( Decisions
            [ ("If you wish to enter, turn to 91.", NoDecision (Goto 91)),
              ("If you wish to wait until you are sure the madman has disappeared and then return to the main street, turn to 7.", NoDecision (Goto 7))
            ]
        )
    ),
    ( 218,
      Chapter
        "218"
        "Your senses reveal that more than just horses are heading towards you. You can just make out the very high shrieks of Giak war-cries in the distance. By the number of cries and curses you estimate that there are over a dozen Giaks, and probably Doomwolves as well. You decide that advertising your existence is perhaps not quite such a good idea after all!\n"
        (NoDecision (Goto 75))
    ),
    ( 219,
      Chapter
        "219"
        "All that remains of you now is embedded five feet into the stairs on which you were standing, beneath a vast granite block.\nYour mission and your life end here.\n"
        (NoDecision GameLost)
    ),
    ( 220,
      Chapter
        "220"
        "The bodyguard unsheathes a scimitar and lunges for your head.\n"
        (EvadeFight 0 234 (FightDetails {_opponent = "Bodyguard", _fcombatSkill = CombatSkill {getCombatSkill = 11}, _fendurance = Endurance {getEndurance = 20}, _fightMod = []}) (Goto 24))
    ),
    ( 221,
      Chapter
        "221"
        "Cautiously, you approach the base of the log wall. The tree trunks are rough-hewn and afford plenty of footholds for your climb. As you reach the top of the wall, you come face to face with a crossbow. The soldier holding it in your face motions for you to descend a wooden ladder to the ground. You do not argue with him. Slowly you descend the ladder.\n"
        (NoDecision (Goto 318))
    ),
    ( 222,
      Chapter
        "222"
        "As you go on you discover a forest path that divides at the point you join it.\n"
        ( Decisions
            [ ("If you wish to use your Kai Discipline of Tracking, turn to 67.", Conditional (HasDiscipline Tracking) (NoDecision (Goto 67))),
              ("If you wish to take the south fork, turn to 140.", NoDecision (Goto 140)),
              ("If you wish to take the east fork, turn to 252.", NoDecision (Goto 252))
            ]
        )
    ),
    ( 223,
      Chapter
        "223"
        "After quite a struggle, you manage to free the heavy trunk from the river bank. Gathering your equipment in a bundle, you stow it on top of the log and then slowly wade out into the river. The current soon takes you and you drift slowly downstream.\nAfter twenty minutes you hear the sound of horses along the left bank.\n"
        ( Decisions
            [ ("If you wish to hide behind the log, turn to 75.", NoDecision (Goto 75)),
              ("If you wish to climb onto the log and prepare to catch the riders' attention, then turn to 175.", NoDecision (Goto 175))
            ]
        )
    ),
    ( 224,
      Chapter
        "224"
        "You have ridden several miles and have seen no sign of refugees or of the enemy. You race on towards a high ridge in the middle distance. You should be able to see the capital from there.\nAs you reach the peak, the sight that meets you on the far side is one of hope-but there is still one challenge you know you have to face.\n"
        (NoDecision (Goto 153))
    ),
    ( 225,
      Chapter
        "225"
        "You recognize the language to be that of the Kakarmi, an intelligent race of forest animals that live in, and care for the forests of Sommerlund. You have nothing to fear from these creatures as they are very timid and gentle in their behaviour. Using your skill of Animal Kinship, you call to them in their strange native tongue.\n"
        ( Decisions
            [ ("If you say \"Do not be afraid, I am a friend,\" turn to 187.", NoDecision (Goto 187)),
              ("If you say \"I am a Kai Lord. I wish you no harm. I must talk with you,\" turn to 39.", NoDecision (Goto 39))
            ]
        )
    ),
    ( 226,
      Chapter
        "226"
        "At first the descent is quite easy, but you soon find it difficult to see clearly and your legs feel very weak. The \"Sleeptooth\" scratches are affecting you, and suddenly you pitch forward and slip head-first into darkness.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 277), (1 % 2, Goto 338)]))
    ),
    ( 227,
      Chapter
        "227"
        "You are now up to your waist in slimy water. The air is thick with small insects that sting your face and clog your nose. Something wraps itself around your leg. It is a Marshviper and you must fight it.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Marshviper", _fcombatSkill = CombatSkill {getCombatSkill = 16}, _fendurance = Endurance {getEndurance = 6}, _fightMod = [OnDamage 271]}) (Goto 348)))
    ),
    ( 228,
      Chapter
        "228"
        "The path continues eastwards but soon disappears into thick undergrowth.\n"
        ( Decisions
            [ ("If you continue east, cutting through the vegetation with your weapon, turn to 140.", NoDecision (Goto 140)),
              ("If you head south to where the bushes are less dense and then press on through the forest, turn to 215.", NoDecision (Goto 215))
            ]
        )
    ),
    ( 229,
      Chapter
        "229"
        "The Kraan hovers above you, raising dust with the beat of its huge black wings. The dust gets into your eyes and nose, and you start to cough. Now the beast attacks.\nYou must fight it to the death. Because of the dust, you must reduce your COMBAT SKILL by 1 point.\nIf you win you have a choice.\n"
        ( Decisions
            [ ("Will you search the body by turning to 267.", NoDecision (Fight (FightDetails {_opponent = "Kraan", _fcombatSkill = CombatSkill {getCombatSkill = 16}, _fendurance = Endurance {getEndurance = 25}, _fightMod = [CombatBonus (CombatSkill {getCombatSkill = -1})]}) (Goto 267))),
              ("Or will you continue along the east path by turning to 125.", NoDecision (Fight (FightDetails {_opponent = "Kraan", _fcombatSkill = CombatSkill {getCombatSkill = 16}, _fendurance = Endurance {getEndurance = 25}, _fightMod = [CombatBonus (CombatSkill {getCombatSkill = -1})]}) (Goto 125)))
            ]
        )
    ),
    ( 230,
      Chapter
        "230"
        "In the far distance, you can make out the silhouette of soldiers on barges that are strung out in a line across the river. You can hear the low growls of Doomwolves returning along the opposite bank.\nFor once you throw caution to the wind and sprint along the river bank towards the barges in the distance.\n"
        (NoDecision (Goto 179))
    ),
    ( 231,
      Chapter
        "231"
        "You are about to ask the price of the potions when the bamboo screen crashes down and a young man leaps at you. He has a long curved dagger in his hand.\nHe is upon you and you must fight for your life.\n"
        (EvadeFight 2 7 (FightDetails {_opponent = "Robber", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 20}, _fightMod = [Timed 4 (OnNotYetWon 203)]}) (Goto 94))
    ),
    ( 232,
      Chapter
        "232"
        "The rough-looking leader approaches you and says, \"Our needs are simple, kind sir. Your money or your life!\"\n"
        ( Decisions
            [ ("If you wish to fight them, turn to 180.", NoDecision (Goto 180)),
              ("If you wish to run, turn to 22.", NoDecision (Goto 22))
            ]
        )
    ),
    ( 233,
      Chapter
        "233"
        "After nearly an hour, you catch up with the horse and succeed in calming him down. You are now north of the cabin, but you are confident of finding your way back.\nMounting the horse, you ride back past the cabin, and press on towards the south once again.\n"
        (NoDecision (Goto 206))
    ),
    ( 234,
      Chapter
        "234"
        "You jump clear of the speeding caravan but land very badly and break your ankle. The pain is terrible and you soon lose consciousness.\nUnfortunately you never wake up, but it may be of interest to you that your head is now adorning the saddle of a Kraan.\nYour life and your mission end here.\n"
        (NoDecision GameLost)
    ),
    ( 235,
      Chapter
        "235"
        "The Prince's horse is indeed a magnificent animal, fast and sure of foot. You gallop along the twisting track as if it were a straight highway, until the noise of battle has disappeared far behind you.\nYou are hungry and must eat a Meal during your ride.\nAfter several miles, the path stops abruptly at a junction. There is a signpost, but it has been hacked down.\n"
        ( Decisions
            [ ("If you wish to use your Kai Discipline of Tracking, turn to 254.", Conditional (HasDiscipline Tracking) (NoDecision (Simple [MustEat Hunt] (Goto 254)))),
              ("If you wish to turn left, go to 32.", NoDecision (Simple [MustEat Hunt] (Goto 32))),
              ("If you wish to turn right, go to 146.", NoDecision (Simple [MustEat Hunt] (Goto 146)))
            ]
        )
    ),
    ( 236,
      Chapter
        "236"
        "The Gem hovers above the mouth of the skeleton king, glowing a fierce red. Suddenly, an explosion of searing crimson flame lashes upwards from the sarcophagus, destroying the Gem completely. You are thrown against the far wall and knocked unconscious.\nWhen you awake, the chamber is completely empty. The skeleton king and the sarcophagus have vanished. You have lost 6 ENDURANCE points, and your initial COMBAT SKILL is reduced by 1 point for the rest of your life. (Remember to erase the Vordak Gem from your Action Chart. If you had two Gems, you need erase only one.)\nYou carefully get to your feet and stagger towards the tunnel.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 6}), SetFlag PermanentSkillReduction, LoseItem (GenSpecial (GenCounter 0)) 99] (Goto 104)))
    ),
    ( 237,
      Chapter
        "237"
        "You make full use of your Kai Discipline and quickly burrow deep into the loose earth of the wooded hillside. Covering yourself with your cloak, you pull a loose branch across your hastily dug shelter.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 265), (1 % 2, Goto 72)]))
    ),
    ( 238,
      Chapter
        "238"
        "The path meanders between several small, wooded hills and eventually leads to a ruined log cabin. It seems that it had burnt down not so long ago, for the ashes are still warm and a haze of smoke still lingers. You sense possible danger here.\n"
        ( Decisions
            [ ("You may leave by the south route by turning to 42.", NoDecision (Goto 42)),
              ("Or you may take the north track by turning to 68.", NoDecision (Goto 68))
            ]
        )
    ),
    ( 239,
      Chapter
        "239"
        "As you push on into the forest, you hear the wings of the Kraan pass above the trees and disappear northwards. You ride on for nearly an hour until you come to a clearing. On the far side is a track that leads off to the south.\n"
        ( Decisions
            [ ("If you wish to enter the clearing and take the south exit, turn to 34.", NoDecision (Goto 34)),
              ("If you would rather skirt the edge of the clearing and pick up the track further on, turn to 118.", NoDecision (Goto 118))
            ]
        )
    ),
    ( 240,
      Chapter
        "240"
        "The path leads along a ridge of wooded hillocks and changes direction towards the east.\n"
        (NoDecision (Goto 79))
    ),
    ( 241,
      Chapter
        "241"
        "The wizard heeds your cry and spins around just in time to loose a searing bolt of energy at the Giak. The creature's head disintegrates in flames and its twitching body falls in a heap at the foot of the pillar. The Giak officer sees you and shouts, \"Ogot...Ogot!\" to his cowering troops, who quickly run away from the ruins to the safety of the forest beyond.\nThe young wizard wipes his brow, and walks towards you, his hand extended in gratitude and friendship.\n"
        (NoDecision (Goto 349))
    ),
    ( 242,
      Chapter
        "242"
        "The lid of the sarcophagus slips to the floor with a dull crunch. You are looking at the remains of an ancient king, who lies still surrounded by his treasure. An ornate crown is still in position on his skull. The jaw of the skeleton is wide open and the darkness of the mouth seems strangely bottomless. A distant rumbling can now be heard from deep in the earth.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Mindshield, turn to 166.", Conditional (HasDiscipline MindShield) (NoDecision (Goto 166))),
              ("otherwise", Conditional (Not (HasDiscipline MindShield)) (Decisions [("If you do not, turn to 9.", NoDecision (Goto 9))]))
            ]
        )
    ),
    ( 243,
      Chapter
        "243"
        "Hurrying through the forest, you stumble and fall down a steep slope which drops you in a heap on a hidden path below. On the path is a dead body. It is a Giak, a spiteful and ghoulish servant of the Darklords. Many centuries ago, their ancestors were used by the Darklords to build for them the infernal city of Helgedad, which lies in the volcanic wastelands beyond the Durncrag range of mountains. The construction of the city was a long and torturous nightmare, and only the strongest of the Giaks survived the heat and poisonous atmospheres of Helgedad. This creature that lies before you is a descendant of these Giak slaves. It has been killed by a sword blow to its head, and by its side lies a Mace. You may take this Weapon if you wish.\n"
        (CanTake (Weapon Mace) 1 (NoDecision (Goto 97)))
    ),
    ( 244,
      Chapter
        "244"
        "Your senses tell you that you are not alone. You are in very great danger. Return to the surface as quickly as you can.\n"
        (NoDecision (Goto 93))
    ),
    ( 245,
      Chapter
        "245"
        "Arrows hit the water above you, and drop harmlessly past as you swim beneath the surface towards the opposite bank.\nQuickly you wade out of the river and dash for the trees. You are now out of range of the Giaks, who remount their Doomwolves and continue their chase.\n"
        (NoDecision (Goto 190))
    ),
    ( 246,
      Chapter
        "246"
        "When the ferry reaches the middle of the lake, the man stops rowing and stands up. He laughs menacingly and pulls back the hood of his cloak to reveal himself. He is a Drakkar and you must fight him.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Drakkar", _fcombatSkill = CombatSkill {getCombatSkill = 15}, _fendurance = Endurance {getEndurance = 23}, _fightMod = []}) (Goto 197)))
    ),
    ( 247,
      Chapter
        "247"
        "The merchant looks angry. He calls to his bodyguard. You must think of something quickly.\n"
        ( Decisions
            [ ("If you decide to offer him something of greater value that you have in your Backpack, turn to 159.", NoDecision (Goto 159)),
              ("If you prepare to fight the bodyguard, turn to 220.", NoDecision (Goto 220))
            ]
        )
    ),
    ( 248,
      Chapter
        "248"
        "You reach the base of the hill and hurry into the forest. After only a few minutes you discover an old forest track.\n"
        ( Decisions
            [ ("If you wish to follow this track north, turn to 44.", NoDecision (Goto 44)),
              ("If you wish to follow this track east, turn to 300.", NoDecision (Goto 300))
            ]
        )
    ),
    ( 249,
      Chapter
        "249"
        "You descend a flight of stone stairs that lead to a large chamber. A macabre sight awaits you. Directly opposite, across the large stone room, is an ornate archway with a corridor leading into the darkness beyond. The strange green light radiates from two lines of skulls each resting on a stone plinth. They face each other to form an eerie walkway across the room.\n"
        ( Decisions
            [ ("If you wish to walk across the room to the archway, turn to 169.", NoDecision (Goto 169)),
              ("If you wish to attack the skulls, turn to 107.", NoDecision (Goto 107))
            ]
        )
    ),
    ( 250,
      Chapter
        "250"
        "Leaping from the top of the trunk, you land in front of two small furry creatures. You recognize that they are Kakarmi, an intelligent race of animals that inhabit and tend the forests of Sommerlund. Before you can apologize for your dramatic entrance, the frightened little creatures scurry off into the forest.\n"
        ( Decisions
            [ ("If you wish to follow them, turn to 186.", NoDecision (Goto 186)),
              ("If you wish to continue, turn to 228.", NoDecision (Goto 228))
            ]
        )
    ),
    ( 251,
      Chapter
        "251"
        "You are lucky, they do not seem to have spotted you. They slowly move on and have soon disappeared along the far side of the ridge. You continue your run.\n"
        (NoDecision (Goto 10))
    ),
    ( 252,
      Chapter
        "252"
        "In the centre of a small clearing you see a group of humans talking excitedly and gesturing wildly with their hands. There are two children, three men, and a woman. Their belongings are wrapped in bundles which they carry slung over their shoulders. Their clothes look well made and expensive but they are dirty and torn.\n"
        ( Decisions
            [ ("If you wish to approach them and ask who they are, turn to 155.", NoDecision (Goto 155)),
              ("If you wish to avoid them and continue onwards on your mission, turn to 70.", NoDecision (Goto 70))
            ]
        )
    ),
    ( 253,
      Chapter
        "253"
        "The Doomwolves are soon on you and you must fight them one at a time.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Doomwolf 1", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 24}, _fightMod = []}) (Fight (FightDetails {_opponent = "Doomwolf 2", _fcombatSkill = CombatSkill {getCombatSkill = 14}, _fendurance = Endurance {getEndurance = 23}, _fightMod = []}) (Fight (FightDetails {_opponent = "Doomwolf 3", _fcombatSkill = CombatSkill {getCombatSkill = 14}, _fendurance = Endurance {getEndurance = 22}, _fightMod = []}) (Fight (FightDetails {_opponent = "Doomwolf 4", _fcombatSkill = CombatSkill {getCombatSkill = 15}, _fendurance = Endurance {getEndurance = 21}, _fightMod = []}) (Goto 278))))))
    ),
    ( 254,
      Chapter
        "254"
        "Your Tracking ability tells you that several trails from the right path lead off in the direction of the left path. They have been made by large wolves. The Darklords use such beasts to scout for their armies. They are vicious creatures and are often ridden by Giaks. The left path leads towards Holmgard, and the right path leads off towards the Durncrag Mountains. The choice of route is yours.\n"
        ( Decisions
            [ ("If you wish to turn left, go to 32.", NoDecision (Goto 32)),
              ("If you wish to turn right, go to 146.", NoDecision (Goto 146))
            ]
        )
    ),
    ( 255,
      Chapter
        "255"
        "The creature that you now face is a Gourgaz, one of a race of cold-blooded reptilian creatures that dwell deep in the treacherous Maakenmire swamps. Their favourite food is human flesh!\nThe Prince's Sword lies at your feet. You may pick up and use this weapon if you wish. The Gourgaz is about to strike at you-you must fight him to the death.\nThis creature is immune to Mindblast.\n"
        (CanTake (Weapon Sword) 1 (NoDecision (Fight (FightDetails {_opponent = "Gourgaz", _fcombatSkill = CombatSkill {getCombatSkill = 20}, _fendurance = Endurance {getEndurance = 30}, _fightMod = [MindblastImmune]}) (Goto 82))))
    ),
    ( 256,
      Chapter
        "256"
        "You are awoken by the cries of Kraan high above you in the clear morning sky. Rubbing your eyes, you peer upwards through the canopy of branches to see three of the loathsome creatures fly off towards the north.\nYou are sure you have not been spotted, but perhaps it would be best to leave now-just in case. You mount your horse and ride south along the highway.\n"
        (NoDecision (Goto 224))
    ),
    ( 257,
      Chapter
        "257"
        "You find a stone portal in the east wall, but there does not appear to be any way of opening it.\n"
        ( Decisions
            [ ("If you wish to examine the statue, turn to 133.", NoDecision (Goto 133)),
              ("If you wish to sit on the seat, turn to 161.", NoDecision (Goto 161))
            ]
        )
    ),
    ( 258,
      Chapter
        "258"
        "Using your Kai Discipline of Mind Over Matter, you untie the ropes binding your hands. You wait for a chance to make a break for it and then sprint as fast as you can into the dense undergrowth. Black arrows whistle past you, but you are soon deep among the trees and safe again. You have lost your Backpack and Weapons but you have your life and limbs intact. You continue to push on into the forest.\n"
        (NoDecision (Simple [LoseItemKind [BackpackSlot, WeaponSlot], LoseItem Backpack 1] (Goto 50)))
    ),
    ( 259,
      Chapter
        "259"
        "The room is getting colder. You gradually notice the smell of sulphur in the air. You can hear chanting in the distance. It sounds as if it is somewhere in another part of this cave. A slit in the stone wall opens, and the end of a black staff begins to appear. Suddenly a bolt of blue lightning leaps from the staff and hits you in the chest.\nAs your life slowly drains away, the last thing you see is an old man dressed in black robes raising a dagger above your throat.\nYour life and your mission end here.\n"
        (NoDecision GameLost)
    ),
    ( 260,
      Chapter
        "260"
        "Swimming towards the bank, you can see the ranger spread-eagled at the water's edge. You reach him but only to find that he has broken his neck in the fall and is dead.\nSuddenly, two Giaks jump on you from above and you must fight them. You are unarmed and must fight the Giaks with your bare hands. Deduct 4 points from your COMBAT SKILL and fight them one at a time.\n"
        ( NoDecision
            ( Fight
                ( FightDetails
                    { _opponent = "Giak 1",
                      _fcombatSkill = CombatSkill {getCombatSkill = 11},
                      _fendurance = Endurance {getEndurance = 18},
                      _fightMod = []
                    }
                )
                ( Fight
                    ( FightDetails
                        { _opponent = "Giak 2",
                          _fcombatSkill = CombatSkill {getCombatSkill = 12},
                          _fendurance = Endurance {getEndurance = 17},
                          _fightMod = []
                        }
                    )
                    (Goto 156)
                )
            )
        )
    ),
    ( 261,
      Chapter
        "261"
        "Sweating, and out of breath, you part the dense undergrowth to see a Kraan hovering over the wagon. Three ghoulish Giaks drop from its back, startling the horses. They advance upon the helpless children with their spears.\n"
        ( Decisions
            [ ("If you wish to run back to the wagon and defend the children, turn to 208.", NoDecision (Goto 208)),
              ("If you want to run deeper into the forest, turn to 264.", NoDecision (Goto 264))
            ]
        )
    ),
    ( 262,
      Chapter
        "262"
        "The merchant takes your Gold and clicks his fingers. His bodyguard attacks you with his scimitar.\n"
        ( Decisions
            [ ("If you wish to fight, turn to 191.", NoDecision (Goto 191)),
              ("If you wish to evade combat, jump clear of the speeding caravan by turning to 234.", NoDecision (Goto 234))
            ]
        )
    ),
    ( 263,
      Chapter
        "263"
        "Carefully, you follow the stream as it makes its way towards the east. Suddenly you notice something in the distance that brings you to a halt.\nLying in the rushing water like a great black dam is a dead Kraan. You creep nearer, under cover of the foliage, until you see three arrows deep in the beast's chest. Trapped beneath the beast is the body of its rider. It is a Giak, a spiteful and malicious servant of the Darklords. Many centuries ago, their ancestors were used by the Darklords to build the infernal city of Helgedad, which lies in the volcanic wastelands beyond the Durncrag range of mountains. The construction of the city was a long and torturous nightmare, and only the strongest Giaks survived the heat and poisonous atmospheres of Helgedad. This creature is a descendant of these Giak slaves. It seems that this one must have drowned. The Giak's pouch contains 3 Gold Crowns. (You may take these if you wish.)\n"
        ( CanTake
            Gold
            3
            ( Decisions
                [ ("You may continue downstream, by turning to 70.", NoDecision (Goto 70)),
                  ("Or you may leave the stream and make your way on foot through the wooded hills to the south by turning to 157.", NoDecision (Goto 157))
                ]
            )
        )
    ),
    ( 264,
      Chapter
        "264"
        "You have not gone far when you hear the sound of battle to the west.\n"
        ( Decisions
            [ ("If you wish to follow the sound, turn to 97.", NoDecision (Goto 97)),
              ("If you would rather continue south, turn to 6.", NoDecision (Goto 6))
            ]
        )
    ),
    ( 265,
      Chapter
        "265"
        "You quickly move off into the forest before more Doomwolves or Kraan appear.\nYou have walked for more than an hour when you reach the top of a rocky hill. The sight that befalls you on the other side is one of hope. But there is also a daunting challenge to be faced.\n"
        (NoDecision (Goto 142))
    ),
    ( 266,
      Chapter
        "266"
        "As the beast writhes in its final death agony on the black stone floor, the portal in the east wall clicks open to reveal a corridor beyond. You quickly dash through the secret door just as it crashes shut.\n"
        (NoDecision (Goto 209))
    ),
    ( 267,
      Chapter
        "267"
        "Covering your nose with your cloak, you cautiously approach the dead beast. The sharp smell of its fetid black blood makes your stomach churn, but you are determined to press on. Then you notice a large saddlebag strapped to its chest. Opening the bag, you find a Message written on an animal skin.\nDeeper in the bag is a Dagger. You may keep both the Message and the Dagger if you wish.\nYou leave the body and continue eastwards along the path.\n"
        (NoDecision (Goto 125))
    ),
    ( 268,
      Chapter
        "268"
        "You black out for only a few minutes before you are revived with a measure of strong spirit. Feeling weary but thankful to be alive, you lean on the shoulders of the King's men and you stagger towards the outer defences.\n"
        (NoDecision (Goto 288))
    ),
    ( 269,
      Chapter
        "269"
        "The madman lies dead at your feet. Two soldiers soon appear at the doorway and immediately congratulate you. They tell you that he was an escaped lunatic whom they had been tracking for the last two days. One of the soldiers gives you 10 Gold Crowns reward money and offers to escort you to the citadel.\n"
        ( CanTake
            Gold
            10
            ( Decisions
                [ ("If you accept his offer, turn to 314.", NoDecision (Goto 314)),
                  ("If you would prefer to trust your own sense of direction, turn to 7.", NoDecision (Goto 7))
                ]
            )
        )
    ),
    ( 270,
      Chapter
        "270"
        "You hear the angry cries of the enemy drift across the lake. You must leave here before more Kraan appear. You mount your steed and push on further into the forest.\n"
        (NoDecision (Goto 21))
    ),
    ( 271,
      Chapter
        "271"
        "You feel very weak. The poison of the snake has entered your bloodstream and you can feel the muscles of your body involuntarily tightening and relaxing. Your legs suddenly collapse beneath you and you feel the slimy water of the marsh close over your head.\nYour life ends here.\n"
        (NoDecision GameLost)
    ),
    ( 272,
      Chapter
        "272"
        "Keeping a watchful eye on the sky above, you move quickly along the track. You recall that this route leads to Fogwood, a small cluster of huts that have been used by a family of charcoal burners for nearly fifty years. After twenty minutes you reach the edge of a clearing where the huts are grouped in a small circle. There is no sign of the usual mist of wood smoke which gives Fogwood its apt name, and the huts are unusually quiet.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Tracking, you may turn to 134.", Conditional (HasDiscipline Tracking) (NoDecision (Goto 134))),
              ("otherwise", Conditional (Not (HasDiscipline Tracking)) (Decisions [("If you do not possess this skill, you prepare your weapon and stealthily approach the huts. Turn to 305.", NoDecision (Goto 305))]))
            ]
        )
    ),
    ( 273,
      Chapter
        "273"
        "The outer fieldworks of the city can now be seen. Drawn across the river is a line of barges chained together to form a floating barricade. You can also see soldiers running along the log walls of the fieldworks, and you can hear the faint noise of battle drifting from the west.\n"
        ( Decisions
            [ ("If you wish to approach the barges, turn to 179.", NoDecision (Goto 179)),
              ("If you wish to take cover in the trees, turn to 51.", NoDecision (Goto 51))
            ]
        )
    ),
    ( 274,
      Chapter
        "274"
        "In your haste to avoid the enemy, you catch your foot in a tree root and you are pitched head over heels in a tumble of dust and leaves. You quickly get to your feet and, crashing through the undergrowth at the base of the hill, you run into the forest. You have been running for nearly ten minutes when you discover that you have lost your Weapon(s). Well, at least you still have your life and your Backpack. Wiping the grime from your face, you push on through the trees ahead.\n"
        (NoDecision (Goto 331))
    ),
    ( 275,
      Chapter
        "275"
        "You have followed this twisting track for about twenty minutes when you hear the beating of wings high above the trees. Looking up you see a large Kraan approaching from the north, its huge black wings casting a gigantic shadow on the trees below.\nOn its back are two creatures armed with long spears. They are Mountain Giaks-small ugly creatures full of hatred and malice. Many centuries ago, their ancestors were used by the Darklords to build the infernal city of Helgedad, which lies in the volcanic wastelands beyond the Durncrag mountain range. The construction of the city was long and torturous, and only the strongest of the creatures survived the heat and poisonous atmosphere of Helgedad.\nQuickly you dive for the shelter of a large fern tree as the Kraan passes overhead. With heart pounding, you pray that your quick reactions have saved you from being spotted.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 345), (1 % 2, Goto 74)]))
    ),
    ( 276,
      Chapter
        "276"
        "Reaching for your weapon you manage to hack your way through the tangle of wood and twisted branches to the clearer forest beyond. Your cloak is torn in several places and your right leg is badly bruised above the knee.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 1})] (Goto 213)))
    ),
    ( 277,
      Chapter
        "277"
        "When you awake, you find yourself lying at the foot of a steep slope in a tangle of tall grasses. Your Backpack and Weapon are missing and your head aches. You cannot tell how long you have been unconscious, but you realize that you must not delay in pressing on with your mission.\nStanding up, you see your Backpack is intact but the Weapon is broken in two and is now useless. Remember to cross it off your Action Chart. (If you have more than one weapon, only one of them is broken-you may choose which one it is.) You quickly pick up your Backpack and move off into the trees ahead.\n"
        (NoDecision (Simple [LoseItemKind [WeaponSlot]] (Goto 113)))
    ),
    ( 278,
      Chapter
        "278"
        "You quickly leave the path and gallop off along the track heading towards the capital. When you reach the point where the Doomwolves stopped, you can see just beyond a meadow the main highway which runs from the northern port of Toran to Holmgard. You should reach the capital by morning.\n"
        (NoDecision (Goto 149))
    ),
    ( 279,
      Chapter
        "279"
        "You clamber over the loose rocks and into the mouth of the cave, and then quickly turn to push a large rock over the entrance.\nAfter a few minutes you see the Giaks on the rocky ledge outside, their evil yellow eyes furtively searching every crevice of the hillside. They are so close that you feel sure that they must spot you any second now.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(7 % 10, Goto 112), (3 % 10, Goto 96)]))
    ),
    ( 280,
      Chapter
        "280"
        "As you begin your climb, you hear the beat of wings approaching from the west. Kraan! By the noise they are making you estimate there are at least ten, perhaps more. You curse your bad luck, for the hillside offers no cover from the sky. If you are attacked during this difficult climb, you will find it nearly impossible to fight back and remain upright at the same time.\n"
        ( Decisions
            [ ("If you decide to draw your weapon and remain completely still, in the hope that the Kraan will not spot you, turn to 327.", NoDecision (Goto 327)),
              ("If you decide to quickly descend the hillside and take cover in the tunnel, turn to 170.", NoDecision (Goto 170))
            ]
        )
    ),
    ( 281,
      Chapter
        "281"
        "As you race through the trees you can hear the horrible cackle of the Giaks close behind you. Soon the trees start to thin out and directly ahead you can see a rocky hillside.\n"
        ( Decisions
            [ ("If you break cover and climb up the hill, turn to 311.", NoDecision (Goto 311)),
              ("If you change direction and continue your run through the forest, turn to 77.", NoDecision (Goto 77))
            ]
        )
    ),
    ( 282,
      Chapter
        "282"
        "Looking above the heads of the crowd, you notice that one of the shops opposite the main gate is the timbered surgery of a city physician. Suddenly, a bold plan springs to mind. Bracing yourself against the tide of bodies, you struggle across to the other side of the street. You quickly enter to find that there is no sign of life, apart from a brightly coloured parrot in its cage by the window.\nTaking a selection of small bottles you slip on a white surgeon's cloak, and fight your way back to the main gate. \"An emergency!\" you bluff, as guards stop and question you. \"It's the royal cook's wife...she's having a baby.\"\nThe guards hesitate for a moment, but you assure them that the matter is most urgent and they decide to let you in. One of the great doors swings open about two feet, and you are roughly pushed through the narrow gap into the courtyard beyond.\n"
        (NoDecision (Goto 11))
    ),
    ( 283,
      Chapter
        "283"
        "You are only ten feet or so away from the robed stranger when the raven squawks a warning to its master who instantly spins around. You are frozen in your tracks by the hideous apparition of a Vordak, a lieutenant of the Darklords and one of the undead. You must fight him.\nDue to the surprise of your attack, you may add 2 points to your COMBAT SKILL for the first round of combat only.\nUnless you have the Kai Discipline of Mindshield, deduct 2 points from your COMBAT SKILL for the second and subsequent rounds of fighting, for the creature is attacking you with the power of its Mindforce as well as with a large black mace!\n"
        (NoDecision (Conditionally [(HasDiscipline MindShield, Fight (FightDetails {_opponent = "Vordak", _fcombatSkill = CombatSkill {getCombatSkill = 17}, _fendurance = Endurance {getEndurance = 25}, _fightMod = [Timed 1 (CombatBonus (CombatSkill {getCombatSkill = 2}))]}) (Goto 123)), (Always True, Fight (FightDetails {_opponent = "Vordak", _fcombatSkill = CombatSkill {getCombatSkill = 19}, _fendurance = Endurance {getEndurance = 25}, _fightMod = [Timed 1 (CombatBonus (CombatSkill {getCombatSkill = 4}))]}) (Goto 123))]))
    ),
    ( 284,
      Chapter
        "284"
        "Your passage through the Graveyard will not be easy, for the ground is broken and covered with a thorny graveweed. This wicked briar tears your cloak and cuts your legs. The air hangs heavy and still. Foul gases seep from open crypts and the haunting murmur of distant whispering fills your ears.\nCarefully, you approach a gap between two ancient pillars, and part the graveweed with your cloaked hand. Suddenly, the ground collapses beneath you and you fall in a tumble of earth and stone.\n"
        (NoDecision (Goto 71))
    ),
    ( 285,
      Chapter
        "285"
        "With a sickening thud, the chunk of marble cracks open the back of the Giak's head. The creature drops to its knees and slowly falls forward, down to the ruins below. Elated by your skill, you race forward to aid the young wizard.\n"
        (NoDecision (Goto 325))
    ),
    ( 286,
      Chapter
        "286"
        "Messengers of death-and ones eager to deliver their news-the Doomwolves surround and then attack you. Valiantly you fight, but it is to no avail for there are too many of them.\nAs your life's blood seeps away and eternal dark approaches, the last sight you remember is the glint of sunlight on the spires of Holmgard.\nYou have failed in your mission.\n"
        (NoDecision GameLost)
    ),
    ( 287,
      Chapter
        "287"
        "The track soon disappears completely into a tangle of thorny brambles and low-branched fir trees.\n"
        ( Decisions
            [ ("If you decide to return to the junction and head east, turn to 13.", NoDecision (Goto 13)),
              ("If you decide to hack your way slowly through the undergrowth in this present direction, turn to 330.", NoDecision (Goto 330))
            ]
        )
    ),
    ( 288,
      Chapter
        "288"
        "As you reach the walls of the fieldworks, the large oak gates open and you are quickly hurried inside. A sergeant, bloodstained and battle-weary, calls to an officer who turns and recognizes your cloak.\n\"My Lord,\" he says. \"Where are the other Kai Masters? We are in desperate need of their wisdom. The Darklords press us most cruelly and casualties are high.\"\nYou inform the brave officer of the terrible fate of your kinsmen, and the urgency of your mission to seek the King's council. Without saying a word, he motions to a soldier to bring forward two horses. You both mount and gallop off towards the high city wall of Holmgard.\n"
        (NoDecision (Goto 129))
    ),
    ( 289,
      Chapter
        "289"
        "The two guards look tired and anxious. They nervously hold their royal halberds in front of themselves, using the weapons to push away anyone who comes too close to the gates. An angry woman attacks one of them, pounding his chest with her clenched fists making him fall against the other guard. All three collapse in a struggling heap of flailing arms and legs. Seeing your chance, you dash forward and pull the large lever which opens the great doors.\nYou slip inside and the doors close without either of the guards seeing you enter.\n"
        (NoDecision (Goto 139))
    ),
    ( 290,
      Chapter
        "290"
        "Inside the long box is a Quarterstaff wrapped in leather. You may take this Weapon if you wish. You close the box and descend the ladder to the clearing below, taking care to use only the sound rungs.\n"
        (CanTake (Weapon Quarterstaff) 1 (NoDecision (Goto 140)))
    ),
    ( 291,
      Chapter
        "291"
        "The two Giaks lie at your feet, their bodies twisted and lifeless. A quick search reveals 6 Gold Crowns, 2 Spears, and a Dagger.\nYou may keep the Gold and take either the Dagger or a Spear. Remember to mark this on your Action Chart.\nThe Kraan flew off during your battle, and the track is now deserted. You adjust your Backpack and continue your mission.\n"
        (CanTake Gold 6 (CanTake (Weapon Dagger) 1 (CanTake (Weapon Spear) 1 (NoDecision (Goto 272)))))
    ),
    ( 292,
      Chapter
        "292"
        "The last thing that you experience of this life is the feeling of being sucked into the void of darkness. No trace of you remains in this world, for you have passed into a realm of timeless existence. You have become a slave of an ancient evil.\nYour adventure ends here.\n"
        (NoDecision GameLost)
    ),
    ( 293,
      Chapter
        "293"
        "With a wave of his hand, Banedon leaves the ruins and you continue your mission, pushing on through the thick woods ahead. You have not gone far when you realize several pairs of yellow eyes are watching you from the undergrowth to your left. Suddenly, a black arrow skims the top of your head. It is a Giak ambush and you must run as fast as you can to escape it.\n"
        (NoDecision (Goto 281))
    ),
    ( 294,
      Chapter
        "294"
        "Staying underwater for as long as you can, you finally surface to see the Giaks far behind you. You have lost your Weapon(s) and Backpack but at least you are still alive.\nYou wade out of the muddy water and continue your journey under cover of the trees that line the right-hand bank.\nPick a number from the Random Number Table.\n"
        (NoDecision (Simple [LoseItemKind [WeaponSlot, BackpackSlot], LoseItem Backpack 1] (Randomly [(3 % 10, Goto 230), (2 % 5, Goto 190), (3 % 10, Goto 321)])))
    ),
    ( 295,
      Chapter
        "295"
        "You have continued your journey for about fifteen minutes when suddenly a black arrow whistles past your head and embeds itself in a tree. Instinctively you duck and draw your weapon.\n"
        ( Decisions
            [ ("If you wish to remain where you are in order to try to spot the hidden archer, turn to 185.", NoDecision (Goto 185)),
              ("If you wish to run for the cover of denser undergrowth, turn to 92.", NoDecision (Goto 92))
            ]
        )
    ),
    ( 296,
      Chapter
        "296"
        "You sense something is wrong. With fighting all around and the forces of the Darklords so near, why has this man stayed in the forest? You feel a strange aura of evil about him and decline his offer.\n"
        (NoDecision (Goto 90))
    ),
    ( 297,
      Chapter
        "297"
        "Using the skills taught to you by your masters in the art of Hunting, you inch your way through the foliage undetected. In less than a minute you are directly behind, and only a few feet from, the stake to which the ranger is tied. The wood is alight and great clouds of smoke are engulfing the poor victim. You take your weapon and run forward, hidden by the smoke. One blow of your weapon is all that is needed to sever his bonds, and you pull him free and back into the safety of the forest. As you press on into the forest, you hear the shrieks of the Giaks as they discover that their prisoner has literally disappeared in a cloud of smoke!\n"
        (NoDecision (Goto 117))
    ),
    ( 298,
      Chapter
        "298"
        "The head of the bird slowly turns and it curses you. An instant later, it flies off above the trees and has soon disappeared. Shocked by what you have heard you are now sure that the fledgling was a scout of the Darklords and is now probably on its way to inform them of your whereabouts.\n"
        ( Decisions
            [ ("If you wish to continue your journey along the track, turn to 121.", NoDecision (Goto 121)),
              ("If you wish to leave the track and continue through the forest instead, turn to 38.", NoDecision (Goto 38))
            ]
        )
    ),
    ( 299,
      Chapter
        "299"
        "You soon realize that you are walking deeper into a wooded marsh. To continue in this direction will be slow and hazardous.\n"
        ( Decisions
            [ ("If you wish to continue, turn to 227.", NoDecision (Goto 227)),
              ("If you wish to change direction and head towards firmer ground, turn to 95.", NoDecision (Goto 95))
            ]
        )
    ),
    ( 300,
      Chapter
        "300"
        "You walk for over an hour, during which time you keep a constant vigil for any sign of Kraan in the sky above. You have twice spotted their tell-tale shadows in the sky and on both occasions your quick wits have saved you from capture. You are now very hungry and must eat a Meal.\n"
        (NoDecision (Goto 13))
    ),
    ( 301,
      Chapter
        "301"
        "Your Kai Discipline reveals that the west path is a dead end.\n"
        (NoDecision (Goto 27))
    ),
    ( 302,
      Chapter
        "302"
        "Pick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(3 % 10, Goto 110), (7 % 10, Goto 285)]))
    ),
    ( 303,
      Chapter
        "303"
        "The forest here is sparse and hilly. It does not give much cover from an attack from the air. You move as quickly as you can from tree to tree, to avoid the Kraan but you can hear the sound of Doomwolves close behind.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Camouflage, turn to 237.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 237))),
              ("otherwise", Conditional (Not (HasDiscipline Camouflage)) (Decisions [("If you do not, turn to 72.", NoDecision (Goto 72))]))
            ]
        )
    ),
    ( 304,
      Chapter
        "304"
        "The Gem feels incredibly hot and burns your hand. Lose 2 ENDURANCE points.\nYou quickly pick it up with the edge of your Kai cloak and slip this Vordak Gem into your Backpack. A Gem that size must be worth hundreds of Crowns. But the Giaks are very close and their arrows whistle past your head as you turn and run for the safety of the forest.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 2}), GainItem (GenSpecial (GenCounter 0)) 1] (Goto 2)))
    ),
    ( 305,
      Chapter
        "305"
        "Through the open doorway of the first hut, you can see the body of a charcoal burner lying face down on the rough stone floor. He has been murdered, stabbed in the back by a spear. All his furniture and belongings have been smashed and broken and not one piece remains intact.\nThis is the evil handiwork of Giaks without any doubt, for they delight in the destruction of all things. A quick check of the other huts reveals a similar story of murder and wreckage. In the last hut that you search, you discover a Giak Spear-proof of your suspicions. You may keep this Weapon if you wish.\nMore determined than ever now to succeed in your mission, you continue along the track.\n"
        (CanTake (Weapon Spear) 1 (NoDecision (Goto 105)))
    ),
    ( 306,
      Chapter
        "306"
        "The sound of battle gradually fades behind you. Suddenly, you are pulled to the ground. Three Drakkarim have dropped from a tree above. You struggle but it is useless for there are too many of them for you and they are very strong.\nThe last thing that you hear is the vicious snarls of Drakkarim as they raise their spears.\nYour life and your mission end here.\n"
        (NoDecision GameLost)
    ),
    ( 307,
      Chapter
        "307"
        "Your climb is swift and easy. It reminds you of the many trees that you climbed and explored near Toran as a child, when you wanted to pick fruit or to look out over the beautiful countryside of Sommerlund.\nPushing open the treehouse door, you see an old hermit huddled in the corner of the small cabin. A look of great relief spreads across his face as he recognizes your green Kai cloak. He tells you that this area is full of Giaks, and that he has counted over forty Kraan flying over his home in the last three hours. They were heading east.\nHe scurries over to a cupboard and returns with a plate of fresh fruit. You thank him and place the fruit in your Backpack. There is enough for one Meal. The hermit also produces a fine Warhammer and lays it upon a table by the door. \"Your need is greater than mine, Kai Lord,\" he says. \"Please take this trusty Warhammer if you so wish.\"\nYou may take this Weapon only if you exchange it for another Weapon already in your possession, for it is the only defence that the hermit has against the enemy.\nThanking the old man, you carefully descend the tree and continue on your mission.\n"
        ( CanTake
            Meal
            1
            ( Decisions
                [ ("echange warhammer", RemoveItemFrom WeaponSlot 1 (CanTake (Weapon Warhammer) 1 (NoDecision (Goto 213)))),
                  ("do not exchange", NoDecision (Goto 213))
                ]
            )
        )
    ),
    ( 308,
      Chapter
        "308"
        "The stable door is open and you can hear the breathing of a horse from inside the darkened interior. Suddenly, the horse senses your presence and rushes past, knocking you to the ground. You lose 1 ENDURANCE point.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Animal Kinship, turn to 122.", Conditional (HasDiscipline AnimalKinship) (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 1})] (Goto 122)))),
              ("If you wish to chase after the runaway horse, turn to 233.", NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 1})] (Goto 233)))
            ]
        )
    ),
    ( 309,
      Chapter
        "309"
        "You have taken less than ten paces when the raven squawks a warning to the stranger. Turning to face you, the robed creature utters a piercing screech that freezes your blood and grips your stomach with fear and panic. It is a Vordak, a lieutenant of the Darklords and one of the undead. Within seconds, a host of Giaks appear at its side and attack you. You fight bravely but you are greatly outnumbered.\nThe last thing you remember is the icy grasp of the Vordak's skeletal fingers as they close around your throat.\nYour life and your mission end here.\n"
        (NoDecision GameLost)
    ),
    ( 310,
      Chapter
        "310"
        "You notice a small faded sign on the wall of a building opposite.\nYou remember that the royal court sessions are held within the citadel and you are sure that the west road must lead there.\n"
        (NoDecision (Goto 37))
    ),
    ( 311,
      Chapter
        "311"
        "The hillside is steep and the earth is loose and slippery. You chance a swift glance over your shoulder and see the two Giaks emerge from the woods. They start to climb after you. About halfway from the peak of the hill, you spot a cave to your right, almost totally hidden by a landslide.\n"
        ( Decisions
            [ ("If you have the Kai Discipline of Camouflage, turn to 324.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 324))),
              ("otherwise", Conditional (Not (HasDiscipline Camouflage)) (Decisions [("If you wish to hide in the cave, turn to 279.", NoDecision (Goto 279)), ("If you wish to avoid the cave and continue your climb to the peak, turn to 47.", NoDecision (Goto 47))]))
            ]
        )
    ),
    ( 312,
      Chapter
        "312"
        "You curse your ill luck. It seems that nature and the Darklords have conspired against you, but it does not shake your determination to reach the King.\nWiping the sticky mud from your clothes, you turn and press on into the forest.\n"
        (NoDecision (Goto 299))
    ),
    ( 313,
      Chapter
        "313"
        "Wiping the foul Giak blood from your weapon, you quickly descend the hillside before the Kraan spots its dead riders. Many times you lose your footing on the loose rocks, falling several feet.\nDeduct 1 ENDURANCE point for cuts and bruises to your legs.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 1})] (Goto 248)))
    ),
    ( 314,
      Chapter
        "314"
        "It takes you nearly an hour to reach the citadel. When you arrive you find that the citizens of Holmgard are in panic and confusion. Your escort approaches the armoured guards at the main entrance and tells them of your urgent message for the King.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(7 % 10, Goto 341), (3 % 10, Goto 98)]))
    ),
    ( 315,
      Chapter
        "315"
        "Wrapped in a bundle of women's clothing is a small velvet purse containing 6 Gold Crowns and a Tablet of Perfumed Soap. You may take these items and continue your journey.\n"
        (CanTake Gold 6 (NoDecision (Goto 213)))
    ),
    ( 316,
      Chapter
        "316"
        "In your haste to avoid the enemy, you catch your foot in a tree root and you are pitched head over heels in a tumble of dust and leaves. Crashing through the undergrowth at the base of the hill, you quickly pick up your weapon and run into the thick forest. The Kraan is no longer circling above, but you can make out the silhouette of two Giaks on the peak of the hill behind.\nWiping the grime from your eyes, you wince as you discover a large bruise on your forehead. Without delay, you run deeper into the safety of the forest.\n"
        (NoDecision (Goto 331))
    ),
    ( 317,
      Chapter
        "317"
        "Instinctively you dive away from the stairs, and land on the stone floor. Your quick reactions have saved your life, for a vast granite block has fallen from the ceiling and crushed the steps, just in front of the lockplate!\nShaken but still in one piece, you get to your feet. A shaft of dull grey light is seeping into the chamber from above, where the stone block was. Through a hole in the ceiling you can see a tangle of graveweed and the cloudy sky above. You clamber out of the tomb and head for the arched south gate of the necropolis as fast as possible. The pointed log walls of the city's outer defence works are now visible.\n"
        (NoDecision (Goto 61))
    ),
    ( 318,
      Chapter
        "318"
        "Two soldiers and a sergeant run towards you, their crossbows aimed at your head. As they get nearer, they recognize your Kai cloak and a look of relief spreads across their faces.\n\"My Lord,\" says the sergeant, \"where are the other Kai Masters? We are in desperate need of their wisdom. The Darklords press us most cruelly and our casualties are high.\"\nYou inform the brave soldier of the fate of your kinsmen, and the urgency of your mission to see the King. He takes you back to the barges where an officer accompanies you on horseback towards the high walls and the main gate of Holmgard.\n"
        (NoDecision (Goto 129))
    ),
    ( 319,
      Chapter
        "319"
        "The slimy creature lets out a long, ghastly death-cry and collapses. You are near to panic and scramble to your feet, grabbing what you think to be your belt from the jaws of the dead beast. You can see light in the far distance, and you sprint for it as fast as you can. When you finally emerge into the daylight, you fall onto the leafy ground and fight for breath in painful gasps.\nSlowly sitting upright, you notice that you are still wearing your belt-you had not lost it after all. What you grabbed from the jaw of the Burrowcrawler was a leather strap with a small pouch and a sheathed Dagger halfway along it. You break open the clasp to find it contains 20 Gold Crowns. You may take both the Dagger and the Crowns if you are able to.\nFeeling a little better now, you gather your Equipment together and push on eastwards into the forest.\n"
        (CanTake Gold 20 (CanTake (Weapon Dagger) 1 (NoDecision (Goto 157))))
    ),
    ( 320,
      Chapter
        "320"
        "As you race across the open field towards the wood, a Kraan dives at you and claws your arm. Before you can fight back, it has flown off again, shrieking with cold malice.\nYou enter the wood, but you have lost 2 ENDURANCE points.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 264)))
    ),
    ( 321,
      Chapter
        "321"
        "You walk for nearly an hour along the twisting river's edge. Beyond the next turn you can hear the faint noise of battle. You carefully climb a steep hillock to get a better view of the area.\n"
        (NoDecision (Goto 273))
    ),
    ( 322,
      Chapter
        "322"
        "After what seems an eternity of struggling, you reach the peak of the steep hill. Behind you, above the canopy of trees, you can see the still smouldering remains of the monastery. To the north, a column of jet-black smoke rises high into the sky. Small orange tongues of flame flicker at its base. Your heart sinks as you realize that the port of Toran is ablaze.\nSuddenly, a piercing cry above warns you that a Kraan is about to attack. It is about thirty yards away and diving for the kill.\n"
        ( Decisions
            [ ("If you are going to stand and fight it as it swoops down, turn to 17.", NoDecision (Goto 17)),
              ("If you are going to evade its attack and slide down the other side of the hill, away from the Kraan, turn to 89.", NoDecision (Goto 89))
            ]
        )
    ),
    ( 323,
      Chapter
        "323"
        "From the top of the tower you can see above the trees in all directions. Far to the north, a column of jet-black smoke rises high into the sky. Small orange tongues of flame flicker at its base. Your heart sinks as you realize that the port of Toran is ablaze. From the southwest, the wind carries the noise of battle. It is close; no more than five miles at most.\nOn the floor of the watchtower is a large oblong box.\n"
        ( Decisions
            [ ("If you wish to open this box, turn to 290.", NoDecision (Goto 290)),
              ("If you would prefer to descend the ladder and leave the tower, taking care to use only the good rungs, turn to 140.", NoDecision (Goto 140))
            ]
        )
    ),
    ( 324,
      Chapter
        "324"
        "You pull up your hood and drop down behind the rocks that litter the mouth of the cave. Holding your breath, you curl up into a tight ball, and completely cover yourself with your green cloak. Only a few minutes later the Giaks clamber over the rocky ledge outside, their evil yellow eyes furtively searching every crevice of the hillside.\nThen cursing in their strange tongue, they leave the ledge and start to climb towards the peak. You silently thank your old Masters for teaching you the Kai Discipline of Camouflage-it has probably saved your life on this occasion.\n"
        ( Decisions
            [ ("If you wish to explore the cave, turn to 33.", NoDecision (Goto 33)),
              ("If you wish to leave and descend the hill in case the Giaks return, turn to 248.", NoDecision (Goto 248))
            ]
        )
    ),
    ( 325,
      Chapter
        "325"
        "Upon seeing you emerge from the woods, the Giak officer shouts \"Ogot! Ogot!\" to his cowering troops, who flee the ruins and run to the safety of the forest.\nShaking his mailed fist at you, the black-clad Giak screams, \"RANEG ROGAG OK-ORGADAKA OKAK ROGAG GAJ!\" before leaving. Surveying the scene of battle, you count over fifteen Giak dead lying among the broken pillars of Raumas.\nThe young wizard wipes his brow and walks towards you, his hand extended in friendship.\n"
        (NoDecision (Goto 349))
    ),
    ( 326,
      Chapter
        "326"
        "You carefully insert the Key and turn it clockwise. You hear a dull *click*-the Key works. You lift out the pin, and the large granite door slowly swings towards you on hidden hinges. The grey half-light of the Graveyard floods into the tomb. The exit is overgrown with graveweed and you suffer many cuts to your face and arms as you fight your way through to the surface.\nLooking back, you see the tomb door slowly close and a cruel inhuman laugh seems to rise out of the very ground on which you stand. In blind panic, you race through the eerie necropolis towards the south gate.\n"
        (NoDecision (Goto 61))
    ),
    ( 327,
      Chapter
        "327"
        "Within a few minutes, you can see the Kraan hovering over a hilltop behind you. At a quick count you can make out at least sixteen of these horrible creatures, each of which has at least two Giaks riding upon its back. They are armed with long spears and wear tall pointed helmets of dull bronze. You hear the excited grunts of the Giaks. They have spotted you.\nYou jump for the entrance of the tunnel some twenty-five feet below, but your boot gets caught in a thorny briar and you hang helplessly upside down-weaponless and vulnerable. Fortunately for you the end is swift: As the first Giak lance pierces your heart, death is instantaneous.\nYour life and your mission end here.\n"
        (NoDecision GameLost)
    ),
    ( 328,
      Chapter
        "328"
        "As the creature dies, its body slowly dissolves into a vile green liquid. You notice that the grass and plants beneath the smoking fluid are beginning to shrivel and die. But a large valuable looking Gem lies on the ground near to the decaying body.\n"
        ( Decisions
            [ ("If you wish to take the Gem, turn to 76.", NoDecision (Goto 76)),
              ("If you would rather leave as quickly as possible, turn to 118.", NoDecision (Goto 118))
            ]
        )
    ),
    ( 329,
      Chapter
        "329"
        "As you descend the ridge towards the Graveyard of the Ancients, you are aware of a strange mist and cloud that swirls all around this grey and forbidding place, blocking the sun and keeping the Graveyard in perpetual gloom.\nA creeping chill seems to penetrate your very bones. Your horse becomes startled and no matter how you urge him on, he refuses to go any nearer to this dreadful place. So you must leave your horse and press on by foot.\n"
        (NoDecision (Goto 284))
    ),
    ( 330,
      Chapter
        "330"
        "Fatigued by your exertions, you stop to rest for a few minutes at a fallen tree. You notice a large bundle, beneath the trunk.\n"
        ( Decisions
            [ ("If you wish to examine the contents of the bundle, turn to 315.", NoDecision (Goto 315)),
              ("If you wish to leave it where it is and continue your mission, turn to 213.", NoDecision (Goto 213))
            ]
        )
    ),
    ( 331,
      Chapter
        "331"
        "Surrounded by thorny briars and closely packed roots, you see the entrance of a tunnel disappearing into the hillside beyond. It is approximately seven feet in height and just over ten feet wide. As you get closer, you can feel a slight breeze coming from the inky blackness. If the other end of this tunnel emerges on the far side of the hill, it could save you many hours of difficult climbing. But it could also harbour unknown danger.\n"
        ( Decisions
            [ ("If you wish to enter the tunnel, turn to 170.", NoDecision (Goto 170)),
              ("If you would prefer to climb the hillside, turn to 280.", NoDecision (Goto 280))
            ]
        )
    ),
    ( 332,
      Chapter
        "332"
        "You walk for nearly ten minutes along a dark and winding corridor, and then start to climb a steep staircase to a small wooden door. The man presses a secret catch and the door opens. You enter a large, plushly decorated bedroom with a huge marble bath that takes up one corner of the room. The man suggests that you refresh yourself here whilst he seeks an audience with the King.\nYou quickly bathe and change into some white robes that have been left out on a large marble table. Shortly, the man returns and leads you through a long corridor lined with exquisite tapestries. You finally arrive at a large door guarded by two soldiers wearing silver armour.\nYou are about to meet the King.\n"
        (NoDecision (Goto 350))
    ),
    ( 333,
      Chapter
        "333"
        "You have cut your way through the thick undergrowth for nearly half an hour when you hear the beat of wings high above the trees. Looking up you can just make out the shape of a Kraan approaching from the north. It is one of the monsters that attacked the monastery and on its back are two grey-skinned creatures armed with long spears.\nThese are Mountain Giaks-evil servants of the Darklords, full of hatred and malice. Many centuries ago, their ancestors were used by the Darklords to build the infernal city of Helgedad, which lies in the volcanic wastelands beyond the Durncrag range of mountains. The construction of the city was long and torturous and only the strongest of the Giaks survived the heat and poisonous atmosphere of Helgedad.\nHidden by the trees, you freeze, keeping absolutely still as the Kraan passes overhead and disappears towards the south. When you are sure that it has gone, you move off once again into the forest.\n"
        (NoDecision (Goto 131))
    ),
    ( 334,
      Chapter
        "334"
        "As the stream vanishes up into the rocky hillside, you can see on the track above four soldiers and their officer. They wear the uniform of the King's army.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Sixth Sense, turn to 48.", Conditional (HasDiscipline SixthSense) (NoDecision (Goto 48))),
              ("If you wish to use the Kai Discipline of Camouflage and wait for them to pass, turn to 73.", Conditional (HasDiscipline Camouflage) (NoDecision (Goto 73))),
              ("If you wish to approach them, turn to 162.", NoDecision (Goto 162))
            ]
        )
    ),
    ( 335,
      Chapter
        "335"
        "As you approach, the black bird flies off above the trees and soon disappears from view. You search the tree on which it was perched but find nothing unusual. Rather than waste any more precious time, you continue off along the track.\n"
        (NoDecision (Goto 121))
    ),
    ( 336,
      Chapter
        "336"
        "You rush into the clearing and take the Giaks completely by surprise. Without a moment's hesitation, you strike out at the one nearest to you. He is dead before his body hits the ground. The other Giaks unsheathe their curved swords and attack you. You must fight them one at a time.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Giak 1", _fcombatSkill = CombatSkill {getCombatSkill = 14}, _fendurance = Endurance {getEndurance = 11}, _fightMod = []}) (Fight (FightDetails {_opponent = "Giak 2", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 11}, _fightMod = []}) (Goto 117))))
    ),
    ( 337,
      Chapter
        "337"
        "Just as you remove the ornate pin, a loud crack deafens you.\nPick a number from the Random Number Table.\n"
        (NoDecision (Randomly [(1 % 2, Goto 219), (1 % 2, Goto 317)]))
    ),
    ( 338,
      Chapter
        "338"
        "When you awake, you find yourself lying at the foot of a steep slope in a tangle of long grasses. Your Backpack and Weapons are missing and your head aches violently. You cannot tell how long you have been unconscious, but you realize that time is running out and you must press on. Standing up, you notice your Backpack and Weapon on the slope above. They must have broken free when you fell. You quickly retrieve them and move off into the trees ahead.\n"
        (NoDecision (Goto 113))
    ),
    ( 339,
      Chapter
        "339"
        "You quickly sidestep just as a long dagger shatters the glass top of the counter. A swarthy youth is attacking you and you must fight him.\n"
        (EvadeFight 0 7 (FightDetails {_opponent = "Robber", _fcombatSkill = CombatSkill {getCombatSkill = 13}, _fendurance = Endurance {getEndurance = 20}, _fightMod = [Timed 4 (OnNotYetWon 203)]}) (Goto 94))
    ),
    ( 340,
      Chapter
        "340"
        "You gallop forward to meet the oncoming Doomwolf and rider, your Weapon raised to strike. The Giak sees you and unsheathes his scimitar. You must fight both Giak and Doomwolf as one enemy.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Giak + Doomwolf", _fcombatSkill = CombatSkill {getCombatSkill = 14}, _fendurance = Endurance {getEndurance = 24}, _fightMod = []}) (Goto 193)))
    ),
    ( 341,
      Chapter
        "341"
        "The guards do not believe your story and refuse to let you enter. Your escort disappears into the crowd and you are left alone to find your way in this confused city.\nShocked, and then dejected by such a rebuff, you are carried along by the crowds until you find yourself at the entrance to the Guildhall. It stands at one side of the Guild Bridge which crosses the River Eledil near where it joins the Holmgulf.\n"
        ( Decisions
            [ ("If you wish to use the Kai Discipline of Tracking, turn to 310.", Conditional (HasDiscipline Tracking) (NoDecision (Goto 310))),
              ("If you wish to enter the Guildhall, turn to 210.", NoDecision (Goto 210)),
              ("If you wish to search for another route into the citadel, turn to 37.", NoDecision (Goto 37))
            ]
        )
    ),
    ( 342,
      Chapter
        "342"
        "As your voice echoes through the trees, the stranger slowly turns to face you. Your heart pounds and your blood freezes as you realize that the stranger is not human. It is a Vordak, a hideous lieutenant of the Darklords and one of the undead. A piercing scream fills your ears, and the creature raises a huge black mace above its head and charges at you. Frozen with horror, you can also feel the Vordak attacking you with the force of its mind.\nDeduct 2 points from your COMBAT SKILL unless you have the Kai Discipline of Mindshield. You must fight this creature. It is immune to Mindblast.\n"
        (NoDecision (Fight (FightDetails {_opponent = "Vordak", _fcombatSkill = CombatSkill {getCombatSkill = 18}, _fendurance = Endurance {getEndurance = 26}, _fightMod = [EnemyMindblast, MindblastImmune]}) (Goto 123)))
    ),
    ( 343,
      Chapter
        "343"
        "You are held by the mass of tangled branches and roots. Eventually you free your right hand, grab your weapon, and hack your way slowly through the foliage to the clearer forest beyond. Your cloak is torn in several places and your left arm is cut and badly bruised above the elbow.\n"
        (NoDecision (Simple [DamagePlayer (Endurance {getEndurance = 2})] (Goto 213)))
    ),
    ( 344,
      Chapter
        "344"
        "You are weak and dizzy. You can no longer feel your legs and they refuse to bear your weight. You try to crawl for the door but the robber jumps on you and pins you to the ground.\n"
        (NoDecision (Goto 60))
    ),
    ( 345,
      Chapter
        "345"
        "You pull up the hood of your green Kai cloak and hold your breath as the Kraan circles above. After a few minutes, you hear the frantic curses of the Giaks. The beating of Kraan wings fades, as they disappear towards the west. Your quick reactions have saved you from capture and likely death.\n"
        ( Decisions
            [ ("You can now return to the track, by turning to 272.", NoDecision (Goto 272)),
              ("Or push on under cover of the trees. Turn to 19.", NoDecision (Goto 19))
            ]
        )
    ),
    ( 346,
      Chapter
        "346"
        "Lodged deep in the rib cage of the skeleton is a Spear. It is in good condition and you may take it if you wish and are able to.\n"
        (NoDecision (Goto 14))
    ),
    ( 347,
      Chapter
        "347"
        "The trees start to thin out, and just ahead you can make out the silhouette of an old log cabin beneath an oak tree. This hut seems to have been abandoned and there is little of apparent value left behind. Opening a small chest near the main door, you discover bunches of twigs that have been tied together with strong twine. One end of each bundle has been coated with pitch. They are Torches. Next to the chest are a Short Sword and a Tinderbox. You may take them and a Torch if you wish but make sure that you mark them on your Action Chart.\nClosing the door of the cabin, you head off along an overgrown path towards the northeast.\n"
        (CanTake (Weapon ShortSword) 1 (CanTake (GenBackpack (GenCounter 0)) 1 (NoDecision (Goto 103))))
    ),
    ( 348,
      Chapter
        "348"
        "Raising your boot to kick away the dead snake, your heart skips a beat as you realize that it was a Red Marshviper. There is no known cure for its venomous bite! You decide that to go any further in this direction would be suicide. Carefully retracing your steps, you eventually reach firm ground and continue on your mission.\n"
        (NoDecision (Goto 95))
    ),
    ( 349,
      Chapter
        "349"
        "He is a young blond-haired youth with deep brooding eyes. His face is lined with exhaustion and the grime of battle, and his long sky-blue robes bear evidence of living rough in the wilds. He shakes your hand and bows. \"My eternal thanks, Kai Lord. My powers are nearly drained. Had you not come to my aid, I fear I would have ended my days atop a Giak lance.\"\nHe is weak and unsteady on his feet. You take his arm and sit him down upon a fallen pillar where you listen intently to what he has to say.\n\"My name is Banedon. I am journeyman to the Brotherhood of the Crystal Star, which is the Magicians' Guild of Toran. My Guildmaster has sent me to your monastery with this urgent message.\" He removes a vellum envelope from inside his robes and hands it to you.\n\"As you see, I have opened the letter and read its contents. When the war started, I was on the highway with two travelling companions. The Kraan attacked us and we lost each other in the forest during our escape.\"\nThe letter is a warning to the Kai Lords that the Darklords have mustered a vast army beyond the Durncrag Range. The Guildmaster urges the Kai to cancel the celebrations of Fehmarn and prepare for war.\n\"I fear we were betrayed,\" says Banedon, his head bowed in sorrow.\n\"One of my order, a brother called Vonotar, had explored the forbidden mysteries of the Black Art. Ten days ago he denounced the Brotherhood and killed one of our Elders. He has since disappeared. It is rumoured that he now aids the Darklords.\"\nYou tell Banedon what has happened at the monastery, and of your mission to warn the King. Silently, he removes a gold chain from around his neck and hands it to you. On the chain is a small Crystal Star Pendant. \"It is the symbol of my Brotherhood, and we are both truly brothers in this hour of darkness. It is a talisman of good fortune-may it protect you on your road ahead.\"\nYou thank him, place the chain around your neck, and slip the Crystal Star inside your shirt. (Remember to mark this on your Action Chart.)\nBanedon bids you farewell. \"We must leave this place lest the Giaks return with more of their loathsome kind to put an end to us. I must return to my Guild. I bid you farewell, my brother. May the luck of the gods go with you.\"\n"
        (CanTake (GenSpecial (GenCounter 3)) 1 (NoDecision (Goto 293)))
    ),
    ( 350,
      Chapter
        "350"
        "You enter the Chamber of State, a magnificent hall decorated lavishly in white and gold. The King and his closest advisers are studying a large map spread upon a marble plinth in the centre of the chamber. Their faces are lined with worry and concentration. A silence fills the hall as you tell of the death of your kinsmen and of your perilous journey to the citadel. As you finish your story, the King approaches and takes your right hand in his.\n\"Lone Wolf, you have selfless courage: the quality of a true Kai Lord. Your journey here has been one of great peril and although your news comes as a grievous blow, the spirit of your determination is like a beacon of hope to us all in this dark hour. You have brought great honour to the memory of your Masters, and for that we praise you.\"\nYou receive the praise and heartfelt thanks of the entire hall-an honour that brings a certain redness to your young face. The King raises his hand and all the voices cease.\n\"You have done all that Sommerlund could have asked of a loyal son, but she is greatly in need of you still. The Darklords are powerful once more and their ambition knows no bounds. Our only hope lies within Durenor with the power that once defeated the Darklords an age ago. Lone Wolf, you are the last of the Kai-you have the skills. Will you journey to Durenor and return with the Sommerswerd, the sword of the sun? Only with that gift of the gods may we crush this evil and save our land.\"\nIf you wish to accept the quest of the Sommerswerd, begin your adventure with Book 2 of the <<Lone Wolf>> adventures:\n<<Fire on the Water>>\n"
        (NoDecision GameWon)
    )
  ]