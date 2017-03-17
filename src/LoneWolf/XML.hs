{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module LoneWolf.XML where

import Control.Lens hiding (children)
import Control.Applicative
import Control.Monad
import Text.XML.Expat.Tree
import Text.XML.Expat.Lens.Generic
import qualified Data.ByteString.Lazy as L
import Data.List
import Text.Megaparsec.String
import Text.Megaparsec (parseMaybe, string, sepBy1, skipMany, anyChar)
import Data.Char (isDigit)
import Data.Data.Lens

import LoneWolf.Chapter
import LoneWolf.Character

data AC = Jmp String ChapterId
        | Combat String Int Int
        | Lost
        deriving (Show, Eq)

makePrisms ''AC

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : (traverse . _2 %~ (x:)) (select xs)

loadXML :: FilePath -> IO String
loadXML xmlpath = unlines . intersperse "    , " . map (mkChapterModule . parseChapter) . toListOf getChapters . fst . parse defaultParseOptions <$> L.readFile xmlpath
   where
      mkChapterModule (cid, Chapter ttl desc pch ) =
          "  (" ++ show cid ++ ", Chapter " ++ unwords [show ttl, show desc, "(", show pch, ")"] ++ ")"
      getChapters = children . traverse . parameterized "id" "title" ./ named "data" ./ parameterized "id" "numbered" ./ named "data" ./ named "section" . parameterized "class" "numbered"

parseChapter :: UNode String -> (ChapterId, Chapter)
parseChapter nd = (rcid, Chapter cid (unlines desc) gdec)
  where
   cid = nd ^. children . traverse . named "meta" ./ named "title" ./ text
   rcid = read cid
   dt = nd ^. children . traverse . named "data" . children
   (desc, choices) = foldMap accumChoices dt
   computedDecision = case toDec choices of
                          Decisions [(_, o)] -> o
                          o -> o
   evadeCombat :: Rounds -> Decision
   evadeCombat nrounds = case reverse choices of
                             (Jmp _ winfight : Jmp _ evadefight : _) -> case extractFightDetails choices of
                                                                           [] -> error ("evadeCombat " ++ show choices)
                                                                           (f:fs) -> EvadeFight nrounds evadefight f $ foldr Fight (Goto winfight) fs
                             _ -> error ("evadeCombat: " ++ show choices)
   threeCond c = computedDecision & _Decisions . ix 0 . _2 %~ Conditional c
                                  & _Decisions . ix 1 . _2 %~ Conditional (Not c)
                                  & _Decisions . ix 2 . _2 %~ Conditional (Not c)
   addEffect :: SimpleOutcome -> Decision -> Decision
   addEffect s = outcomePlate %~ \d -> case d of
                                           Simple lst nxt -> Simple (s:lst) nxt
                                           _ -> Simple [s] d
   gdec = case rcid of
          350 -> NoDecision GameWon
          10 -> CanTake TicketVol2 1 computedDecision
          12 -> Decisions [ ("with sixth sense", Conditional (HasDiscipline SixthSense) (NoDecision (Randomly [(2/10, Goto 58), (3/10, Goto 167), (5/10, Goto 329)])) )
                          , ("no sixth sense", Conditional (HasDiscipline SixthSense) (NoDecision (Randomly [(4/10, Goto 58), (3/10, Goto 167), (3/10, Goto 329)])) )
                          ]
          15 -> CanTake Backpack 1
                    $ CanTake (Weapon BroadSword) 1
                    $ CanTake (Weapon Mace) 1
                    $ CanTake (Weapon Quarterstaff) 1
                    $ CanTake HealingPotion 1
                    $ CanTake Meal 3
                    $ CanTake Gold 12
                    $ NoDecision (Goto 244)
          17 -> computedDecision & addEffect (DamagePlayer 5)
          21 -> NoDecision (Randomly [ (1/10, Simple [GainItem Gold (n * 3 - 1)] (Goto 314)) | n <- [1..10]])
          27 -> NoDecision (Simple [HealPlayer 2] (Goto 312))
          29 -> NoDecision (Simple [DamagePlayer 2] (Goto 222))
          31 -> computedDecision & addEffect (HealPlayer 6)
          32 -> computedDecision & addEffect (MustEat Hunt)
          35 -> threeCond (HasDiscipline Tracking)
          36 -> NoDecision (Conditionally [ (HasItem Laumspur 1, Goto 145)
                                          , (HasDiscipline Healing, Goto 210)
                                          , (botherwise, Goto 275)
                                          ] )
          37 -> computedDecision & addEffect (MustEat Hunt)
          39 -> NoDecision (Conditionally [ (HasItem TicketVol2 1, Goto 346), (botherwise, Goto 156) ])
          40 -> computedDecision & addEffect FullHeal & addEffect (GainItem PotentPotion 1)
          41 -> computedDecision & _Decisions . ix 0 . _2 . _NoDecision %~ Simple [HealPlayer 1]
          47 -> NoDecision (Conditionally [ (HasItem PasswordVol2 1, Goto 111), (botherwise, Goto 307) ])
          52 -> NoDecision (Conditionally [ (HasItem (Weapon MagicSpear) 1, Goto 338), (botherwise, Goto 234) ])
          55 -> Canbuy (Weapon BroadSword) 12 computedDecision
          57 -> NoDecision (Randomly [ (1/10, Simple [LoseItem Gold n] (Goto 282)) | n <- [1..10] ])
          59 -> NoDecision (Conditionally [ (HasItem (Weapon MagicSpear) 1, Goto 332), (botherwise, Goto 311) ])
          60 -> computedDecision & outcomePlate . _Fight . _1 . fightMod %~ (Timed 2 PlayerInvulnerable :)
          62 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasItem SealHammerdalVol2 1)
                                 & _Decisions . ix 1 . _2 %~ Conditional (HasItem DocumentsVol2 1)
                                 & _Decisions . ix 2 . _2 %~ Conditional (Not (HasItem DocumentsVol2 1) .&&. Not (HasItem SealHammerdalVol2 1))
          63 -> threeCond (HasDiscipline AnimalKinship)
          64 -> threeCond (HasDiscipline SixthSense)
          66 -> computedDecision & outcomePlate . _Fight . _1 . fightMod %~ ([MindblastImmune, Undead] ++)
          69 -> NoDecision (Conditionally [ (HasDiscipline MindShield, Goto 311)
                                          , (botherwise, Simple [DamagePlayer 2] (Goto 311))
                                          ] )
          70 -> NoDecision (Goto 44) -- no crystal star pendant
          72 -> computedDecision & addEffect (HealPlayer 1)
          75 -> computedDecision & _Decisions . ix 0 . _2 .~ Conditional (HasItem Gold 10) (NoDecision (Simple [LoseItem Gold 10, GainItem WhitePassVol2 1] (Goto 142)))
          76 -> computedDecision & CanTake Gold 2 & CanTake (Weapon Dagger) 1
          78 -> computedDecision & outcomePlate %~ Simple [DamagePlayer 1, LoseItem ChainMail 99]
          79 -> computedDecision & CanTake (Weapon Sommerswerd) 1
          80 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasItem SealHammerdalVol2 1)
          86 -> computedDecision & CanTake Gold 3 & CanTake (Weapon Mace) 1
          88 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasDiscipline Camouflage)
          90 -> evadeCombat 0
          91 -> Decisions $ nub $ do
              let titems = [ Backpack, Weapon Quarterstaff, Meal, Meal, Weapon Dagger ]
              (item1, remaining) <- select titems
              item2 <- remaining
              guard (item1 < item2 || (item1 == Meal && item2 == Meal))
              return ("Take " ++ show item1 ++ " and " ++ show item2, CanTake item1 1 (CanTake item2 1 (NoDecision (Goto 245))))
          102 -> threeCond (HasDiscipline Tracking)
          103 -> Decisions [ ("Consume now", NoDecision (Simple [HealPlayer 3] (Goto 249)))
                           , ("Take Laumspur", CanTake Laumspur 1 (NoDecision (Goto 249)))
                           ]
          106 -> CanTake (Weapon MagicSpear) 1 (computedDecision & outcomePlate . _Fight . _1 . fightMod %~ (EnemyMindblast :))
          108 -> computedDecision & addEffect (DamagePlayer 2)
          110 -> evadeCombat 0
          116 -> NoDecision (Randomly [ (1/10, Simple [GainItem Gold (n + 4)] (Goto 314)) | n <- [1..10]])
          117 -> computedDecision & _Decisions . ix 0 . _2 .~ moneyCond 3 (Goto 37)
                                  & _Decisions . ix 1 . _2 .~ moneyCond 1 (Goto 148)
          118 -> threeCond (HasDiscipline AnimalKinship)
          122 -> NoDecision (Conditionally [ (HasDiscipline SixthSense, Goto 96), (botherwise, Randomly [(1/2, Goto 46), (1/2, Goto 112)]) ])
          123 -> computedDecision & CanTake (Weapon Sommerswerd) 1
          124 -> computedDecision & CanTake (Weapon Dagger) 1 & CanTake Gold 42 & CanTake (Weapon ShortSword) 1
          127 -> computedDecision & addEffect (MustEat Hunt)
          128 -> computedDecision & outcomePlate . _Fight . _1 . fightMod %~ (Undead :)
          131 -> evadeCombat 0 & biplate . fightMod %~ (BareHanded :)
          132 -> CanTake (Weapon Spear) 1 computedDecision
          134 -> NoDecision (Conditionally [ (HasItem (Weapon MagicSpear) 1, Goto 38), (botherwise, Goto 304) ])
          136 -> computedDecision & _Decisions . ix 0 . _2 .~ moneyCond 20 (Goto 10)
          139 -> CanTake Meal 2 computedDecision
          141 -> computedDecision & outcomePlate %~ Simple [DamagePlayer 2, LoseItem ChainMail 99]
          142 -> CanTake WhitePassVol2 1 computedDecision
          144 -> CanTake Meal 2 (computedDecision & addEffect (LoseItemKind [PouchSlot]))
          145 -> computedDecision & addEffect (DamagePlayer 5)
          148 -> computedDecision & addEffect (MustEat Hunt)
          150 -> computedDecision & addEffect (MustEat Hunt)
          154 -> computedDecision & addEffect (DamagePlayer 2)
          160 -> NoDecision (Goto 268) -- shunt annoying choices
          157 -> evadeCombat 0
          162 -> evadeCombat 0
          164 -> threeCond (HasDiscipline SixthSense)
          165 -> computedDecision & addEffect (LoseItem SealHammerdalVol2 1)
          168 -> NoDecision (Conditionally [ (HasItem Gold 1, Simple [LoseItem Gold 1] (Goto 314))
                                           , (botherwise, Goto 25)
                                           ] )
          176 -> threeCond (HasDiscipline SixthSense)
          181 -> computedDecision & Canbuy (Weapon Sword) 4
                                  & Canbuy (Weapon Dagger) 2
                                  & Canbuy (Weapon ShortSword) 3
                                  & Canbuy (Weapon Warhammer) 6
                                  & Canbuy (Weapon Spear) 5
                                  & Canbuy (Weapon Mace) 4
                                  & Canbuy Backpack 1
          185 -> evadeCombat 0
          187 -> computedDecision & CanTake (Weapon Spear) 2 & CanTake (Weapon Spear) 2 & CanTake Gold 6
          189 -> computedDecision & addEffect (DamagePlayer 2)
          194 -> computedDecision & addEffect (LoseItemKind [PouchSlot, BackpackSlot, WeaponSlot, SpecialSlot])
          195 -> NoDecision (Conditionally [ (HasItem Gold 1, Simple [LoseItem Gold 1] (Goto 249))
                                           , (botherwise, Goto 50)
                                           ] )
          196 -> computedDecision & addEffect (LoseItem SealHammerdalVol2 1)
          198 -> computedDecision & addEffect (DamagePlayer 1)
          217 -> computedDecision & _Decisions . ix 0 . _2 .~ moneyCond 1 (Goto 199)
          219 -> computedDecision & addEffect (DamagePlayer 3)
          220 -> CanTake Gold 23 computedDecision
          226 -> computedDecision & _Decisions . ix 0 . _2 .~ moneyCond 2 (Goto 56)
          231 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasDiscipline Tracking)
                                  & _Decisions . ix 1 . _2 %~ Conditional (Not (HasDiscipline Tracking))
                                  & _Decisions . ix 2 . _2 %~ Conditional (Not (HasDiscipline Tracking))
                                  & _Decisions . ix 3 . _2 %~ Conditional (Not (HasDiscipline Tracking))
                                  & _Decisions . ix 4 . _2 %~ Conditional (Not (HasDiscipline Tracking))
                                  & CanTake SealHammerdalVol2 1
                                  & CanTake Gold 5
                                  & CanTake (Weapon Dagger) 1
          232 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasDiscipline SixthSense)
                                  & _Decisions . ix 3 . _2 %~ Conditional (HasItem SealHammerdalVol2 1)
          233 -> computedDecision & _Decisions . ix 0 . _2 .~ moneyCond 3 (Goto 37)
                                  & _Decisions . ix 1 . _2 .~ moneyCond 1 (Goto 148)

          235 -> CanTake (Weapon ShortSword) 1 computedDecision
          238 -> CanTake Gold 1 (Special Cartwheel)
          240 -> Decisions [ ("Has healing", Conditional (HasDiscipline Healing) (computedDecision & addEffect FullHeal ))
                           , ("Hasn't healing", Conditional (Not (HasDiscipline Healing)) (computedDecision & addEffect HalfHeal ))
                           ]
          244 -> threeCond (HasDiscipline Tracking)
          246 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (HasItem WhitePassVol2 1)
                                  & _Decisions . ix 1 . _2 %~ Conditional (HasItem RedPassVol2 1)
          250 -> computedDecision & _Decisions . ix 2 . _2 %~ Conditional (HasItem SealHammerdalVol2 1)
          254 -> threeCond (HasDiscipline SixthSense)
          258 -> threeCond (HasDiscipline SixthSense)
          260 -> CanTake (Weapon Sword) 1 computedDecision
          262 -> computedDecision & CanTake (Weapon Sword) 1
                                  & CanTake (Weapon Mace) 1
                                  & CanTake (Weapon Quarterstaff) 1
                                  & CanTake Gold 6
                                  & CanTake Meal 6
          263 -> CanTake RedPassVol2 1 computedDecision
          265 -> threeCond (HasDiscipline Tracking)
          266 -> computedDecision & Cansell (Weapon Sword) 3
                                  & Cansell (Weapon Dagger) 1
                                  & Cansell (Weapon BroadSword) 6
                                  & Cansell (Weapon ShortSword) 2
                                  & Cansell (Weapon Warhammer) 5
                                  & Cansell (Weapon Spear) 4
                                  & Cansell (Weapon Mace) 3
                                  & Cansell (Weapon Axe) 2
                                  & Cansell (Weapon Quarterstaff) 2
                                  & Canbuy (Weapon Sword) 4
                                  & Canbuy (Weapon Dagger) 2
                                  & Canbuy (Weapon BroadSword) 7
                                  & Canbuy (Weapon ShortSword) 3
                                  & Canbuy (Weapon Warhammer) 6
                                  & Canbuy (Weapon Spear) 5
                                  & Canbuy (Weapon Mace) 4
                                  & Canbuy (Weapon Axe) 3
                                  & Canbuy (Weapon Quarterstaff) 3
          268 -> evadeCombat 2
          271 -> threeCond (HasDiscipline Camouflage)
          274 -> computedDecision & CanTake (Weapon Sword) 1
                                  & CanTake (Weapon Mace) 1
                                  & CanTake Gold 6
          276 -> let [fd] = extractFightDetails choices
                 in  Decisions [ ("mindblast", Conditional (HasDiscipline MindBlast) (NoDecision (Goto 14)))
                               , ("otherwise", Conditional (Not (HasDiscipline MindBlast)) (NoDecision (Fight (fd & fightMod %~ (FakeFight 192 :)) (Goto 305))))
                               ]
          283 -> computedDecision & Canbuy (Weapon Sword) 4
                                  & Canbuy (Weapon Dagger) 2
                                  & Canbuy (Weapon BroadSword) 6
                                  & Canbuy (Weapon Spear) 5
                                  & Canbuy Meal 2
                                  & Canbuy Meal 2
                                  & Canbuy Meal 2
                                  & Canbuy Backpack 1
          284 -> computedDecision & addEffect (MustEat Hunt)
          289 -> computedDecision & _Decisions . ix 0 . _2 . outcomePlate %~ Simple [LoseItem SealHammerdalVol2 1, GainItem Gold 40]
          299 -> computedDecision & _Decisions . ix 0 . _2 . outcomePlate %~ Simple [LoseItem (Weapon MagicSpear) 1]
                                  & _Decisions . ix 1 . _2 %~ Conditional (HasItem (Weapon MagicSpear) 1)
          296 -> evadeCombat 0
          298 -> evadeCombat 0 & biplate . fightMod %~ (BareHanded :)
          301 -> computedDecision & CanTake (Weapon ShortSword) 1
                                  & CanTake (Weapon Dagger) 1
                                  & CanTake Gold 3
          302 -> NoDecision (Goto 15) -- identical chapter
          305 -> CanTake Gold 5 computedDecision
          306 -> computedDecision & outcomePlate . _Fight . _1 . fightMod %~ (DoubleDamage :)
          307 -> computedDecision & _Decisions . ix 1 . _2 %~ Conditional (HasItem SealHammerdalVol2 1)
          308 -> Special Portholes
          313 -> computedDecision & addEffect (DamagePlayer 4)
          314 -> threeCond (HasDiscipline Hunting) & _Decisions . ix 2 . _2 . outcomePlate %~ Simple [MustEat NoHunt]
          315 -> threeCond (HasDiscipline MindOverMatter)
          321 -> Decisions [ ("Has food", Conditional (HasItem Meal 1) (computedDecision & addEffect (LoseItem Meal 1)))
                           , ("No food", Conditional (Not (HasItem Meal 1)) (computedDecision & addEffect (DamagePlayer 2) ))
                           ]
          327 -> CanTake RedPassVol2 1 (computedDecision & addEffect (LoseItem Gold 6))
          328 -> computedDecision & _Decisions . ix 0 . _2 %~ Conditional (Always False)
                                  & _Decisions . ix 1 . _2 %~ Conditional (HasDiscipline Tracking)
                                  & _Decisions . ix 2 . _2 %~ Conditional (Not (HasDiscipline Tracking))
                                  & _Decisions . ix 3 . _2 %~ Conditional (Not (HasDiscipline Tracking))
          329 -> CanTake Gold 10 computedDecision
          330 -> computedDecision & addEffect (DamagePlayer 5)
          331 -> computedDecision & CanTake (Weapon Sword) 1
                                  & CanTake (Weapon Dagger) 1
                                  & CanTake Gold 3
          332 -> evadeCombat 0 & _EvadeFight . _3 . fightMod %~ (EnemyMindblast :)
          334 -> threeCond (HasDiscipline Tracking)
          337 -> threeCond (HasDiscipline Hunting) & addEffect (LoseItemKind [BackpackSlot, WeaponSlot])
          338 -> computedDecision & addEffect (DamagePlayer 2)
          339 -> NoDecision (Conditionally [ (HasItem Gold 1, Simple [LoseItem Gold 1] (Goto 249))
                                           , (botherwise, Goto 50)
                                           ] )
          342 -> computedDecision & _Decisions . ix 0 . _2 .~ moneyCond 1 (Goto 72)
                                  & _Decisions . ix 1 . _2 .~ moneyCond 2 (Goto 56)
          346 -> Decisions [ ("Buy a meal, and a room", moneyCond 2 (Goto 280))
                           , ("Just the room", moneyCond 1 (Simple [MustEat NoHunt] (Goto 280)))
                           , ("Nothing", NoDecision (Simple [MustEat NoHunt] (Goto 205)))
                           ]
          347 -> computedDecision & addEffect (DamagePlayer 1)
          348 -> evadeCombat 2
          _ -> computedDecision

parseCondition :: String -> Maybe BoolCond
parseCondition = parseMaybe pcond
 where
  disc :: Parser Discipline
  disc =   (SixthSense <$ string "Sixth Sense")
       <|> (AnimalKinship <$ string "Animal Kinship")
       <|> (MindOverMatter <$ string "Mind Over Matter")
       <|> (Tracking <$ string "Tracking")
       <|> (Camouflage <$ string "Camouflage")
       <|> (Healing <$ string "Healing")
  pcond :: Parser BoolCond
  pcond = do
    void $ string "If you have the Kai Discipline of "
    ds <- disc `sepBy1` string " or "
    let cnd = case ds of
                  [x] -> HasDiscipline x
                  [x,y] -> COr (HasDiscipline x) (HasDiscipline y)
                  _ -> error ("cnd: " ++  show ds)
    skipMany (anyChar)
    return cnd

toDec :: [AC] -> Decision
toDec [Lost] = NoDecision GameLost
toDec a | all (has _Jmp) a = parseJump (jumps a)
  where
    parseJump jmps = case (mapM randomRange jmps, jmps) of
                         (Just rnd, _) -> NoDecision (Randomly rnd)
                         (_, [(s1, cid1), (s2, cid2)]) -> case parseCondition s1 of
                                                              Nothing -> Decisions [(s1, NoDecision cid1), (s2, NoDecision cid2)]
                                                              Just cnd -> NoDecision (Conditionally [(cnd, cid1), (botherwise, cid2)])
                         _ -> Decisions $ map (\(s,cid) -> (s, NoDecision cid)) jmps
    randomRange (s, cid) = case (msum (map (\p -> stripPrefix p s) [ "If the number that you have picked is "
                                                                   , "If the number you have picked is "
                                                                   , "If the number you have chosen is "
                                                                   , "If the number is "
                                                                   , "If it is "
                                                                   ] ) ) of
                               Just ( x : '-' : y : _ ) -> Just (fromIntegral (1 + read [y] - read [x] :: Int) / 10, cid)
                               Just ( x : ',' : _) | isDigit x -> Just (1, cid)
                               _ -> Nothing

toDec events =
   if null fds
       then Decisions $ jmps & traverse . _2 %~ NoDecision
       else case jmps of
                [(_, goto)] -> NoDecision (foldr Fight goto fds)
                _ -> error ("toDec: " ++ show jmps)
  where
    fds = extractFightDetails events
    jmps = events ^.. traverse . _Jmp . to (\(s,d) -> (s, Goto d))

extractFightDetails :: [AC] -> [FightDetails]
extractFightDetails = toListOf (traverse . _Combat . to mkFd)
  where
    mkFd (d, cs, end) = FightDetails d (CombatSkill cs) (Endurance end) mods
      where
        mods | "Helghast" `isSuffixOf` d = [Undead, MindblastImmune]
             | d == "Dorier + Ganon" || d == "Ganon + Dorier" = [MindblastImmune, Timed 1 (CombatBonus 2)]
             | otherwise = []

jumps :: [AC] -> [(String, ChapterOutcome)]
jumps =  map (\(Jmp s d) -> (s, Goto d))

accumChoices :: UNode String -> ([String], [AC])
accumChoices n
    = case n of
          Text _ -> mempty
          Element "illustration" _ _ -> mempty
          Element "p" [] chlds -> ( [foldMap jdesc chlds], [])
          Element "li" [] chlds -> ( [concatMap ((" * " ++) .  view text) chlds], [])
          Element "ul" [] chlds -> foldMap accumChoices (filter isElement chlds)
          Element "signpost" [] content -> ([foldMap jdesc content], [])
          Element "choice" [("idref",dest)] choiceDesc
            -> ([], [ toJump dest choiceDesc ])
          Element "combat" [] [Element "enemy" [] enemyDesc,Element "enemy-attribute" [("class","combatskill")] [Text cs],Element "enemy-attribute" [("class","endurance")] [Text end]]
            -> ([], [ Combat (foldMap jdesc enemyDesc) (read cs) (read end) ] )
          Element "deadend" _ [Text d] -> ([d], [Lost])
          _ -> error ("accumChoices:" ++ show n)

toJump :: String -> [UNode String] -> AC
toJump s nodes = case reverse nodes of
                   (Text "." : Element "link-text" [] [Text d] : rst) -> Jmp (foldMap jdesc (reverse rst) ++ d ++ ".") (read (drop 4 s))
                   _ -> error ("toJump: " ++ show nodes)

jdesc :: UNode String -> String
jdesc u = case u of
              Text f                                         -> f
              Element "ch.apos" [] []                        -> "'"
              Element "ch.plus" [] []                        -> "+"
              Element "ch.endash" [] []                      -> "-"
              Element "ch.emdash" [] []                      -> "-"
              Element "ch.ellips" [] []                      -> "..."
              Element "ch.thinspace" [] []                   -> " "
              Element "ch.eacute" [] []                      -> "é"
              Element "ch.frac12" [] []                      -> "1/2"
              Element "footref" _ _                          -> ""
              Element "typ" [("class","attribute")] [Text t] -> t
              Element "a" _ [Text t]                         -> t
              Element "onomatopoeia" _ [Text t]              -> "*" ++ t ++ "*"
              Element "quote" [] cnt                         -> "\"" ++ foldMap jdesc cnt ++ "\""
              Element "bookref" _ cnt                        -> foldMap jdesc cnt
              Element "em" [] cnt                            -> "*" ++ foldMap jdesc cnt ++ "*"
              Element "cite" [] cnt                          -> "<<" ++ foldMap jdesc cnt ++ ">>"
              _                                              -> error ("jdesc: " ++ show u)
