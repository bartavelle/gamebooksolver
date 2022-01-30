{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module LoneWolf.XML.Gen where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens hiding (children)
import Control.Monad (msum)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import LoneWolf.Chapter
import LoneWolf.Character
import Text.Megaparsec (MonadParsec (try), Parsec, parseMaybe, takeRest)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.XML.Expat.Lens.Generic (children, named, text, (./))
import Text.XML.Expat.Tree (NodeClass (isElement), NodeG (Element, Text), UNode)

data AC
  = Jmp String ChapterId
  | Combat String Int Int [FightModifier]
  | Lost
  deriving (Show, Eq)

makePrisms ''AC

type Parser = Parsec Void String

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : (traverse . _2 %~ (x :)) (select xs)

anyweaponskill :: BoolCond
anyweaponskill = foldl1 COr (map (HasDiscipline . WeaponSkill) [minBound .. maxBound])

anyweapon :: BoolCond
anyweapon = foldl1 COr (map (\w -> HasItem (Weapon w) 1) [minBound .. maxBound])

moduleHeader :: Book -> String
moduleHeader b =
  unlines
    [ "module LoneWolf.RawBook." ++ show b ++ " where",
      "",
      "import Data.Ratio ((%))",
      "import LoneWolf.Chapter",
      "import LoneWolf.Character",
      "",
      "chapters :: [(ChapterId, Chapter)]",
      "chapters = ["
    ]

takeItems :: Foldable t => t (Item, Int) -> Decision -> Maybe Decision
takeItems itms d = Just (foldr (uncurry CanTake) d itms)

parseChapterBookGen :: Book -> (ChapterId -> [AC] -> Decision -> Maybe Decision) -> UNode String -> Maybe (ChapterId, Chapter)
parseChapterBookGen book override nd = (rcid,) . Chapter cid (unlines cdesc) <$> override rcid choices computedDecision
  where
    cid = nd ^. children . traverse . named "meta" ./ named "title" ./ text
    rcid = read cid
    dt = nd ^. children . traverse . named "data" . children
    (cdesc, choices) = case traverse accumChoices dt of
      Right x -> mconcat x
      Left rr -> error ("AT(1) " ++ show rcid ++ ": " ++ rr)
    computedDecision = case toDec book choices of
      Right (Decisions [(_, o)]) -> o
      Right o -> o
      Left rr -> error ("AT(2) " ++ show rcid ++ ": " ++ rr)

disc :: Parser Discipline
disc =
  (SixthSense <$ string "Sixth Sense")
    <|> (AnimalKinship <$ string "Animal Kinship")
    <|> (MindOverMatter <$ string "Mind Over Matter")
    <|> (Tracking <$ string "Tracking")
    <|> (Camouflage <$ string "Camouflage")
    <|> (Healing <$ string "Healing")
    <|> (Hunting <$ string "Hunting")
    <|> (MindShield <$ string "Mindshield")

itemp :: Book -> Parser Item
itemp b = case M.lookup b itemNames of
  Nothing -> fail "?"
  Just cnt -> asum (map checkitem (M.toList cnt))
  where
    checkitem (r, nm) = nm <$ string r

-- itemp Book02 =
--   (vordakGem <$ "Vordak Gem")
--     <|> (goldenKey <$ "Golden Key")
--     <|> (silverKey <$ "Silver Key")
-- itemp Book05 =
--   (gaolerKeyB05 <$ "Gaoler's Keys")
--     <|> (Weapon Sommerswerd <$ "Sommerswerd")
--     <|> (copperKeyB05 <$ "Copper Key")
--     <|> (ropeB05 <$ "Rope")
--     <|> (blackCubeB05 <$ "Black Crystal Cube")
--     <|> (blowpipeSleepDart <$ "Blowpipe and Sleep Dart")
--     <|> (oedeHerb <$ "Oede herb")
--     <|> (prismB05 <$ "Prism")
--     <|> (blueStoneTriangleB05 <$ "Blue Stone Triangle")
-- itemp _ = fail "?"

data CondType
  = TInconditional BoolCond
  | TConditional BoolCond
  | NoCond
  deriving (Show)

parseRoundLimit :: String -> Maybe Int
parseRoundLimit = parseMaybe (prs <|> prs_within)
  where
    prs_within :: Parser Int
    prs_within = "If you win the combat within " *> number <* " rounds" <* takeRest
    prs :: Parser Int
    prs =
      ( "If you win and the fight lasts for "
          <|> "If you win and the fight lasts "
          <|> "If you win the combat and the fight lasts "
          <|> "If you win the combat and it lasts for "
          <|> "If you win this combat in "
          <|> "If you win the combat in "
      )
        *> number
        <* ( " rounds of combat or less"
               <|> " rounds or less"
           )
        <* takeRest
    number = decimal <|> (4 <$ "four") <|> (5 <$ "five") <|> (7 <$ "seven")

parseCondition :: Book -> String -> CondType
parseCondition book = fromMaybe NoCond . parseMaybe (TConditional <$> ccond <|> TInconditional <$> incond)
  where
    pitem = fmap (`HasItem` 1) (itemp book)
    hdisc = HasDiscipline <$> disc
    pdisc = try pdiscor <|> try pdiscand <|> hdisc
    pdiscor = COr <$> hdisc <* (" or of " <|> " or ") <*> hdisc
    pdiscand = CAnd <$> hdisc <* " and " <*> hdisc

    ccond :: Parser BoolCond
    ccond =
      "If you wish to use the Kai discipline of " *> pdisc <* takeRest
        <|> "If you wish to use the Kai Discipline of " *> pdisc <* takeRest
        <|> "If you possess and wish to use the Kai Discipline of " *> pdisc <* takeRest
        <|> "If you possess either the Kai Discipline of " *> pdisc <* takeRest
        <|> "If you have the Kai Discipline of either " *> pdisc <* takeRest
        <|> "If you wish to use your Kai Discipline of " *> pdisc <* takeRest
        <|> ("If you have a " <|> "If you possess some ") *> pitem <* (" and wish to " <|> " and decide ") <* takeRest

    incond :: Parser BoolCond
    incond =
      "If you possess the Kai Disciplines of both " *> pdisc <* takeRest
        <|> "If you have the Kai Discipline of " *> pdisc <* takeRest
        <|> "If you have the Kai Discipline either of " *> pdisc <* takeRest
        <|> "If you possess the Kai Disciplines of " *> pdisc <* takeRest
        <|> "If you possess the Kai Discipline of " *> pdisc <* takeRest
        <|> try ("If you possess a " *> pitem <* takeRest)
        <|> try ("If you possess an " *> pitem <* takeRest)
        <|> try ("If you possess the " *> pitem <* takeRest)

toDec :: Book -> [AC] -> Either String Decision
toDec _ [Lost] = pure (NoDecision GameLost)
toDec book a | all (has _Jmp) a = parseJump (jumps a)
  where
    parseJump jmps = case (mapM randomRange jmps, map (\(s, o) -> (s, o, parseCondition book s)) jmps) of
      (Just rnd, _) -> pure (NoDecision (Randomly rnd))
      (_, rm) -> parseConds rm
    parseConds :: [(String, ChapterOutcome, CondType)] -> Either String Decision
    parseConds lst =
      case lst of
        (dsc, co, TInconditional bc) : rm -> do
          subs <- parseConds rm
          pure $
            Decisions
              [ (dsc, Conditional bc (NoDecision co)),
                ("otherwise", Conditional (Not bc) subs)
              ]
        _ -> pure $ Decisions (map simpleCond lst)
    simpleCond :: (String, ChapterOutcome, CondType) -> (String, Decision)
    simpleCond (dsc, oc, ct) =
      case ct of
        TInconditional bc -> ("!!!I!!! " ++ dsc, Conditional bc (NoDecision oc))
        TConditional bc -> (dsc, Conditional bc (NoDecision oc))
        _ -> (dsc, NoDecision oc)
toDec _ events =
  if null fds
    then pure $ Decisions $ jmps & traverse . _2 %~ NoDecision
    else case jmps of
      [(_, goto)] -> pure $ NoDecision (foldr Fight goto fds)
      [(desclose, Goto onlose), (twin, Goto onwin)]
        | [f] <- fds,
          "If you lose any ENDURANCE points" `isInfixOf` desclose && "without losing any ENDURANCE" `isInfixOf` twin ->
          pure (NoDecision (Fight (f & fightMod .~ [OnDamage onlose]) (Goto onwin)))
      [(_, Goto onwin), (tevade, Goto onevade)]
        | "evade" `isInfixOf` tevade,
          f : fs <- fds ->
          pure $ EvadeFight 0 onevade f (foldr Fight (Goto onwin) fs)
      [(tevade, Goto onevade), (_, Goto onwin)]
        | "evade" `isInfixOf` tevade || "avoid combat" `isInfixOf` tevade,
          f : fs <- fds ->
          pure $ EvadeFight 0 onevade f (foldr Fight (Goto onwin) fs)
      [(tlessthan, Goto onless), (_, Goto onmore)]
        | Just roundlimit <- parseRoundLimit tlessthan -> pure $ NoDecision (foldr Fight (Goto onless) (fds & traverse . fightMod %~ (Timed roundlimit (OnNotYetWon onmore) :)))
      [(_, Goto onmore), (tlessthan, Goto onless)]
        | Just roundlimit <- parseRoundLimit tlessthan -> pure $ NoDecision (foldr Fight (Goto onless) (fds & traverse . fightMod %~ (Timed roundlimit (OnNotYetWon onmore) :)))
      [(tlost, glost), (twin, gwin), (teq, geq)]
        | [f] <- fds,
          ("lose more ENDURANCE points than" `isInfixOf` tlost || "higher ENDURANCE point loss than the enemy" `isInfixOf` tlost)
            && ("enemy loses more ENDURANCE points than you" `isInfixOf` twin || "loses more ENDURANCE points than you" `isInfixOf` twin)
            && "exactly the same number of ENDURANCE" `isInfixOf` teq ->
          pure (NoDecision (OneRound f glost geq gwin))
      _ | Just rroll <- mapM randomRange jmps -> pure $ NoDecision (foldr Fight (Randomly rroll) fds)
      _ -> Left ("toDec: " ++ show jmps)
  where
    fds = extractFightDetails events
    jmps = events ^.. traverse . _Jmp . to (second Goto)

randomRange :: (String, b) -> Maybe (Rational, b)
randomRange (s, cid) = case msum
  ( map
      (`stripPrefix` s)
      [ "If the number that you have picked is ",
        "If the number you pick is ",
        "If the number picked is ",
        "If the number you have picked is ",
        "If the number you have chosen is ",
        "If the number is ",
        "If you have chosen a number that is ",
        "If you have picked a number that is ",
        "If you have picked a number ",
        "If you have picked ",
        "If it is ",
        "If you pick "
      ]
  ) of
  Just (x : '-' : y : _) -> Just (fromIntegral (1 + read [y] - read [x] :: Int) / 10, cid)
  Just (x : ',' : _) | isDigit x -> Just (1 / 10, cid)
  _ -> Nothing

extractFightDetails :: [AC] -> [FightDetails]
extractFightDetails = toListOf (traverse . _Combat . to mkFd)
  where
    mkFd (d, cs, end, mds) = FightDetails d (CombatSkill cs) (Endurance (fromIntegral end)) (mds ++ mods)
      where
        mods
          | "Helghast" `isSuffixOf` d = [Undead, MindblastImmune]
          | d == "Dorier + Ganon" || d == "Ganon + Dorier" = [MindblastImmune, Timed 1 (CombatBonus 2)]
          | otherwise = []

jumps :: [AC] -> [(String, ChapterOutcome)]
jumps = map (\(Jmp s d) -> (s, Goto d))

accumChoices :: UNode String -> Either String ([String], [AC])
accumChoices n =
  case n of
    Text _ -> pure mempty
    Element "choice" [] chlds -> (\i -> ([concat i], [])) <$> traverse jdesc chlds
    Element "poetry" _ _ -> pure mempty
    Element "blockquote" _ _ -> pure mempty
    Element "illustration" _ _ -> pure mempty
    Element "p" [] chlds -> (\i -> ([concat i], [])) <$> traverse jdesc chlds
    Element "li" [] chlds -> pure ([concatMap ((" * " ++) . view text) chlds], [])
    Element "ul" [] chlds -> mconcat <$> traverse accumChoices (filter isElement chlds)
    Element "signpost" [] content -> (\i -> ([concat i], [])) <$> traverse jdesc content
    Element "puzzle" _ chlds -> (\i -> ([concat i], [])) <$> traverse jdesc chlds
    Element "choice" [("idref", dest)] choiceDesc -> (\i -> ([], [i])) <$> toJump dest choiceDesc
    Element "combat" [] [Element "enemy" [] enemyDesc, Element "enemy-attribute" [("class", "combatskill")] [Text cs], Element "enemy-attribute" [("class", "endurance")] [Text end]] ->
      (\i -> ([], [Combat (concat i) (read cs) (read end) []])) <$> traverse jdesc enemyDesc
    Element "deadend" _ (Text d : _) -> pure ([d], [Lost])
    _ -> Left ("accumChoices:" ++ show n)

toJump :: String -> [UNode String] -> Either String AC
toJump s nodes = case dropWhile isFootRef (reverse nodes) of
  (Text concl : Element "link-text" [] [Text d] : rst)
    | " and " `isPrefixOf` concl || concl `elem` [".", "?"] || ". (Ignore any wounds" `isPrefixOf` concl -> do
      cnt <- traverse jdesc (reverse rst)
      pure (Jmp (concat cnt ++ d ++ ".") (read (drop 4 s)))
    | concl == " to choose your next course of action." && d == "return to 10" -> Right undefined
    | otherwise -> Left ("toJump, concl = " ++ show concl ++ " text=" ++ show d)
  [Text " as a Special Item.)", Element "a" [("idref", "action")] [Text "Action Chart"], Text ". (Put it on your wrist and mark it on your ", Element "link-text" [] [Text "turn to 236"], Text "If you wish to take the Gold Bracelet, "] -> Right undefined
  rr -> Left ("toJump: " ++ show rr)
  where
    isFootRef (Element "footref" _ _) = True
    isFootRef _ = False

jdesc :: UNode String -> Either String String
jdesc u = case u of
  Text f -> pure f
  Element "choose" _ _ -> pure mempty
  Element "ch.acirc" [] [] -> pure "â"
  Element "ch.apos" [] [] -> pure "'"
  Element "ch.plus" [] [] -> pure "+"
  Element "ch.endash" [] [] -> pure "-"
  Element "ch.minus" [] [] -> pure "-"
  Element "ch.emdash" [] [] -> pure "-"
  Element "ch.ellips" [] [] -> pure "..."
  Element "ch.thinspace" [] [] -> pure " "
  Element "ch.eacute" [] [] -> pure "é"
  Element "ch.frac12" [] [] -> pure "1/2"
  Element "footref" _ _ -> pure ""
  Element "typ" [("class", "attribute")] [Text t] -> pure t
  Element "a" _ [Text t] -> pure t
  Element "onomatopoeia" _ [Text t] -> pure ("*" ++ t ++ "*")
  Element "quote" [] cnt -> (\i -> "\"" ++ concat i ++ "\"") <$> traverse jdesc cnt
  Element "bookref" _ cnt -> mconcat <$> traverse jdesc cnt
  Element "em" [] cnt -> (\i -> "*" ++ concat i ++ "*") <$> traverse jdesc cnt
  Element "cite" [] cnt -> (\i -> "<<" ++ concat i ++ ">>") <$> traverse jdesc cnt
  Element "foreign" _ cnt -> concat <$> traverse jdesc cnt
  Element "strong" _ cnt -> concat <$> traverse jdesc cnt
  _ -> Left ("jdesc: " ++ show u)