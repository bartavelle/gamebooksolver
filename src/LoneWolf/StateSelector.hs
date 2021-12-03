{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LoneWolf.StateSelector (Log (..), Selector (..), selectChar, parseSelector) where

import Control.Lens ((%~), (&), (^?), _Just, _Left)
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Char (isSpace)
import Data.Void (Void)
import LoneWolf.Chapter (ChapterId)
import LoneWolf.Character
import LoneWolf.Rules
import Text.Megaparsec (MonadParsec (eof, takeWhile1P, try), Parsec, errorBundlePretty, parse, (<|>))
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal, symbol)
import Text.Read (readMaybe)

data Log a
  = P a
  | And (Log a) (Log a)
  | Or (Log a) (Log a)
  | Not (Log a)
  deriving (Show, Read, Eq)

matcher :: (a -> Bool) -> Log a -> Bool
matcher p l =
  case l of
    P x -> p x
    And a b -> matcher p a && matcher p b
    Or a b -> matcher p a || matcher p b
    Not x -> not (matcher p x)

data Selector
  = HasItem Item
  | WithHp Ordering Endurance
  | InChapter ChapterId
  deriving (Show, Read, Eq)

selectChar :: Log Selector -> NextStep -> Bool
selectChar sel ns = matcher m sel
  where
    (cid, cvar) = case ns of
      HasLost c -> (Just c, Nothing)
      HasWon v -> (Nothing, Just v)
      NewChapter c v _ -> (Just c, Just v)
    inv = cvar ^? _Just . equipment
    end = cvar ^? _Just . curendurance
    m (HasItem i) = maybe False (hasItem i) inv
    m (WithHp ord target) = maybe False (\e -> compare e target == ord) end
    m (InChapter c) = Just c == cid

type Parser = Parsec Void String

selector :: Parser Selector
selector = try $ do
  s <- lx (takeWhile1P Nothing (not . isSpace))
  case s of
    "hp" -> WithHp <$> lx ordering <*> lx decimal
    "chapter" -> InChapter <$> lx decimal
    _ | Just itm <- readMaybe s -> pure (HasItem itm)
    _ | Just w <- readMaybe s -> pure (HasItem (Weapon w))
    _ -> fail ("Unknown keyword " ++ s)

parens :: Parser a -> Parser a
parens p = symbol space "(" *> p <* symbol space ")"

lx :: Parser a -> Parser a
lx p = p <* space

logparser :: forall a. Parser a -> Parser (Log a)
logparser sub = lx (makeExprParser term table)
  where
    term = lx (parens (logparser sub) <|> (P <$> sub))
    table :: [[Operator Parser (Log a)]]
    table =
      [ [prefix "!" Not, prefix "not" Not],
        [binary "or" Or, binary "||" Or],
        [binary "and" And, binary "&&" And]
      ]
    binary name f = InfixL (f <$ symbol space name)
    prefix name f = Prefix (f <$ symbol space name)

ordering :: Parser Ordering
ordering = (GT <$ symbol space ">") <|> (LT <$ symbol space "<" <|> (EQ <$ symbol space "="))

parseSelector :: String -> Either String (Log Selector)
parseSelector s = parse (space *> logparser selector <* eof) "input" s & _Left %~ errorBundlePretty