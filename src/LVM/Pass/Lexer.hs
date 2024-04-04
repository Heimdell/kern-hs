
module LVM.Pass.Lexer where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe

import Input
import LVM.Name
import LVM.Phase.Raw
import ParsingTools

reserved name = name `elem` words "let match def do lambda"

-- Разобрать символ начала имени.
--
nameCharStart :: Parser Char
nameCharStart = satisfy \c ->
  not (isSpace c || isDigit c || elem c "!%$#(){}[]\"'.,;#")

-- Разобрать символ продолжения имени.
--
nameChar :: Parser Char
nameChar = satisfy \c ->
  not (isSpace c || elem c "(){}[]\";")

-- Разобрать пробельный символ.
--
space :: Parser ()
space = void (satisfy isSpace) <|> lineComment ";"

-- Сделать токен из парсера.
--
token :: String -> Parser a -> Parser a
token msg p = msg <?> (p <* many space)

tok :: String -> Parser ()
tok str = token str (slug str)

-- Разобрать имя.
--
name :: Parser Name
name = token "name" do
  (pos, n) <- backtrack do
    pos <- getPosition
    h <- nameCharStart
    t <- many nameChar
    let n = h : t
    guard (not (reserved n))
    return (pos, n)
  return Name {pos, raw = n, index = 0}

-- Разобрать числовой литерал.
--
float :: Parser Double
float = token "number" do
  (s, a) <- backtrack do
    s <- optional do char '-'
    a <- some     do satisfy isDigit
    return (s, a)
  b <- optional do
    char '.'
    some do satisfy isDigit
  return (read (maybe "" pure s <> a <> maybe "" ("." <>) b))

-- Разобрать символьный литерал.
--
charLiteral :: Parser Char
charLiteral = do
    notFollowedBy do oneOf "\\\""
    satisfy (const True)
  <|> do
    char '\\'
    (    '\n' <$ char 'n'
     <|> '\t' <$ char 't'
     <|> '\\' <$ char '\\'
     <|> '\"' <$ char '\"'
     )

-- Разобрать строковый литерал.
--
stringLiteral :: Parser String
stringLiteral = token "string" do
  char '\"' *> many charLiteral <* "closing \"" <?> char '\"'

-- Разобрать булевый литерал.
--
boolean :: Parser Bool
boolean = token "bool" do True <$ slug "#true" <|> False <$ slug "#false"

-- Виды доступных скобок.
--
data Brace
  = Round
  | Square
  | Curly

-- Разобрать открывающую скобку.
--
opening :: Brace -> Parser ()
opening = \case
  Round  -> token "(" do slug "("
  Square -> token "[" do slug "["
  Curly  -> token "{" do slug "{"


-- Разобрать закрывающую скобку.
--
closing :: Brace -> Parser ()
closing = \case
  Round  -> token ")" do slug ")"
  Square -> token "]" do slug "]"
  Curly  -> token "}" do slug "}"

parens :: Brace -> Parser a -> Parser a
parens br p = do
  _   <- opening br
  res <- p
  _   <- closing br
  return res