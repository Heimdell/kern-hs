
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

module Parser where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe

import Input
import Machine
import ParsingTools

-- Разобрать символ начала имени.
--
nameCharStart :: Parser Char
nameCharStart = satisfy \c ->
  not (isSpace c || isDigit c || elem c "(){}[]\"'.,;#")

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

-- Разобрать имя.
--
name :: Parser String
name = token "name" do
  pure (:) <*> nameCharStart <*> many nameChar

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
data Brace = Round | Square | Curly

-- Разобрать открывающую скобку.
--
opening :: Parser Brace
opening = token "([{"
  $   Round  <$ slug "("
  <|> Square <$ slug "["
  <|> Curly  <$ slug "{"

-- Разобрать закрывающую скобку.
--
closing :: Brace -> Parser ()
closing = \case
  Round  -> token ")" do slug ")"
  Square -> token "]" do slug "]"
  Curly  -> token "}" do slug "}"

-- Разобрать атомарное выражение.
--
atom :: Parser Atom
atom = foldr (<|>) empty
  [ Number <$> float
  , String <$> stringLiteral
  , Name   <$> name
  , Bool   <$> boolean
  ]

-- Разобрать выражение с квотацией.
--
quoted :: Parser Value -> Parser Value
quoted end = do
    "quotation" <?> token "'" do char '\''
    q <- quoted end
    return $ Cons (Atom (Name "quote")) (Cons q Nil)
  <|>
    end

-- Разобрать выражение.
--
value :: Parser Value
value = quoted (list <|> Atom <$> atom)

-- Разобрать список.
--
list :: Parser Value
list = foldr (<|>) empty
  [ Nil <$ token "()" do slug "()"
  , do
      br <- opening
      s  <- chain
      closing br
      return s
  ]

-- Разобрать последовательность выражений.
--
chain :: Parser Value
chain = do
  x  <- value
  xs <- many value
  ms <- optional do
    "dotted pair" <?> token "." do slug "."
    value
  return $ Cons x (foldr Cons (fromMaybe Nil ms) xs)
