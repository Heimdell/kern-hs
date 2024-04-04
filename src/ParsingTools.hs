{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

module ParsingTools where

import Control.Applicative
import Control.Monad (ap, liftM, void)
import Data.Set qualified as Set
import Data.Maybe

import Input

import Debug.Trace

-- Результат вызова парсера.
--
data ParseResult a = ParseResult
  { result    :: Maybe a         -- удалось что-нибудь отпарсить?
  , expected  :: Set.Set String  -- что собирались отпарсить?
  , remaining :: Maybe Stream    -- был ли поглощён текст, и если да, то где мы сейчас?
  }

-- Парсер.
--
newtype Parser a = Parser
  { run :: Stream -> ParseResult a
  }

instance Monad Parser where
  -- Продолжить разбор, опираясь на результат парсера.
  --
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser run >>= continue = Parser \input -> do
    let res = run input
    case res.result of
      Nothing -> res { result = Nothing }
      Just a -> do
        let res1 = (continue a).run (fromMaybe input res.remaining)
        case res1.remaining of
          Nothing -> res1
            { expected  = res.expected <> res1.expected
            , remaining = res.remaining
            }
          Just _  -> res1


instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser \input -> ParseResult
    { result    = Just a
    , expected  = Set.empty
    , remaining = Nothing
    }

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

instance Functor Parser where
  fmap = liftM

instance Alternative Parser where
  empty :: Parser a
  empty = Parser \input -> ParseResult
    { result    = Nothing
    , expected  = Set.empty
    , remaining = Nothing
    }

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser left <|> Parser right = Parser \input -> do
    let res = left input
    case (res.result, res.remaining) of
      (Nothing, Nothing) -> do
        let res1 = right input
        case res1.remaining of
          Nothing -> res1 { expected = res.expected <> res1.expected }
          Just _  -> res1

      _ -> res

-- Запуск парсера.
--
runParser :: Parser a -> Stream -> Either String a
runParser parser input = do
  let res       = parser.run input
  let remaining = fromMaybe input res.remaining
  case res.result of
    Just a  -> return a
    Nothing ->
      Left do
        let pre = prefix remaining
        pre <> " " <> currentLine remaining <> "\n" <>
          pre <> " " <> replicate (remaining.position.column - 1) ' ' <> "^\n" <>
          pre <> " " <> "expected: " <> show (Set.toList res.expected)

-- Тестовый запуск парсера.
--
testParser :: Parser a -> Stream -> a
testParser = (either (error . ("\n" <>)) id .) . runParser

-- Поднять предикат над символом до парсера.
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy isGood = Parser \input ->
  case uncons input of
    Just (char, rest) | isGood char -> ParseResult
      { result    = Just char
      , expected  = Set.empty
      , remaining = Just rest
      }
    _ -> ParseResult
      { result    = Nothing
      , expected  = Set.empty
      , remaining = Nothing
      }

-- Разобрать один конкретный символ.
--
char :: Char -> Parser Char
char = satisfy . (==)

-- Разобрать один из символов переданной строки.
--
oneOf :: String -> Parser Char
oneOf = foldr (<|>) empty . map char

-- Успех, если ввод закончился.
--
endOfStream :: Parser ()
endOfStream = "end of file" <?> Parser \input ->
  case uncons input of
    Just (char, rest) -> ParseResult
      { result    = Nothing
      , expected  = Set.empty
      , remaining = Nothing
      }
    _ -> ParseResult
      { result    = Just ()
      , expected  = Set.empty
      , remaining = Nothing
      }

-- Внести изменения в результат разбора.
--
manipulate :: (ParseResult a -> ParseResult b) -> Parser a -> Parser b
manipulate f (Parser run) = Parser (f . run)

-- Включить откат для парсера
--
backtrack :: Parser a -> Parser a
backtrack = manipulate \parse@ParseResult {result, remaining} ->
  case result of
    Nothing -> parse {remaining = Nothing}
    _       -> parse

-- Подменить сообщение об ошибке.
--
(<?>) :: String -> Parser a -> Parser a
(<?>) msg = manipulate \parse@ParseResult {result, remaining} ->
  case (result, remaining) of
    (Nothing, Nothing) -> parse {expected = Set.singleton msg}
    _                  -> parse

-- Отрицание парсера.
--
notFollowedBy :: Parser a -> Parser ()
notFollowedBy = manipulate \parse@ParseResult {result} ->
  case result of
    Nothing -> parse {remaining = Nothing, result = Just ()}
    _       -> parse {remaining = Nothing, result = Nothing}

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x  <- p
  xs <- many (sep >> p)
  return (x : xs)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = fromMaybe [] <$> optional do
  sepBy1 p sep

-- Разобрать заданную последовательность символов.
--
slug :: String -> Parser ()
slug str = str <?> void do
  backtrack do
    traverse (satisfy . (==)) str

-- Разобрать комментарий-однострочник.
--
lineComment :: String -> Parser ()
lineComment start = do
  slug start
  many do satisfy ('\n' /=)
  satisfy ('\n' ==)
  pure ()

getPosition :: Parser Position
getPosition = Parser \input -> ParseResult
  { result    = Just input.position
  , remaining = Nothing
  , expected  = Set.empty
  }

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty
