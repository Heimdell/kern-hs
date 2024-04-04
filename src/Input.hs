{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

module Input where

import Data.Array

-- Поток символов.
--
data Stream = Stream
  { position :: Position
  , source   :: Array Int Char  -- срать я хотел на символы, не влезающие в Char
  }

-- Позиция в потоке.
--
data Position = Position
  { offset, line, column :: Int
  , filename :: String
  }

instance Show Position where
  show pos = pos.filename <> ":" <> show pos.line <> ":" <> show pos.column

-- Следующий символ.
--
uncons :: Stream -> Maybe (Char, Stream)
uncons stream@Stream {position, source}
  | position.offset <= snd (bounds source) = do
      let c = source ! position.offset
      return (c, stream { position = advance c position })
  | otherwise =
      Nothing

-- Переместить позицию на 1 символ.
--
advance :: Char -> Position -> Position
advance '\n' pos = pos
  { offset = pos.offset + 1
  , line   = pos.line   + 1
  , column = 1
  }

advance _ pos = pos
  { offset = pos.offset + 1
  , column = pos.column + 1
  }

-- Извлечь текущую строку из позиции.
--
currentLine :: Stream -> String
currentLine Stream {position, source} = reverse before <> after
  where
    before = trace (position.offset - 1) (-1)
    after  = trace  position.offset        1

    within ix (lo, hi) = ix >= lo && ix <= hi

    trace point delta
      | point `within` bounds source
      && source ! point /= '\n' =
        (source ! point) : trace (point + delta) delta

      | otherwise = []

-- Извлечь строковое представление текущей позиции из потока.
--
prefix :: Stream -> String
prefix Stream {position} =
       position.filename <> ":" <>
  show position.line     <> ":" <>
  show position.column   <> ">"

-- Создать поток из строки.
--
fromString :: FilePath -> String -> Stream
fromString filename source = Stream
  { source = listArray (1, length source) source
  , position = Position
    { offset = 1
    , line   = 1
    , column = 1
    , filename
    }
  }

-- Создать поток из имени файла.
--
fromFile :: FilePath -> IO Stream
fromFile filename = fromString filename <$> readFile filename

test :: String
test = prefix tester <> " " <> currentLine tester
  where
    stream   = fromString "test.nowhere" "abcde\nfghik\nlmnop"
    position = stream.position { offset = 9 }
    tester   = stream {position}