
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

import Control.Monad
import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import System.Environment
import System.Console.Isocline

import ParsingTools
import Parser
import Machine
import Eval
import Stdlib
import Input

import Data.Void

echo :: LispM Void
echo =
  forever do
    line <- liftIO do readline "KERN"
    case runParser (many space *> value <* endOfStream) $ fromString "stdin" line of
      Left err -> liftIO do putStrLn err
      Right val -> do
        val <- tryError do eval val
        liftIO do either (putStrLn . ("ERROR: " <>) . show) print val

main = do
  getArgs >>= \case
    [] -> do
      _ <- runLispM stdlib echo
      return ()

    [file] -> do
      main1 file

    _ -> do
      error "USAGE: kern | kern FILE"

load :: String -> LispM Void
load fname = do
  stream <- liftIO do fromFile fname
  case runParser (many space *> value <* endOfStream) stream of
    Left err -> liftIO do putStrLn err
    Right val -> do
      val <- tryError do eval val
      liftIO do either (putStrLn . ("ERROR: " <>) . show) print val
  echo

main1 :: String -> IO ()
main1 fname = do
  _ <- runLispM stdlib (load fname)
  return ()
