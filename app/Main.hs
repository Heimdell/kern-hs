
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
import LVM.Pass.Lexer
import LVM.Pass.Parser
import LVM.Pass.Run
import LVM.Prog
import LVM.Phase.Raw
import LVM.Phase.Runtime
import Input

import Polysemy
import Polysemy.Error

echo :: VM m => Sem m ()
echo = do
  line <- liftIO do readlineMaybe "KERN"
  case line of
    Nothing -> return ()
    Just line -> do
      case runParser (many space *> stmt <* endOfStream) $ fromString "stdin" line of
        Left err -> do
          liftIO do putStrLn err
          echo
        Right stmt -> do
          withStmt stmt \case
              Nothing -> echo
              Just val -> do
                liftIO do print val
                echo
            `catch` \(e :: Report) -> do
              liftIO do putStrLn $ "ERROR: " <> show e
              echo

main = do
  setHistory ".kern" 200
  getArgs >>= \case
    [] -> do
      _ <- runVM dispatch echo
      putStrLn "Bye!"

    [file] -> do
      main1 file

    _ -> do
      error "USAGE: kern | kern FILE"

load :: VM m => String -> Sem m a -> Sem m (Maybe a)
load fname k = do
  stream <- liftIO do fromFile fname
  case runParser (many space *> stmts <* endOfStream) stream of
    Left err -> do
      liftIO do putStrLn err
      return Nothing
    Right stmts -> do
      withStmts stmts do
        Just <$> k

main1 :: String -> IO ()
main1 fname = do
  runVM dispatch do
    load fname do
      echo
  return ()
