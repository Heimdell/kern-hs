
module LVM.Pass.Parser where

import Control.Applicative
import Data.Fix
import Data.Map qualified as Map

import ParsingTools

import LVM.Pass.Lexer
import LVM.Prog
import LVM.Name
import LVM.Phase.Raw

import Debug.Trace

{-
data Prog_ self
  = Var Name

  | Lam [Name] self
  | App self [self]

  | Let [(Name, self)] self

  | Sym Name [self]
  | Match self (Map.Map Name ([Name], self))

  | Rec [(Name, self)]
  | Get self Name

  | BIF String
  | Num Double
  | Str String
  deriving stock (Show, Functor, Foldable, Traversable)
-}

stmts :: Parser [Stmt Prog]
stmts = many do
  stmt <* tok ";"

stmt :: Parser (Stmt Prog)
stmt = choice [def, force]
  where
    def = do
      _     <- tok "let"
      decls <- do
          n <- name
          _ <- tok "="
          p <- prog
          return (n, p)
        `sepBy` tok ","
      return $ Let decls

    force = do
      p <- prog
      return $ Force p

prog :: Parser Prog
prog = app
  where
    atom = choice
      [ num
      , var
      , bif
      , str
      , rec_
      , parens Round prog
      , lam
      , sym
      , match
      ]
    app = do
      pos  <- getPosition
      core <- get
      args <- many get
      return $ foldl (\f arg -> pos :> App f arg) core args

    get = do
      pos  <- getPosition
      core <- atom
      fs   <- many do
        tok "."
        name
      return $ foldl (\box field -> pos :> Get box field) core fs

    pin p = pure (:>) <*> getPosition <*> p

    do_ :: Parser Prog
    do_ = pin do
      _     <- tok "do"
      ss    <- stmts
      _     <- tok "return"
      res   <- prog
      return $ Do ss res

    var :: Parser Prog
    var = pin do Var <$> name

    bif :: Parser Prog
    bif = pin do
      _ <- char '$'
      n <- name
      _ <- tok "/"
      i <- count
      return $ BIF n.raw i

    num :: Parser Prog
    num = pin do Num <$> float

    str :: Parser Prog
    str = pin do Str <$> stringLiteral

    match :: Parser Prog
    match = pin do
      _    <- tok "match"
      subj <- prog
      _    <- tok "with"
      alts <- parens Curly do
        many do
          c    <- ctor
          arg  <- name
          _    <- tok "=>"
          b    <- prog
          _    <- tok ";"
          return (c, (arg, b))
      return $ Match subj $ Map.fromList alts

    rec_ :: Parser Prog
    rec_ = pin do
      parens Curly do
        fields <- flip sepBy (tok ",") do
          field <- name
          _     <- tok "="
          p     <- prog
          return (field, p)
        return $ Rec fields

    -- get :: Parser (Prog -> Prog)
    -- get = do
    --   pos   <- getPosition
    --   _     <- tok "."
    --   field <- name
    --   return \p -> pos :> Get p field

    sym :: Parser Prog
    sym = pin do
      c <- ctor
      x <- get
      return $ Sym c x

    -- app :: Parser (Prog -> Prog)
    -- app = do
    --   pos <- getPosition
    --   xs <- parens Round do prog `sepBy` tok ","
    --   return \p -> pos :> App p xs

    t :: (Show a) => String -> Parser a -> Parser a
    t msg p = do
      res <- p
      traceM (msg <> " " <> show res)
      return res

    lam :: Parser Prog
    lam = do
      pos <- getPosition
      tok "\\"
      args <- name `sepBy` tok ","
      _    <- tok "=>"
      body <- prog
      return $ foldr (\arg b -> pos :> Lam arg b) body args