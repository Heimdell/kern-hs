
module LVM.Pass.Parser where

import Control.Applicative
import Data.Fix
import Data.Map qualified as Map

import ParsingTools

import LVM.Pass.Lexer
import LVM.Prog
import LVM.Name
import LVM.Phase.Raw

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

stmt :: Parser (Stmt Prog)
stmt = choice [def, force]
  where
    def = do
      _     <- tok "def"
      decls <- parens Square do
        many do
          parens Round do
            n <- name
            p <- prog
            return (n, p)
      return $ Def decls

    force = do
      _ <- tok "!"
      p <- prog
      return $ Force p

prog :: Parser Prog
prog = do
  pos <- getPosition
  f <- choice
    [ atom
    , construct
    ]
  return $ pos :> f
  where
    atom      =                 choice [var, bif, num, str, rec_]
    construct = parens Round do choice [lam, app, let_, sym, match, get, do_]

    do_ :: Parser (Prog_ Prog)
    do_ = do
      _     <- tok "do"
      stmts <- many stmt
      res   <- prog
      return $ Do stmts res

    var :: Parser (Prog_ Prog)
    var = Var <$> name

    bif :: Parser (Prog_ Prog)
    bif = do
      _ <- char '$'
      BIF . (.raw) <$> name

    num :: Parser (Prog_ Prog)
    num = Num <$> float

    str :: Parser (Prog_ Prog)
    str = Str <$> stringLiteral

    let_ :: Parser (Prog_ Prog)
    let_ = do
      _     <- tok "let"
      decls <- parens Square do
        many do
          parens Round do
            n <- name
            p <- prog
            return (n, p)
      b <- prog
      return $ Let decls b

    match :: Parser (Prog_ Prog)
    match = do
      _    <- tok "match"
      subj <- prog
      alts <- parens Curly do
        many do
          (ctor, arg) <- parens Round do
            _    <- char '%'
            ctor <- name
            arg  <- name
            return (ctor, arg)
          b <- prog
          return (ctor, (arg, b))
      return $ Match subj $ Map.fromList alts

    rec_ :: Parser (Prog_ Prog)
    rec_ = parens Curly do
      fields <- many do
        _     <- char '#'
        field <- name
        p     <- prog
        return (field, p)
      return $ Rec fields

    get :: Parser (Prog_ Prog)
    get = do
      _     <- char '#'
      field <- name
      p     <- prog
      return $ Get p field

    sym :: Parser (Prog_ Prog)
    sym = do
      _    <- char '%'
      ctor <- name
      x    <- prog
      return $ Sym ctor x

    app :: Parser (Prog_ Prog)
    app = do
      f  <- prog
      xs <- many prog
      return $ App f xs

    lam :: Parser (Prog_ Prog)
    lam = do
      tok "lambda"
      args <- parens Square do many name
      body <- prog
      return (Lam args body)