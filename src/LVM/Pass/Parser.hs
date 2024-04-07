
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
      , backtrack group0
      , group
      , lam
      , sym
      , match
      , list
      ]

    group = do
      pos <- getPosition
      parens Round do
        ts <- tupleExpr
        return case ts of
          [h] -> h
          ts  -> pos :> Sym "Tuple" (pos :> Rec (zip ["_" <> show n | n <- [1..]] ts))

    group0 = do
      pos <- getPosition
      parens Round do
        return $ pos :> Sym "Tuple" (pos :> Rec [])

    tupleExpr = do
      h <- prog
      t <- optional do
        tok ","
        tupleExpr
      return $ [h] <> maybe [] id t

    app = do
      pos  <- getPosition
      core <- get
      args <- many get
      return $ foldl (\f arg -> pos :> App f arg) core args

    list = do
      parens Square do
        listExpr

    listExpr = do
        tok "..."
        prog
      <|> do
        pos <- getPosition
        h <- prog
        t <- optional do
          tok ","
          listExpr
        return $
          pos :> Sym "Cons"
            (pos :> Rec
              [ ("head", h)
              , ("tail", maybe (pos :> Sym "Nil" (pos :> Rec [])) id t)
              ]
            )
      <|> do
        pos <- getPosition
        return $ pos :> Sym "Nil" (pos :> Rec [])

    get = do
      pos  <- getPosition
      core <- atom
      fs   <- many do
        tok "."
        name
      return $ foldl (\box field -> pos :> Get box field.raw) core fs

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
      _    <- tok "case"
      subj <- prog
      _    <- tok "of"
      as   <- parens Curly do alt `sepBy` tok "|"
      return $ Match subj as

    alt :: Parser (Alt Prog)
    alt = do
      p <- pat
      _ <- tok "=>"
      b <- prog
      return $ Alt p b

    pat :: Parser Pattern
    pat = choice
      [ do
          c <- ctor
          p <- pat
          return (PSym c.raw p)

      , parens Curly do
          fs <- do
              f <- name
              p <- optional do
                tok "="
                pat
              return (maybe (f.raw, PVar f) (f.raw,) p)
            `sepBy` tok ","
          return (PRec fs)
      , PNum <$> float
      , PStr <$> stringLiteral
      , PVar <$> name

      , parens Square do
          pListExpr

      , backtrack pGroup0
      , pGroup
      ]

    pGroup = do
      pos <- getPosition
      parens Round do
        ts <- pTupleExpr
        return case ts of
          [h] -> h
          ts  -> PSym "Tuple" (PRec (zip ["_" <> show n | n <- [1..]] ts))

    pGroup0 = do
      pos <- getPosition
      parens Round do
        return $ PSym "Tuple" (PRec [])

    pTupleExpr = do
      h <- pat
      t <- optional do
        tok ","
        pTupleExpr
      return $ [h] <> maybe [] id t


    pListExpr :: Parser Pattern
    pListExpr = do
        tok "..."
        pat
      <|> do
        pos <- getPosition
        h <- pat
        t <- optional do
          tok ","
          pListExpr
        return $
          PSym "Cons"
            (PRec
              [ ("head", h)
              , ("tail", maybe (PSym "Nil" (PRec [])) id t)
              ]
            )
      <|> do
        return $ PSym "Nil" (PRec [])

    rec_ :: Parser Prog
    rec_ = pin do
      parens Curly do
        fields <- flip sepBy (tok ",") do
          field <- name
          _     <- tok "="
          p     <- prog
          return (field.raw, p)
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
      return $ Sym c.raw x

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