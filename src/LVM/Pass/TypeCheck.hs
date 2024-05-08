
module LVM.Pass.TypeCheck where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Map qualified as Map
import Data.Fix
import Data.Foldable

import Input
import LVM.Name
import LVM.Type
import LVM.Kind
import LVM.Prog
import LVM.Phase.Raw

import Unifier.Term
import Unifier.Scheme
import Unifier.Unify
import Unifier.Rename

data TCError
  = KindError (UError Kind_ Name)
  | TypeError (UError Type_ Name)
  | ScopeError ScopeError
  | NoTensorComponent String Name TypeExpr
  | IsNotStructuralType Type
  | IsNotUnion Type
  | IsNotRecord Type
  | CannotInferConstructor

data ScopeError
  = Undefined Name String
  | Shadowing Name Position

data Env = Env
  { kinds   :: Map.Map Name Kind
  , structs :: Map.Map Name TypeExpr
  , values  :: Map.Map Name (Scheme (Term Type_) Name)
  }

type TC =
    ReaderT  Env
  ( StateT  (UState Type_ Name, UState Kind_ Name)
  ( ExceptT (Position, TCError)
  ( RenameT  Name
    Identity
  ) ) )

type TCTypes =
    StateT (UState Type_ Name)
  ( Either (UError Type_ Name) )

type TCKinds =
    StateT (UState Kind_ Name)
  ( Either (UError Kind_ Name) )

types :: Position -> TCTypes a -> TC a
types pos ma = do
  (ty, ki) <- get
  case runStateT ma ty of
    Left err -> throwError (pos, TypeError err)
    Right (a, ty') -> do
      put (ty', ki)
      return a

kinds :: Position -> TCKinds a -> TC a
kinds pos ma = do
  (ty, ki) <- get
  case runStateT ma ki of
    Left err -> throwError (pos, KindError err)
    Right (a, ki') -> do
      put (ty, ki')
      return a

findKind :: Name -> TC Kind
findKind name = do
  asks (Map.lookup name . (.kinds)) >>= \case
    Nothing -> do
      throwError (name.pos, ScopeError (Undefined name "type"))

    Just ki -> do
      return ki

findTypeStructure :: Name -> TC TypeExpr
findTypeStructure name = do
  asks (Map.lookup name . (.structs)) >>= \case
    Nothing -> do
      throwError (name.pos, IsNotStructuralType (Term (TConst_ name)))

    Just struct -> do
      return struct

findValue :: Name -> TC Type
findValue name = do
  asks (Map.lookup name . (.values)) >>= \case
    Nothing -> do
      throwError (name.pos, ScopeError (Undefined name "value"))

    Just struct -> do
      instantiate struct

withType :: Name -> Scheme (Term Type_) Name -> TC a -> TC a
withType n t ma = do
  local (\s -> s { values = Map.insert n t s.values }) do
    ma

newKind :: Name -> TC Kind
newKind name = do
  Pure <$> rename name

newType :: Name -> TC Type
newType name = do
  Pure <$> rename name

inferKind :: Position -> Type -> TC Kind
inferKind pos = \case
  Pure tvar -> return (Pure tvar)
  Term (TApply_ f x) -> do
    r <- newKind "k"
    f <- inferKind pos f
    x <- inferKind pos x
    kinds pos do f =:= Term (KArrow_ x r)
    return r

  Term (TConst_ n) -> do
    findKind n

  Term (TArrow_ d c) -> do
    kd <- inferKind pos d
    kc <- inferKind pos c
    kinds pos do
      kd =:= Term Star_
      kc =:= Term Star_
    return $ Term Star_

match :: Position -> Type -> [Alt Prog] -> Type -> TC ()
match pos ty [] res = return ()
match pos ty (alt : alts) res = do
  matchAlt pos ty alt  res
  match    pos ty alts res

matchAlt :: Position -> Type -> Alt Prog -> Type -> TC ()
matchAlt pos ty Alt {pat, body} res = do
  withPat pos ty pat do
    checkType res body

deconstructType :: Position -> Type -> TC (Name, [Type])
deconstructType pos (Term (TConst_ n)) = return (n, [])
deconstructType pos (Term (TApply_ f x)) = do
  (root, args) <- deconstructType pos f
  return (root, args ++ [x])
deconstructType pos t = throwError (pos, IsNotStructuralType t)

checkType :: Type -> Prog -> TC ()
checkType ty root@(Fix (Compose (pos, prog))) = case prog of
  Lam n body -> do
    td <- newType "d"
    tc <- newType "c"
    types pos do
      ty =:= Term (TArrow_ td tc)
    withType n (Scheme [] td) do
      checkType tc body

  Sym n prog -> do
    ctor <- ctorsOf pos ty
    ty'  <- ctor n
    checkType ty' prog

  Rec fs -> do
    field <- fieldsOf pos ty
    for_ fs \(f, prog) -> do
      ty' <- field f
      checkType ty prog

  Var n -> do
    t <- findValue n
    types pos do
      ty =:= t

  App f x -> do
    tx <- newType "x"
    tf <- checkType (Term (TApply_ tx ty)) f
    checkType tx x

  Match subj alts -> do
    tSubj <- newType "subj"
    checkType tSubj subj
    match pos tSubj alts ty

  Get subj f -> do
    tBox <- newType "box"
    checkType tBox subj
    field <- fieldsOf pos tBox
    ty' <- field f
    types pos do
      ty =:= ty'

  BIF name ty' -> do
    types pos do
      ty =:= ty'

  Num {} -> types pos do ty =:= Term (TConst_ "Num")
  Str {} -> types pos do ty =:= Term (TConst_ "String")

  Ann prog ty' -> do
    types pos do ty =:= ty'
    checkType ty' prog

  Do stmts ret -> do
    withStmts stmts do
      checkType ty ret

withStmts :: [Stmt Prog] -> TC a -> TC a
withStmts [] k = k
withStmts (stmt : stmts) k = do
  withStmt stmt do
    withStmts stmts do
      k

withStmt :: Stmt Prog -> TC a -> TC a
withStmt stmt k = case stmt of
  TypeSig tname kind -> withKind tname kind k

withPat :: Position -> Type -> Pattern -> TC a -> TC a
withPat pos ty pat k = case pat of
  PSym n pat -> do
    ctor <- ctorsOf pos ty
    ty'  <- ctor n
    withPat pos ty' pat k

  PRec fs -> do
    field <- fieldsOf pos ty
    withRecPat pos field fs k

  PNum {} -> do
    types pos do
      ty =:= Term (TConst_ "Num")
    k
  PStr {} -> do
    types pos do
      ty =:= Term (TConst_ "String")
    k
  PVar n -> withType n (Scheme [] ty) k

withRecPat :: Position -> (String -> TC Type) -> [(String, Pattern)] -> TC a -> TC a
withRecPat pos field [] k = k
withRecPat pos field ((f, pat) : pats) k = do
  ty <- field f
  withPat pos ty pat do
    withRecPat pos field pats do
      k

ctorsOf :: Position -> Type -> TC (String -> TC Type)
ctorsOf pos ty = do
  (root, arg) <- deconstructType pos ty
  tensor      <- findTypeStructure root
  inst        <- instantiateWith arg tensor
  case inst of
    Union fs -> do
      return \n -> do
        case Map.lookup n fs of
          Just ty' -> return ty'
          Nothing  -> throwError (pos, NoTensorComponent n root tensor)

    Record {} -> do
      throwError (pos, IsNotUnion ty)

fieldsOf :: Position -> Type -> TC (String -> TC Type)
fieldsOf pos ty = do
  (root, arg) <- deconstructType pos ty
  tensor      <- findTypeStructure root
  inst        <- instantiateWith arg tensor
  case inst of
    Record fs -> do
      return \n -> do
        case Map.lookup n fs of
          Just ty' -> return ty'
          Nothing  -> throwError (pos, NoTensorComponent n root tensor)

    Union {} -> do
      throwError (pos, IsNotRecord ty)