{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -freverse-errors #-}

module Interop where

import Control.Monad.Except

import Machine

-- Интерфейсы для обмена информации с KERN.
--
class                    HasTypeName a where typename :: String
class (HasTypeName a) => ToLisp      a where toLisp   :: a -> Value
class (HasTypeName a) => FromLisp    a where fromLisp :: Value -> Either Error a

eitherToLispM :: Either Error a -> LispM a
eitherToLispM = either throwError pure

instance HasTypeName Value  where typename = "any"
instance HasTypeName Symbol where typename = "symbol"
instance HasTypeName Bool   where typename = "bool"
instance HasTypeName Double where typename = "number"
instance HasTypeName String where typename = "string"
instance HasTypeName ()     where typename = "()"
instance HasTypeName Env    where typename = "env"

instance (HasTypeName res, HasTypeName arg) => HasTypeName (SF arg res) where
  typename = typename @arg <> " => " <> typename @res

instance (HasTypeName a, HasTypeName b) => HasTypeName (a, b) where
  typename = typename @a <> " * " <> typename @b

instance (HasTypeName a, HasTypeName b) => HasTypeName (Either a b) where
  typename = typename @a <> " | " <> typename @b

instance {-# OVERLAPPABLE #-} (HasTypeName a) => HasTypeName [a] where
  typename = "[" <> typename @a <> "]"


instance ToLisp ()     where toLisp () = Nil
instance ToLisp Value  where toLisp    = id
instance ToLisp Bool   where toLisp    = Atom . Bool
instance ToLisp Double where toLisp    = Atom . Number
instance ToLisp String where toLisp    = Atom . String
instance ToLisp Env    where toLisp    = Atom . Env

instance (ToLisp res, FromLisp arg) => ToLisp (SF arg res) where
  toLisp fun = Atom $ Func \a -> do
    a <- eitherToLispM do fromLisp a  -- парсим аргументы в хаскелль-типы
    b <- fun.run a                    -- делегируем выполнение встроенной функции
    return (toLisp b)                 -- упаковываем результат в KERN Value

instance (ToLisp a, ToLisp b) => ToLisp (a, b) where
  toLisp (a, b) = Cons {car = toLisp a, cdr = toLisp b}

instance (ToLisp a, ToLisp b) => ToLisp (Either a b) where
  toLisp = either toLisp toLisp

instance {-# OVERLAPPABLE #-} (ToLisp a) => ToLisp [a] where
  toLisp = foldr (Cons . toLisp) Nil


instance FromLisp Value where
  fromLisp = pure

instance FromLisp Symbol where
  fromLisp = \case
    Atom (Name name) -> return (Symbol name)
    it               -> throwError $ TypeError (typename @Symbol) it

instance FromLisp Env where
  fromLisp = \case
    Atom (Env env) -> return env
    it             -> throwError $ TypeError (typename @Env) it

instance FromLisp String where
  fromLisp = \case
    Atom (String name) -> return name
    it                 -> throwError $ TypeError (typename @String) it

instance FromLisp Bool where
  fromLisp = \case
    Atom (Bool name) -> return name
    it               -> throwError $ TypeError (typename @Bool) it

instance FromLisp Double where
  fromLisp = \case
    Atom (Number name) -> return name
    it                 -> throwError $ TypeError (typename @Double) it

instance (FromLisp a, FromLisp b) => FromLisp (a, b) where
  fromLisp (Cons {car, cdr}) = do
    a <- fromLisp car
    b <- fromLisp cdr
    return (a, b)

  fromLisp it = throwError $ TypeError (typename @(a, b)) it

instance (FromLisp a, FromLisp b) => FromLisp (Either a b) where
  fromLisp val = do
    (Left <$> fromLisp val) `catchError` \_ ->
      Right <$> fromLisp val

instance {-# OVERLAPPABLE #-} (FromLisp a) => FromLisp [a] where
  fromLisp = \case
    Cons {car, cdr} -> pure (:) <*> fromLisp car <*> fromLisp cdr
    Nil             -> pure []
    it              -> throwError $ TypeError (typename @[a]) it

instance FromLisp () where
  fromLisp = \case
    Nil -> pure ()
    it  -> throwError $ TypeError (typename @()) it
