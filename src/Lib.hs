{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), gets, modify')

import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import Data.Vector qualified as V

import GHC.Generics (Generic)

type Stack ∷ Type
newtype Stack = MkStack {getStack ∷ Vector Integer}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Semigroup, Monoid)

type AppState ∷ Type
data AppState = MkAppState
  { buffer ∷ [Text]
  , stack ∷ Stack
  }
  deriving stock (Generic, Show)

instance Semigroup AppState where
  a <> b =
    a
      & #buffer <>~ (b ^. #buffer)
      & #stack <>~ (b ^. #stack)

instance Monoid AppState where
  mempty = MkAppState{buffer = mempty, stack = mempty}

type Op ∷ Type → Type
type Op a = ∀ {m ∷ Type → Type}. (MonadState AppState m) ⇒ m a

process ∷ (MonadIO m, MonadState AppState m) ⇒ Text → m ()
process = \case
  "+" → add
  "-" → sub
  "dup" → dup
  "drop" → Lib.drop
  "swap" → swap
  "over" → Lib.over
  "rot" → rot
  "." → period
  "dump" → dump
  a → push (parseInteger a)

parseInteger ∷ Text → Integer
parseInteger = either error fst . decimal

addToStack ∷ Integer → Stack → Stack
addToStack n = MkStack . V.cons n . getStack

push ∷ Integer → Op ()
push n = modify' (#stack %~ addToStack n)

pop ∷ Op Integer
pop = do
  s ← gets (getStack . (.stack))
  if not (null s)
    then do
      let (h, t) = V.splitAt 1 s
      old ← get
      put old{stack = MkStack t}
      pure $ V.head h
    else error "Stack underflow!"

add ∷ Op ()
add = do
  a ← pop
  b ← pop
  push (a + b)

sub ∷ Op ()
sub = do
  a ← pop
  b ← pop
  push (a - b)

dup ∷ Op ()
dup = do
  a ← pop
  push a
  push a

drop ∷ Op ()
drop = do
  _ ← pop
  pure ()

swap ∷ Op ()
swap = do
  a ← pop
  b ← pop
  push a
  push b

over ∷ Op ()
over = do
  a ← pop
  b ← pop
  push b
  push a
  push b

rot ∷ Op ()
rot = do
  a ← pop
  b ← pop
  c ← pop
  push b
  push a
  push c

period ∷ (MonadIO m, MonadState AppState m) ⇒ m ()
period = pop >>= liftIO . putStrLn . ("Result: " <>) . show

dump ∷ (MonadIO m, MonadState AppState m) ⇒ m ()
dump = get >>= liftIO . print
