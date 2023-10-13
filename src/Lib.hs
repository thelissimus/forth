{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib (module Lib) where

import Control.Monad.State (MonadState (get, put), gets, modify')

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import Data.Vector qualified as V

type Stack ∷ Type
newtype Stack = MkStack {getStack ∷ Vector Integer}
  deriving newtype (Show, Eq, Semigroup, Monoid)

type AppState ∷ Type
data AppState = MkAppState
  { buffer ∷ [Text]
  , stack ∷ Stack
  }
  deriving stock (Show)

process ∷ (MonadState AppState m) ⇒ Text → m ()
process = \case
  "+" → add
  "-" → sub
  "dup" → dup
  "drop" → Lib.drop
  "swap" → swap
  "over" → over
  "rot" → rot
  a → push (parseInteger a)

parseInteger ∷ Text → Integer
parseInteger = either error fst . decimal

addToStack ∷ Integer → Stack → Stack
addToStack e (MkStack s) = MkStack $ V.cons e s

push ∷ (MonadState AppState m) ⇒ Integer → m ()
push a = modify' (\s → s{stack = addToStack a s.stack})

pop ∷ (MonadState AppState m) ⇒ m Integer
pop = do
  (MkStack s) ← gets (.stack)
  if length s <= 1
    then do
      let (h, t) = V.splitAt 1 s
      old ← get
      put $ old{stack = MkStack t}
      pure $ V.head h
    else error "Stack underflow!"

add ∷ (MonadState AppState m) ⇒ m ()
add = do
  a ← pop
  b ← pop
  push (a + b)

sub ∷ (MonadState AppState m) ⇒ m ()
sub = do
  a ← pop
  b ← pop
  push (a - b)

dup ∷ (MonadState AppState m) ⇒ m ()
dup = do
  a ← pop
  push a
  push a

drop ∷ (MonadState AppState m) ⇒ m ()
drop = do
  _ ← pop
  pure ()

swap ∷ (MonadState AppState m) ⇒ m ()
swap = do
  a ← pop
  b ← pop
  push a
  push b

over ∷ (MonadState AppState m) ⇒ m ()
over = do
  a ← pop
  b ← pop
  push b
  push a
  push b

rot ∷ (MonadState AppState m) ⇒ m ()
rot = do
  a ← pop
  b ← pop
  c ← pop
  push b
  push a
  push c
