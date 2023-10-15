{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib (module Lib) where

import Control.Lens
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get, state), modify')

import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import Data.Vector qualified as V

import GHC.Generics (Generic)

type Stack ∷ Type
newtype Stack = MkStack (Vector Integer)
  deriving stock (Generic)
  deriving newtype (Show, Eq, Semigroup, Monoid)
  deriving anyclass (Wrapped)

type AppState ∷ Type
data AppState = MkAppState
  { buffer ∷ ![Text]
  , stack ∷ !Stack
  }
  deriving stock (Generic, Show)

instance Semigroup AppState where
  a <> b =
    a
      & #buffer <>~ (b ^. #buffer)
      & #stack <>~ (b ^. #stack)

instance Monoid AppState where
  mempty = MkAppState{buffer = mempty, stack = mempty}

type AppError ∷ Type
data AppError
  = ParseError !String
  | StackUnderflow
  deriving stock (Show)

type Op ∷ (Type → Type) → Type → Type
type Op m a = (MonadState AppState m, MonadError AppError m) ⇒ m a

process ∷ Text → Op m ()
process = \case
  "+" → add
  "-" → sub
  "dup" → dup
  "drop" → Lib.drop
  "swap" → swap
  "over" → Lib.over
  "rot" → rot
  input → parseInteger input >>= push

parseInteger ∷ (MonadError AppError m) ⇒ Text → m Integer
parseInteger = either (throwError . ParseError) (pure . fst) . decimal

push ∷ (MonadState AppState m) ⇒ Integer → m ()
push n = modify' (#stack . _Wrapped' %~ V.cons n)

pop ∷ Op m Integer
pop = get >>= maybe (throwError StackUnderflow) update . V.uncons . (^. #stack . _Wrapped')
 where
  update (h, t) = state \s → (h, s & #stack . _Wrapped' .~ t)

add ∷ Op m ()
add = do
  a ← pop
  b ← pop
  push (a + b)

sub ∷ Op m ()
sub = do
  a ← pop
  b ← pop
  push (a - b)

dup ∷ Op m ()
dup = do
  a ← pop
  push a
  push a

drop ∷ Op m ()
drop = void pop

swap ∷ Op m ()
swap = do
  a ← pop
  b ← pop
  push a
  push b

over ∷ Op m ()
over = do
  a ← pop
  b ← pop
  push b
  push a
  push b

rot ∷ Op m ()
rot = do
  a ← pop
  b ← pop
  c ← pop
  push b
  push a
  push c
