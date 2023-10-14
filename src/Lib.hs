{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib (module Lib) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, state), StateT (runStateT), modify')

import Data.Function (fix)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
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

push ∷ Integer → Op ()
push n = modify' (#stack . _Wrapped' %~ V.cons n)

pop ∷ Op Integer
pop = state \old →
  let s = old ^. #stack . _Wrapped'
   in if not (null s)
        then let (h, t) = V.splitAt 1 s in (V.head h, old & #stack . _Wrapped' .~ t)
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
drop = void pop

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

repl ∷ (MonadIO m) ⇒ m ()
repl = void $ runStateT (fix \loop → liftIO TIO.getLine >>= process >> loop) mempty
