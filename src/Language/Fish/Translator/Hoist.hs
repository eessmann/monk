{-# LANGUAGE DeriveTraversable #-}

module Language.Fish.Translator.Hoist
  ( Hoisted (..),
    hoist,
    emit,
    emitMany,
    fromPair,
    toPair,
    prependHoist,
    beginIfNeeded,
    beginHoisted,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST (FishCommand (Begin), FishStatement (Stmt), FishType (TStatus))

-- | A value paired with statements that must run before it.
data Hoisted a = MkHoisted
  { hoistedPrelude :: [FishStatement],
    hoistedValue :: a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Hoisted where
  pure = MkHoisted []
  MkHoisted pre f <*> MkHoisted pre' val = MkHoisted (pre <> pre') (f val)

instance Monad Hoisted where
  MkHoisted pre val >>= f =
    let MkHoisted pre' val' = f val
     in MkHoisted (pre <> pre') val'

hoist :: [FishStatement] -> a -> Hoisted a
hoist = MkHoisted

emit :: FishStatement -> Hoisted ()
emit stmt = MkHoisted [stmt] ()

emitMany :: [FishStatement] -> Hoisted ()
emitMany stmts = MkHoisted stmts ()

fromPair :: ([FishStatement], a) -> Hoisted a
fromPair (pre, val) = MkHoisted pre val

toPair :: Hoisted a -> ([FishStatement], a)
toPair (MkHoisted pre val) = (pre, val)

prependHoist :: [FishStatement] -> Hoisted a -> Hoisted a
prependHoist pre (MkHoisted pre' val) = MkHoisted (pre <> pre') val

beginIfNeeded :: [FishStatement] -> FishCommand TStatus -> FishCommand TStatus
beginIfNeeded pre cmd =
  case pre of
    [] -> cmd
    _ -> Begin (NE.fromList (pre <> [Stmt cmd])) []

beginHoisted :: Hoisted (FishCommand TStatus) -> FishCommand TStatus
beginHoisted (MkHoisted pre cmd) = beginIfNeeded pre cmd
