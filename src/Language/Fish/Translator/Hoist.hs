{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
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
data Hoisted a = Hoisted
  { hoistedPrelude :: [FishStatement],
    hoistedValue :: a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Hoisted where
  pure = Hoisted []
  Hoisted pre f <*> Hoisted pre' val = Hoisted (pre <> pre') (f val)

instance Monad Hoisted where
  Hoisted pre val >>= f =
    let Hoisted pre' val' = f val
     in Hoisted (pre <> pre') val'

hoist :: [FishStatement] -> a -> Hoisted a
hoist = Hoisted

emit :: FishStatement -> Hoisted ()
emit stmt = Hoisted [stmt] ()

emitMany :: [FishStatement] -> Hoisted ()
emitMany stmts = Hoisted stmts ()

fromPair :: ([FishStatement], a) -> Hoisted a
fromPair (pre, val) = Hoisted pre val

toPair :: Hoisted a -> ([FishStatement], a)
toPair (Hoisted pre val) = (pre, val)

prependHoist :: [FishStatement] -> Hoisted a -> Hoisted a
prependHoist pre (Hoisted pre' val) = Hoisted (pre <> pre') val

beginIfNeeded :: [FishStatement] -> FishCommand TStatus -> FishCommand TStatus
beginIfNeeded pre cmd =
  case pre of
    [] -> cmd
    _ -> Begin (NE.fromList (pre <> [Stmt cmd])) []

beginHoisted :: Hoisted (FishCommand TStatus) -> FishCommand TStatus
beginHoisted (Hoisted pre cmd) = beginIfNeeded pre cmd
