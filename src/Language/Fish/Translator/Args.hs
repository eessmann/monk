{-# LANGUAGE GADTs #-}

module Language.Fish.Translator.Args
  ( Arg (..),
    argExpr,
    argRedirect,
    renderArg,
    renderArgs,
  )
where

import Language.Fish.AST

data Arg where
  ArgExpr :: Typeable t => FishExpr t -> Arg
  ArgRedirect :: Redirect -> Arg

argExpr :: Typeable t => FishExpr t -> Arg
argExpr = ArgExpr

argRedirect :: Redirect -> Arg
argRedirect = ArgRedirect

renderArg :: Arg -> ExprOrRedirect
renderArg (ArgExpr expr) = ExprVal expr
renderArg (ArgRedirect redir) = RedirectVal redir

renderArgs :: [Arg] -> [ExprOrRedirect]
renderArgs = map renderArg
