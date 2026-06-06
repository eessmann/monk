module Language.Fish.Translator.Args
  ( Arg,
    argExpr,
    argRedirect,
    renderArg,
    renderArgs,
  )
where

import Language.Fish.DSL.Internal
  ( Arg (UnsafeArgExpr, UnsafeArgRedirect),
    ArgumentType,
    Expr (UnsafeExpr),
    lowerArg,
  )
import Language.Fish.Translator.DSL
  ( ExprOrRedirect,
    FishExpr,
    Redirect,
  )

argExpr :: (ArgumentType t, Typeable t) => FishExpr t -> Arg
argExpr = lowerableExprArg

argRedirect :: Redirect -> Arg
argRedirect = UnsafeArgRedirect

renderArg :: Arg -> ExprOrRedirect
renderArg = lowerArg

renderArgs :: [Arg] -> [ExprOrRedirect]
renderArgs = map renderArg

lowerableExprArg :: (ArgumentType t, Typeable t) => FishExpr t -> Arg
lowerableExprArg = UnsafeArgExpr . UnsafeExpr
