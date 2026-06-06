module Language.Fish.Translator.Args
  ( Arg,
    argExpr,
    argRedirect,
    renderArg,
    renderArgs,
    attachArgsToCommand,
    attachArgsToStatement,
  )
where

import Language.Fish.DSL (Arg)
import Language.Fish.Translator.Construction
  ( argExpr,
    argRedirect,
    attachRedirectsToCommand,
    attachRedirectsToStatement,
    renderArg,
    renderArgs,
  )
import Language.Fish.Translator.Types
  ( FishCommand,
    FishStatement,
    FishType (TStatus),
  )

attachArgsToCommand :: [Arg] -> FishCommand TStatus -> FishCommand TStatus
attachArgsToCommand args = attachRedirectsToCommand (renderArgs args)

attachArgsToStatement :: [Arg] -> FishStatement -> FishStatement
attachArgsToStatement args = attachRedirectsToStatement (renderArgs args)
