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

import Language.Fish.Translator.Syntax
  ( Arg,
    FishCommand,
    FishStatement,
    FishType (TStatus),
    argExpr,
    argRedirect,
    attachRedirectsToCommand,
    attachRedirectsToStatement,
    renderArg,
    renderArgs,
  )

attachArgsToCommand :: [Arg] -> FishCommand TStatus -> FishCommand TStatus
attachArgsToCommand args = attachRedirectsToCommand (renderArgs args)

attachArgsToStatement :: [Arg] -> FishStatement -> FishStatement
attachArgsToStatement args = attachRedirectsToStatement (renderArgs args)
