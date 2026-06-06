module Language.Fish.Translator.Commands.Read
  ( translateRead,
    translateReadM,
    parseReadArgs,
    parseReadArgsDetailed,
    ReadParseResult (..),
  )
where

import Language.Fish.Translator.Commands.Read.Exact (translateReadExactM)
import Language.Fish.Translator.Commands.Read.Parse
  ( exactReadDelim,
    parseReadArgs,
    parseReadArgsDetailed,
  )
import Language.Fish.Translator.Commands.Read.Types (ReadParseResult (..))
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Syntax
import Language.Fish.Translator.Variables (translateTokenToExprOrRedirect)
import ShellCheck.AST (Token)

-- | read [flags] VARS...
translateRead :: [Token] -> FishCommand TStatus
translateRead ts =
  let (flags, vars, unsupported) = parseReadArgs ts [] [] False
   in if unsupported
        then Command "read" (map translateTokenToExprOrRedirect ts)
        else Read flags vars

translateReadM :: [Token] -> TranslateM (FishCommand TStatus)
translateReadM ts =
  let parsed = parseReadArgsDetailed ts [] [] [] False False
   in if readUnsupported parsed
        then pure (Command "read" (map translateTokenToExprOrRedirect ts))
        else case exactReadDelim parsed of
          Just spec -> translateReadExactM spec
          Nothing -> pure (Read (readFlags parsed) (readVars parsed))
