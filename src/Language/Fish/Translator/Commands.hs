module Language.Fish.Translator.Commands
  ( translateSimpleCommand,
    translateSimpleCommandM,
    translateCommandTokens,
    translateCommandTokensM,
    translateExit,
    translateSource,
    translateEval,
    translateExec,
    translateRead,
    translateReadM,
    translateTokensToStatusCmd,
    translateTokenToStatusCmd,
    translateTokensToStatusCmdM,
    translateTokenToStatusCmdM,
    translateCommandTokensToStatus,
    isSingleBracketTest,
    toNonEmptyStmtList,
    concatWithSpaces,
    stripTimePrefix,
  )
where

import Language.Fish.Translator.Commands.Args (concatWithSpaces)
import Language.Fish.Translator.Commands.CommandTokens
  ( translateCommandTokens,
    translateCommandTokensM,
    translateCommandTokensToStatus,
    translateEval,
    translateExec,
    translateExit,
    translateRead,
    translateReadM,
    translateSource,
  )
import Language.Fish.Translator.Commands.SimpleCommand
  ( translateSimpleCommand,
    translateSimpleCommandM,
  )
import Language.Fish.Translator.Commands.Status
  ( translateTokenToStatusCmd,
    translateTokenToStatusCmdM,
    translateTokensToStatusCmd,
    translateTokensToStatusCmdM,
  )
import Language.Fish.Translator.Commands.Tests (isSingleBracketTest)
import Language.Fish.Translator.Commands.Time (stripTimePrefix)
import Language.Fish.Translator.Statement (toNonEmptyStmtList)
