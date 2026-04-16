module Language.Fish.Translator.Statement
  ( toNonEmptyStmtList,
    isEmptyStatement,
    wrapPrelude,
    statusCommandBlock,
    noteBestEffortSubshell,
    translateSubshellStatement,
    translateSubshellStatusCommand,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Hoist (beginIfNeeded)
import Language.Fish.Translator.Monad
  ( TranslateM,
    WarningCode (..),
    noteUnsupported,
  )

-- | Drop empty statements and convert to NonEmpty if possible.
toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (not . isEmptyStatement) stmts)

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

wrapPrelude :: [FishStatement] -> FishCommand TStatus -> FishCommand TStatus
wrapPrelude = beginIfNeeded

statusCommandBlock :: [FishCommand TStatus] -> FishCommand TStatus
statusCommandBlock cmds =
  case cmds of
    [] -> Command "true" []
    (cmd : rest) -> Begin (Stmt cmd NE.:| map Stmt rest) []

noteBestEffortSubshell :: TranslateM ()
noteBestEffortSubshell = do
  _ <- noteUnsupported BestEffortSubshell Nothing
  pure ()

translateSubshellStatement :: NE.NonEmpty FishStatement -> TranslateM FishStatement
translateSubshellStatement body = do
  note <- noteUnsupported BestEffortSubshell Nothing
  pure (StmtList [note, Stmt (Begin body [])])

translateSubshellStatusCommand :: [FishCommand TStatus] -> TranslateM (FishCommand TStatus)
translateSubshellStatusCommand cmds = do
  noteBestEffortSubshell
  pure (statusCommandBlock cmds)
