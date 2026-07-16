{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Read.Exact
  ( translateReadExactM,
  )
where

import Control.Monad.State.Strict (gets)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Language.Fish.Translator.Commands.Read.Runtime
  ( captureHelperCommandToFile,
    captureHelperExpr,
    collectCommandExpr,
    currentIfsExpr,
    ensureReadDelimHelper,
    helperPipelineStatusExpr,
    jobListFromCommand,
    pipelineFromCommands,
    statusFromVarCommand,
  )
import Language.Fish.Translator.Commands.Read.Types
import Language.Fish.Translator.Monad
  ( TranslateM,
    TranslateState (..),
    TranslationContext (..),
  )
import Language.Fish.Translator.Types
import Prelude hiding (gets)

translateReadExactM :: ExactReadDelim -> TranslateM (FishCommand TStatus)
translateReadExactM spec = do
  scopedTargets <- resolveExactReadTargets spec
  case buildNativeSingleReadBody spec scopedTargets of
    Just body -> pure (Begin body [])
    Nothing -> do
      ensureReadDelimHelper
      pure (Begin (buildExactReadBody spec scopedTargets) [])

-- Fish 4.6 can read one character at a time without spawning a helper. For a
-- raw, single-variable, non-newline delimiter this is enough to preserve the
-- unread suffix, Bash's IFS-whitespace trimming, and the observable 0/1 read
-- status. More involved modes retain the Python exact path below.
buildNativeSingleReadBody :: ExactReadDelim -> [ScopedReadTarget] -> Maybe (NE.NonEmpty FishStatement)
buildNativeSingleReadBody spec scopedTargets = do
  delimiter <- case erdDelimiter spec of
    ExactReadDelimited value
      | value /= "\n" -> Just value
    _ -> Nothing
  guard (erdRaw spec)
  guard (isNothing (erdPrompt spec))
  guard (not (erdSilent spec))
  guard (isNothing (erdTimeout spec))
  guard (isNothing (erdNChars spec))
  guard (isNothing (erdFD spec))
  case erdTarget spec of
    ExactReadVars [_] -> pure ()
    _ -> Nothing
  (target, scopeFlags) <- case scopedTargets of
    [singleTarget] -> Just singleTarget
    _ -> Nothing
  pure
    ( setIfsStmt
        NE.:| [ setIfsWhitespaceStmt,
                Stmt (Set [SetLocal] nativeValueVar (ExprListLiteral [])),
                Stmt (Set [SetLocal] nativeStatusVar (ExprListLiteral [ExprLiteral "1"])),
                nativeReadLoop delimiter,
                nativeAssignStmt target scopeFlags,
                Stmt
                  ( Command
                      "test"
                      [ ExprVal (ExprVariable (VarScalar nativeStatusVar)),
                        ExprVal (ExprLiteral "-eq"),
                        ExprVal (ExprLiteral "0")
                      ]
                  )
              ]
    )

nativeValueVar :: Text
nativeValueVar = "__monk_read_value"

nativeStatusVar :: Text
nativeStatusVar = "__monk_read_status"

nativeCharVar :: Text
nativeCharVar = "__monk_read_char"

nativeIfsWhitespaceVar :: Text
nativeIfsWhitespaceVar = "__monk_read_ifs_ws"

setIfsWhitespaceStmt :: FishStatement
setIfsWhitespaceStmt =
  Stmt
    ( Set
        [SetLocal]
        nativeIfsWhitespaceVar
        (collectCommandExpr replaceCommand)
    )
  where
    replaceCommand =
      Command
        "string"
        [ ExprVal (ExprLiteral "replace"),
          ExprVal (ExprLiteral "--all"),
          ExprVal (ExprLiteral "--regex"),
          ExprVal (ExprLiteral "[^ \\t\\n]"),
          ExprVal (ExprLiteral ""),
          ExprVal (ExprLiteral "--"),
          ExprVal (ExprVariable (VarScalar "__monk_read_ifs"))
        ]

nativeReadLoop :: Text -> FishStatement
nativeReadLoop delimiter =
  Stmt
    ( While
        (jobListFromCommand (Read [ReadNChars "1", ReadLocal] [nativeCharVar]))
        (Stmt delimiterBranch NE.:| [])
        []
    )
  where
    delimiterBranch =
      If
        ( jobListFromCommand
            ( Command
                "test"
                [ ExprVal (ExprVariable (VarScalar nativeCharVar)),
                  ExprVal (ExprLiteral "="),
                  ExprVal (ExprLiteral delimiter)
                ]
            )
        )
        ( Stmt (Set [] nativeStatusVar (ExprListLiteral [ExprLiteral "0"]))
            NE.:| [Stmt Break]
        )
        [ Stmt
            ( Set
                [SetAppend]
                nativeValueVar
                (ExprListLiteral [ExprVariable (VarScalar nativeCharVar)])
            )
        ]
        []

nativeAssignStmt :: Text -> [SetFlag] -> FishStatement
nativeAssignStmt target scopeFlags =
  Stmt
    ( Set
        scopeFlags
        target
        (ExprCommandSubst (Stmt trimCommand NE.:| []))
    )
  where
    trimCommand =
      Command
        "string"
        [ ExprVal (ExprLiteral "trim"),
          ExprVal (ExprLiteral "--chars"),
          ExprVal (ExprVariable (VarScalar nativeIfsWhitespaceVar)),
          ExprVal (ExprLiteral "--"),
          ExprVal (ExprCommandSubst (Stmt joinValueCommand NE.:| []))
        ]
    joinValueCommand =
      Command
        "string"
        [ ExprVal (ExprLiteral "join"),
          ExprVal (ExprLiteral ""),
          ExprVal (ExprLiteral "--"),
          ExprVal (ExprVariable (VarAll nativeValueVar))
        ]

resolveExactReadTargets :: ExactReadDelim -> TranslateM [ScopedReadTarget]
resolveExactReadTargets =
  traverse (\name -> (name,) <$> readScopeFlags name)
    . exactReadTargetVars
    . erdTarget

buildExactReadBody :: ExactReadDelim -> [ScopedReadTarget] -> NE.NonEmpty FishStatement
buildExactReadBody spec scopedTargets =
  setIfsStmt
    NE.:| ( captureStatements spec
              <> emitExactReadAssignments spec scopedTargets
              <> [finishReadStatusStmt]
          )

captureStatements :: ExactReadDelim -> [FishStatement]
captureStatements spec =
  case exactReadCaptureStrategy spec of
    CaptureViaPipeline ->
      [ setCapturedFieldsStmt spec,
        setCapturedStatusFromPipelineStmt
      ]
    CaptureViaFile ->
      [ setCaptureFileStmt,
        runCaptureToFileStmt spec,
        setCapturedStatusFromStatusStmt,
        setCapturedFieldsFromFileStmt,
        removeCaptureFileStmt
      ]

exactReadCaptureStrategy :: ExactReadDelim -> ExactReadCaptureStrategy
exactReadCaptureStrategy spec =
  case erdFD spec of
    Just _ -> CaptureViaFile
    Nothing -> CaptureViaPipeline

setIfsStmt :: FishStatement
setIfsStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_ifs"
        currentIfsExpr
    )

setCaptureFileStmt :: FishStatement
setCaptureFileStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_capture_file"
        (ExprCommandSubst (Stmt (Command "mktemp" []) NE.:| []))
    )

runCaptureToFileStmt :: ExactReadDelim -> FishStatement
runCaptureToFileStmt = Stmt . captureHelperCommandToFile

setCapturedFieldsStmt :: ExactReadDelim -> FishStatement
setCapturedFieldsStmt spec =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_fields"
        (captureHelperExpr spec)
    )

setCapturedStatusFromPipelineStmt :: FishStatement
setCapturedStatusFromPipelineStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_status"
        (ExprListLiteral [helperPipelineStatusExpr])
    )

setCapturedStatusFromStatusStmt :: FishStatement
setCapturedStatusFromStatusStmt =
  Stmt
    ( Command
        "set"
        [ ExprVal (ExprLiteral "--local"),
          ExprVal (ExprLiteral "__monk_read_status"),
          ExprVal (ExprSpecialVar SVStatus)
        ]
    )

setCapturedFieldsFromFileStmt :: FishStatement
setCapturedFieldsFromFileStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_fields"
        ( ExprCommandSubst
            ( Stmt
                ( Pipeline
                    ( pipelineFromCommands
                        captureFileCommand
                        [Command "string" [ExprVal (ExprLiteral "split0")]]
                    )
                )
                NE.:| []
            )
        )
    )
  where
    captureFileCommand =
      Command
        "cat"
        [ExprVal (ExprVariable (VarScalar "__monk_read_capture_file"))]

removeCaptureFileStmt :: FishStatement
removeCaptureFileStmt =
  Stmt
    ( Command
        "rm"
        [ ExprVal (ExprLiteral "-f"),
          ExprVal (ExprVariable (VarScalar "__monk_read_capture_file"))
        ]
    )

emitExactReadAssignments :: ExactReadDelim -> [ScopedReadTarget] -> [FishStatement]
emitExactReadAssignments spec scopedTargets =
  case erdTarget spec of
    ExactReadArray _ ->
      case scopedTargets of
        [(varName, scopeFlags)] ->
          [ Stmt
              ( Set
                  scopeFlags
                  varName
                  (ExprVariable (VarAll "__monk_read_fields"))
              )
          ]
        _ -> []
    ExactReadVars _ ->
      zipWith assignIndexedVar [1 ..] scopedTargets

assignIndexedVar :: Int -> ScopedReadTarget -> FishStatement
assignIndexedVar idx (varName, scopeFlags) =
  Stmt
    ( Set
        scopeFlags
        varName
        ( ExprListLiteral
            [ ExprVariable
                ( VarIndex
                    "__monk_read_fields"
                    (IndexSingle (ExprNumLiteral idx))
                )
            ]
        )
    )

finishReadStatusStmt :: FishStatement
finishReadStatusStmt = Stmt (statusFromVarCommand "__monk_read_status")

exactReadTargetVars :: ExactReadTarget -> [Text]
exactReadTargetVars = \case
  ExactReadArray name -> [name]
  ExactReadVars names -> names

readScopeFlags :: Text -> TranslateM [SetFlag]
readScopeFlags name = do
  ctx <- gets context
  let localFlag = if inFunction ctx then SetFunction else SetLocal
  pure $
    if Set.member name (localVars ctx)
      then [localFlag]
      else [SetGlobal]
