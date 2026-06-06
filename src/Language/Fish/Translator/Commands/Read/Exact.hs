{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Read.Exact
  ( translateReadExactM,
  )
where

import Control.Monad.State.Strict (gets)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Language.Fish.Translator.Commands.Read.Runtime
  ( assignHelperExpr,
    captureHelperCommandToFile,
    captureHelperExpr,
    collectCommandExpr,
    currentIfsExpr,
    ensureReadDelimHelper,
    helperPipelineStatusExpr,
    statusFromVarCommand,
  )
import Language.Fish.Translator.Commands.Read.Types
import Language.Fish.Translator.DSL
import Language.Fish.Translator.Monad
  ( TranslateM,
    TranslateState (..),
    TranslationContext (..),
  )
import Prelude hiding (gets)

translateReadExactM :: ExactReadDelim -> TranslateM (FishCommand TStatus)
translateReadExactM spec = do
  ensureReadDelimHelper
  scopedTargets <- resolveExactReadTargets spec
  pure (Begin (buildExactReadBody spec scopedTargets) [])

resolveExactReadTargets :: ExactReadDelim -> TranslateM [ScopedReadTarget]
resolveExactReadTargets =
  traverse (\name -> (name,) <$> readScopeFlags name)
    . exactReadTargetVars
    . erdTarget

buildExactReadBody :: ExactReadDelim -> [ScopedReadTarget] -> NE.NonEmpty FishStatement
buildExactReadBody spec scopedTargets =
  setIfsStmt
    NE.:| ( captureStatements spec
              <> [setAssignedFieldsStmt spec]
              <> emitExactReadAssignments spec scopedTargets
              <> [finishReadStatusStmt]
          )

captureStatements :: ExactReadDelim -> [FishStatement]
captureStatements spec =
  case exactReadCaptureStrategy spec of
    CaptureViaPipeline ->
      [ setCapturedValueStmt spec,
        setCapturedStatusFromPipelineStmt
      ]
    CaptureViaFile ->
      [ setCaptureFileStmt,
        runCaptureToFileStmt spec,
        setCapturedStatusFromStatusStmt,
        setCapturedValueFromFileStmt,
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

setCapturedValueStmt :: ExactReadDelim -> FishStatement
setCapturedValueStmt spec =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_value"
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

setCapturedValueFromFileStmt :: FishStatement
setCapturedValueFromFileStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_value"
        (collectCommandExpr captureFileCommand)
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

setAssignedFieldsStmt :: ExactReadDelim -> FishStatement
setAssignedFieldsStmt spec =
  Stmt
    ( Set
        [SetLocal]
        "__monk_read_fields"
        (assignHelperExpr spec)
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
