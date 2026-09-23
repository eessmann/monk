{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Invocation effects consume the ordered arguments built by Words.
module Language.Fish.Translator.Invocation (InvocationMaterializer (..), invocationMaterializer) where

import Control.Monad.State.Strict (gets)
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Context
import Language.Fish.Translator.Emission (Emission, emit, renderEmission)
import Language.Fish.Translator.HelperRegistry qualified as Helpers
import Language.Fish.Translator.Identifier (compilerCommandName, compilerIdentifier)
import Language.Fish.Translator.Native qualified as Native
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Region qualified as Region
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Session.Request qualified as Request
import Language.Fish.Translator.Statement
import Language.Fish.Translator.Traps qualified as Traps
import Language.Fish.Translator.Words qualified as Words
import Monk.Translation.Types
import Prelude hiding (gets, words)

data InvocationMaterializer scope = InvocationMaterializer
  { materializePrefixedInvoke :: Bool -> [(P.Storage, Text, P.Scalar)] -> P.CallTarget -> [P.Word] -> Materialize scope (Emission [FishStatement]),
    materializeInvokeWords :: Bool -> P.CallTarget -> [P.Word] -> Materialize scope (Emission [FishStatement]),
    materializeInvocation :: Bool -> P.CallTarget -> Materialize scope ([SomeArgument] -> [FishStatement]),
    materializeWriter :: Text -> Materialize scope ([SomeArgument] -> FishStatement)
  }

invocationMaterializer :: forall scope. Words.WordMaterializer scope -> InvocationMaterializer scope
invocationMaterializer words = InvocationMaterializer lowerPrefixedInvoke lowerInvokeWords lowerInvocation directWriter
  where
    lowerPrefixedInvoke :: Bool -> [(P.Storage, Text, P.Scalar)] -> P.CallTarget -> [P.Word] -> Materialize scope (Emission [FishStatement])
    lowerPrefixedInvoke suppressed assignments target wordsValue = do
      arguments <- Words.materializeWords words wordsValue
      prefix <- gets materialPrefix
      frozen <- forM assignments $ \(_, name, scalar) -> do
        value <- Words.materializeScalar words scalar
        actual <- bindingName name
        saved <- fresh "prefix_value"
        let installValue operand = [assign [SetLocal, SetExport, SetUnpath] actual operand, assign [SetLocal] (compilerIdentifier prefix <> "binding_export_" <> actual) (ExprLiteral "--export"), assignList [SetLocal] (compilerIdentifier prefix <> "binding_environment_" <> actual) (ExprListLiteral [])]
        pure (assign [SetLocal] saved (ExprLiteral ""), fmap (\expression -> installValue expression <> [assign [] saved (scalarVar actual)]) value, installValue (scalarVar saved))
      body <- lowerInvocation suppressed target
      let declarations = [declaration | (declaration, _, _) <- frozen]
          expansion = renderEmission (traverse_ (>>= emit) [statements | (_, statements, _) <- frozen] >> pure [])
          installation = concat [statements | (_, _, statements) <- frozen]
      pure $ do
        fields <- Region.fieldsEmission arguments
        emit (declarations <> [Stmt (Begin (bodyNE expansion) [])])
        pure [Stmt (Begin (bodyNE (installation <> body fields)) [])]

    lowerInvokeWords :: Bool -> P.CallTarget -> [P.Word] -> Materialize scope (Emission [FishStatement])
    lowerInvokeWords suppressed target wordsValue = do
      supervised <- gets materialSession
      case (supervised, target, traverse literalWord wordsValue >>= Native.nativeEcho) of
        (False, P.Builtin "echo", Just (newline, output)) -> do
          writer <- directWriter "echo-bytes"
          captured <- captureStatus
          guards <- errexitGuard suppressed
          let outputField = SomeArgument (ScalarArgument (ExprLiteral (T.intercalate " " output <> if newline then "\n" else "")))
          pure (pure ([writer [outputField], captured] <> guards))
        _ -> do
          arguments <- Words.materializeWords words wordsValue
          body <- lowerInvocation suppressed target
          prefix <- gets materialPrefix
          let endpoint = \case P.OneField P.ProcessSubstitution {} -> True; _ -> False
          pure (fmap (\fields -> body fields <> [Session.request prefix Request.ReleaseSubstitutions | any endpoint wordsValue]) (Region.fieldsEmission arguments))

    lowerInvocation :: Bool -> P.CallTarget -> Materialize scope ([SomeArgument] -> [FishStatement])
    lowerInvocation suppressed target = do
      capture <- captureStatus
      guardStatements <- errexitGuard suppressed
      supervised <- gets materialSession
      prefix <- gets materialPrefix
      case target of
        P.Builtin name | supervised && name `elem` ["printf", "echo"] -> do
          site <- diagnosticSite
          origin <- diagnosticOriginExpression
          sourceStatus <- runtimeName "status"
          traps <- gets materialTraps
          incoming <- fresh "writer_incoming"
          let terminate = if traps then [assign [SetGlobal] (compilerIdentifier prefix <> "pending_signal") (ExprLiteral "13"), Traps.exitWithStatusAt prefix origin (scalarVar incoming)] else [Session.request prefix Request.FinishBrokenPipe, builtin "exit" [arg (ExprLiteral "141")]]
          pure $ \fields -> [assign [SetLocal] incoming (scalarVar sourceStatus) | traps] <> [Session.request prefix (Request.Run (Request.singleBody (Request.WriterStage site (if name == "printf" then Request.Printf else Request.Echo) fields))), capture, ifStatements [testEquals (scalarVar sourceStatus) "141"] terminate []] <> guardStatements
        P.Function name -> do
          suppression <- runtimeName "suppress"
          saved <- fresh "caller_suppression"
          pure $ \fields ->
            let call = Stmt (Command (compilerCommandName name) (map argumentExpression fields))
             in ( [assign [SetLocal] saved (scalarVar suppression)]
                    <> [assign [] suppression (ExprLiteral "1") | suppressed]
                    <> [call, capture, assign [] suppression (scalarVar saved)]
                    <> guardStatements
                )
        P.Builtin name | name `elem` ["printf", "echo"] -> do
          writer <- directWriter name
          pure (\fields -> [writer fields, capture] <> guardStatements)
        P.Builtin name -> pure (\fields -> [builtin (if name == ":" then "true" else name) (map argumentExpression fields), capture] <> guardStatements)
        P.External name -> do
          needProgram (RequiresCommand name) "Explicit external command dispatch"
          helper <- externalHelper name
          (origin, line) <- diagnosticOrigin
          pure (\fields -> [Stmt (Command (compilerCommandName helper) (map (arg . ExprLiteral) [origin, line] <> map argumentExpression fields)), capture] <> guardStatements)

    directWriter :: Text -> Materialize scope ([SomeArgument] -> FishStatement)
    directWriter name = do
      needNative NativeWrite "Bash output errno and signal semantics"
      prefix <- gets materialPrefix
      helpers <- gets materialHelpers
      unless (NativeRuntime.nativeWriterName prefix `elem` Helpers.names helpers) $
        registerHelpers [NativeRuntime.nativeWriterDefinition prefix]
      range <- gets materialRange
      pure (NativeRuntime.nativeWriterInvocation prefix range name . map argumentExpression)

    literalWord :: P.Word -> Maybe Text
    literalWord (P.OneField (P.Literal value)) = Just value
    literalWord _ = Nothing
