{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Ordered scalar, field, declaration and pattern materialization.
module Language.Fish.Translator.Words (WordMaterializer (..), Recursion (..), wordMaterializer) where

import Control.Monad.State.Strict (gets)
import Data.ByteString qualified as BS
import Language.Bash.Plan qualified as P
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.ArithmeticDiagnostic (arithmeticDiagnostic)
import Language.Fish.Translator.ArithmeticPlan qualified as Arithmetic
import Language.Fish.Translator.Binding qualified as Binding
import Language.Fish.Translator.Child qualified as Child
import Language.Fish.Translator.Context
import Language.Fish.Translator.Emission (Emission, emit, renderEmission)
import Language.Fish.Translator.Pattern qualified as Pattern
import Language.Fish.Translator.Primitive qualified as Primitive
import Language.Fish.Translator.Region qualified as Region
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Session.Request qualified as Request
import Language.Fish.Translator.Statement
import Language.Fish.Translator.Traps qualified as Traps
import Monk.Compiler.Index (Cardinality (FieldSequence), Domain (Fields))
import Monk.Translation.Types
import Numeric (showOct)
import Prelude hiding (force, gets)

data WordMaterializer scope = WordMaterializer
  { materializeWords :: [P.Word] -> Materialize scope (Region.ClosedFields FieldSequence),
    materializeScalar :: P.Scalar -> Materialize scope (Emission (FishExpr TStr)),
    materializeArrayWrite :: P.Storage -> Text -> [P.Word] -> Bool -> Materialize scope [FishStatement],
    materializeDeclarations :: [P.Declaration] -> Materialize scope [FishStatement],
    materializePatternParts :: P.Pattern -> Materialize scope (Emission [(Bool, FishExpr TStr)]),
    materializePatterns :: Identifier -> [P.Pattern] -> Materialize scope [FishStatement]
  }

data Recursion scope = Recursion
  { materializeChild :: Child.ChildMode -> P.ChildRegion -> Materialize scope Child.ChildInvocation,
    materializeSessionStage :: P.ChildRegion -> Materialize scope (Emission (Request.SomeStage, [FishStatement])),
    materializeSessionRequest :: Materialize scope SessionEmitter,
    cleanupChild :: Child.ChildInvocation -> [FishStatement]
  }

wordMaterializer :: forall scope. Recursion scope -> WordMaterializer scope
wordMaterializer callbacks = WordMaterializer lowerWords lowerScalar lowerArrayWrite lowerDeclarations lowerPatternParts lowerPatterns
  where
    lowerWords :: [P.Word] -> Materialize scope (Region.ClosedFields FieldSequence)
    lowerWords values = Region.concatFields <$> traverse lowerWord values

    lowerWord :: P.Word -> Materialize scope (Region.ClosedFields FieldSequence)
    lowerWord word = P.withWord word (fmap Region.sequenceFields . lowerField)

    lowerField :: P.Value Fields cardinality -> Materialize scope (Region.ClosedFields cardinality)
    lowerField = \case
      P.ExpandedFieldValues parts ->
        Region.closedSequence <$> do
          frozen <- Region.runTaggedScalars $ forM parts $ \part -> do
            let (mode, scalar) = case part of P.QuotedExpansion value -> (Primitive.QuotedExpansion, value); P.LiteralExpansion value -> (Primitive.LiteralExpansion, value); P.SplitExpansion value -> (Primitive.SplitExpansion, value)
            value <- Region.captureScalar (fresh "expansion_part") (lowerScalar scalar)
            pure (mode, value)
          ifs <- runtimeName "ifs"
          needNative NativeExpansion "Composed quote-aware splitting and pathname expansion"
          native <- runtimeName "native"
          temporary <- fresh "expanded_fields"
          pure $ do
            values <- frozen
            emit (captureList temporary (nulCaptureStatement (Primitive.primitiveStatement (identifierText native) (Primitive.ExpandFields (scalarVar ifs) values))))
            pure [listArgument temporary]
      P.PathnameFieldValues patternValue ->
        Region.closedSequence <$> do
          parts <- lowerPatternParts patternValue
          needNative NativeGlob "Quote-aware pathname expansion"
          native <- runtimeName "native"
          needProgram (RequiresFishFeature NulDelimitedCapture) "Pathname byte transport"
          temporary <- fresh "pathname_fields"
          pure $ do
            values <- parts
            emit (captureList temporary (Pattern.expandPathname (identifierText native) values))
            pure [listArgument temporary]
      P.ScalarField (P.Literal value) -> pure (Region.closedSingle (ExprLiteral value))
      P.ScalarField scalar -> Region.runFields (Region.scalarField <$> Region.captureScalar (fresh "field") (lowerScalar scalar))
      P.SplitFieldValues scalar ->
        Region.closedSequence <$> do
          value <- lowerScalar scalar
          ifs <- runtimeName "ifs"
          needNative NativeSplit "Bash IFS field splitting"
          native <- runtimeName "native"
          needProgram (RequiresFishFeature NulDelimitedCapture) "IFS field transport"
          temporary <- fresh "fields"
          pure $ do
            expression <- value
            emit (captureList temporary (nulCaptureStatement (Primitive.primitiveStatement (identifierText native) (Primitive.SplitFields (scalarVar ifs) expression))))
            pure [listArgument temporary]
      P.QuotedArgumentValues (P.Literal "") (P.Literal "") False -> pure (Region.closedSequence (pure [listArgument "argv"]))
      P.QuotedArgumentValues before after force -> lowerQuotedFields "argv" "argv" before after force
      P.QuotedArrayValues name (P.Literal "") (P.Literal "") False ->
        Region.closedSequence <$> do
          target <- bindingName name
          temporary <- fresh "array_fields"
          pure (emit [assignList [SetLocal] temporary (ExprVariable (VarAll target))] >> pure [listArgument temporary])
      P.QuotedArrayValues name before after force -> do
        target <- bindingName name
        lowerQuotedFields "array" target before after force

    lowerQuotedFields :: Text -> Identifier -> P.Scalar -> P.Scalar -> Bool -> Materialize scope (Region.ClosedFields FieldSequence)
    lowerQuotedFields role target before after force = do
      prefix <- lowerScalar before
      savedPrefix <- fresh (role <> "_prefix")
      suffix <- lowerScalar after
      needNative NativeArgv "Quoted argument prefix and suffix cardinality"
      native <- runtimeName "native"
      needProgram (RequiresFishFeature NulDelimitedCapture) "Quoted argument field transport"
      temporary <- fresh (role <> "_fields")
      pure $ Region.closedSequence $ do
        prefixValue <- prefix
        emit [assign [SetLocal] savedPrefix prefixValue]
        suffixValue <- suffix
        let producer = Primitive.primitiveStatement (identifierText native) (Primitive.QuotedArguments (scalarVar savedPrefix) suffixValue force (ExprVariable (VarAll target)))
        emit (captureList temporary (nulCaptureStatement producer))
        pure [listArgument temporary]

    listArgument :: Identifier -> SomeArgument
    listArgument = SomeArgument . ListArgument . ExprVariable . VarAll

    lowerArrayWrite :: P.Storage -> Text -> [P.Word] -> Bool -> Materialize scope [FishStatement]
    lowerArrayWrite storage name values append = do
      arguments <- lowerWords values
      target <- bindingName name
      status <- setSourceStatus (ExprLiteral "0")
      grouped <- gets materialAssignment
      pure $ renderEmission $ do
        fields <- Region.fieldsEmission arguments
        let operands = [arg (ExprVariable (VarAll target)) | append] <> map argumentExpression fields
        pure (arrayWrite storage (identifierText target) (identifierText target) operands <> [status | not grouped])

    captureList :: Identifier -> FishExpr (TList TStr) -> [FishStatement]
    captureList name expression =
      [ assignList [SetLocal] name (ExprListLiteral []),
        Stmt (Begin (assign [SetLocal] "fish_read_limit" (ExprLiteral "0") :| [assignList [] name expression]) [])
      ]

    nulCaptureStatement :: FishStatement -> FishExpr (TList TStr)
    nulCaptureStatement producer = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] producer [PipeTo [] (builtin "string" [arg (ExprLiteral "split0")])])) :| [])

    lowerScalar :: P.Scalar -> Materialize scope (Emission (FishExpr TStr))
    lowerScalar = \case
      P.Literal value -> pure (pure (ExprLiteral value))
      P.ByteLiteral bytes -> do
        temporary <- fresh "byte_literal"
        needProgram (RequiresFishFeature NulDelimitedCapture) "ANSI quoted byte transport"
        let escaped = mconcat ["\\" <> toText (showOct byte "") | byte <- BS.unpack bytes]
            producer = builtin "printf" [arg (ExprLiteral "%b\\0"), arg (ExprLiteral escaped)]
        pure (emit (captureList temporary (nulCaptureStatement producer)) >> pure (scalarVar temporary))
      P.PlatformBytes darwinBytes linuxBytes -> do
        temporary <- fresh "platform_bytes"
        needNative NativePlatformBytes "Platform-dependent Bash ANSI quoted bytes"
        helper <- runtimeName "native"
        pure (emit (captureList temporary (nulCaptureStatement (Primitive.primitiveStatement (identifierText helper) (Primitive.PlatformBytes darwinBytes linuxBytes)))) >> pure (scalarVar temporary))
      P.AppendValue name value -> do
        rhs <- lowerScalar value
        saved <- fresh "append_rhs"
        target <- bindingName name
        pure $ do
          expression <- rhs
          emit [assign [SetLocal] saved expression]
          pure (ExprStringConcat (scalarVar target) (scalarVar saved))
      P.ParameterPatternTransform operation scalar patternValue -> do
        value <- lowerScalar scalar
        subject <- fresh "parameter_subject"
        parts <- lowerPatternParts patternValue
        needNative NativePatternParts "Quote-aware byte parameter pattern"
        helper <- runtimeName "native"
        temporary <- fresh "parameter_transform"
        needProgram (RequiresFishFeature NulDelimitedCapture) "Parameter byte transport"
        pure $ do
          expression <- value
          emit [assign [SetLocal] subject expression]
          patternParts <- parts
          emit (captureList temporary (nulCaptureStatement (Primitive.primitiveStatement (identifierText helper) (Primitive.TrimPattern operation (scalarVar subject) patternParts))))
          pure (scalarVar temporary)
      P.ParameterTransform operation scalar patternValue replacement -> do
        value <- lowerScalar scalar
        needNative NativePattern "Bounded byte parameter operation"
        helper <- runtimeName "native"
        temporary <- fresh "parameter_transform"
        needProgram (RequiresFishFeature NulDelimitedCapture) "Parameter byte transport"
        pure $ do
          expression <- value
          emit (captureList temporary (nulCaptureStatement (Primitive.primitiveStatement (identifierText helper) (Primitive.ReplacePattern operation expression patternValue replacement))))
          pure (scalarVar temporary)
      P.AlternateValue name nullSensitive alternative -> do
        target <- bindingName name
        temporary <- fresh "parameter_alternate"
        value <- lowerScalar alternative
        let exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (identifierText target <> "[1]"))]
            nonempty = builtin "test" [arg (ExprLiteral "-n"), arg (scalarVar target)]
            predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
        pure (emit [assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate (renderEmission (fmap (\expression -> [assign [] temporary expression]) value)) []] >> pure (scalarVar temporary))
      P.Variable name -> pure . scalarVar <$> bindingName name
      P.ArrayElement name index -> do
        target <- bindingName name
        pure (pure (ExprQuotedVariable (VarIndex target (IndexSingle (ExprNumLiteral (index + 1))))))
      P.ArrayLength name -> do
        target <- bindingName name
        pure (pure (ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll target))] :| [])))
      P.PositionalAlternate index nullSensitive alternative -> do
        temporary <- fresh "positional_alternate"
        value <- lowerScalar alternative
        let current = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index)))
            exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral ("argv[" <> show index <> "]"))]
            nonempty = builtin "test" [arg (ExprLiteral "-n"), arg current]
            predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
        pure (emit [assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate (renderEmission (fmap (\expression -> [assign [] temporary expression]) value)) []] >> pure (scalarVar temporary))
      P.PositionalDefault index nullSensitive alternative -> do
        temporary <- fresh "positional_default"
        fallback <- lowerScalar alternative
        let current = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index)))
            exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral ("argv[" <> show index <> "]"))]
            nonempty = builtin "test" [arg (ExprLiteral "-n"), arg current]
            predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
        pure (emit [assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate [assign [] temporary current] (renderEmission (fmap (\expression -> [assign [] temporary expression]) fallback))] >> pure (scalarVar temporary))
      P.Positional index -> pure (pure (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index)))))
      P.ArgumentCount -> pure (pure (ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll "argv"))] :| [])))
      P.LastBackgroundPid -> do name <- runtimeName "last_pid"; pure (pure (scalarVar name))
      P.LastStatus -> pure . scalarVar <$> runtimeName "status"
      P.Concat values | Just literals <- traverse (\case P.Literal value -> Just value; _ -> Nothing) values -> pure (pure (ExprLiteral (mconcat literals)))
      P.Concat values -> Region.runScalar (Region.concatScalars <$> traverse (Region.captureScalar (fresh "scalar_part") . lowerScalar) values)
      P.ProcessSubstitution direction region -> do
        prefix <- gets materialPrefix
        needNative NativePipePaths "Inherited pipe pathname endpoints"
        needProgram (RequiresPlatformCapability PipeDescriptorPaths) "Owned process substitution pipe descriptors"
        stage <- materializeSessionStage callbacks region
        emitter <- materializeSessionRequest callbacks
        temporary <- fresh "process_endpoint"
        pure $ do
          (Request.SomeStage body, cleanup) <- stage
          let request = Request.Substitution (case direction of P.ProcessInput -> Request.Input; P.ProcessOutput -> Request.Output) (Request.singleBody body)
          emit ([emitSessionRequest emitter request, assign [SetLocal] temporary (Session.endpointPath prefix)] <> cleanup)
          pure (scalarVar temporary)
      P.Substitute region -> do
        invocation <- materializeChild callbacks Child.SubstitutionChild region
        prefix <- fresh "capture"
        result <-
          either
            (\message -> lift (Left (planDiagnostic "capture-materialization" message :| [])))
            pure
            (Child.materializeCapture (identifierText prefix) (P.childRange region) invocation)
        traverse_ mergeRequirement (Child.captureRequirements result)
        needProgram (RequiresFishFeature NulDelimitedCapture) "Command substitution byte and status transport"
        marker <- runtimeName "substitution_executed"
        lastStatus <- runtimeName "substitution_status"
        status <- setSourceStatus (Child.captureStatus result)
        let finish = [status, assign [] marker (ExprLiteral "1"), assign [] lastStatus (Child.captureStatus result)]
            check =
              ifStatements
                [testEquals (Child.captureError result) ""]
                finish
                [builtin "printf" [arg (ExprLiteral "monk: child transport failed\n"), RedirectVal (DuplicateRedirect 1 WriteTo 2)], builtin "exit" [arg (ExprLiteral "125")]]
        pure (emit (Child.captureStatements result <> cleanupChild callbacks invocation <> [check]) >> pure (Child.captureValue result))
      P.DefaultValue storage name nullSensitive assignValue alternative -> do
        target <- bindingName name
        temporary <- fresh "parameter"
        value <- lowerScalar alternative
        prefix <- gets materialPrefix
        let exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (identifierText target <> "[1]"))]
            nonempty = builtin "test" [arg (ExprLiteral "-n"), arg (scalarVar target)]
            predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
            otherwiseBody = renderEmission (fmap (\expression -> [assign [] temporary expression] <> (if assignValue then Binding.writeBinding (Binding.bindingRuntime prefix (identifierText target)) storage (scalarVar temporary) else [])) value)
        pure (emit [assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate [assign [] temporary (scalarVar target)] otherwiseBody] >> pure (scalarVar temporary))
      P.ArithmeticValue site expression bindings -> do
        result <- arithmeticMaterialization expression bindings
        traps <- gets materialTraps
        prefix <- gets materialPrefix
        origin <- diagnosticOriginExpression
        let terminate = if traps then Traps.exitWithStatusAt prefix origin (ExprLiteral "1") else builtin "exit" [arg (ExprLiteral "1")]
        pure $ do
          emit (Arithmetic.arithmeticStatements result <> [ifStatements [testEquals (Arithmetic.arithmeticError result) ""] [] (arithmeticDiagnostic False site result <> [terminate])])
          pure (Arithmetic.arithmeticValue result)

    lowerDeclarations :: [P.Declaration] -> Materialize scope [FishStatement]
    lowerDeclarations declarations = do
      frozen <- forM declarations $ \declaration -> do
        let value = case declaration of P.DeclareLocal _ _ scalar -> scalar; P.DeclareExport _ _ scalar -> scalar
        case value of
          Nothing -> pure (pure (), declaration, Nothing)
          Just scalar -> do
            result <- lowerScalar scalar
            name <- fresh "declaration_value"
            pure (result >>= \expression -> emit [assign [SetLocal] name expression], declaration, Just (scalarVar name))
      prefix <- gets materialPrefix
      installed <- forM frozen $ \(_, declaration, value) -> case declaration of
        P.DeclareLocal freshBinding name _ -> do
          target <- bindingName name
          temporary <- fresh "binding"
          pure (Binding.declareLocal (identifierText temporary) (Binding.bindingRuntime prefix (identifierText target)) freshBinding value)
        P.DeclareExport storage name _ -> do
          target <- bindingName name
          pure (Binding.declareExport (Binding.bindingRuntime prefix (identifierText target)) storage value)
      status <- setSourceStatus (ExprLiteral "0")
      pure $ renderEmission $ do
        traverse_ (\(emission, _, _) -> emission) frozen
        pure (concat installed <> [status])

    lowerPatternParts :: P.Pattern -> Materialize scope (Emission [(Bool, FishExpr TStr)])
    lowerPatternParts (P.MkPattern parts) = Region.runTaggedScalars $ forM parts $ \part -> do
      let (active, scalar) = case part of P.LiteralPattern value -> (False, value); P.ActivePattern value -> (True, value)
      value <- case scalar of
        P.Literal literal -> pure (Region.literalScalar literal)
        _ -> Region.captureScalar (fresh "pattern_part") (lowerScalar scalar)
      pure (active, value)

    lowerPatterns :: Identifier -> [P.Pattern] -> Materialize scope [FishStatement]
    lowerPatterns _ [] = pure [builtin "false" []]
    lowerPatterns input (patternValue : remaining) = do
      parts <- lowerPatternParts patternValue
      when (Pattern.sourceRequiresRuntime patternValue) (needNative NativePattern "Lazy quote-aware case pattern")
      native <- runtimeName "native"
      tailValue <- lowerPatterns input remaining
      pure $ renderEmission $ fmap (\values -> [ifStatements [Pattern.matchPattern (identifierText native) (scalarVar input) values] [builtin "true" []] tailValue]) parts

-- Source entry and exported functions own their entire temporary lifetime.
-- The first block-local capture preserves incoming status without overwriting a
-- caller binding; all observable guards precede helper/global/user effects.
