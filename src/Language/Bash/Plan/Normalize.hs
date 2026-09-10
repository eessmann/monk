{-# LANGUAGE LambdaCase #-}

module Language.Bash.Plan.Normalize
  ( normalizeSource,
    normalizeDocument,
    beginNormalization,
    NormalizationResult (..),
    SourceDocument (..),
  )
where

import Control.Monad (ap)
import Control.Monad.State.Strict (get, gets, put)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, toUpper)
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Arithmetic.Source qualified as ArithmeticSource
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Effects (admitPipeline, closeChildRegion, mayWriteBuiltin)
import Language.Bash.Plan.Effects qualified as Effects
import Language.Fish.DSL (SourcePos (..), SourceRange (..))
import Monk.Runtime.Integer qualified as Integer
import Monk.Translation.Contract (validateCallerContract)
import Monk.Translation.Types
import Numeric (showHex)
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralString)
import ShellCheck.Interface (ParseResult (..), Position (..))
import Prelude hiding (get, gets, identity, local, put)

data Normalization = Normalization
  { nConfig :: TranslateConfig,
    nPositions :: M.Map Id (Position, Position),
    nConstants :: M.Map Text Text,
    nNumeric :: S.Set Text,
    nContinueNumeric :: M.Map Int (S.Set Text),
    nBreakNumeric :: M.Map Int (S.Set Text),
    nVariables :: S.Set Text,
    nFunctions :: S.Set Text,
    nLocalFunctions :: S.Set Text,
    nResolutionFunctions :: S.Set Text,
    nDefinitions :: M.Map Text (Text, Int),
    nFunctionDependencies :: M.Map Text (M.Map Text (Text, Int)),
    nCurrentDependencies :: M.Map Text (Text, Int),
    nAllFunctions :: S.Set Text,
    nActive :: [Text],
    nLocals :: S.Set Text,
    nDirect :: Bool,
    nLoop :: Int,
    nReserved :: S.Set Text,
    nDocument :: Maybe Text,
    nSourceStack :: [Text],
    nInSource :: Bool,
    nSourceArgvOwned :: Bool,
    nChild :: Bool,
    nResolutionStable :: Bool,
    nCommandLine :: Int,
    nFunctionBodies :: M.Map Text [P.Statement],
    nSourceReturns :: S.Set P.SourceEntryContext,
    nDescriptors :: S.Set Int,
    nWritableDescriptors :: S.Set Int,
    nDirectoryFacts :: Directory.DirectoryFacts,
    nDirectoryOutcomes :: Maybe (Directory.DirectoryFacts, Directory.DirectoryFacts)
  }

-- | The IO graph driver supplies immutable documents only when authoritative
-- normalization reaches an executable source occurrence.
data SourceDocument = SourceDocument Text ParseResult

data NormalizationResult a
  = NormalizationFailed (NonEmpty Diagnostic)
  | NormalizationComplete a
  | NormalizationNeedsSource P.SourceRequest (SourceDocument -> NormalizationResult a)

instance Functor NormalizationResult where
  fmap f = \case
    NormalizationFailed errors -> NormalizationFailed errors
    NormalizationComplete value -> NormalizationComplete (f value)
    NormalizationNeedsSource request resume -> NormalizationNeedsSource request (fmap f . resume)

instance Applicative NormalizationResult where
  pure = NormalizationComplete
  (<*>) = ap

instance Monad NormalizationResult where
  NormalizationFailed errors >>= _ = NormalizationFailed errors
  NormalizationComplete value >>= next = next value
  NormalizationNeedsSource request resume >>= next = NormalizationNeedsSource request (resume >=> next)

type Normalize = StateT Normalization NormalizationResult

normalizeSource :: TranslateConfig -> ParseResult -> Either (NonEmpty Diagnostic) P.SourcePlan
normalizeSource cfg = withoutSources . startNormalization cfg Nothing

normalizeDocument :: TranslateConfig -> Text -> ParseResult -> Either (NonEmpty Diagnostic) P.SourcePlan
normalizeDocument cfg document = withoutSources . startNormalization cfg (Just document)

beginNormalization :: TranslateConfig -> Text -> ParseResult -> NormalizationResult P.SourcePlan
beginNormalization cfg document = startNormalization cfg (Just document)

withoutSources :: NormalizationResult a -> Either (NonEmpty Diagnostic) a
withoutSources = \case
  NormalizationFailed errors -> Left errors
  NormalizationComplete value -> Right value
  NormalizationNeedsSource request _ -> Left (diagnostic (P.sourceRequestRange request) "source-environment" "Literal source requires an explicit graph environment" :| [])

startNormalization :: TranslateConfig -> Maybe Text -> ParseResult -> NormalizationResult P.SourcePlan
startNormalization cfg document parsed = do
  when
    (entryMode cfg == Standalone && callerContract cfg /= emptyCallerContract)
    (NormalizationFailed (diagnostic Nothing "caller-contract-mode" "Standalone execution cannot carry a caller contract" :| []))
  either
    (\message -> NormalizationFailed (diagnostic Nothing "caller-contract" message :| []))
    pure
    (validateCallerContract (callerContract cfg))
  root <- maybe (NormalizationFailed (diagnostic Nothing "parse-root" "Missing Bash parse root" :| [])) pure (prRoot parsed)
  let imports = callerContract cfg
      initial =
        Normalization
          cfg
          (prTokenPositions parsed)
          (M.singleton "IFS" " \t\n")
          mempty
          mempty
          mempty
          (initializedImports imports)
          (M.keysSet (callerFunctions imports))
          mempty
          mempty
          (M.fromList [(name, ("<import>", negate index - 1)) | (index, name) <- zip [0 ..] (M.keys (callerFunctions imports))])
          mempty
          mempty
          (functionNames root)
          []
          mempty
          True
          0
          (sourceNames root <> contractNames imports)
          document
          [documentName parsed]
          False
          False
          False
          True
          1
          mempty
          mempty
          (S.fromList [0, 1, 2])
          (S.fromList [1, 2])
          (Directory.MkDirectoryFacts Directory.InitialDirectory False)
          Nothing
  when
    (entryMode cfg == Sourceable && callerAmbientEffects imports /= NoRelevantAmbientEffects)
    (NormalizationFailed (diagnostic Nothing "caller-effects" "Sourceable execution requires explicit no-relevant-ambient-effects obligations" :| []))
  (statement, final) <- runStateT (normalizeStatement root) initial
  pure (P.SourcePlan cfg [statement] (nReserved final))

documentName :: ParseResult -> Text
documentName parsed = maybe "<input>" (toText . posFile . fst . snd) (M.lookupMin (prTokenPositions parsed))

diagnostic :: Maybe SourceRange -> Text -> Text -> Diagnostic
diagnostic range code message = MkDiagnostic (MkDiagnosticCode ("monk.semantic." <> code)) PhaseTranslate DiagnosticError Unsafe message range

reject :: Token -> Text -> Text -> Normalize a
reject token code message = do
  range <- tokenRange token
  lift (NormalizationFailed (diagnostic range code message :| []))

tokenRange :: Token -> Normalize (Maybe SourceRange)
tokenRange token = gets (fmap convert . M.lookup (getId token) . nPositions)
  where
    convert (start, end) = MkSourceRange (point start) (point end)
    point p = MkSourcePos (toText (posFile p)) (fromInteger (posLine p)) (fromInteger (posColumn p))

normalizeStatement :: Token -> Normalize P.Statement
normalizeStatement token = do
  range <- tokenRange token
  modify' (\s -> s {nCommandLine = maybe 1 (srcLine . rangeStart) range, nDirectoryOutcomes = Nothing})
  P.Statement range <$> case token of
    T_Script _ _ body -> P.Sequence <$> normalizeStatements body
    T_BraceGroup _ body -> P.Sequence <$> normalizeStatements body
    T_Redirecting _ [] body -> statementNode <$> normalizeStatement body
    T_Redirecting _ redirects body -> normalizeRedirected token redirects body
    T_Annotation _ _ body -> statementNode <$> normalizeStatement body
    T_Pipeline _ [] [body] -> statementNode <$> normalizeStatement body
    T_Pipeline _ pipes body
      | all ordinaryPipe pipes,
        length body > 1 -> do
          childrenValue <- traverse (normalizeChild token . (: [])) body
          stages <- maybe (reject token "empty-pipeline" "A pipeline needs stages") pure (NE.nonEmpty childrenValue)
          either (reject token "pipeline-signal-lifetime") pure (admitPipeline stages)
          pure (P.Pipeline stages)
    T_Subshell _ body -> P.Subshell <$> normalizeChild token body
    T_Banged _ body -> do
      value <- nested body
      modify' (\flow -> flow {nDirectoryOutcomes = fmap (\(success, failure) -> (failure, success)) (nDirectoryOutcomes flow)})
      pure (P.Negate value)
    T_AndIf _ a b -> normalizeAndOr P.And a b
    T_OrIf _ a b -> normalizeAndOr P.Or a b
    T_IfExpression _ branches other -> normalizeIf branches other
    T_WhileExpression _ condition body -> normalizeLoop token False condition body
    T_UntilExpression _ condition body -> normalizeLoop token True condition body
    T_ForArithmetic _ initial predicate increment body -> do
      (initialValue, initialExit) <- arithmeticStatement initial
      before <- get
      modify' (\s -> s {nDirect = False, nLoop = nLoop s + 1, nBreakNumeric = M.delete (nLoop s + 1) (nBreakNumeric s), nContinueNumeric = M.delete (nLoop s + 1) (nContinueNumeric s), nConstants = mempty})
      (predicateValue0, predicateExit) <- arithmeticStatement predicate
      let predicateValue = case (predicate, predicateValue0) of
            (TA_Sequence _ [], P.Statement location (P.ArithmeticCommand site _ bindings)) -> P.Statement location (P.ArithmeticCommand site (A.ArithmeticLiteral 1) bindings)
            _ -> predicateValue0
      bodyValue <- normalizeStatements body
      modify' (\s -> (joinContinueNumeric s) {nConstants = mempty})
      (incrementValue, incrementExit) <- arithmeticStatement increment
      after <- get
      unless
        (nNumeric before `S.isSubsetOf` nNumeric after)
        (reject token "loop-numeric-flow" "Arithmetic loop invalidates an integer fact required by the next iteration")
      checkDirectoryLoop token before after (initialValue : predicateValue : incrementValue : bodyValue)
      let exits = foldl' joinStates (loopExit before after) (initialExit <> predicateExit <> incrementExit)
      put exits {nContinueNumeric = nContinueNumeric before, nBreakNumeric = nBreakNumeric before}
      pure (P.ArithmeticFor initialValue predicateValue incrementValue bodyValue)
    T_ForIn _ name values body -> do
      checkedName token (toText name)
      storage <- storageFor token False (toText name)
      wordsValue <- normalizeWords values
      before <- get
      let binder = toText name
          numericWords = all (\case P.OneField (P.Literal value) -> numericLiteral value; _ -> False) wordsValue
      modify' (\s -> s {nVariables = S.insert binder (nVariables s), nConstants = mempty, nNumeric = (if numericWords then S.insert else S.delete) binder (nNumeric s), nDirect = False, nLoop = nLoop s + 1, nBreakNumeric = M.delete (nLoop s + 1) (nBreakNumeric s), nContinueNumeric = M.delete (nLoop s + 1) (nContinueNumeric s)})
      bodyValue <- traverse normalizeStatement body
      modify' joinContinueNumeric
      after <- get
      unless
        (S.delete binder (nNumeric before) `S.isSubsetOf` nNumeric after)
        (reject token "loop-numeric-flow" "Loop body invalidates a numeric fact required at the next iteration")
      checkDirectoryLoop token before after bodyValue
      put (loopExit before after)
      pure (P.ForLoop storage (toText name) wordsValue bodyValue)
    T_Function _ _ _ name body -> normalizeFunction token (toText name) body
    T_CaseExpression _ value arms -> do
      scalar <- normalizeScalar value
      before <- get
      put before {nDirect = False}
      armValues <- forM arms $ \(ending, patterns, body) -> do
        entry <- get
        patternsValue <- forM patterns $ \patternToken -> do
          prefix <- get
          patternValue <- normalizePattern patternToken
          afterPattern <- get
          put (joinStates prefix afterPattern)
          pure patternValue
        bodyValue <- traverse normalizeStatement body
        afterBody <- get
        put (joinStates entry afterBody)
        pure (P.CaseArm patternsValue bodyValue (case ending of CaseBreak -> P.StopCase; CaseFallThrough -> P.FallThrough; CaseContinue -> P.Retest))
      modify' (\flow -> flow {nDirect = nDirect before, nConstants = mempty})
      pure (P.Case scalar armValues)
    T_SimpleCommand _ assignments command -> normalizeCommand token assignments command
    T_Condition _ _ condition -> normalizeCondition condition
    T_Arithmetic {} -> do
      (site, expression, bindings) <- normalizeArithmeticAt token
      pure (P.ArithmeticCommand site expression bindings)
    _ -> reject token "unsupported-syntax" ("No admitted semantics for " <> tokenKind token <> " in statement context")
  where
    statementNode (P.Statement _ node) = node
    ordinaryPipe (T_Pipe _ "|") = True
    ordinaryPipe _ = False
    arithmeticStatement value = do
      ((site, expression, bindings), (numeric, variables), potentialFailure) <- normalizeArithmeticRegion value
      errorExit <- get
      -- The next loop clause executes only after successful header evaluation.
      modify' (\flow -> flow {nNumeric = numeric, nVariables = variables})
      range <- tokenRange value
      pure (P.Statement range (P.ArithmeticCommand site expression bindings), [errorExit | potentialFailure])

normalizeRedirected :: Token -> [Token] -> Token -> Normalize P.StatementNode
normalizeRedirected _ redirects function@(T_Function _ _ _ name body) = normalizeFunctionWith redirects function (toText name) body
normalizeRedirected parent redirects body = do
  before <- get
  operations <- traverse normalizeRedirect redirects
  writable <- gets nWritableDescriptors
  value <- normalizeStatement body
  after <- get
  unless
    (nLocals before == nLocals after)
    (reject parent "redirect-local-scope" "A local declaration inside a redirected compound needs an explicit function-frame lifetime")
  when
    (not (S.member 1 writable) && mayWriteBuiltin (nFunctionBodies before) [value])
    (reject parent "redirect-closed-writer" "A builtin writing a closed stdout needs its original error and owner termination semantics")
  put after {nDescriptors = nDescriptors before, nWritableDescriptors = nWritableDescriptors before}
  pure (P.Redirected operations value)

normalizeRedirect :: Token -> Normalize P.Redirection
normalizeRedirect token = case token of
  T_FdRedirect _ source operation -> case operation of
    T_IoDuplicate _ operator target -> do
      input <- direction operator
      descriptor <- sourceNumber source input
      if target == "-"
        then modify' (\flow -> flow {nDescriptors = S.delete descriptor (nDescriptors flow), nWritableDescriptors = S.delete descriptor (nWritableDescriptors flow)}) >> pure (P.CloseDescriptor descriptor input)
        else do
          targetNumber <- number target
          available <- gets nDescriptors
          unless (S.member targetNumber available) (reject token "redirect-closed-source" "Duplicating a closed descriptor needs owned Bash error semantics")
          modify' (\flow -> flow {nDescriptors = S.insert descriptor (nDescriptors flow), nWritableDescriptors = (if S.member targetNumber (nWritableDescriptors flow) then S.insert else S.delete) descriptor (nWritableDescriptors flow)})
          pure (P.DuplicateDescriptor descriptor targetNumber input)
    T_IoFile _ operator file -> do
      input <- direction operator
      descriptor <- sourceNumber source input
      unless
        (getLiteralString file == Just "/dev/null")
        (reject token "redirect-file" "File opens require owned descriptor and failure semantics; only the stable null device is admitted")
      modify' (\flow -> flow {nDescriptors = S.insert descriptor (nDescriptors flow), nWritableDescriptors = (if input then S.delete else S.insert) descriptor (nWritableDescriptors flow)})
      pure (P.NullDescriptor descriptor input)
    _ -> reject token "redirect-shape" "No owned semantics for this redirection operation"
  _ -> reject token "redirect-shape" "Expected an explicit descriptor operation"
  where
    direction = \case
      T_Less {} -> pure True
      T_LESSAND {} -> pure True
      T_Greater {} -> pure False
      T_GREATAND {} -> pure False
      T_DGREAT {} -> pure False
      T_CLOBBER {} -> pure False
      _ -> reject token "redirect-mode" "This descriptor mode has no admitted primitive"
    sourceNumber "" input = pure (if input then 0 else 1)
    sourceNumber value _ = number value
    number value = case value of
      "0" -> pure 0
      "1" -> pure 1
      "2" -> pure 2
      _ -> reject token "redirect-descriptor" "Only standard descriptors zero through two are admitted"

normalizeChild :: Token -> [Token] -> Normalize P.ChildRegion
normalizeChild token body = do
  before <- get
  range <- tokenRange token
  modify' (\s -> s {nDirect = False, nLoop = 0, nChild = True})
  bodyValue <- normalizeStatements body
  after <- get
  child <- either (reject token "child-snapshot") pure (closeChildRegion range (nFunctionBodies before) bodyValue)
  put
    before
      { nReserved = nReserved before <> nReserved after,
        nCurrentDependencies = nCurrentDependencies before <> nCurrentDependencies after
      }
  pure child

contractNames :: CallerContract -> S.Set Text
contractNames contract =
  M.keysSet (callerVariables contract)
    <> M.keysSet (callerFunctions contract)
    <> S.fromList (map functionTarget (M.elems (callerFunctions contract)))
    <> callerExportedFunctions contract

nested :: Token -> Normalize P.Statement
nested token = do
  direct <- gets nDirect
  modify' (\s -> s {nDirect = False})
  value <- normalizeStatement token
  modify' (\s -> s {nDirect = direct})
  pure value

normalizeAndOr :: (P.Statement -> P.Statement -> P.StatementNode) -> Token -> Token -> Normalize P.StatementNode
normalizeAndOr constructor a b = do
  left <- nested a
  before <- get
  let onSuccess = case constructor left left of P.And {} -> True; _ -> False
      select success flow = case nDirectoryOutcomes flow of
        Just (yes, no) -> flow {nDirectoryFacts = if success then yes else no}
        Nothing -> flow
  put (select onSuccess before)
  right <- nested b
  after <- get
  put (joinStates (select (not onSuccess) before) after)
  pure (constructor left right)

joinStates :: Normalization -> Normalization -> Normalization
joinStates before after =
  before
    { nConstants = M.mergeWithKey (\_ a b -> if a == b then Just a else Nothing) (const mempty) (const mempty) (nConstants before) (nConstants after),
      nVariables = nVariables before `S.intersection` nVariables after,
      nNumeric = nNumeric before `S.intersection` nNumeric after,
      nContinueNumeric = M.unionWith S.intersection (nContinueNumeric before) (nContinueNumeric after),
      nBreakNumeric = M.unionWith S.intersection (nBreakNumeric before) (nBreakNumeric after),
      nReserved = nReserved before <> nReserved after,
      nResolutionStable = nResolutionStable before && nResolutionStable after,
      nDirectoryFacts = Directory.joinDirectoryFacts (nDirectoryFacts before) (nDirectoryFacts after),
      nDirectoryOutcomes = Nothing,
      nResolutionFunctions = nResolutionFunctions before <> nResolutionFunctions after,
      nCurrentDependencies = nCurrentDependencies before <> nCurrentDependencies after,
      nSourceReturns = nSourceReturns before <> nSourceReturns after
    }

normalizeIf :: [([Token], [Token])] -> [Token] -> Normalize P.StatementNode
normalizeIf [] other = P.Sequence <$> traverse nested other
normalizeIf ((condition, body) : remaining) other = do
  conditionValue <- traverse nested condition
  before <- get
  forM_ (nDirectoryOutcomes before) $ \(success, _) -> modify' (\flow -> flow {nDirectoryFacts = success})
  bodyValue <- traverse nested body
  yes <- get
  put before
  forM_ (nDirectoryOutcomes before) $ \(_, failure) -> modify' (\flow -> flow {nDirectoryFacts = failure})
  otherValue <- normalizeIf remaining other
  no <- get
  put (joinStates yes no) {nDirect = nDirect before}
  pure (P.Conditional conditionValue bodyValue [P.Statement Nothing otherValue])

normalizeLoop :: Token -> Bool -> [Token] -> [Token] -> Normalize P.StatementNode
normalizeLoop token inverted condition body = do
  before <- get
  modify' (\s -> s {nConstants = mempty, nDirect = False, nLoop = nLoop s + 1, nBreakNumeric = M.delete (nLoop s + 1) (nBreakNumeric s), nContinueNumeric = M.delete (nLoop s + 1) (nContinueNumeric s)})
  conditionValue <- traverse normalizeStatement condition
  bodyValue <- traverse normalizeStatement body
  modify' joinContinueNumeric
  after <- get
  unless
    (nNumeric before `S.isSubsetOf` nNumeric after)
    (reject token "loop-numeric-flow" "Loop body invalidates a numeric fact required at the next iteration")
  checkDirectoryLoop token before after (conditionValue <> bodyValue)
  put (loopExit before after)
  pure (P.WhileLoop inverted conditionValue bodyValue)

-- Break exits bypass trailing writes; no fallthrough constant is an exit proof.
loopExit :: Normalization -> Normalization -> Normalization
loopExit before after =
  let joined = joinStates before after
   in joined {nConstants = mempty, nNumeric = nNumeric joined `S.intersection` M.findWithDefault (nNumeric joined) (nLoop after) (nBreakNumeric after), nContinueNumeric = nContinueNumeric before, nBreakNumeric = nBreakNumeric before}

-- Continue edges bypass subsequent statements. Intersect their finite numeric
-- facts with ordinary fallthrough before admitting the owned backedge.
joinContinueNumeric :: Normalization -> Normalization
joinContinueNumeric flow = flow {nNumeric = nNumeric flow `S.intersection` M.findWithDefault (nNumeric flow) (nLoop flow) (nContinueNumeric flow)}

-- A relative dependency cannot freeze the first iteration's cwd when a
-- directory transition invalidates that fact on the loop backedge.
checkDirectoryLoop :: Token -> Normalization -> Normalization -> [P.Statement] -> Normalize ()
checkDirectoryLoop token before after body =
  when
    (nDirectoryFacts before /= nDirectoryFacts after && any varyingSource body)
    (reject token "source-directory-loop" "A relative source in a directory-changing loop requires an invariant absolute execution cwd")
  where
    varyingSource (P.Statement _ node) = case node of
      P.SourceBody request nestedBody ->
        (not (T.isPrefixOf "/" (P.sourceRequestTarget request)) && not (maybe False (T.isPrefixOf "/") (P.sourceRequestWorkingDirectory request))) || any varyingSource nestedBody
      P.Sequence values -> any varyingSource values
      P.AssignmentCommand _ values -> any varyingSource values
      P.Redirected _ value -> varyingSource value
      P.And left right -> any varyingSource [left, right]
      P.Or left right -> any varyingSource [left, right]
      P.Negate value -> varyingSource value
      P.Conditional condition yes no -> any varyingSource (condition <> yes <> no)
      P.WhileLoop _ condition values -> any varyingSource (condition <> values)
      P.ForLoop _ _ _ values -> any varyingSource values
      P.ArithmeticFor initial predicate increment values -> any varyingSource (initial : predicate : increment : values)
      P.Case _ arms -> any (\(P.CaseArm _ values _) -> any varyingSource values) arms
      P.Approximate _ values -> any varyingSource values
      _ -> False

normalizeFunction :: Token -> Text -> Token -> Normalize P.StatementNode
normalizeFunction = normalizeFunctionWith []

normalizeFunctionWith :: [Token] -> Token -> Text -> Token -> Normalize P.StatementNode
normalizeFunctionWith redirects token name body = do
  checkedName token name
  cfg <- gets nConfig
  when
    (entryMode cfg == Sourceable && not (S.member name (callerExportedFunctions (callerContract cfg))))
    (reject token "undeclared-function-export" "Sourceable function definitions must be declared caller exports")
  direct <- gets nDirect
  active <- gets nActive
  unless (direct && null active) (reject token "function-context" "Conditional or nested function definitions need call-time binding analysis")
  stack <- gets nSourceStack
  let Id definitionId = getId token
      identity = (fromMaybe "<input>" (viaNonEmpty last stack), definitionId)
  modify' (\s -> s {nFunctions = S.insert name (nFunctions s), nLocalFunctions = S.insert name (nLocalFunctions s), nDefinitions = M.insert name identity (nDefinitions s)})
  before <- get
  modify' (\s -> s {nConstants = mempty, nNumeric = mempty, nVariables = if entryMode cfg == Sourceable then initializedImports (callerContract cfg) else nVariables s, nActive = [name], nLocals = mempty, nContinueNumeric = mempty, nBreakNumeric = mempty, nLoop = 0, nDirect = True, nResolutionStable = True, nCurrentDependencies = mempty})
  bodyValue <-
    if null redirects
      then case body of
        T_BraceGroup _ statements -> normalizeStatements statements
        T_Redirecting _ [] (T_BraceGroup _ statements) -> normalizeStatements statements
        _ -> (: []) <$> normalizeStatement body
      else do
        range <- tokenRange token
        redirected <- normalizeRedirected token redirects body
        pure [P.Statement range redirected]
  after <- get
  put
    before
      { nReserved = nReserved before <> nReserved after,
        nResolutionFunctions = (if nResolutionStable after && nDirectoryFacts before == nDirectoryFacts after then S.delete else S.insert) name (nResolutionFunctions before),
        nFunctionDependencies = M.insert name (nCurrentDependencies after) (nFunctionDependencies before),
        nFunctionBodies = M.insert name bodyValue (nFunctionBodies before)
      }
  pure (P.DefineFunction name bodyValue)

normalizeCommand :: Token -> [Token] -> [Token] -> Normalize P.StatementNode
normalizeCommand token assignments = \case
  [] -> P.AssignmentCommand False <$> traverse (normalizeAssignment False) assignments
  headToken : arguments -> do
    unless (null assignments) (reject token "command-prefix" "Command-prefix assignment lifetime is not yet materialized")
    name <- resolveHead headToken
    functions <- gets nFunctions
    if S.member name functions
      then do
        active <- gets nActive
        descriptors <- gets nDescriptors
        writable <- gets nWritableDescriptors
        unless (descriptors == S.fromList [0, 1, 2] && S.fromList [1, 2] `S.isSubsetOf` writable) (reject token "call-descriptor-context" "Function invocation requires its admitted open standard descriptor context")
        when (name `elem` active) (reject token "recursion" "Recursive calls are outside the finite initial call envelope")
        cfg <- gets nConfig
        localDefinitions <- gets nLocalFunctions
        resolutionFunctions <- gets nResolutionFunctions
        definitions <- gets nDefinitions
        summaries <- gets nFunctionDependencies
        possible <- gets nAllFunctions
        let dependencies = M.findWithDefault mempty name summaries
        unless
          (all (\(dependency, identity) -> if identity == ("<absent>", 0) then M.notMember dependency definitions && not (S.member dependency possible) else M.lookup dependency definitions == Just identity) (M.toList dependencies))
          (reject token "call-binding-context" "A function dependency was redefined after this body was normalized")
        locals <- gets nLocals
        initialized <- gets nVariables
        when
          (not (S.member name localDefinitions) && not (locals `S.isSubsetOf` initialized))
          (reject token "import-unset-environment" "Imported functions cannot observe an uninitialized local environment fallback")
        let target
              | S.member name localDefinitions = name
              | otherwise = maybe name functionTarget (M.lookup name (callerFunctions (callerContract cfg)))
            importedEffects =
              maybe False (not . S.null . S.intersection (S.fromList resolutionVariables) . functionWrites) (M.lookup name (callerFunctions (callerContract cfg)))
            importedDirectoryEffects = M.findWithDefault noDirectoryPermissions name (callerFunctionDirectories (callerContract cfg))
            writesDirectory access = access `elem` [WriteDirectory, ReadWriteDirectory]
            directoryAfterImport facts =
              Directory.MkDirectoryFacts
                (if writesDirectory (directoryCwd importedDirectoryEffects) then Directory.UnknownDirectory else Directory.directoryLocation facts)
                (Directory.directoryPreviousProved facts && not (writesDirectory (directoryOldpwd importedDirectoryEffects)))
        checkedCommand token target
        wordsValue <- normalizeWords arguments
        modify'
          ( \s ->
              s
                { nConstants = mempty,
                  nNumeric = mempty,
                  nResolutionStable = nResolutionStable s && not (S.member name resolutionFunctions || importedEffects),
                  nDirectoryFacts = if S.member name resolutionFunctions || importedEffects then Directory.MkDirectoryFacts Directory.UnknownDirectory False else directoryAfterImport (nDirectoryFacts s),
                  nCurrentDependencies = nCurrentDependencies s <> dependencies <> maybe mempty (M.singleton name) (M.lookup name definitions)
                }
          )
        pure (P.Invoke (P.Function target) wordsValue)
      else do
        allFunctions <- gets nAllFunctions
        when (S.member name allFunctions) (reject token "call-binding" "The function's call-time definition is not definite")
        active <- gets nActive
        unless (null active || name `elem` ["command", "builtin"]) $
          modify' (\flow -> flow {nCurrentDependencies = M.insert name ("<absent>", 0) (nCurrentDependencies flow)})
        builtinCommand name arguments
  where
    builtinCommand name arguments = case name of
      "eval" -> reject token "eval" "Runtime Bash program evaluation has no admitted translation"
      "local" -> do
        direct <- gets nDirect
        active <- gets nActive
        unless
          (direct && not (null active))
          (reject token "local-context" "Local bindings require a direct function-body declaration")
        normalizeDeclarations True token arguments
      "export" -> normalizeDeclarations False token arguments
      "readonly" -> do
        cfg <- gets nConfig
        unless
          (allowsApproximation cfg ReadonlyUnchecked)
          (reject token "readonly" "Readonly enforcement requires the named readonly-unchecked approximation")
        when
          (entryMode cfg == Sourceable)
          (reject token "readonly-caller" "Readonly caller binding attributes cannot be approximated by this sourceable contract")
        unless (length arguments == 1) (reject token "readonly-form" "Readonly approximation admits one explicit assignment; multiple operands need declaration expansion sequencing")
        values <- traverse (normalizeAssignment False) arguments
        range <- tokenRange token
        pure (P.Approximate ReadonlyUnchecked [P.Statement range (P.AssignmentCommand True values)])
      "unset" -> do
        names <- traverse (literalName token) arguments
        traverse_ (storageFor token False) names
        active <- gets nActive
        unless (null active) (reject token "unset-dynamic-local" "Function unset needs a modeled dynamic binding deletion")
        locals <- gets nLocals
        when (any (`S.member` locals) names) (reject token "unset-local" "Deleting local bindings requires explicit frame restoration")
        modify'
          ( \s ->
              s
                { nConstants = foldr M.delete (nConstants s) names,
                  nNumeric = nNumeric s <> S.fromList names,
                  nVariables = nVariables s S.\\ S.fromList names,
                  nResolutionStable = nResolutionStable s && not (any (`elem` resolutionVariables) names)
                }
          )
        pure (P.Sequence [P.Statement Nothing (P.Erase nameValue) | nameValue <- names])
      "shift" -> do
        sourced <- gets nInSource
        owned <- gets nSourceArgvOwned
        active <- gets nActive
        mode <- gets (entryMode . nConfig)
        unless
          ((not sourced && (mode == Standalone || not (null active))) || (sourced && owned))
          (reject token "source-argv-mutation" "Shift requires an owned argv; literal sources need a provably nonempty effective argument list")
        count <- case traverse getLiteralString arguments of
          Just [] -> pure 1
          Just [literal]
            | not (null literal),
              all isDigit literal,
              Just value <- readMaybe literal :: Maybe Integer,
              value <= min (toInteger (maxBound :: Int)) (2 ^ (63 :: Int) - 1) ->
                pure (fromInteger value)
          _ -> reject token "shift-operand" "Shift requires one nonnegative literal count"
        pure (P.ShiftArguments count)
      "cd" -> normalizeDirectory token name arguments
      "pwd" -> normalizeDirectory token name arguments
      "pushd" -> normalizeDirectory token name arguments
      "popd" -> normalizeDirectory token name arguments
      "test" -> normalizeFixedTest token "test" arguments
      "[" -> normalizeFixedTest token "[" arguments
      "set" -> normalizeSet token arguments
      "return" -> do
        active <- gets nActive
        mode <- gets (entryMode . nConfig)
        sourced <- gets nInSource
        when (null active && mode == Standalone && not sourced) (reject token "return-context" "Return requires an owned function or source boundary")
        value <- optionalStatus arguments
        when (sourced && null active) $ modify' (\s -> s {nSourceReturns = S.insert (entryContext s) (nSourceReturns s)})
        pure (P.Return value)
      "exit" -> P.Exit <$> optionalStatus arguments
      "break" -> loopControl P.Break arguments
      "continue" -> loopControl P.Continue arguments
      "source" -> normalizeSourceCall token arguments
      "." -> normalizeSourceCall token arguments
      "builtin" -> case arguments of
        next : rest -> do
          selected <- literalName token next
          unless (selected `elem` ("local" : "export" : "cd" : "pwd" : "pushd" : "popd" : safeBuiltins)) (reject token "builtin" "The selected builtin has no admitted operand semantics")
          builtinCommand selected rest
        [] -> reject token "builtin" "Builtin requires a command operand"
      "command" -> case arguments of
        next : rest -> do
          selected <- resolveHead next
          if selected `elem` ("local" : "export" : "cd" : "pwd" : "pushd" : "popd" : safeBuiltins)
            then builtinCommand selected rest
            else do
              when (selected `elem` unsupportedBuiltins) (reject token "command-builtin" "This dispatched builtin has no admitted semantics")
              P.Invoke (P.External selected) <$> normalizeWords rest
        [] -> reject token "command" "Command requires an executable operand"
      _ -> do
        when (name `elem` unsupportedBuiltins) (reject token "builtin" ("No admitted semantics for builtin " <> name))
        when (name == "printf") (validatePrintf token arguments)
        cfg <- gets nConfig
        when
          (entryMode cfg == Sourceable && name `notElem` safeBuiltins)
          (reject token "ambient-dispatch" "Sourceable external dispatch requires an explicit import")
        P.Invoke (if name `elem` safeBuiltins then P.Builtin name else P.External name) <$> normalizeWords arguments
    optionalStatus [] = pure Nothing
    optionalStatus [value] = do
      scalar <- normalizeScalar value
      constants <- gets nConstants
      let literal text = case readMaybe (toString text) :: Maybe Integer of
            Just number | show number == text, number >= negate (2 ^ (63 :: Int)), number < 2 ^ (63 :: Int) -> pure (Just (P.Literal (show (number `mod` 256))))
            _ -> reject token "status-argument" "Status operands require canonical signed-64-bit decimal data"
      case scalar of
        P.Literal text -> literal text
        P.Variable name | Just text <- M.lookup name constants -> literal text
        P.LastStatus -> pure (Just scalar)
        _ -> reject token "status-argument" "A status operand needs a proven bounded decimal value or the previous status"
    optionalStatus _ = reject token "status-argument" "Exit and return accept at most one status argument"
    loopControl node [] = do
      depth <- gets nLoop
      if depth > 0
        then do
          case node of
            P.Continue -> modify' (\s -> s {nContinueNumeric = M.insertWith S.intersection depth (nNumeric s) (nContinueNumeric s)})
            P.Break -> modify' (\s -> s {nBreakNumeric = M.insertWith S.intersection depth (nNumeric s) (nBreakNumeric s)})
            _ -> pure ()
          pure node
        else reject token "loop-control" "Loop control has no owned target"
    loopControl _ _ = reject token "loop-control-depth" "Only the immediate owned loop target is currently admitted"

normalizeDirectory :: Token -> Text -> [Token] -> Normalize P.StatementNode
normalizeDirectory token command operands = do
  cfg <- gets nConfig
  unless (stableDirectoryEnabled cfg) (reject token "directory-contract" "Directory operations require the stable directory contract")
  arguments <- normalizeWords operands
  constants <- gets nConstants
  directoryFacts <- gets nDirectoryFacts
  let known = \case
        P.Literal text -> Just text
        P.Variable name -> M.lookup name constants
        P.Concat parts -> T.concat <$> traverse known parts
        _ -> Nothing
      literalWord = \case P.OneField value -> known value; _ -> Nothing
  values <- maybe (reject token "directory-path" "Directory operands require one proved ordinary path") pure (traverse literalWord arguments)
  operation <- case (command, values) of
    ("pwd", []) -> pure (Directory.PrintDirectory False)
    ("pwd", ["-L"]) -> pure (Directory.PrintDirectory False)
    ("pwd", ["-P"]) -> pure (Directory.PrintDirectory True)
    ("popd", []) -> pure Directory.PopDirectory
    ("pushd", [path]) -> Directory.PushDirectory <$> proved path
    ("cd", ["-"]) | Directory.directoryPreviousProved directoryFacts -> pure Directory.ChangePreviousDirectory
    ("cd", ["-"]) -> case M.lookup "OLDPWD" constants of
      Just path | Directory.proveDirectoryPath path -> pure Directory.ChangePreviousDirectory
      _ -> reject token "directory-oldpwd" "cd - requires a proved ordinary OLDPWD path"
    ("cd", [path]) | not (T.isPrefixOf "-" path) -> Directory.ChangeDirectory <$> proved path
    ("cd", ["--", path]) -> Directory.ChangeDirectory <$> proved path
    ("cd", ["-L", path]) -> Directory.ChangeDirectory <$> proved path
    ("cd", ["-L", "--", path]) -> Directory.ChangeDirectory <$> proved path
    _ -> reject token "directory-form" "Directory options or implicit operands are outside the stable directory envelope"
  when (entryMode cfg == Sourceable) $
    unless
      (maybe False (`Directory.permitsDirectory` Directory.directoryPermissionsFor operation) (callerDirectory (callerContract cfg)))
      (reject token "directory-permission" "The caller contract does not grant this operation's cwd/PWD/OLDPWD/stack access")
  case operation of
    Directory.PrintDirectory _ -> pure ()
    _ -> modify' (\flow -> let before = nDirectoryFacts flow; success = Directory.successfulDirectory operation before in flow {nDirectoryFacts = Directory.joinDirectoryFacts success before, nDirectoryOutcomes = Just (success, before), nConstants = M.delete "OLDPWD" (M.delete "PWD" (nConstants flow))})
  pure (P.DirectoryOperation operation)
  where
    proved path = if Directory.proveDirectoryPath path then pure path else reject token "directory-path" "Directory path has interior parent cancellation or an unsupported shape"

safeBuiltins :: [Text]
safeBuiltins = ["echo", "printf", "true", "false", ":", "test", "["]

unsupportedBuiltins :: [Text]
unsupportedBuiltins = ["read", "declare", "typeset", "readonly", "export", "shopt", "trap", "exec", "shift", "wait", "cd", "pushd", "popd", "enable", "hash", "alias", "unalias", "getopts", "readarray", "mapfile", "let", "caller", "jobs", "fg", "bg", "disown", "umask", "ulimit", "bind", "help", "history", "complete", "compgen", "compopt", "dirs", "fc", "kill", "logout", "pwd", "suspend", "times", "type"]

normalizeFixedTest :: Token -> Text -> [Token] -> Normalize P.StatementNode
normalizeFixedTest token name arguments = do
  operands <-
    if name == "["
      then case reverse arguments of
        end : rest | getLiteralString end == Just "]" -> pure (reverse rest)
        _ -> reject token "test-terminator" "Bracket test needs a literal closing bracket"
      else pure arguments
  wordsValue <- normalizeWords operands
  values <- forM wordsValue $ \case
    P.OneField scalar -> pure scalar
    _ -> reject token "test-cardinality" "Fixed-arity tests require one field per operand"
  fixed values
  where
    invoke values = P.Invoke (P.Builtin "test") (map P.OneField values)
    fixed [] = pure (P.Invoke (P.Builtin "false") [])
    fixed [value] = pure (invoke [P.Literal "-n", value])
    fixed [P.Literal "!", value] = P.Negate . P.Statement Nothing <$> fixed [value]
    fixed [P.Literal operator, value] | operator `elem` ["-n", "-z", "-e", "-f", "-d", "-r", "-w", "-x", "-s", "-L", "-h"] = pure (invoke [P.Literal operator, value])
    fixed [left, P.Literal operator, right]
      | operator `elem` ["=", "!="] = pure (invoke [left, P.Literal operator, right])
      | operator `elem` ["-eq", "-ne", "-lt", "-le", "-gt", "-ge"] = do
          constants <- gets nConstants
          let canonical text = case readMaybe (toString text) :: Maybe Integer of
                Just value -> show value == text && value >= negate (2 ^ (63 :: Int)) && value < 2 ^ (63 :: Int)
                Nothing -> False
              admitted = \case
                P.Literal value -> canonical value
                P.Variable variable -> maybe False canonical (M.lookup variable constants)
                P.ArithmeticValue {} -> True
                P.ArgumentCount -> True
                P.LastStatus -> True
                _ -> False
          unless (admitted left && admitted right) (reject token "test-number" "Numeric test requires proven canonical signed decimal data")
          pure (freezeComparison constants operator left right)
    fixed (P.Literal "!" : values) | length values `elem` [2, 3] = P.Negate . P.Statement Nothing <$> fixed values
    fixed _ = reject token "test-operands" "Test requires an admitted fixed-arity operation"

validatePrintf :: Token -> [Token] -> Normalize ()
validatePrintf token arguments = case arguments of
  marker : remaining | getLiteralString marker == Just "--" -> validatePrintf token remaining
  formatToken : operands -> do
    formatValue <- maybe invalidFormat (pure . toText) (getLiteralString formatToken)
    when (T.isPrefixOf "-" formatValue) (reject token "printf-option" "Printf options require an owned builtin operation")
    conversions <- maybe invalidFormat pure (formatConversions formatValue)
    unless (null conversions) $ forM_ (zip [0 :: Int ..] operands) $ \(index, operand) ->
      when (listToMaybe (drop (index `mod` length conversions) conversions) == Just 'd') $ do
        literal <- maybe invalidNumber (pure . toText) (getLiteralString operand)
        unless (canonicalDecimal literal) invalidNumber
  [] -> reject token "printf-format" "Printf needs a format operand"
  where
    invalidFormat :: Normalize a
    invalidFormat = reject token "printf-format" "Printf requires a literal %s/%d/%% format with admitted byte escapes"
    invalidNumber :: Normalize a
    invalidNumber = reject token "printf-number" "Numeric printf requires literal canonical signed-64-bit decimal data"
    canonicalDecimal value = case readMaybe (toString value) :: Maybe Integer of
      Just number -> show number == value && number >= negate (2 ^ (63 :: Int)) && number < 2 ^ (63 :: Int)
      Nothing -> False
    formatConversions value = case T.uncons value of
      Nothing -> Just []
      Just ('%', rest) -> case T.uncons rest of
        Just ('%', remaining) -> formatConversions remaining
        Just (conversion, remaining) | conversion `elem` ("sd" :: String) -> (conversion :) <$> formatConversions remaining
        _ -> Nothing
      Just ('\\', rest) -> case T.uncons rest of
        Just (escape, remaining) | escape `elem` ("abefnrtv\\" :: String) -> formatConversions remaining
        Just (escape, remaining) | isOctDigit escape -> formatConversions (dropDigits 2 isOctDigit remaining)
        Just ('x', remaining) | Just (digit, _) <- T.uncons remaining, isHexDigit digit -> formatConversions (dropDigits 2 isHexDigit remaining)
        _ -> Nothing
      Just (_, remaining) -> formatConversions remaining
    dropDigits :: Int -> (Char -> Bool) -> Text -> Text
    dropDigits maximumCount predicate = go maximumCount
      where
        go 0 value = value
        go count value = case T.uncons value of
          Just (character, remaining) | predicate character -> go (count - 1) remaining
          _ -> value

normalizeSet :: Token -> [Token] -> Normalize P.StatementNode
normalizeSet token arguments = case traverse getLiteralString arguments of
  Just ["-e"] -> setOption P.Errexit True
  Just ["+e"] -> setOption P.Errexit False
  Just ["-o", "pipefail"] -> setOption P.Pipefail True
  Just ["+o", "pipefail"] -> setOption P.Pipefail False
  _ -> case arguments of
    marker : rest | getLiteralString marker == Just "--" -> do
      sourced <- gets nInSource
      sourceable <- gets ((== Sourceable) . entryMode . nConfig)
      when (sourced || sourceable) (reject token "source-argv-mutation" "Source may not mutate the caller argv through a generated boundary")
      P.SetArguments <$> normalizeWords rest
    _ -> reject token "set-option" "Only explicit argv and errexit/pipefail option transitions are admitted"
  where
    setOption option enabled = do
      sourceable <- gets ((== Sourceable) . entryMode . nConfig)
      when sourceable (reject token "sourceable-option-effect" "Persistent caller option effects are not yet represented by the sourceable contract")
      pure (P.SetOption option enabled)

initializedImports :: CallerContract -> S.Set Text
initializedImports = M.keysSet . M.filter (\(ScalarBinding access _ _) -> access /= OutputBinding) . callerVariables

readBinding :: Token -> Text -> Normalize ()
readBinding token name = do
  cfg <- gets nConfig
  let directoryRead = maybe False (\permissions -> case name of "PWD" -> directoryPwd permissions `elem` [ReadDirectory, ReadWriteDirectory]; "OLDPWD" -> directoryOldpwd permissions `elem` [ReadDirectory, ReadWriteDirectory]; _ -> False) (callerDirectory (callerContract cfg))
  when (entryMode cfg == Sourceable && name /= "#" && not directoryRead) $ do
    locals <- gets nLocals
    initialized <- gets nVariables
    unless
      (S.member name locals || (M.member name (callerVariables (callerContract cfg)) && S.member name initialized))
      (reject token "undeclared-binding-read" "Sourceable reads require a declared initialized scalar binding")

storageFor :: Token -> Bool -> Text -> Normalize P.Storage
storageFor token local name = do
  when (name == "PWD") (reject token "directory-pwd-write" "Direct PWD mutation is outside the stable directory contract")
  cfg <- gets nConfig
  when (stableDirectoryEnabled cfg && local && name == "OLDPWD") (reject token "directory-oldpwd-scope" "Stable directory state requires a global OLDPWD")
  when (name == "OLDPWD") $ modify' (\flow -> flow {nDirectoryFacts = (nDirectoryFacts flow) {Directory.directoryPreviousProved = False}})
  when (stableDirectoryEnabled cfg && name `elem` ["CDPATH", "dirstack"]) (reject token "directory-state-write" "Direct directory contract state mutation is not represented")
  when
    (entryMode cfg == Sourceable && name == "IFS")
    (reject token "sourceable-ifs-effect" "Persistent caller IFS effects are not yet represented by the sourceable contract")
  active <- gets nActive
  locals <- gets nLocals
  if local
    then pure P.Local
    else
      if entryMode cfg == Standalone
        then pure (if null active then P.Global else P.Visible)
        else
          if name == "OLDPWD" && maybe False ((`elem` [WriteDirectory, ReadWriteDirectory]) . directoryOldpwd) (callerDirectory (callerContract cfg))
            then pure P.Global
            else
              if S.member name locals
                then pure P.Visible
                else case M.lookup name (callerVariables (callerContract cfg)) of
                  Just (ScalarBinding access scope exported)
                    | access /= InputBinding ->
                        pure (if null active && scope == GlobalBinding then P.CallerGlobal exported else P.CallerVisible exported)
                  _ -> reject token "undeclared-binding-write" "Sourceable writes require a declared writable scalar binding"

normalizeDeclarations :: Bool -> Token -> [Token] -> Normalize P.StatementNode
normalizeDeclarations local parent operands = do
  when (null operands) (reject parent "declaration-inspection" "Declaration inspection requires an explicit binding operation")
  -- Expansions observe the command-entry binding scope, with expansion effects
  -- sequenced, before any declaration installs its destination.
  values <- forM operands $ \operand -> do
    (name, value) <- case operand of
      T_Assignment _ Assign name [] value -> pure (toText name, Just value)
      _ -> (,Nothing) <$> literalName parent operand
    checkedName operand name
    when (not local && name == "IFS") (reject operand "export-ifs" "Exported IFS requires an explicit source environment alias")
    storage <- storageFor operand local name
    scalar <- traverse normalizeScalar value
    cfg <- gets nConfig
    initialized <- gets nVariables
    locals <- gets nLocals
    when
      (not local && entryMode cfg == Sourceable && isNothing scalar && not (S.member name initialized || S.member name locals))
      (reject operand "export-unset-caller" "Exporting an unset caller binding requires a persistent caller attribute contract")
    pure (name, storage, scalar)
  declarations <- forM values $ \(name, storage, value) -> do
    locals <- gets nLocals
    let freshLocal = local && not (S.member name locals)
    modify'
      ( \flow ->
          flow
            { nLocals = if local then S.insert name (nLocals flow) else nLocals flow,
              nVariables = case value of Just _ -> S.insert name (nVariables flow); Nothing | freshLocal -> S.delete name (nVariables flow); _ -> nVariables flow,
              nNumeric = case value of Just scalar -> (if numericScalar scalar then S.insert else S.delete) name (nNumeric flow); Nothing | freshLocal -> S.insert name (nNumeric flow); _ -> nNumeric flow,
              nConstants = case value of Just scalar -> maybe (M.delete name (nConstants flow)) (\literal -> M.insert name literal (nConstants flow)) (scalarLiteral scalar); Nothing | freshLocal -> M.insert name "" (nConstants flow); _ -> nConstants flow,
              nResolutionStable = nResolutionStable flow && name `notElem` resolutionVariables
            }
      )
    pure (if local then P.DeclareLocal freshLocal name value else P.DeclareExport storage name value)
  pure (P.DeclarationCommand declarations)

normalizeAssignment :: Bool -> Token -> Normalize P.Statement
normalizeAssignment local token = do
  (name, value, appendValue) <- case token of
    T_Assignment _ Assign name [] value -> pure (toText name, value, False)
    T_Assignment _ Append name [] value -> pure (toText name, value, True)
    _ | local, isJust (getLiteralString token) -> reject token "unset-local-declaration" "Bare local declarations require an unset binding with owned scope lifetime"
    _ -> reject token "assignment-shape" "Only replacing scalar assignments are admitted; arrays and append assignments need storage plans"
  checkedName token name
  rhs <- normalizeScalar value
  when appendValue (readBinding token name)
  let valuePlan = if appendValue then P.AppendValue name rhs else rhs
  storage <- storageFor token local name
  modify'
    ( \s ->
        s
          { nVariables = S.insert name (nVariables s),
            nLocals = if local then S.insert name (nLocals s) else nLocals s,
            nResolutionStable = nResolutionStable s && name `notElem` resolutionVariables,
            nNumeric = (if numericScalar valuePlan then S.insert else S.delete) name (nNumeric s),
            nConstants = case scalarLiteral valuePlan of Just literal -> M.insert name literal (nConstants s); Nothing -> M.delete name (nConstants s)
          }
    )
  range <- tokenRange token
  pure (P.Statement range (P.Assign storage name valuePlan))

resolveHead :: Token -> Normalize Text
resolveHead token = case getLiteralString token of
  Just name -> checkedCommand token (toText name) >> pure (toText name)
  Nothing -> do
    scalar <- normalizeScalar token
    constants <- gets nConstants
    case scalar of
      P.Variable name | Just value <- M.lookup name constants -> checkedCommand token value >> pure value
      _ -> reject token "dynamic-command" "The command head has no single proven binding"

literalName :: Token -> Token -> Normalize Text
literalName parent token = case getLiteralString token of
  Just value -> checkedName token (toText value) >> pure (toText value)
  Nothing -> reject parent "literal-name" "This operation requires a literal binding name"

checkedCommand :: Token -> Text -> Normalize ()
checkedCommand token name =
  unless
    (not (T.null name) && not (T.isPrefixOf "-" name) && T.all (\c -> isAlphaNum c || c `elem` ("_./:+-[" :: String)) name)
    (reject token "command-name" "Command identity needs a safely materializable literal name")

checkedName :: Token -> Text -> Normalize ()
checkedName token name = do
  stable <- gets (stableDirectoryEnabled . nConfig)
  unless (validName name) (reject token "name" "Binding names must be nonempty portable identifiers")
  when
    (not (stable && name == "PWD") && name `elem` ["fish_read_limit", "_", "status", "pipestatus", "argv", "fish_pid", "last_pid", "version", "SHLVL", "PWD", "PPID", "UID", "EUID", "RANDOM", "SRANDOM", "SECONDS", "LINENO", "SHELLOPTS", "BASHOPTS"])
    (reject token "reserved-binding" "This Bash name conflicts with target shell state")
  modify' (\s -> s {nReserved = S.insert name (nReserved s)})

asciiLetter :: Char -> Bool
asciiLetter character = isAsciiLower character || isAsciiUpper character

validName :: Text -> Bool
validName name = case T.uncons name of
  Just (leading, rest) -> (asciiLetter leading || leading == '_') && T.all (\c -> asciiLetter c || isDigit c || c == '_') rest
  Nothing -> False

-- Brace distribution precedes every expansion. Duplicated parser occurrences
-- are normalized afresh, so writes and command substitutions run per result.
normalizeWords :: [Token] -> Normalize [P.Word]
normalizeWords values = concat <$> traverse (expandBraces >=> traverse normalizeWord) values

expandBraces :: Token -> Normalize [Token]
expandBraces token = case token of
  T_BraceExpansion _ alternatives -> concat <$> traverse expandBraces alternatives
  T_NormalWord identity parts -> do
    alternatives <- traverse expandBraces parts
    pure [T_NormalWord identity combination | combination <- sequence alternatives]
  _ -> pure [token]

normalizeWord :: Token -> Normalize P.Word
normalizeWord token = case token of
  T_NormalWord _ parts | any isGlobPart parts -> P.PathnameFields <$> literalPathname token parts
  T_NormalWord _ parts -> normalizeParts token parts
  other -> normalizeParts token [other]
  where
    isGlobPart T_Glob {} = True
    isGlobPart _ = False

literalPathname :: Token -> [Token] -> Normalize P.Pattern
literalPathname token parts = P.MkPattern . concat <$> traverse fragment parts
  where
    fragment node@(T_Glob _ value) = do
      unless
        (toText value `elem` ["*", "?"])
        (reject node "pathname-pattern" "The initial pathname envelope supports only active star and question mark")
      pure [P.ActivePattern (P.Literal (toText value))]
    fragment (T_DoubleQuoted _ values) = concat <$> traverse quoted values
    fragment node = quoted node
    quoted (T_Literal _ value) = pure [P.LiteralPattern (P.Literal (toText value))]
    quoted (T_SingleQuoted _ value) = pure [P.LiteralPattern (P.Literal (toText value))]
    quoted _ = reject token "pathname-fragments" "Literal pathname patterns cannot contain dynamic expansion fragments"

normalizeParts :: Token -> [Token] -> Normalize P.Word
normalizeParts token parts = do
  pieces <- concat <$> traverse wordPieces parts
  case [() | ArgvPiece <- pieces] of
    [] -> do
      constants <- gets nConstants
      let knownIfs = M.lookup "IFS" constants
          inert (ScalarPiece True (P.Variable name)) = maybe False (\ifs -> maybe False (\value -> not (T.null value) && not (T.any (`T.elem` (ifs <> "*?[")) value)) (M.lookup name constants)) knownIfs
          inert (ScalarPiece True (P.ArithmeticValue {})) = maybe False (\ifs -> not (T.any (`T.elem` ifs) "0123456789-")) knownIfs
          inert (ScalarPiece False _) = True
          inert _ = False
          split = any isSplit pieces && not (all inert pieces)
      when (split && length pieces /= 1) (reject token "mixed-splitting" "Mixed unquoted field splitting requires a field concatenation plan")
      let value = compact [scalar | ScalarPiece _ scalar <- pieces]
      when split $ do
        unless
          (noPathnameExpansion constants value)
          (reject token "pathname-expansion" "Unquoted fields require proven absence of pathname patterns until the owned glob operation is available")
      pure ((if split then P.SplitFields else P.OneField) value)
    [_] -> do
      let (before, after0) = break isArgv pieces
          after = drop 1 after0
      when (any isSplit (before <> after)) (reject token "argv-splitting" "Quoted argv cannot share an unquoted splitting region")
      pure (P.QuotedArguments (compact [s | ScalarPiece _ s <- before]) (compact [s | ScalarPiece _ s <- after]) (not (null before && null after)))
    _ -> reject token "argv-products" "Multiple argv splices require an explicit product plan"
  where
    isSplit (ScalarPiece split _) = split
    isSplit ArgvPiece = False
    isArgv ArgvPiece = True
    isArgv _ = False

-- The field splitter is exact only when no later pathname expansion is possible.
-- This fact belongs to the normalized word, before materialization.
noPathnameExpansion :: M.Map Text Text -> P.Scalar -> Bool
noPathnameExpansion constants = \case
  P.Literal value -> safe value
  P.Variable name -> maybe False safe (M.lookup name constants)
  P.LastStatus -> True
  P.ArgumentCount -> True
  P.ArithmeticValue {} -> True
  P.Concat values -> all (noPathnameExpansion constants) values
  _ -> False
  where
    safe = not . T.any (`elem` ("*?[" :: String))

data Piece = ScalarPiece Bool P.Scalar | ArgvPiece

wordPieces :: Token -> Normalize [Piece]
wordPieces = \case
  T_DoubleQuoted _ parts -> concat <$> traverse quotedPiece parts
  token@(T_DollarBraced _ _ inner) -> do
    if parameterText inner == Just "@" then reject token "unquoted-argv" "Unquoted argv needs per-argument field splitting" else (: []) . ScalarPiece True <$> normalizeScalar token
  token@(T_DollarExpansion {}) -> (: []) . ScalarPiece True <$> normalizeScalar token
  token -> (: []) . ScalarPiece False <$> normalizeScalar token
  where
    quotedPiece (T_DollarBraced _ _ inner) | parameterText inner == Just "@" = pure [ArgvPiece]
    quotedPiece token = (: []) . ScalarPiece False <$> normalizeScalarIn True token

normalizeScalar :: Token -> Normalize P.Scalar
normalizeScalar = normalizeScalarIn False

normalizeScalarIn :: Bool -> Token -> Normalize P.Scalar
normalizeScalarIn quoted token = case token of
  T_Literal _ value -> pure (P.Literal (toText value))
  T_SingleQuoted _ value -> pure (P.Literal (if quoted then "'" <> toText value <> "'" else toText value))
  T_DollarSingleQuoted _ value -> do
    bytes <- either (reject token "ansi-quoted-escape") pure (ansiBytes value)
    pure (either (const (P.ByteLiteral bytes)) P.Literal (decodeUtf8' bytes))
  T_NormalWord _ parts -> compact <$> traverse (normalizeScalarIn quoted) parts
  T_DoubleQuoted _ parts -> compact <$> traverse (normalizeScalarIn True) parts
  T_DollarBraced _ _ inner -> normalizeParameter quoted token inner
  T_DollarExpansion _ body -> P.Substitute <$> normalizeChild token body
  T_DollarBracket {} -> do
    (site, expression, bindings) <- normalizeArithmeticAt token
    pure (P.ArithmeticValue site expression bindings)
  T_DollarArithmetic {} -> do
    (site, expression, bindings) <- normalizeArithmeticAt token
    pure (P.ArithmeticValue site expression bindings)
  _ -> reject token "word" ("No scalar semantics for " <> tokenKind token)

normalizeParameter :: Bool -> Token -> Token -> Normalize P.Scalar
normalizeParameter quoted token inner = case parameterText inner of
  Just "?" -> pure P.LastStatus
  Just "#" -> pure P.ArgumentCount
  Just name | Just index <- positional name -> pure (P.Positional index)
  Just name | validName name -> checkedName token name >> readBinding token name >> pure (P.Variable name)
  _ -> do
    (name, suffix, remaining) <- case inner of
      T_NormalWord _ (T_Literal _ leading : rest) -> let (name, suffix) = T.span (\c -> isAlphaNum c || c == '_') (toText leading) in pure (name, suffix, rest)
      T_Literal _ leading -> let (name, suffix) = T.span (\c -> isAlphaNum c || c == '_') (toText leading) in pure (name, suffix, [])
      _ -> reject token "parameter" "Parameter operation needs a literal scalar name and operator"
    let target = maybe (P.Variable name) P.Positional (positional name)
    unless (isJust (positional name)) (checkedName token name >> readBinding token name)
    case asum [(operator,) <$> T.stripPrefix operator suffix | operator <- [":=", ":-", ":+", "=", "-", "+"]] of
      Just (operator, literalPrefix) -> do
        before <- get
        alternative <- compact . (P.Literal literalPrefix :) <$> traverse (normalizeScalarIn quoted) remaining
        after <- get
        put (joinStates before after)
        let nullSensitive = T.isPrefixOf ":" operator
            assigning = T.isSuffixOf "=" operator
            alternate = T.isSuffixOf "+" operator
        case positional name of
          Just index -> do
            when assigning (reject token "positional-modifier" "Positional assignment cannot modify an ordinary scalar binding")
            pure ((if alternate then P.PositionalAlternate else P.PositionalDefault) index nullSensitive alternative)
          Nothing -> do
            when assigning (modify' (\s -> s {nConstants = M.delete name (nConstants s), nNumeric = S.delete name (nNumeric s), nVariables = S.insert name (nVariables s), nResolutionStable = nResolutionStable s && name `notElem` resolutionVariables}))
            storage <- if assigning then storageFor token False name else pure P.Visible
            pure (if alternate then P.AlternateValue name nullSensitive alternative else P.DefaultValue storage name nullSensitive assigning alternative)
      Nothing -> do
        literalTail <- maybe (reject token "parameter-pattern" "Parameter patterns and replacements must be literal") (pure . mconcat) (traverse parameterText remaining)
        let modifier = suffix <> literalTail
        case asum [(operation,) <$> T.stripPrefix spelling modifier | (spelling, operation) <- [("##", "trim-prefix-long"), ("#", "trim-prefix-short"), ("%%", "trim-suffix-long"), ("%", "trim-suffix-short")]] of
          Just (operation, patternValue) -> do
            when (T.any (`elem` ['[', '\\']) patternValue) (reject token "parameter-pattern" "Trim admits only literal bytes, star and question mark")
            pure (P.ParameterTransform operation target patternValue "")
          Nothing -> case T.stripPrefix "/" modifier of
            Just replacementSpec -> do
              let (operation, spec) = maybe ("replace-first", replacementSpec) ("replace-all",) (T.stripPrefix "/" replacementSpec)
                  (needle, tailValue) = T.breakOn "/" spec
                  replacement = fromMaybe "" (T.stripPrefix "/" tailValue)
              when (T.null needle || T.any (`elem` ['*', '?', '[', '\\', '#', '%']) needle || T.any (`elem` ['&', '\\']) replacement) (reject token "parameter-replacement" "Replacement requires a nonempty literal needle and literal replacement bytes")
              pure (P.ParameterTransform operation target needle replacement)
            Nothing -> reject token "parameter" "Parameter modifier has no admitted scalar operation"
  where
    positional :: Text -> Maybe Int
    positional name | T.all isDigit name, not (T.null name), Just index <- readMaybe (toString name), index > 0 = Just index
    positional _ = Nothing

-- Bash ANSI quotes are byte strings in the admitted C locale. NUL ends the
-- scalar; non-ASCII Unicode escapes remain their canonical textual spelling.
ansiBytes :: String -> Either Text ByteString
ansiBytes input = BS.takeWhile (/= 0) . BS.concat <$> go input
  where
    go :: String -> Either Text [ByteString]
    go [] = pure []
    go ('\\' : code : rest)
      | Just byte <- lookup code [('?', 63), ('a', 7), ('b', 8), ('e', 27), ('E', 27), ('f', 12), ('n', 10), ('r', 13), ('t', 9), ('v', 11), ('\\', 92), ('\'', 39), ('"', 34)] = (BS.singleton byte :) <$> go rest
      | isOctDigit code = number 8 3 (code : rest) ""
      | code == 'x' = number 16 2 rest "\\x"
      | code `elem` ['u', 'U'] = do
          let (digits, remaining) = takeDigits isHexDigit (if code == 'u' then 4 else 8) rest
              value = foldl' (\n d -> n * 16 + toInteger (digitToInt d)) 0 digits
              width = if value <= 65535 then 4 else 8
              hex = map toUpper (showHex value "")
              rendered
                | value > 2147483647 = BS.empty
                | value < 128 = BS.singleton (fromInteger value)
                | otherwise = encodeUtf8 (toText ((if width == 4 then "\\u" else "\\U") <> replicate (max 0 (width - length hex)) '0' <> hex))
          if null digits then (encodeUtf8 (toText ['\\', code]) :) <$> go rest else (rendered :) <$> go remaining
      | code == 'c' = case rest of
          '\\' : '\\' : remaining -> (BS.singleton 28 :) <$> go remaining
          character : remaining | ord character < 128 -> (BS.singleton (fromIntegral (if character == '?' then 127 else ord (toUpper character) `mod` 32)) :) <$> go remaining
          _ -> Left "ANSI control escape requires an ASCII operand"
      | otherwise = (encodeUtf8 (toText ['\\', code]) :) <$> go rest
    go (character : rest) = (encodeUtf8 (toText [character]) :) <$> go rest
    number :: Int -> Int -> String -> Text -> Either Text [ByteString]
    number base count sourceDigits fallback = do
      let (digits, remaining) = takeDigits (if base == 8 then isOctDigit else isHexDigit) count sourceDigits
          value = foldl' (\n d -> n * base + digitToInt d) 0 digits
          bytes = if null digits then encodeUtf8 fallback else BS.singleton (fromIntegral (value `mod` 256))
      (bytes :) <$> go remaining
    takeDigits predicate count value = let digits = take count (takeWhile predicate value) in (digits, drop (length digits) value)

normalizePattern :: Token -> Normalize P.Pattern
normalizePattern token = P.MkPattern <$> patternParts token
  where
    patternParts node = case node of
      T_NormalWord _ parts -> concat <$> traverse patternParts parts
      T_DoubleQuoted _ parts -> traverse (fmap P.LiteralPattern . normalizeScalarIn True) parts
      T_Glob _ value -> get >>= \before -> active before node (P.Literal (toText value))
      T_DollarBraced {} -> expanded node
      T_DollarExpansion {} -> expanded node
      _ -> (: []) . P.LiteralPattern <$> normalizeScalar node
    expanded node = do
      before <- get
      value <- normalizeScalar node
      active before node value
    active before node value = do
      unless
        (simpleValue before value)
        (reject node "pattern-envelope" "Active patterns require proven star/question-mark semantics; bracket, escape, and unknown patterns are excluded")
      pure [P.ActivePattern value]
    simpleValue before = \case
      P.Literal literal -> simplePattern literal
      P.Variable name -> simpleBinding before name
      P.DefaultValue _ name _ _ fallback -> simpleBinding before name && simpleValue before fallback
      P.ArithmeticValue {} -> True
      P.ArgumentCount -> True
      P.LastStatus -> True
      P.Concat parts -> all (simpleValue before) parts
      _ -> False
    simpleBinding before name = maybe False simplePattern (M.lookup name (nConstants before)) || S.member name (nNumeric before)
    simplePattern = not . T.any (`elem` ("[\\" :: String))

normalizeCondition :: Token -> Normalize P.StatementNode
normalizeCondition token = case token of
  TC_Nullary _ SingleBracket value -> normalizeFixedTest token "test" [value]
  TC_Unary _ SingleBracket "!" value -> P.Negate . P.Statement Nothing <$> normalizeCondition value
  TC_Unary _ SingleBracket operator value -> normalizeFixedTest token "test" [T_NormalWord (getId token) [T_Literal (getId token) operator], value]
  TC_Binary _ SingleBracket operator left right -> normalizeFixedTest token "test" [left, T_NormalWord (getId token) [T_Literal (getId token) operator], right]
  TC_Group _ DoubleBracket inner -> normalizeCondition inner
  TC_And _ DoubleBracket "&&" left right -> lazyCondition P.And left right
  TC_Or _ DoubleBracket "||" left right -> lazyCondition P.Or left right
  TC_Unary _ DoubleBracket "!" value -> P.Negate . P.Statement Nothing <$> normalizeCondition value
  TC_Nullary _ _ value -> P.Invoke (P.Builtin "test") . (P.OneField (P.Literal "-n") :) . (: []) . P.OneField <$> normalizeScalar value
  TC_Unary _ _ operator value
    | toText operator `elem` ["-n", "-z", "-e", "-f", "-d", "-r", "-w", "-x"] ->
        P.Invoke (P.Builtin "test") . (P.OneField (P.Literal (toText operator)) :) . (: []) . P.OneField <$> normalizeScalar value
  TC_Binary _ _ operator left right
    | toText operator `elem` ["=", "==", "!="] ->
        P.PatternCondition (operator == "!=") <$> normalizeScalar left <*> normalizePattern right
  TC_Binary _ _ operator left right | toText operator `elem` ["-eq", "-ne", "-lt", "-le", "-gt", "-ge"] -> do
    a <- normalizeScalar left
    b <- normalizeScalar right
    known <- gets nNumeric
    let valid = \case
          P.Literal value -> numericLiteral value
          P.Variable name -> S.member name known
          P.LastStatus -> True
          P.ArgumentCount -> True
          P.ArithmeticValue {} -> True
          _ -> False
    unless
      (valid a && valid b)
      (reject token "arithmetic-comparison" "Numeric comparison operands must be proven integer data, not runtime expression strings")
    constants <- gets nConstants
    pure (freezeComparison constants (toText operator) a b)
  _ -> reject token "condition" "Condition operators require exact operand and pattern semantics"
  where
    lazyCondition constructor left right = do
      leftValue <- normalizeCondition left
      before <- get
      rightValue <- normalizeCondition right
      after <- get
      put (joinStates before after)
      pure (constructor (P.Statement Nothing leftValue) (P.Statement Nothing rightValue))

freezeComparison :: M.Map Text Text -> Text -> P.Scalar -> P.Scalar -> P.StatementNode
freezeComparison constants operator left right =
  let writes = Effects.effectWrites (Effects.scalarEffects left <> Effects.scalarEffects right)
      freeze original@(P.Variable name) | S.notMember name writes = maybe original P.Literal (M.lookup name constants)
      freeze value = value
   in P.NumericCondition operator (freeze left) (freeze right)

compact :: [P.Scalar] -> P.Scalar
compact [] = P.Literal ""
compact [value] = value
compact values = maybe (P.Concat values) (P.Literal . mconcat) (traverse scalarLiteral values)

scalarLiteral :: P.Scalar -> Maybe Text
scalarLiteral (P.Literal value) = Just value
scalarLiteral _ = Nothing

parameterText :: Token -> Maybe Text
parameterText = \case
  T_Literal _ value -> Just (toText value)
  T_ParamSubSpecialChar _ value -> Just (toText value)
  T_NormalWord _ values -> mconcat <$> traverse parameterText values
  _ -> Nothing

functionNames :: Token -> S.Set Text
functionNames token = case token of
  T_Function _ _ _ name _ -> S.singleton (toText name) <> foldMap functionNames (children token)
  _ -> foldMap functionNames (children token)

sourceNames :: Token -> S.Set Text
sourceNames token = maybe mempty S.singleton (parameterText token) <> foldMap sourceNames (children token)

children :: Token -> [Token]
children (OuterToken _ inner) = toList inner

resolutionVariables :: [Text]
resolutionVariables = ["PATH", "CDPATH", "HOME", "OLDPWD"]

normalizeStatements :: [Token] -> Normalize [P.Statement]
normalizeStatements [] = pure []
normalizeStatements (token : remaining) = do
  statement <- normalizeStatement token
  rest <- if continues statement then normalizeStatements remaining else pure []
  pure (statement : rest)
  where
    continues (P.Statement _ node) = case node of
      P.Return {} -> False
      P.Exit {} -> False
      P.Break -> False
      P.Continue -> False
      P.Sequence statements -> all continues statements
      P.Conditional _ yes no -> any continues yes || null no || any continues no
      _ -> True

-- Return edges carry only finite semantic facts; no executable syntax or
-- continuation is retained. Source completion joins these with fallthrough.
entryContext :: Normalization -> P.SourceEntryContext
entryContext flow = P.SourceEntryContext (nDefinitions flow) (nVariables flow) (nConstants flow) (nLocals flow) (nNumeric flow) (nDirectoryFacts flow)

joinSourceExit :: Normalization -> P.SourceEntryContext -> Normalization
joinSourceExit flow facts =
  let shared :: (Eq a) => M.Map Text a -> M.Map Text a -> M.Map Text a
      shared = M.mergeWithKey (\_ a b -> if a == b then Just a else Nothing) (const mempty) (const mempty)
      definitions = shared (nDefinitions flow) (P.sourceEntryDefinitions facts)
      names = M.keysSet definitions
   in flow
        { nDefinitions = definitions,
          nFunctions = nFunctions flow `S.intersection` names,
          nLocalFunctions = nLocalFunctions flow `S.intersection` names,
          nFunctionBodies = M.restrictKeys (nFunctionBodies flow) names,
          nConstants = shared (nConstants flow) (P.sourceEntryConstants facts),
          nVariables = nVariables flow `S.intersection` P.sourceEntryVariables facts,
          nLocals = nLocals flow `S.intersection` P.sourceEntryLocals facts,
          nNumeric = nNumeric flow `S.intersection` P.sourceEntryNumericVariables facts,
          nDirectoryFacts = Directory.joinDirectoryFacts (nDirectoryFacts flow) (P.sourceEntryDirectoryFacts facts),
          nDirectoryOutcomes = Nothing
        }

normalizeSourceCall :: Token -> [Token] -> Normalize P.StatementNode
normalizeSourceCall token arguments = case arguments of
  pathToken : argv -> do
    literal <- maybe (reject token "computed-source" "Source requires a literal immutable dependency") (pure . toText) (getLiteralString pathToken)
    when (T.null literal) (reject token "source-target" "Source requires a nonempty target")
    before <- get
    unless (nResolutionStable before || T.isPrefixOf "/" literal) (reject token "source-resolution-state" "Source resolution follows an unsupported environment mutation")
    unless (null (nActive before)) (reject token "source-function-context" "Source in a function needs an invocation-time dependency context")
    when (nChild before) (reject token "source-child-context" "Source in child execution needs an isolated dependency materialization")
    when
      (Directory.directoryLocation (nDirectoryFacts before) == Directory.UnknownDirectory && not (T.isPrefixOf "/" literal))
      (reject token "source-directory-state" "Relative source resolution requires a known execution cwd on this control edge")
    argvValue <- normalizeWords argv
    range <- tokenRange token
    let Id ordinal = getId token
        entry = entryContext before
        request = P.SourceRequest ordinal range literal argvValue P.SharedSource (nSourceStack before) entry (case Directory.directoryLocation (nDirectoryFacts before) of Directory.KnownDirectory path -> Just path; Directory.RelativeDirectory path -> Just path; _ -> Nothing)
    SourceDocument text parsed <- lift (NormalizationNeedsSource request NormalizationComplete)
    let name = documentName parsed
    when (name `elem` nSourceStack before) (reject token "source-cycle" "Recursive source cycles are outside the initial envelope")
    root <- maybe (reject token "source-parse" "Dependency has no parse root") pure (prRoot parsed)
    modify'
      ( \s ->
          s
            { nDocument = Just text,
              nPositions = prTokenPositions parsed,
              nSourceStack = nSourceStack before <> [name],
              nInSource = True,
              nSourceArgvOwned = any P.guaranteesField argvValue,
              nLoop = 0,
              nSourceReturns = mempty,
              nAllFunctions = nAllFunctions s <> functionNames root,
              nReserved = nReserved s <> sourceNames root
            }
      )
    body <- normalizeStatement root
    after <- get
    put (foldl' joinSourceExit after (nSourceReturns after))
    modify'
      ( \s ->
          s
            { nDocument = nDocument before,
              nPositions = nPositions before,
              nSourceStack = nSourceStack before,
              nInSource = nInSource before,
              nSourceArgvOwned = nSourceArgvOwned before,
              nLoop = nLoop before,
              nSourceReturns = nSourceReturns before
            }
      )
    pure (P.SourceBody request [body])
  [] -> reject token "source-target" "Source requires a target operand"

numericScalar :: P.Scalar -> Bool
numericScalar (P.ArithmeticValue {}) = True
numericScalar value = maybe False numericLiteral (scalarLiteral value)

numericLiteral :: Text -> Bool
numericLiteral value = T.null value || isRight (A.normalizeArithmetic (T_Literal (Id 0) (toString value)))

normalizeArithmeticAt :: Token -> Normalize (ArithmeticSource.ArithmeticSite, A.ArithmeticExpr, M.Map Text P.Storage)
normalizeArithmeticAt token = do
  (value, _, _) <- normalizeArithmeticRegion token
  pure value

normalizeArithmeticRegion :: Token -> Normalize ((ArithmeticSource.ArithmeticSite, A.ArithmeticExpr, M.Map Text P.Storage), (S.Set Text, S.Set Text), Bool)
normalizeArithmeticRegion token = do
  expression <- either (reject token "arithmetic-shape") pure (A.normalizeArithmetic token)
  source <- gets nDocument >>= maybe (reject token "arithmetic-source" "Exact arithmetic errors require the immutable original source document") pure
  positions <- gets nPositions
  commandLine <- gets nCommandLine
  site <- either (reject token "arithmetic-source") pure (ArithmeticSource.arithmeticSite commandLine source positions token expression)
  before <- get
  let constants = nConstants before
  validate expression
  success <- get
  let potentialFailure = mayFail (freezeConstants (M.withoutKeys constants (writes expression)) expression)
  when potentialFailure $ do
    after <- get
    -- A failed arithmetic command may continue its enclosing statement list.
    -- Writes after the failure are not definite; partial writes before it also
    -- prevent restoring an entry constant even when the final value agrees.
    put (joinStates before after) {nConstants = M.withoutKeys (nConstants (joinStates before after)) (writes expression)}
  bindings <- M.fromList <$> traverse (\name -> (name,) <$> storageFor token False name) (S.toList (writes expression))
  pure ((site, freezeConstants (M.withoutKeys constants (writes expression)) expression, bindings), (nNumeric success, nVariables success), potentialFailure)
  where
    mayFail expression | isJust (A.constantValue expression) = False
    mayFail expression = case expression of
      A.ArithmeticLocated _ inner -> mayFail inner
      A.ArithmeticUnary _ inner -> mayFail inner
      A.ArithmeticBinary operator left right -> failingOperator operator right || mayFail left || mayFail right
      A.ArithmeticAssign _ operator right -> maybe False (`failingOperator` right) operator || mayFail right
      A.ArithmeticConditional predicate yes no -> any mayFail [predicate, yes, no]
      A.ArithmeticSequence expressions -> any mayFail expressions
      _ -> False
    failingOperator operator right = case operator of
      A.Divide -> maybe True (== 0) (A.constantValue right)
      A.Remainder -> maybe True (== 0) (A.constantValue right)
      A.Power -> maybe True (< 0) (A.constantValue right)
      _ -> False
    -- Freeze only reads whose binding is not written anywhere in this region.
    -- Source validation and error origins use the original operation tree.
    freezeConstants constants = \case
      A.ArithmeticLocated location inner -> A.ArithmeticLocated location (freezeConstants constants inner)
      original@(A.ArithmeticVariable name) -> maybe original A.ArithmeticLiteral (M.lookup name constants >>= rightToMaybe . Integer.parseNumber . encodeUtf8)
      A.ArithmeticUnary operator inner -> A.ArithmeticUnary operator (freezeConstants constants inner)
      A.ArithmeticBinary operator left right -> A.ArithmeticBinary operator (freezeConstants constants left) (freezeConstants constants right)
      A.ArithmeticAssign name operator right -> A.ArithmeticAssign name operator (freezeConstants constants right)
      A.ArithmeticConditional predicate yes no -> A.ArithmeticConditional (freezeConstants constants predicate) (freezeConstants constants yes) (freezeConstants constants no)
      A.ArithmeticSequence expressions -> A.ArithmeticSequence (fmap (freezeConstants constants) expressions)
      other -> other
    readNumeric name = do
      readBinding token name
      known <- gets nNumeric
      unless (name == "#" || S.member name known) (reject token "arithmetic-binding" "Arithmetic variable values must be proven numeric, not runtime expression strings")
    written name = do
      checkedName token name
      modify' (\s -> s {nNumeric = S.insert name (nNumeric s), nVariables = S.insert name (nVariables s), nConstants = M.delete name (nConstants s), nResolutionStable = nResolutionStable s && name `notElem` resolutionVariables})
    validate = \case
      A.ArithmeticLocated _ expression -> validate expression
      A.ArithmeticLiteral _ -> pure ()
      A.ArithmeticVariable name -> readNumeric name
      A.ArithmeticUnary _ expression -> validate expression
      A.ArithmeticBinary operator left right -> do
        validate left
        before <- get
        validate right
        when (operator `elem` [A.LogicalAnd, A.LogicalOr]) (get >>= put . joinStates before)
      A.ArithmeticAssign name operator expression -> do
        when (isJust operator) (readNumeric name)
        validate expression
        written name
      A.ArithmeticUpdate _ _ name -> readNumeric name >> written name
      A.ArithmeticConditional predicate yes no -> do
        validate predicate
        before <- get
        validate yes
        firstBranch <- get
        put before
        validate no
        secondBranch <- get
        put (joinStates firstBranch secondBranch)
      A.ArithmeticSequence expressions -> traverse_ validate expressions
    writes = \case
      A.ArithmeticLocated _ expression -> writes expression
      A.ArithmeticLiteral _ -> mempty
      A.ArithmeticVariable _ -> mempty
      A.ArithmeticUnary _ expression -> writes expression
      A.ArithmeticBinary _ left right -> writes left <> writes right
      A.ArithmeticAssign name _ expression -> S.insert name (writes expression)
      A.ArithmeticUpdate _ _ name -> S.singleton name
      A.ArithmeticConditional predicate yes no -> writes predicate <> writes yes <> writes no
      A.ArithmeticSequence expressions -> foldMap writes expressions

-- Every unknown constructor is rejected. This exhaustive name mapping also
-- makes parser dependency additions visible at compile time.
tokenKind :: Token -> Text
tokenKind = \case
  T_AND_IF {} -> "T_AND_IF"
  T_AndIf {} -> "T_AndIf"
  T_Annotation {} -> "T_Annotation"
  T_Arithmetic {} -> "T_Arithmetic"
  T_Array {} -> "T_Array"
  T_Assignment {} -> "T_Assignment"
  T_Backgrounded {} -> "T_Backgrounded"
  T_Backticked {} -> "T_Backticked"
  T_Bang {} -> "T_Bang"
  T_Banged {} -> "T_Banged"
  T_BatsTest {} -> "T_BatsTest"
  T_BraceExpansion {} -> "T_BraceExpansion"
  T_BraceGroup {} -> "T_BraceGroup"
  T_CLOBBER {} -> "T_CLOBBER"
  T_Case {} -> "T_Case"
  T_CaseExpression {} -> "T_CaseExpression"
  T_CoProc {} -> "T_CoProc"
  T_CoProcBody {} -> "T_CoProcBody"
  T_Condition {} -> "T_Condition"
  T_DGREAT {} -> "T_DGREAT"
  T_DLESS {} -> "T_DLESS"
  T_DLESSDASH {} -> "T_DLESSDASH"
  T_DSEMI {} -> "T_DSEMI"
  T_Do {} -> "T_Do"
  T_DollarArithmetic {} -> "T_DollarArithmetic"
  T_DollarBraceCommandExpansion {} -> "T_DollarBraceCommandExpansion"
  T_DollarBraced {} -> "T_DollarBraced"
  T_DollarBracket {} -> "T_DollarBracket"
  T_DollarDoubleQuoted {} -> "T_DollarDoubleQuoted"
  T_DollarExpansion {} -> "T_DollarExpansion"
  T_DollarSingleQuoted {} -> "T_DollarSingleQuoted"
  T_Done {} -> "T_Done"
  T_DoubleQuoted {} -> "T_DoubleQuoted"
  T_EOF {} -> "T_EOF"
  T_Elif {} -> "T_Elif"
  T_Else {} -> "T_Else"
  T_Esac {} -> "T_Esac"
  T_Extglob {} -> "T_Extglob"
  T_FdRedirect {} -> "T_FdRedirect"
  T_Fi {} -> "T_Fi"
  T_For {} -> "T_For"
  T_ForArithmetic {} -> "T_ForArithmetic"
  T_ForIn {} -> "T_ForIn"
  T_Function {} -> "T_Function"
  T_GREATAND {} -> "T_GREATAND"
  T_Glob {} -> "T_Glob"
  T_Greater {} -> "T_Greater"
  T_HereDoc {} -> "T_HereDoc"
  T_HereString {} -> "T_HereString"
  T_If {} -> "T_If"
  T_IfExpression {} -> "T_IfExpression"
  T_In {} -> "T_In"
  T_Include {} -> "T_Include"
  T_IndexedElement {} -> "T_IndexedElement"
  T_IoDuplicate {} -> "T_IoDuplicate"
  T_IoFile {} -> "T_IoFile"
  T_LESSAND {} -> "T_LESSAND"
  T_LESSGREAT {} -> "T_LESSGREAT"
  T_Lbrace {} -> "T_Lbrace"
  T_Less {} -> "T_Less"
  T_Literal {} -> "T_Literal"
  T_Lparen {} -> "T_Lparen"
  T_NEWLINE {} -> "T_NEWLINE"
  T_NormalWord {} -> "T_NormalWord"
  T_OR_IF {} -> "T_OR_IF"
  T_OrIf {} -> "T_OrIf"
  T_ParamSubSpecialChar {} -> "T_ParamSubSpecialChar"
  T_Pipe {} -> "T_Pipe"
  T_Pipeline {} -> "T_Pipeline"
  T_ProcSub {} -> "T_ProcSub"
  T_Rbrace {} -> "T_Rbrace"
  T_Redirecting {} -> "T_Redirecting"
  T_Rparen {} -> "T_Rparen"
  T_Script {} -> "T_Script"
  T_Select {} -> "T_Select"
  T_SelectIn {} -> "T_SelectIn"
  T_Semi {} -> "T_Semi"
  T_SimpleCommand {} -> "T_SimpleCommand"
  T_SingleQuoted {} -> "T_SingleQuoted"
  T_SourceCommand {} -> "T_SourceCommand"
  T_Subshell {} -> "T_Subshell"
  T_Then {} -> "T_Then"
  T_UnparsedIndex {} -> "T_UnparsedIndex"
  T_Until {} -> "T_Until"
  T_UntilExpression {} -> "T_UntilExpression"
  T_While {} -> "T_While"
  T_WhileExpression {} -> "T_WhileExpression"
  TA_Assignment {} -> "TA_Assignment"
  TA_Binary {} -> "TA_Binary"
  TA_Expansion {} -> "TA_Expansion"
  TA_Parenthesis {} -> "TA_Parenthesis"
  TA_Sequence {} -> "TA_Sequence"
  TA_Trinary {} -> "TA_Trinary"
  TA_Unary {} -> "TA_Unary"
  TA_Variable {} -> "TA_Variable"
  TC_And {} -> "TC_And"
  TC_Binary {} -> "TC_Binary"
  TC_Empty {} -> "TC_Empty"
  TC_Group {} -> "TC_Group"
  TC_Nullary {} -> "TC_Nullary"
  TC_Or {} -> "TC_Or"
  TC_Unary {} -> "TC_Unary"
