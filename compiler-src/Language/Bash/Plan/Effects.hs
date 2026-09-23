{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- | Finite effects over owned semantic nodes, used to close child snapshots.
module Language.Bash.Plan.Effects
  ( Effects,
    Effect (..),
    effectReads,
    effectWrites,
    effectFunctions,
    effectArrays,
    effectKinds,
    effectExternalEnvironment,
    effectMayEnableErrexit,
    effectSubstitution,
    effectPipefail,
    effectSession,
    effectTraps,
    effectDirectory,
    NativeEligible,
    NativeRegionProof,
    proveNativeRegion,
    nativeRegionStatement,
    statementEffects,
    scalarEffects,
    wordEffects,
    programVariables,
    closeChildRegion,
    admitPipeline,
    mayWriteBuiltin,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Directory qualified as Directory
import Monk.Source.Location (SourceRange)

-- | Capabilities are finite flags; binding and function identities remain
-- finite value-level sets rather than type-level maps.
data Effect = ExternalEnvironment | MayEnableErrexit | Substitution | Pipefail | Session | Traps | Directory | Output
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Effects = MkEffects
  { effectReads :: Set Text,
    effectWrites :: Set Text,
    effectFunctions :: Set Text,
    effectArrays :: Set Text,
    effectKinds :: Set Effect
  }
  deriving stock (Eq, Show)

instance Semigroup Effects where
  MkEffects ar aw af aa ak <> MkEffects br bw bf ba bk = MkEffects (ar <> br) (aw <> bw) (af <> bf) (aa <> ba) (ak <> bk)

instance Monoid Effects where
  mempty = MkEffects mempty mempty mempty mempty mempty

effect :: Effect -> Effects
effect kind = mempty {effectKinds = S.singleton kind}

effectWhen :: Bool -> Effect -> Effects
effectWhen enabled kind = if enabled then effect kind else mempty

effectExternalEnvironment, effectMayEnableErrexit, effectSubstitution, effectPipefail, effectSession, effectTraps, effectDirectory :: Effects -> Bool
effectExternalEnvironment = S.member ExternalEnvironment . effectKinds
effectMayEnableErrexit = S.member MayEnableErrexit . effectKinds
effectSubstitution = S.member Substitution . effectKinds
effectPipefail = S.member Pipefail . effectKinds
effectSession = S.member Session . effectKinds
effectTraps = S.member Traps . effectKinds
effectDirectory = S.member Directory . effectKinds

-- | Native regions can read scalar bindings and produce bytes. Writes,
-- arrays, dynamic calls and other capabilities stay with the materializer.
type family NativeEligible (kind :: Effect) :: Bool where
  NativeEligible Output = True
  NativeEligible ExternalEnvironment = False
  NativeEligible MayEnableErrexit = False
  NativeEligible Substitution = False
  NativeEligible Pipefail = False
  NativeEligible Session = False
  NativeEligible Traps = False
  NativeEligible Directory = False

data NativeEffect (kind :: Effect) where
  NativeOutput :: NativeEffect Output

data EligibleEffect where
  EligibleEffect :: (NativeEligible kind ~ True) => NativeEffect kind -> EligibleEffect

-- The certified statement travels with its evidence. A proof for an earlier
-- statement cannot be supplied alongside unrelated executable syntax.
type role NativeRegionProof nominal

data NativeRegionProof scope = NativeRegionProof (P.Statement scope) [EligibleEffect]

proveNativeRegion :: P.Statement scope -> Maybe (NativeRegionProof scope)
proveNativeRegion statement = do
  let effects = statementEffects statement
  guard (S.null (effectWrites effects) && S.null (effectArrays effects) && S.null (effectFunctions effects))
  evidence <- traverse eligible (S.toList (effectKinds effects))
  pure (NativeRegionProof statement evidence)
  where
    eligible Output = Just (EligibleEffect NativeOutput)
    eligible ExternalEnvironment = Nothing
    eligible MayEnableErrexit = Nothing
    eligible Substitution = Nothing
    eligible Pipefail = Nothing
    eligible Session = Nothing
    eligible Traps = Nothing
    eligible Directory = Nothing

nativeRegionStatement :: NativeRegionProof scope -> P.Statement scope
nativeRegionStatement (NativeRegionProof statement _) = statement

-- | Every scalar touched by the owned program, including function bodies.
programVariables :: [P.Statement scope] -> Set Text
programVariables statements =
  let effects = foldMap statementEffects statements
   in effectReads effects <> effectWrites effects

readName :: Text -> Effects
readName name = mempty {effectReads = S.singleton name}

writeName :: Text -> Effects
writeName name = mempty {effectWrites = S.singleton name}

arrayName :: Text -> Effects
arrayName name = mempty {effectArrays = S.singleton name}

statementEffects :: P.Statement scope -> Effects
statementEffects (P.Statement _ node) = case node of
  P.Sequence body -> foldMap statementEffects body
  P.Redirected redirects statement -> foldMap redirectEffects redirects <> statementEffects statement
  P.AssignmentCommand _ body -> foldMap statementEffects body
  P.DeclarationCommand declarations -> foldMap declarationEffects declarations
  P.Invoke target wordsValue ->
    foldMap wordEffects wordsValue <> case target of
      P.Function name -> mempty {effectFunctions = S.singleton name} <> effect MayEnableErrexit <> effect Output
      P.Builtin name -> effectWhen (name `elem` ["printf", "echo"]) Output
      P.External _ -> effect ExternalEnvironment <> effect Output
  P.PrefixedInvoke assignments target values -> foldMap (\(_, name, scalar) -> writeName name <> scalarEffects scalar) assignments <> statementEffects (P.Statement Nothing (P.Invoke target values))
  P.Assign _ name scalar -> writeName name <> scalarEffects scalar
  P.AssignArray _ name values -> arrayName name <> writeName name <> foldMap wordEffects values
  P.AppendArray _ name values -> arrayName name <> readName name <> writeName name <> foldMap wordEffects values
  P.AssignArrayElement _ name _ scalar -> arrayName name <> writeName name <> scalarEffects scalar
  P.Erase name -> writeName name
  P.SetArguments _ values -> foldMap wordEffects values
  P.ShiftArguments _ _ -> mempty
  P.ArithmeticFor initial body -> statementEffects initial <> P.withArithmeticBody body (\_ _ predicate increment values -> foldMap statementEffects (predicate : increment : values))
  P.DirectoryOperation operation ->
    effect Directory <> effect Output <> case operation of
      Directory.PrintDirectory _ -> mempty
      Directory.ChangeDirectory _ -> writeName "OLDPWD"
      Directory.ChangePreviousDirectory -> readName "OLDPWD" <> writeName "OLDPWD"
      Directory.PushDirectory _ -> readName "dirstack" <> writeName "dirstack" <> writeName "OLDPWD"
      Directory.PopDirectory -> readName "dirstack" <> writeName "dirstack" <> writeName "OLDPWD"
  P.SetOption option _ -> effect (case option of P.Errexit -> MayEnableErrexit; P.Pipefail -> Pipefail)
  P.And left right -> statementEffects left <> statementEffects right
  P.Or left right -> statementEffects left <> statementEffects right
  P.Negate value -> statementEffects value
  P.Conditional condition yes no -> foldMap statementEffects (condition <> yes <> no)
  P.WhileLoop _ body -> P.withWhileBody body (\_ _ condition values -> foldMap statementEffects (condition <> values))
  P.ForLoop _ name values body -> (if name == "_" then mempty else writeName name) <> foldMap wordEffects values <> P.withForBody body (\_ _ -> foldMap statementEffects)
  P.Case scalar arms -> scalarEffects scalar <> foldMap (\(P.CaseArm patterns body _) -> foldMap patternEffects patterns <> foldMap statementEffects body) arms
  P.PatternCondition _ scalar patternValue -> scalarEffects scalar <> patternEffects patternValue
  P.NumericCondition _ left right -> scalarEffects left <> scalarEffects right
  P.Approximate _ body -> foldMap statementEffects body
  P.DefineFunction _ body -> P.bodyEffects statementEffects body
  P.SourceBody request body -> foldMap wordEffects (P.sourceRequestArguments request) <> P.bodyEffects statementEffects body
  P.Subshell child -> childEffects child
  P.Pipeline children -> foldMap childEffects children <> effect Pipefail
  P.SupervisedPipeline children -> foldMap childEffects children <> effect Pipefail <> effect Session
  P.Background child -> childEffects child <> effect Session
  P.Wait wordsValue -> foldMap wordEffects wordsValue <> effect Session
  P.Read _ target -> (readName "IFS" <> case target of P.ReadReply _ -> writeName "REPLY"; P.ReadScalars names -> foldMap (writeName . snd) names; P.ReadArray _ name -> writeName name <> arrayName name) <> effect Session
  P.PrefixedRead assignments options target -> foldMap (\(_, name, scalar) -> writeName name <> scalarEffects scalar) assignments <> statementEffects (P.Statement Nothing (P.Read options target))
  P.SetTrap _ body -> foldMap (P.bodyEffects statementEffects) body <> effect Session <> effect Traps <> effect MayEnableErrexit
  P.ArithmeticCommand _ expression _ -> arithmeticEffects expression
  P.Return _ value -> foldMap scalarEffects value
  P.Exit value -> foldMap scalarEffects value
  P.Break _ -> mempty
  P.Continue _ -> mempty

redirectEffects :: P.Redirection -> Effects
redirectEffects = \case
  P.OpenDescriptor _ _ path -> scalarEffects path <> effect Session
  P.InputDescriptor _ input _ -> scalarEffects input <> effect Session
  P.DuplicateDescriptor target source _ -> effectWhen (target > 2 || source > 2) Session
  P.CloseDescriptor target _ -> effectWhen (target > 2) Session
  P.NullDescriptor target _ -> effectWhen (target > 2) Session

declarationEffects :: P.Declaration -> Effects
declarationEffects = \case
  P.DeclareLocal _ name scalar -> writeName name <> foldMap scalarEffects scalar
  P.DeclareExport _ name scalar -> writeName name <> foldMap scalarEffects scalar

wordEffects :: P.Word -> Effects
wordEffects = \case
  P.OneField value -> scalarEffects value
  P.SplitFields value -> readName "IFS" <> scalarEffects value
  P.PathnameFields patternValue -> patternEffects patternValue
  P.QuotedArguments before after _ -> scalarEffects before <> scalarEffects after
  P.QuotedArray name before after _ -> arrayName name <> readName name <> scalarEffects before <> scalarEffects after
  P.ExpandedWord parts -> readName "IFS" <> foldMap (\case P.QuotedExpansion value -> scalarEffects value; P.LiteralExpansion value -> scalarEffects value; P.SplitExpansion value -> scalarEffects value) parts

scalarEffects :: P.Scalar -> Effects
scalarEffects = \case
  P.Literal _ -> mempty
  P.ByteLiteral _ -> mempty
  P.PlatformBytes _ _ -> mempty
  P.AlternateValue name _ value -> readName name <> scalarEffects value
  P.ParameterTransform _ value _ _ -> scalarEffects value
  P.ParameterPatternTransform _ value patternValue -> scalarEffects value <> patternEffects patternValue
  P.AppendValue name value -> readName name <> scalarEffects value
  P.Variable name -> readName name
  P.ArrayElement name _ -> arrayName name <> readName name
  P.ArrayLength name -> arrayName name <> readName name
  P.Positional _ -> mempty
  P.PositionalDefault _ _ value -> scalarEffects value
  P.PositionalAlternate _ _ value -> scalarEffects value
  P.ArgumentCount -> mempty
  P.LastStatus -> mempty
  P.LastBackgroundPid -> effect Session
  P.Concat values -> foldMap scalarEffects values
  P.Substitute child -> childEffects child <> effect Substitution
  P.ProcessSubstitution _ child -> childEffects child <> effect Session
  P.DefaultValue _ name _ assigning value -> readName name <> (if assigning then writeName name else mempty) <> scalarEffects value
  P.ArithmeticValue _ expression _ -> arithmeticEffects expression

patternEffects :: P.Pattern -> Effects
patternEffects (P.MkPattern parts) = foldMap (\case P.LiteralPattern scalar -> scalarEffects scalar; P.ActivePattern scalar -> scalarEffects scalar) parts

childEffects :: P.ChildRegion -> Effects
childEffects child =
  let nested = P.bodyEffects statementEffects (P.childBody child) <> foldMap (P.bodyEffects statementEffects) (P.childFunctions child)
   in mempty {effectReads = P.childVariables child, effectArrays = P.childArrays child, effectKinds = S.delete ExternalEnvironment (effectKinds nested)} <> effectWhen (P.childNeedsEnvironment child) ExternalEnvironment

arithmeticEffects :: A.ArithmeticExpr -> Effects
arithmeticEffects = \case
  A.ArithmeticLocated _ value -> arithmeticEffects value
  A.ArithmeticLiteral _ -> mempty
  A.ArithmeticVariable "#" -> mempty
  A.ArithmeticVariable name -> readName name
  A.ArithmeticUnary _ value -> arithmeticEffects value
  A.ArithmeticBinary _ left right -> arithmeticEffects left <> arithmeticEffects right
  A.ArithmeticAssign name operator value -> writeName name <> (if isJust operator then readName name else mempty) <> arithmeticEffects value
  A.ArithmeticUpdate _ _ name -> readName name <> writeName name
  A.ArithmeticConditional condition yes no -> foldMap arithmeticEffects [condition, yes, no]
  A.ArithmeticSequence values -> foldMap arithmeticEffects values

closeChildRegion :: Maybe SourceRange -> Map Text (P.ScopedBody Control.FunctionRootKind) -> P.ScopedBody Control.ChildRootKind -> Either Text P.ChildRegion
closeChildRegion range available body = do
  let initial = P.bodyEffects statementEffects body
  (functions, effects) <- close mempty initial (effectFunctions initial)
  let variables = effectReads effects <> effectWrites effects
      unsupported = S.filter specialBinding variables
  unless (S.null unsupported) (Left ("Child snapshot cannot preserve target-special or Bash introspection bindings: " <> T.intercalate ", " (S.toAscList unsupported)))
  pure (P.MkChildRegion range body functions variables (effectArrays effects) (effectExternalEnvironment effects))
  where
    close complete effects pending = case S.minView pending of
      Nothing -> pure (complete, effects)
      Just (name, remaining)
        | M.member name complete -> close complete effects remaining
        | otherwise -> do
            functionBody <- maybe (Left ("Child execution needs an owned function body for " <> name)) Right (M.lookup name available)
            let nestedEffects = P.bodyEffects statementEffects functionBody
            close (M.insert name functionBody complete) (effects <> nestedEffects) (remaining <> (effectFunctions nestedEffects S.\\ M.keysSet complete))
    specialBinding name =
      name `elem` ["PATH", "CDPATH", "MANPATH"]
        || T.isPrefixOf "BASH" name
        || name `elem` ["PPID", "FUNCNAME", "SHELLOPTS", "DIRSTACK", "PIPESTATUS", "RANDOM", "SRANDOM", "SECONDS", "LINENO", "EPOCHSECONDS", "EPOCHREALTIME", "GROUPS", "UID", "EUID"]

-- | A builtin's broken-pipe lifetime differs from an ordinary external
-- command. Until owned signal termination is modeled, require each consumer
-- downstream of a possible builtin writer to drain its input unconditionally.
admitPipeline :: NonEmpty P.ChildRegion -> Either Text ()
admitPipeline regions = check (toList regions)
  where
    check [] = pure ()
    check (region : later) = do
      unless
        (null later || not (P.withScopedBody (P.childBody region) (\_ -> mayWriteBuiltin (P.childFunctions region))) || all drains later)
        (Left "A builtin pipeline writer requires every downstream stage to be an unconditional stdin-draining cat or literal tr a-z A-Z; broken-pipe owner termination is not yet modeled")
      check later
    drains region = P.withScopedBody (P.childBody region) $ \_ statements -> case reverse (concatMap flatten statements) of
      P.Statement _ (P.Invoke (P.External "cat") []) : prefix -> all safeAssignment prefix
      P.Statement _ (P.Invoke (P.External "tr") [P.OneField (P.Literal "a-z"), P.OneField (P.Literal "A-Z")]) : prefix -> all safeAssignment prefix
      _ -> False
    flatten :: P.Statement scope -> [P.Statement scope]
    flatten (P.Statement _ (P.Sequence body)) = concatMap flatten body
    flatten statement = [statement]
    safeAssignment :: P.Statement scope -> Bool
    safeAssignment (P.Statement _ node) = case node of
      P.Assign _ _ (P.Literal _) -> True
      P.AssignmentCommand _ body -> all safeAssignment body
      P.Sequence body -> all safeAssignment body
      _ -> False

mayWriteBuiltin :: Map Text (P.ScopedBody Control.FunctionRootKind) -> [P.Statement scope] -> Bool
mayWriteBuiltin functions = any (writesBuiltin functions)

writesBuiltin :: Map Text (P.ScopedBody Control.FunctionRootKind) -> P.Statement scope -> Bool
writesBuiltin functions (P.Statement _ node) = case node of
  P.Invoke (P.Builtin name) _ -> name `elem` ["printf", "echo"]
  P.PrefixedInvoke _ target values -> writesBuiltin functions (P.Statement Nothing (P.Invoke target values))
  P.Invoke (P.Function name) _ -> maybe True (\body -> P.withScopedBody body (\_ -> any (writesBuiltin functions))) (M.lookup name functions)
  P.Sequence body -> any (writesBuiltin functions) body
  P.Redirected _ statement -> writesBuiltin functions statement
  P.AssignmentCommand _ body -> any (writesBuiltin functions) body
  P.And left right -> writesBuiltin functions left || writesBuiltin functions right
  P.Or left right -> writesBuiltin functions left || writesBuiltin functions right
  P.Negate value -> writesBuiltin functions value
  P.Conditional condition yes no -> any (writesBuiltin functions) (condition <> yes <> no)
  P.WhileLoop _ body -> P.withWhileBody body (\_ _ condition values -> any (writesBuiltin functions) (condition <> values))
  P.ForLoop _ _ _ body -> P.withForBody body (\_ _ -> any (writesBuiltin functions))
  P.Case _ arms -> any (\(P.CaseArm _ body _) -> any (writesBuiltin functions) body) arms
  P.Approximate _ body -> any (writesBuiltin functions) body
  P.SourceBody _ body -> P.withScopedBody body (\_ -> any (writesBuiltin functions))
  P.Subshell child -> P.withScopedBody (P.childBody child) (\_ -> any (writesBuiltin (P.childFunctions child)))
  P.Pipeline children -> let final = last children in P.withScopedBody (P.childBody final) (\_ -> any (writesBuiltin (P.childFunctions final)))
  P.SupervisedPipeline children -> let final = last children in P.withScopedBody (P.childBody final) (\_ -> any (writesBuiltin (P.childFunctions final)))
  P.Background _ -> False
  P.Wait _ -> False
  P.Read {} -> False
  P.PrefixedRead {} -> False
  P.SetTrap {} -> False
  P.Invoke (P.External _) _ -> False
  P.DeclarationCommand {} -> False
  P.Assign {} -> False
  P.AssignArray {} -> False
  P.AppendArray {} -> False
  P.AssignArrayElement {} -> False
  P.Erase {} -> False
  P.SetArguments {} -> False
  P.ShiftArguments {} -> False
  P.ArithmeticFor initial body -> writesBuiltin functions initial || P.withArithmeticBody body (\_ _ predicate increment values -> any (writesBuiltin functions) (predicate : increment : values))
  P.DirectoryOperation {} -> True
  P.SetOption {} -> False
  P.PatternCondition {} -> False
  P.NumericCondition {} -> False
  P.DefineFunction {} -> False
  P.ArithmeticCommand {} -> False
  P.Return {} -> False
  P.Exit {} -> False
  P.Break _ -> False
  P.Continue _ -> False
