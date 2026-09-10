-- | Finite effects over owned semantic nodes, used to close child snapshots.
module Language.Bash.Plan.Effects
  ( Effects (..),
    statementEffects,
    scalarEffects,
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
import Language.Bash.Plan.Directory qualified as Directory
import Language.Fish.DSL (SourceRange)

data Effects = MkEffects
  { effectReads :: Set Text,
    effectWrites :: Set Text,
    effectFunctions :: Set Text,
    effectExternalEnvironment :: Bool,
    effectMayEnableErrexit :: Bool,
    effectSubstitution :: Bool,
    effectPipefail :: Bool
  }
  deriving stock (Eq, Show)

instance Semigroup Effects where
  MkEffects ar aw af ae ax as ap <> MkEffects br bw bf be bx bs bp = MkEffects (ar <> br) (aw <> bw) (af <> bf) (ae || be) (ax || bx) (as || bs) (ap || bp)

instance Monoid Effects where
  mempty = MkEffects mempty mempty mempty False False False False

-- | Every scalar touched by the owned program, including function bodies.
programVariables :: [P.Statement] -> Set Text
programVariables statements =
  let effects = foldMap statementEffects statements
   in effectReads effects <> effectWrites effects

readName :: Text -> Effects
readName name = mempty {effectReads = S.singleton name}

writeName :: Text -> Effects
writeName name = mempty {effectWrites = S.singleton name}

statementEffects :: P.Statement -> Effects
statementEffects (P.Statement _ node) = case node of
  P.Sequence body -> foldMap statementEffects body
  P.Redirected _ statement -> statementEffects statement
  P.AssignmentCommand _ body -> foldMap statementEffects body
  P.DeclarationCommand declarations -> foldMap declarationEffects declarations
  P.Invoke target wordsValue ->
    foldMap wordEffects wordsValue <> case target of
      P.Function name -> mempty {effectFunctions = S.singleton name, effectMayEnableErrexit = True}
      P.Builtin _ -> mempty
      P.External _ -> mempty {effectExternalEnvironment = True}
  P.Assign _ name scalar -> writeName name <> scalarEffects scalar
  P.Erase name -> writeName name
  P.SetArguments values -> foldMap wordEffects values
  P.ShiftArguments _ -> mempty
  P.ArithmeticFor initial predicate increment body -> foldMap statementEffects (initial : predicate : increment : body)
  P.DirectoryOperation operation -> case operation of
    Directory.PrintDirectory _ -> mempty
    Directory.ChangeDirectory _ -> writeName "OLDPWD"
    Directory.ChangePreviousDirectory -> readName "OLDPWD" <> writeName "OLDPWD"
    Directory.PushDirectory _ -> readName "dirstack" <> writeName "dirstack" <> writeName "OLDPWD"
    Directory.PopDirectory -> readName "dirstack" <> writeName "dirstack" <> writeName "OLDPWD"
  P.SetOption option _ -> mempty {effectMayEnableErrexit = option == P.Errexit, effectPipefail = option == P.Pipefail}
  P.And left right -> statementEffects left <> statementEffects right
  P.Or left right -> statementEffects left <> statementEffects right
  P.Negate value -> statementEffects value
  P.Conditional condition yes no -> foldMap statementEffects (condition <> yes <> no)
  P.WhileLoop _ condition body -> foldMap statementEffects (condition <> body)
  P.ForLoop _ name values body -> writeName name <> foldMap wordEffects values <> foldMap statementEffects body
  P.Case scalar arms -> scalarEffects scalar <> foldMap (\(P.CaseArm patterns body _) -> foldMap patternEffects patterns <> foldMap statementEffects body) arms
  P.PatternCondition _ scalar patternValue -> scalarEffects scalar <> patternEffects patternValue
  P.NumericCondition _ left right -> scalarEffects left <> scalarEffects right
  P.Approximate _ body -> foldMap statementEffects body
  P.DefineFunction _ body -> foldMap statementEffects body
  P.SourceBody request body -> foldMap wordEffects (P.sourceRequestArguments request) <> foldMap statementEffects body
  P.Subshell child -> childEffects child
  P.Pipeline children -> (foldMap childEffects children) {effectPipefail = True}
  P.ArithmeticCommand _ expression _ -> arithmeticEffects expression
  P.Return value -> foldMap scalarEffects value
  P.Exit value -> foldMap scalarEffects value
  P.Break -> mempty
  P.Continue -> mempty

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

scalarEffects :: P.Scalar -> Effects
scalarEffects = \case
  P.Literal _ -> mempty
  P.ByteLiteral _ -> mempty
  P.AlternateValue name _ value -> readName name <> scalarEffects value
  P.ParameterTransform _ value _ _ -> scalarEffects value
  P.AppendValue name value -> readName name <> scalarEffects value
  P.Variable name -> readName name
  P.Positional _ -> mempty
  P.PositionalDefault _ _ value -> scalarEffects value
  P.PositionalAlternate _ _ value -> scalarEffects value
  P.ArgumentCount -> mempty
  P.LastStatus -> mempty
  P.Concat values -> foldMap scalarEffects values
  P.Substitute child -> (childEffects child) {effectSubstitution = True}
  P.DefaultValue _ name _ assigning value -> readName name <> (if assigning then writeName name else mempty) <> scalarEffects value
  P.ArithmeticValue _ expression _ -> arithmeticEffects expression

patternEffects :: P.Pattern -> Effects
patternEffects (P.MkPattern parts) = foldMap (\case P.LiteralPattern scalar -> scalarEffects scalar; P.ActivePattern scalar -> scalarEffects scalar) parts

childEffects :: P.ChildRegion -> Effects
childEffects child =
  let nested = foldMap statementEffects (P.childStatements child <> concat (M.elems (P.childFunctions child)))
   in mempty {effectReads = P.childVariables child, effectExternalEnvironment = P.childNeedsEnvironment child, effectMayEnableErrexit = effectMayEnableErrexit nested, effectSubstitution = effectSubstitution nested, effectPipefail = effectPipefail nested}

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

closeChildRegion :: Maybe SourceRange -> Map Text [P.Statement] -> [P.Statement] -> Either Text P.ChildRegion
closeChildRegion range available body = do
  let initial = foldMap statementEffects body
  (functions, effects) <- close mempty initial (effectFunctions initial)
  let variables = effectReads effects <> effectWrites effects
      unsupported = S.filter specialBinding variables
  unless (S.null unsupported) (Left ("Child snapshot cannot preserve target-special or Bash introspection bindings: " <> T.intercalate ", " (S.toAscList unsupported)))
  pure (P.MkChildRegion range body functions variables (effectExternalEnvironment effects))
  where
    close complete effects pending = case S.minView pending of
      Nothing -> pure (complete, effects)
      Just (name, remaining)
        | M.member name complete -> close complete effects remaining
        | otherwise -> do
            functionBody <- maybe (Left ("Child execution needs an owned function body for " <> name)) Right (M.lookup name available)
            let nestedEffects = foldMap statementEffects functionBody
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
        (null later || not (mayWriteBuiltin (P.childFunctions region) (P.childStatements region)) || all drains later)
        (Left "A builtin pipeline writer requires every downstream stage to be an unconditional stdin-draining cat or literal tr a-z A-Z; broken-pipe owner termination is not yet modeled")
      check later
    drains region = case reverse (concatMap flatten (P.childStatements region)) of
      P.Statement _ (P.Invoke (P.External "cat") []) : prefix -> all safeAssignment prefix
      P.Statement _ (P.Invoke (P.External "tr") [P.OneField (P.Literal "a-z"), P.OneField (P.Literal "A-Z")]) : prefix -> all safeAssignment prefix
      _ -> False
    flatten (P.Statement _ (P.Sequence body)) = concatMap flatten body
    flatten statement = [statement]
    safeAssignment (P.Statement _ node) = case node of
      P.Assign _ _ (P.Literal _) -> True
      P.AssignmentCommand _ body -> all safeAssignment body
      P.Sequence body -> all safeAssignment body
      _ -> False

mayWriteBuiltin :: Map Text [P.Statement] -> [P.Statement] -> Bool
mayWriteBuiltin functions = any (writesBuiltin functions)

writesBuiltin :: Map Text [P.Statement] -> P.Statement -> Bool
writesBuiltin functions (P.Statement _ node) = case node of
  P.Invoke (P.Builtin name) _ -> name `elem` ["printf", "echo"]
  P.Invoke (P.Function name) _ -> maybe True (any (writesBuiltin functions)) (M.lookup name functions)
  P.Sequence body -> any (writesBuiltin functions) body
  P.Redirected _ statement -> writesBuiltin functions statement
  P.AssignmentCommand _ body -> any (writesBuiltin functions) body
  P.And left right -> writesBuiltin functions left || writesBuiltin functions right
  P.Or left right -> writesBuiltin functions left || writesBuiltin functions right
  P.Negate value -> writesBuiltin functions value
  P.Conditional condition yes no -> any (writesBuiltin functions) (condition <> yes <> no)
  P.WhileLoop _ condition body -> any (writesBuiltin functions) (condition <> body)
  P.ForLoop _ _ _ body -> any (writesBuiltin functions) body
  P.Case _ arms -> any (\(P.CaseArm _ body _) -> any (writesBuiltin functions) body) arms
  P.Approximate _ body -> any (writesBuiltin functions) body
  P.SourceBody _ body -> any (writesBuiltin functions) body
  P.Subshell child -> any (writesBuiltin (P.childFunctions child)) (P.childStatements child)
  P.Pipeline children -> let final = last children in any (writesBuiltin (P.childFunctions final)) (P.childStatements final)
  P.Invoke (P.External _) _ -> False
  P.DeclarationCommand {} -> False
  P.Assign {} -> False
  P.Erase {} -> False
  P.SetArguments {} -> False
  P.ShiftArguments {} -> False
  P.ArithmeticFor initial predicate increment body -> any (writesBuiltin functions) (initial : predicate : increment : body)
  P.DirectoryOperation {} -> True
  P.SetOption {} -> False
  P.PatternCondition {} -> False
  P.NumericCondition {} -> False
  P.DefineFunction {} -> False
  P.ArithmeticCommand {} -> False
  P.Return {} -> False
  P.Exit {} -> False
  P.Break -> False
  P.Continue -> False
