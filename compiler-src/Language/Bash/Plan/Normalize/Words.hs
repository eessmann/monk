{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Bash.Plan.Normalize.Words
  ( WordCallbacks (..),
    WordNormalizer (..),
    wordNormalizer,
    processToken,
  )
where

import Control.Monad.State.Strict (get, gets, put)
import Data.Char (isAlphaNum, isDigit)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Arithmetic.Source qualified as ArithmeticSource
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Normalize.Context
import Language.Bash.Plan.Normalize.Flow
import Language.Bash.Plan.Normalize.Literal
import Language.Bash.Plan.Normalize.State
import Language.Bash.Plan.Normalize.Syntax
import Language.Bash.Plan.Operator qualified as Operator
import ShellCheck.AST
import Prelude hiding (get, gets, identity, local, put)

-- | Only executable child regions and arithmetic re-enter the syntax core.
data WordCallbacks scope = WordCallbacks
  { normalizeWordChild :: Token -> [Token] -> Normalize scope P.ChildRegion,
    normalizeWordArithmetic :: Token -> Normalize scope (ArithmeticSource.ArithmeticSite, A.ArithmeticExpr, M.Map Text P.Storage)
  }

data WordNormalizer scope = WordNormalizer
  { normalizedProcess :: Token -> Normalize scope P.Scalar,
    normalizedWords :: [Token] -> Normalize scope [P.Word],
    normalizedConsumerWords :: Text -> [Token] -> Normalize scope [P.Word],
    normalizedWord :: Token -> Normalize scope P.Word,
    normalizedScalar :: Token -> Normalize scope P.Scalar,
    normalizedScalarIn :: Bool -> Token -> Normalize scope P.Scalar,
    normalizedPattern :: Token -> Normalize scope P.Pattern
  }

wordNormalizer :: forall scope. WordCallbacks scope -> WordNormalizer scope
wordNormalizer callbacks = WordNormalizer normalizeProcess normalizeWords normalizeConsumerWords normalizeWord normalizeScalar normalizeScalarIn normalizePattern
  where
    -- Brace distribution precedes every expansion. Duplicated parser occurrences
    -- are normalized afresh, so writes and command substitutions run per result.
    normalizeWords :: [Token] -> Normalize scope [P.Word]
    normalizeWords values = concat <$> traverse (expandBraces >=> traverse normalizeWord) values

    -- Endpoint pathnames may only reach commands whose admitted use consumes bytes.
    normalizeConsumerWords :: Text -> [Token] -> Normalize scope [P.Word]
    normalizeConsumerWords command = fmap concat . traverse operand
      where
        operand value = case processToken value of
          Just endpoint | command `elem` ["cat", "diff", "cmp", "tee", "wc"] -> (: []) . P.OneField <$> normalizeProcess endpoint
          _ -> normalizeWords [value]

    normalizeProcess :: Token -> Normalize scope P.Scalar
    normalizeProcess token@(T_ProcSub _ direction body) = do
      requireSession token
      evaluated <- gets nEvaluatedPrograms
      unless (null evaluated) (reject token "eval-process-diagnostic" "Deferred source cannot own process substitution diagnostics")
      mode <- case direction of
        "<" -> pure P.ProcessInput
        ">" -> pure P.ProcessOutput
        _ -> reject token "process-direction" "Unknown process substitution direction"
      P.ProcessSubstitution mode <$> normalizeWordChild callbacks token body
    normalizeProcess token = reject token "process-shape" "Process substitution requires an entire endpoint word"

    expandBraces :: Token -> Normalize scope [Token]
    expandBraces token = case token of
      T_BraceExpansion _ alternatives -> concat <$> traverse expandBraces alternatives
      T_NormalWord identity parts -> do
        alternatives <- traverse expandBraces parts
        pure [T_NormalWord identity combination | combination <- sequence alternatives]
      _ -> pure [token]

    normalizeWord :: Token -> Normalize scope P.Word
    normalizeWord token = case token of
      T_NormalWord _ parts | any isGlobPart parts && all simplePatternPart parts -> P.PathnameFields <$> literalPathname token parts
      T_NormalWord _ parts -> normalizeParts token parts
      other -> normalizeParts token [other]
      where
        isGlobPart T_Glob {} = True
        isGlobPart _ = False
        simplePatternPart (T_Glob _ value) = value `elem` ["*", "?"]
        simplePatternPart T_Literal {} = True
        simplePatternPart T_SingleQuoted {} = True
        simplePatternPart (T_DoubleQuoted _ values) = all (\case T_Literal {} -> True; T_SingleQuoted {} -> True; _ -> False) values
        simplePatternPart _ = False

    literalPathname :: Token -> [Token] -> Normalize scope P.Pattern
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

    normalizeParts :: Token -> [Token] -> Normalize scope P.Word
    normalizeParts token parts = do
      pieces <- concat <$> traverse wordPieces parts
      case [piece | piece <- pieces, isArgv piece] of
        [] -> do
          constants <- gets nConstants
          let knownIfs = M.lookup "IFS" constants
              inert (ScalarPiece True (P.Variable name)) = maybe False (\ifs -> maybe False (\value -> not (T.null value) && not (T.any (`T.elem` (ifs <> "*?[")) value)) (M.lookup name constants)) knownIfs
              inert (ScalarPiece True (P.ArithmeticValue {})) = maybe False (\ifs -> not (T.any (`T.elem` ifs) "0123456789-")) knownIfs
              inert (ScalarPiece False _) = True
              inert _ = False
              split = any isSplit pieces && not (all inert pieces)
          let value = compact [scalar | ScalarPiece _ scalar <- pieces]
              composed = any isGlob pieces || (split && (length pieces /= 1 || not (noPathnameExpansion constants value)))
              expansion = \case
                ScalarPiece True scalar -> P.SplitExpansion scalar
                ScalarPiece False scalar -> P.QuotedExpansion scalar
                GlobPiece patternValue -> P.LiteralExpansion (P.Literal patternValue)
                _ -> P.QuotedExpansion (P.Literal "")
          pure (if composed then P.ExpandedWord (map expansion pieces) else (if split then P.SplitFields else P.OneField) value)
        [splice] -> do
          let (before, after0) = break isArgv pieces
              after = drop 1 after0
          when (any isSplit (before <> after) || any isGlob (before <> after)) (reject token "argv-splitting" "Quoted argv cannot share an unquoted splitting region")
          let prefix = compact [value | ScalarPiece _ value <- before]
              suffix = compact [value | ScalarPiece _ value <- after]
              forceField = not (null before && null after)
          pure (case splice of ArrayPiece name -> P.QuotedArray name prefix suffix forceField; _ -> P.QuotedArguments prefix suffix forceField)
        _ -> reject token "argv-products" "Multiple argv splices require an explicit product plan"
      where
        isSplit (ScalarPiece split _) = split
        isSplit ArgvPiece = False
        isSplit (ArrayPiece _) = False
        isSplit (GlobPiece _) = False
        isGlob (GlobPiece _) = True
        isGlob _ = False
        isArgv ArgvPiece = True
        isArgv (ArrayPiece _) = True
        isArgv _ = False

    -- The field splitter is exact only when no later pathname expansion is possible.
    -- This fact belongs to the normalized word, before materialization.

    wordPieces :: Token -> Normalize scope [Piece]
    wordPieces = \case
      T_Glob _ value -> pure [GlobPiece (toText value)]
      T_DoubleQuoted _ [] -> pure [ScalarPiece False (P.Literal "")]
      T_DoubleQuoted _ parts -> concat <$> traverse quotedPiece parts
      token@(T_DollarBraced _ _ inner) -> do
        if parameterText inner == Just "@" then reject token "unquoted-argv" "Unquoted argv needs per-argument field splitting" else (: []) . ScalarPiece True <$> normalizeScalar token
      token@(T_DollarExpansion {}) -> (: []) . ScalarPiece True <$> normalizeScalar token
      token -> (: []) . ScalarPiece False <$> normalizeScalar token
      where
        quotedPiece (T_DollarBraced _ _ inner) | parameterText inner == Just "@" = pure [ArgvPiece]
        quotedPiece token@(T_DollarBraced _ _ inner)
          | Just name <- parameterText inner >>= T.stripSuffix "[@]", validName name = arrayReadable token name >> pure [ArrayPiece name]
        quotedPiece token = (: []) . ScalarPiece False <$> normalizeScalarIn True token

    normalizeScalar :: Token -> Normalize scope P.Scalar
    normalizeScalar = normalizeScalarIn False

    normalizeScalarIn :: Bool -> Token -> Normalize scope P.Scalar
    normalizeScalarIn quoted token = case token of
      T_Literal _ value -> pure (P.Literal (toText value))
      T_SingleQuoted _ value -> pure (P.Literal (if quoted then "'" <> toText value <> "'" else toText value))
      T_DollarSingleQuoted _ value -> do
        darwin <- either (reject token "ansi-quoted-escape") pure (ansiBytes True value)
        linux <- either (reject token "ansi-quoted-escape") pure (ansiBytes False value)
        pure (if darwin /= linux then P.PlatformBytes darwin linux else either (const (P.ByteLiteral darwin)) P.Literal (decodeUtf8' darwin))
      T_NormalWord _ parts -> compact <$> traverse (normalizeScalarIn quoted) parts
      T_DoubleQuoted _ parts -> compact <$> traverse (normalizeScalarIn True) parts
      T_DollarBraced _ _ inner -> normalizeParameter quoted token inner
      T_DollarExpansion _ body -> do
        evaluated <- gets nEvaluatedPrograms
        unless (null evaluated) (reject token "eval-substitution-diagnostic" "Eval command substitution needs exact nested warning source locations")
        P.Substitute <$> normalizeWordChild callbacks token body
      T_DollarBracket {} -> do
        (site, expression, bindings) <- normalizeWordArithmetic callbacks token
        pure (P.ArithmeticValue site expression bindings)
      T_DollarArithmetic {} -> do
        (site, expression, bindings) <- normalizeWordArithmetic callbacks token
        pure (P.ArithmeticValue site expression bindings)
      _ -> reject token "word" ("No scalar semantics for " <> tokenKind token)

    normalizeParameter :: Bool -> Token -> Token -> Normalize scope P.Scalar
    normalizeParameter quoted token inner = case parameterText inner of
      Just "?" -> pure P.LastStatus
      Just "!" -> requireSession token >> pure P.LastBackgroundPid
      Just "#" -> pure P.ArgumentCount
      Just name | Just index <- positional name -> pure (P.Positional index)
      Just name | validName name -> do
        checkedName token name
        readBinding token name
        arrays <- gets nArrays
        if M.member name arrays then arrayReadable token name >> pure (P.ArrayElement name 0) else pure (P.Variable name)
      Just spelling | Just name <- T.stripPrefix "#" spelling >>= T.stripSuffix "[@]", validName name -> arrayReadable token name >> pure (P.ArrayLength name)
      Just spelling | Just (name, index) <- indexedParameter spelling -> do
        arrayReadable token name
        pure (P.ArrayElement name index)
      _ -> do
        (name, suffix, remaining) <- case inner of
          T_NormalWord _ (T_Literal _ leading : rest) -> let (name, suffix) = T.span (\c -> isAlphaNum c || c == '_') (toText leading) in pure (name, suffix, rest)
          T_Literal _ leading -> let (name, suffix) = T.span (\c -> isAlphaNum c || c == '_') (toText leading) in pure (name, suffix, [])
          _ -> reject token "parameter" "Parameter operation needs a literal scalar name and operator"
        rejectArrayScalar token name
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
                when assigning (modify' (\s -> updateNormalization id (\currentFacts -> currentFacts {factConstants = M.delete name (nConstants s), factNumeric = S.delete name (nNumeric s), factVariables = S.insert name (nVariables s), factResolutionStable = nResolutionStable s && name `notElem` resolutionVariables}) id s))
                storage <- if assigning then storageFor token False name else pure P.Visible
                pure (if alternate then P.AlternateValue name nullSensitive alternative else P.DefaultValue storage name nullSensitive assigning alternative)
          Nothing ->
            case asum [(operation,) <$> T.stripPrefix spelling suffix | (spelling, operation) <- [("##", Operator.PrefixLong), ("#", Operator.PrefixShort), ("%%", Operator.SuffixLong), ("%", Operator.SuffixShort)]] of
              Just (operation, literalPrefix) -> P.ParameterPatternTransform operation target <$> normalizeTrimPattern token literalPrefix remaining
              Nothing -> do
                literalTail <- maybe (reject token "parameter-pattern" "Parameter replacements must be literal") (pure . mconcat) (traverse parameterText remaining)
                case T.stripPrefix "/" (suffix <> literalTail) of
                  Just replacementSpec -> do
                    let (operation, spec) = maybe (Operator.ReplaceFirst, replacementSpec) (Operator.ReplaceAll,) (T.stripPrefix "/" replacementSpec)
                        (needle, tailValue) = T.breakOn "/" spec
                        replacement = fromMaybe "" (T.stripPrefix "/" tailValue)
                    when (T.null needle || T.any (`elem` ['*', '?', '[', '\\', '#', '%']) needle || T.any (`elem` ['&', '\\']) replacement) (reject token "parameter-replacement" "Replacement requires a nonempty literal needle and literal replacement bytes")
                    pure (P.ParameterTransform operation target needle replacement)
                  Nothing -> reject token "parameter" "Parameter modifier has no admitted scalar operation"
      where
        positional :: Text -> Maybe Int
        positional name | T.all isDigit name, not (T.null name), Just index <- readMaybe (toString name), index > 0 = Just index
        positional _ = Nothing

    -- Parameter trim syntax keeps source backslashes in literal AST fragments.
    -- The runtime pattern tokenizer consumes them once; quotes instead protect the
    -- enclosed fragment, independently of outer quotes around the expansion.
    normalizeTrimPattern :: Token -> Text -> [Token] -> Normalize scope P.Pattern
    normalizeTrimPattern token leading rest = do
      prefix <- active token (P.Literal leading)
      P.MkPattern . (prefix <>) . concat <$> traverse fragment rest
      where
        fragment node = case node of
          T_NormalWord _ values -> concat <$> traverse fragment values
          T_Literal _ value -> active node (P.Literal (toText value))
          T_Glob _ value -> active node (P.Literal (toText value))
          T_ParamSubSpecialChar _ value -> active node (P.Literal (toText value))
          T_SingleQuoted _ value -> pure [P.LiteralPattern (P.Literal (toText value))]
          T_DoubleQuoted _ values -> (: []) . P.LiteralPattern . compact <$> traverse (normalizeScalarIn True) values
          T_DollarSingleQuoted {} -> (: []) . P.LiteralPattern <$> normalizeScalar node
          _ -> normalizeScalar node >>= active node
        active node value = do
          constants <- gets nConstants
          numeric <- gets nNumeric
          let safe = \case
                P.Literal literal -> not (T.any (`elem` ("()" :: String)) literal)
                P.Variable name -> maybe False (not . T.any (`elem` ("()" :: String))) (M.lookup name constants) || S.member name numeric
                P.ArithmeticValue {} -> True
                P.ArgumentCount -> True
                P.LastStatus -> True
                P.Concat values -> all safe values
                _ -> False
          unless (safe value) (reject node "parameter-pattern" "Active trim patterns require a proof excluding extended-pattern syntax")
          pure [P.ActivePattern value]

    normalizePattern :: Token -> Normalize scope P.Pattern
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
        simpleValue :: Normalization scope -> P.Scalar -> Bool
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

data Piece = ScalarPiece Bool P.Scalar | ArgvPiece | ArrayPiece Text | GlobPiece Text

processToken :: Token -> Maybe Token
processToken value@T_ProcSub {} = Just value
processToken (T_NormalWord _ [value]) = processToken value
processToken _ = Nothing
