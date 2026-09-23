{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Bash.Plan.Normalize.Commands
  ( normalizeRead,
    normalizeDirectory,
    safeBuiltins,
    unsupportedBuiltins,
    normalizeFixedTest,
    validatePrintf,
    normalizeSet,
    freezeComparison,
  )
where

import Control.Monad.State.Strict (gets)
import Data.Char (isHexDigit, isOctDigit)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Effects qualified as Effects
import Language.Bash.Plan.Facts (DenseLength (DynamicLength))
import Language.Bash.Plan.Normalize.Context
import Language.Bash.Plan.Normalize.Control qualified as Control
import Language.Bash.Plan.Normalize.Literal
import Language.Bash.Plan.Normalize.State
import Language.Bash.Plan.Normalize.Words qualified as Words
import Language.Bash.Plan.Operator qualified as Operator
import Monk.Translation.Types
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralString)
import Prelude hiding (get, gets, identity, local, put)

normalizeRead :: Words.WordNormalizer scope -> Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeRead wordOperations token arguments = do
  requireSession token
  evaluated <- gets nEvaluatedPrograms
  unless (null evaluated) (reject token "eval-read-diagnostic" "Eval read needs exact nested diagnostic source locations")
  (options, arrayTarget, names) <- parseOptions (P.ReadOptions False "\n" Nothing 0) Nothing arguments
  descriptors <- gets nDescriptors
  unless (S.member (P.readDescriptor options) descriptors) (reject token "read-descriptor" "Read requires an explicitly owned descriptor")
  target <- case arrayTarget of
    Just name -> do
      unless (null names) (reject token "read-array-operands" "Array reads currently require no additional scalar destinations")
      storage <- arrayStorage token False name
      rememberArray name DynamicLength
      pure (P.ReadArray storage name)
    Nothing -> case names of
      [] -> P.ReadReply <$> destination "REPLY"
      _ -> P.ReadScalars <$> traverse (\name -> (,name) <$> destination name) names
  pure (P.Read options target)
  where
    destination name = do
      checkedName token name
      rejectArrayScalar token name
      storage <- storageFor token False name
      modify' (\flow -> updateNormalization id (\currentFacts -> currentFacts {factVariables = S.insert name (nVariables flow), factConstants = M.delete name (nConstants flow), factNumeric = S.delete name (nNumeric flow), factResolutionStable = nResolutionStable flow && name `notElem` resolutionVariables}) id flow)
      pure storage
    literal operand = do
      scalar <- Words.normalizedScalar wordOperations operand
      maybe (reject operand "read-operand" "Read options and destination names require literal operands") pure (scalarLiteral scalar)
    parseOptions options arrayTarget [] = pure (options, arrayTarget, [])
    parseOptions options arrayTarget (operand : rest) = do
      spelling <- literal operand
      if spelling == "--"
        then (options,arrayTarget,) <$> traverse literal rest
        else case T.stripPrefix "-" spelling of
          Just flags | not (T.null flags) -> parseFlags options arrayTarget (T.unpack flags) rest
          _ -> (options,arrayTarget,) <$> traverse literal (operand : rest)
    parseFlags options arrayTarget [] rest = parseOptions options arrayTarget rest
    parseFlags options arrayTarget ('r' : flags) rest = parseFlags options {P.readRaw = True} arrayTarget flags rest
    parseFlags options arrayTarget (flag : flags) rest
      | flag `elem` ("duna" :: String) = do
          (value, remaining) <-
            if null flags
              then case rest of
                operand : tailTokens -> (,tailTokens) <$> literal operand
                [] -> reject token "read-option-argument" "Read option is missing its literal operand"
              else pure (toText flags, rest)
          case flag of
            'd' -> parseOptions options {P.readDelimiter = value} arrayTarget remaining
            'u' -> do
              descriptor <- descriptorNumber token value
              parseOptions options {P.readDescriptor = descriptor} arrayTarget remaining
            'n' -> do
              count <- maybe (reject token "read-count" "Read character counts require a bounded nonnegative decimal literal") pure (decimalIndex value)
              parseOptions options {P.readCount = Just count} arrayTarget remaining
            _ -> checkedName token value >> parseOptions options (Just value) remaining
    parseFlags _ _ _ _ = reject token "read-option" "Read admits only literal -r, -d, -n, -u and -a options"

normalizeDirectory :: Words.WordNormalizer scope -> Token -> Text -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeDirectory wordOperations token command operands = do
  cfg <- gets nConfig
  unless (stableDirectoryEnabled cfg) (reject token "directory-contract" "Directory operations require the stable directory contract")
  arguments <- Words.normalizedWords wordOperations operands
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
    _ -> modify' (\flow -> let before = nDirectoryFacts flow; success = Directory.successfulDirectory operation before in updateNormalization id (\currentFacts -> currentFacts {factDirectoryFacts = Directory.joinDirectoryFacts success before, factDirectoryOutcomes = Just (success, before), factConstants = M.delete "OLDPWD" (M.delete "PWD" (nConstants flow))}) id flow)
  pure (P.DirectoryOperation operation)
  where
    proved path = if Directory.proveDirectoryPath path then pure path else reject token "directory-path" "Directory path has interior parent cancellation or an unsupported shape"

safeBuiltins :: [Text]
safeBuiltins = ["echo", "printf", "true", "false", ":", "test", "["]

unsupportedBuiltins :: [Text]
unsupportedBuiltins = ["read", "declare", "typeset", "readonly", "export", "shopt", "trap", "exec", "shift", "wait", "cd", "pushd", "popd", "enable", "hash", "alias", "unalias", "getopts", "readarray", "mapfile", "let", "caller", "jobs", "fg", "bg", "disown", "umask", "ulimit", "bind", "help", "history", "complete", "compgen", "compopt", "dirs", "fc", "kill", "logout", "pwd", "suspend", "times", "type"]

normalizeFixedTest :: Words.WordNormalizer scope -> Token -> Text -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeFixedTest wordOperations token name arguments = do
  operands <-
    if name == "["
      then case reverse arguments of
        end : rest | getLiteralString end == Just "]" -> pure (reverse rest)
        _ -> reject token "test-terminator" "Bracket test needs a literal closing bracket"
      else pure arguments
  wordsValue <- Words.normalizedWords wordOperations operands
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
      | Just comparison <- Operator.parseNumericComparison operator = do
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
          pure (freezeComparison constants comparison left right)
    fixed (P.Literal "!" : values) | length values `elem` [2, 3] = P.Negate . P.Statement Nothing <$> fixed values
    fixed _ = reject token "test-operands" "Test requires an admitted fixed-arity operation"

validatePrintf :: Token -> [Token] -> Normalize scope ()
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
    invalidFormat :: Normalize scope a
    invalidFormat = reject token "printf-format" "Printf requires a literal %s/%d/%% format with admitted byte escapes"
    invalidNumber :: Normalize scope a
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

normalizeSet :: Words.WordNormalizer scope -> Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeSet wordOperations token arguments = case traverse getLiteralString arguments of
  Just ["-e"] -> setOption P.Errexit True
  Just ["+e"] -> setOption P.Errexit False
  Just ["-o", "pipefail"] -> setOption P.Pipefail True
  Just ["+o", "pipefail"] -> setOption P.Pipefail False
  _ -> case arguments of
    marker : rest | getLiteralString marker == Just "--" -> do
      control <- gets nControl
      target <- maybe (reject token "source-argv-mutation" "Source may not mutate the caller argv through a generated boundary") pure (Control.setArgumentsTarget control)
      P.SetArguments target <$> Words.normalizedWords wordOperations rest
    _ -> reject token "set-option" "Only explicit argv and errexit/pipefail option transitions are admitted"
  where
    setOption option enabled = do
      sourceable <- gets ((== Sourceable) . entryMode . nConfig)
      when sourceable (reject token "sourceable-option-effect" "Persistent caller option effects are not yet represented by the sourceable contract")
      pure (P.SetOption option enabled)

freezeComparison :: M.Map Text Text -> Operator.NumericComparison -> P.Scalar -> P.Scalar -> P.StatementNode scope
freezeComparison constants comparison left right =
  let writes = Effects.effectWrites (Effects.scalarEffects left <> Effects.scalarEffects right)
      freeze original@(P.Variable name) | S.notMember name writes = maybe original P.Literal (M.lookup name constants)
      freeze value = value
   in P.NumericCondition comparison (freeze left) (freeze right)
