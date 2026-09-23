{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Bash.Plan.Normalize.Context
  ( SourceDocument (..),
    NormalizationResult (..),
    Normalize,
    scopedControl,
    scopedLoop,
    documentName,
    diagnostic,
    reject,
    tokenRange,
    runtimeTokenRange,
    contractNames,
    requireSession,
    initializedImports,
    readBinding,
    storageFor,
    rejectArrayScalar,
    arrayReadable,
    arrayStorage,
    rememberArray,
    literalName,
    checkedCommand,
    checkedFunctionCommand,
    checkedName,
    resolutionVariables,
    descriptorNumber,
  )
where

import Control.Monad (ap)
import Control.Monad.State.Strict (gets)
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Facts (ArrayShape (..), DenseLength (..))
import Language.Bash.Plan.Normalize.Literal
import Language.Bash.Plan.Normalize.State
import Language.Fish.DSL.Executable (commandName)
import Monk.Source.Location (SourcePos (..), SourceRange (..))
import Monk.Translation.Types
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralString)
import ShellCheck.Interface (ParseResult (..), Position (..))
import Prelude hiding (get, gets, identity, local, put)

-- | The IO graph driver supplies immutable documents only when authoritative
-- normalization reaches an executable source occurrence.
data SourceDocument = SourceDocument Text ParseResult Text

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

type Normalize scope = StateT (Normalization scope) NormalizationResult

scopedControl ::
  (forall result. Control.Control outer -> (forall scope. Control.BodyRoot kind scope -> Control.Control scope -> result) -> result) ->
  (forall scope. Control.BodyRoot kind scope -> Normalization outer -> Normalize scope (value, Normalization outer)) ->
  Normalize outer value
scopedControl enter action = StateT $ \before -> enter (nControl before) $ \root control -> do
  ((value, after), _) <- runStateT (action root before) (rebaseNormalization control before)
  pure (value, after)

scopedLoop ::
  (forall scope. Control.LoopTarget scope -> Normalization outer -> Normalize scope (value, Normalization outer)) ->
  Normalize outer value
scopedLoop action = StateT $ \before -> Control.withLoopControl (nControl before) $ \target control -> do
  ((value, after), _) <- runStateT (action target before) (enterNormalizationLoop target (rebaseNormalization control before))
  pure (value, after)

documentName :: ParseResult -> Text
documentName parsed = maybe "<input>" (toText . posFile . fst . snd) (M.lookupMin (prTokenPositions parsed))

diagnostic :: Maybe SourceRange -> Text -> Text -> Diagnostic
diagnostic range code message = MkDiagnostic (MkDiagnosticCode ("monk.semantic." <> code)) PhaseTranslate DiagnosticError Unsafe message range

reject :: Token -> Text -> Text -> Normalize scope a
reject token code message = do
  range <- tokenRange token
  lift (NormalizationFailed (diagnostic range code message :| []))

tokenRange :: Token -> Normalize scope (Maybe SourceRange)
tokenRange token = gets (fmap convert . M.lookup (getId token) . nPositions)
  where
    convert (start, end) = MkSourceRange (point start) (point end)
    point p = MkSourcePos (toText (posFile p)) (fromInteger (posLine p)) (fromInteger (posColumn p))

-- Runtime spelling belongs to the source occurrence; canonical parser positions
-- remain authoritative for discovery, cycles, diagnostics and function identity.
runtimeTokenRange :: Token -> Normalize scope (Maybe SourceRange)
runtimeTokenRange token = do
  origin <- gets nRuntimeOrigin
  fmap (\range -> range {rangeStart = (rangeStart range) {srcFile = origin}, rangeEnd = (rangeEnd range) {srcFile = origin}}) <$> tokenRange token

contractNames :: CallerContract -> S.Set Text
contractNames contract =
  M.keysSet (callerVariables contract)
    <> M.keysSet (callerFunctions contract)
    <> S.fromList (map functionTarget (M.elems (callerFunctions contract)))
    <> callerExportedFunctions contract

requireSession :: Token -> Normalize scope ()
requireSession token = do
  mode <- gets (entryMode . nConfig)
  unless (mode == Standalone) (reject token "session-context" "Owned jobs require standalone execution")

initializedImports :: CallerContract -> S.Set Text
initializedImports = M.keysSet . M.filter (\(ScalarBinding access _ _) -> access /= OutputBinding) . callerVariables

readBinding :: Token -> Text -> Normalize scope ()
readBinding token name = do
  cfg <- gets nConfig
  let directoryRead = maybe False (\permissions -> case name of "PWD" -> directoryPwd permissions `elem` [ReadDirectory, ReadWriteDirectory]; "OLDPWD" -> directoryOldpwd permissions `elem` [ReadDirectory, ReadWriteDirectory]; _ -> False) (callerDirectory (callerContract cfg))
  when (entryMode cfg == Sourceable && name /= "#" && not directoryRead) $ do
    locals <- gets nLocals
    initialized <- gets nVariables
    unless
      (S.member name locals || (M.member name (callerVariables (callerContract cfg)) && S.member name initialized))
      (reject token "undeclared-binding-read" "Sourceable reads require a declared initialized scalar binding")

storageFor :: Token -> Bool -> Text -> Normalize scope P.Storage
storageFor token local name = do
  when (name == "PWD") (reject token "directory-pwd-write" "Direct PWD mutation is outside the stable directory contract")
  cfg <- gets nConfig
  when (stableDirectoryEnabled cfg && local && name == "OLDPWD") (reject token "directory-oldpwd-scope" "Stable directory state requires a global OLDPWD")
  when (name == "OLDPWD") $ modify' (\flow -> updateNormalization id (\currentFacts -> currentFacts {factDirectoryFacts = (nDirectoryFacts flow) {Directory.directoryPreviousProved = False}}) id flow)
  when (stableDirectoryEnabled cfg && name `elem` ["CDPATH", "dirstack"]) (reject token "directory-state-write" "Direct directory contract state mutation is not represented")
  when
    (entryMode cfg == Sourceable && name == "IFS")
    (reject token "sourceable-ifs-effect" "Persistent caller IFS effects are not yet represented by the sourceable contract")
  active <- gets nFunction
  locals <- gets nLocals
  if local
    then pure P.Local
    else
      if entryMode cfg == Standalone
        then pure (if isNothing active then P.Global else P.Visible)
        else
          if name == "OLDPWD" && maybe False ((`elem` [WriteDirectory, ReadWriteDirectory]) . directoryOldpwd) (callerDirectory (callerContract cfg))
            then pure P.Global
            else
              if S.member name locals
                then pure P.Visible
                else case M.lookup name (callerVariables (callerContract cfg)) of
                  Just (ScalarBinding access scope exported)
                    | access /= InputBinding ->
                        pure (if isNothing active && scope == GlobalBinding then P.CallerGlobal exported else P.CallerVisible exported)
                  _ -> reject token "undeclared-binding-write" "Sourceable writes require a declared writable scalar binding"

rejectArrayScalar :: Token -> Text -> Normalize scope ()
rejectArrayScalar token name = do
  arrays <- gets nArrays
  when (M.member name arrays) (reject token "array-scalar-operation" "This scalar operation does not preserve indexed array storage")

arrayReadable :: Token -> Text -> Normalize scope ()
arrayReadable token name = do
  checkedName token name
  arrays <- gets nArrays
  case M.lookup name arrays of
    Just (DenseArray _) -> pure ()
    _ -> reject token "array-shape" "An indexed array read requires proved owned dense storage"

arrayStorage :: Token -> Bool -> Text -> Normalize scope P.Storage
arrayStorage token local name = do
  cfg <- gets nConfig
  unless (entryMode cfg == Standalone) (reject token "array-context" "Owned arrays currently require standalone execution")
  target <- gets (Control.loopTarget . nControl)
  unless (isNothing target) (reject token "array-loop-write" "Array mutations inside loops require a stable indexed storage proof")
  checkedName token name
  when (name `elem` resolutionVariables || name `elem` ["IFS", "PWD", "OLDPWD"]) (reject token "array-special-binding" "Special shell state cannot use ordinary indexed array storage")
  storageFor token local name

rememberArray :: Text -> DenseLength -> Normalize scope ()
rememberArray name size = modify' (\flow -> updateNormalization id (\currentFacts -> currentFacts {factArrays = M.insert name (DenseArray size) (nArrays flow), factVariables = S.insert name (nVariables flow), factNumeric = S.delete name (nNumeric flow), factConstants = M.delete name (nConstants flow)}) id flow)

literalName :: Token -> Token -> Normalize scope Text
literalName parent token = case getLiteralString token of
  Just value -> checkedName token (toText value) >> pure (toText value)
  Nothing -> reject parent "literal-name" "This operation requires a literal binding name"

checkedCommand :: Token -> Text -> Normalize scope ()
checkedCommand token name =
  unless
    (not (T.null name) && not (T.isPrefixOf "-" name) && T.all (\c -> isAlphaNum c || c `elem` ("_./:+-[" :: String)) name)
    (reject token "command-name" "Command identity needs a safely materializable literal name")

checkedFunctionCommand :: Token -> Text -> Normalize scope ()
checkedFunctionCommand token name = do
  checkedCommand token name
  either (reject token "function-name") (const (pure ())) (commandName name)

checkedName :: Token -> Text -> Normalize scope ()
checkedName token name = do
  stable <- gets (stableDirectoryEnabled . nConfig)
  unless (validName name) (reject token "name" "Binding names must be nonempty portable identifiers")
  when
    (not (stable && name == "PWD") && name `elem` ["fish_read_limit", "_", "status", "pipestatus", "argv", "fish_pid", "last_pid", "version", "SHLVL", "PWD", "PPID", "UID", "EUID", "RANDOM", "SRANDOM", "SECONDS", "LINENO", "SHELLOPTS", "BASHOPTS"])
    (reject token "reserved-binding" "This Bash name conflicts with target shell state")
  modify' (\s -> updateNormalization id id (\currentDiscoveries -> currentDiscoveries {discoveredReserved = S.insert name (nReserved s)}) s)

resolutionVariables :: [Text]
resolutionVariables = ["PATH", "CDPATH", "HOME", "OLDPWD"]

descriptorNumber :: Token -> Text -> Normalize scope Int
descriptorNumber token value = do
  descriptor <- maybe (reject token "redirect-descriptor" "Descriptor numbers must be decimal literals from zero through 255") pure (decimalIndex value)
  when (descriptor > 255) (reject token "redirect-descriptor" "Descriptor numbers above 255 are outside the owned initial envelope")
  when (descriptor > 2) (requireSession token)
  pure descriptor
