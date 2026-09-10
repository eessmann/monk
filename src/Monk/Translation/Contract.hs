{-# LANGUAGE OverloadedStrings #-}

-- | Decode the deliberately small, versioned sourceable caller contract.
module Monk.Translation.Contract (parseCallerContract, validateCallerContract) where

import Data.Aeson (Object, Value, eitherDecodeStrict', withObject, (.!=), (.:), (.:?))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Monk.Translation.Types

parseCallerContract :: Text -> Either Text CallerContract
parseCallerContract input = do
  value <- first toText (eitherDecodeStrict' (encodeUtf8 input))
  parsed <- first toText (parseEither contract value)
  validateCallerContract parsed
  pure parsed

contract :: Value -> Parser CallerContract
contract = withObject "caller contract" $ \obj -> do
  version <- obj .: "version" :: Parser Int
  unless (version `elem` [1, 2]) (fail "unsupported caller contract version; expected 1 or 2")
  keysOnly (["version", "variables", "functions", "exportedFunctions", "ambientEffects"] <> ["directory" | version == 2]) obj
  vars <- obj .:? "variables" .!= mempty >>= namedEntries variable
  functionObjects <- obj .:? "functions" .!= mempty
  funcs <- namedEntries (function version) functionObjects
  directoryEffects <- if version == 2 then namedEntries functionDirectory functionObjects else pure mempty
  directories <- if version == 2 then obj .:? "directory" >>= traverse directory else pure Nothing
  exportedNames <- obj .:? "exportedFunctions" .!= []
  let exports = Set.fromList exportedNames
  ambient <- obj .:? "ambientEffects" .!= ("unknown" :: Text)
  effects <- case ambient of
    "unknown" -> pure UnknownAmbientEffects
    "none" -> pure NoRelevantAmbientEffects
    _ -> fail "ambientEffects must be unknown or none"
  pure (MkCallerContract vars funcs exports effects directories directoryEffects)

-- | Semantic validation shared by decoded and programmatic configurations.
-- Parsing owns only the JSON representation and version; every translation
-- entrypoint validates these obligations before seeding imported flow facts.
validateCallerContract :: CallerContract -> Either Text ()
validateCallerContract caller = do
  forM_ (M.toList (callerFunctionDirectories caller)) $ \(name, effects) -> do
    unless (M.member name funcs) (Left "directory effects require a declared function")
    when (effects /= noDirectoryPermissions && isNothing (callerDirectory caller)) (Left "function directory effects require a stable directory contract")
    forM_ (callerDirectory caller) $ \allowed ->
      unless (and (zipWith grants (directoryFields allowed) (directoryFields effects))) (Left "function directory effects exceed caller permissions")
  traverse_ validName (M.keys vars <> M.keys funcs <> Set.toList exports)
  forM_ (M.elems funcs) $ \f -> do
    validName (functionTarget f)
    when
      (functionTarget f `elem` ["and", "begin", "break", "builtin", "case", "command", "continue", "else", "end", "eval", "exec", "for", "function", "if", "not", "or", "return", "set", "source", "status", "string", "switch", "test", "while"])
      (Left "function target is reserved Fish syntax")
    when (Set.member (functionTarget f) exports) (Left "an imported target conflicts with an exported function name")
    traverse_ validName (functionReads f <> functionWrites f)
    forM_ (Set.toList (functionReads f)) $ \name ->
      unless
        (maybe False readable (M.lookup name vars))
        (Left ("function reads undeclared/unreadable scalar: " <> name))
    forM_ (Set.toList (functionWrites f)) $ \name ->
      unless
        (maybe False writable (M.lookup name vars))
        (Left ("function writes undeclared/unwritable scalar: " <> name))
  where
    directoryFields permissions = [directoryCwd permissions, directoryPwd permissions, directoryOldpwd permissions, directoryStack permissions]
    grants _ NoDirectoryAccess = True
    grants ReadWriteDirectory _ = True
    grants allowed requested = allowed == requested
    vars = callerVariables caller
    funcs = callerFunctions caller
    exports = callerExportedFunctions caller
    readable (ScalarBinding access _ _) = access /= OutputBinding
    writable (ScalarBinding access _ _) = access /= InputBinding

namedEntries :: (Value -> Parser a) -> Object -> Parser (M.Map Text a)
namedEntries parser obj =
  M.fromList
    <$> forM
      (KM.toList obj)
      ( \(key, value) -> do
          let name = K.toText key
          result <- parser value
          pure (name, result)
      )

variable :: Value -> Parser VariableContract
variable = withObject "scalar variable contract" $ \obj -> do
  keysOnly ["access", "scope", "exported"] obj
  access <-
    obj .: "access" >>= \case
      "read" -> pure InputBinding
      "write" -> pure OutputBinding
      "read-write" -> pure InputOutputBinding
      (_ :: Text) -> fail "variable access must be read, write or read-write"
  scope <-
    obj .: "scope" >>= \case
      "visible" -> pure VisibleBinding
      "global" -> pure GlobalBinding
      (_ :: Text) -> fail "variable scope must be visible or global"
  exported <- obj .:? "exported" .!= False
  pure (ScalarBinding access scope (if exported then ExportedBinding else UnexportedBinding))

function :: Int -> Value -> Parser FunctionContract
function version = withObject "function contract" $ \obj -> do
  keysOnly (["target", "reads", "writes"] <> ["directory" | version == 2]) obj
  target <- obj .: "target"
  readNames <- obj .:? "reads" .!= []
  writeNames <- obj .:? "writes" .!= []
  pure (MkFunctionContract target (Set.fromList readNames) (Set.fromList writeNames))

directory :: Value -> Parser DirectoryPermissions
directory = withObject "stable directory contract" $ \obj -> do
  keysOnly ["contract", "cwd", "PWD", "OLDPWD", "stack"] obj
  profile <- obj .: "contract" :: Parser Text
  unless (profile == "stable") (fail "directory contract must be stable")
  directoryPermissions obj

functionDirectory :: Value -> Parser DirectoryPermissions
functionDirectory = withObject "function directory effects" $ \obj ->
  obj .:? "directory" >>= maybe (pure noDirectoryPermissions) (withObject "directory effects" $ \effects -> keysOnly ["cwd", "PWD", "OLDPWD", "stack"] effects >> directoryPermissions effects)

directoryPermissions :: Object -> Parser DirectoryPermissions
directoryPermissions obj = MkDirectoryPermissions <$> permission "cwd" <*> permission "PWD" <*> permission "OLDPWD" <*> permission "stack"
  where
    permission key =
      obj .:? key .!= ("none" :: Text) >>= \case
        "none" -> pure NoDirectoryAccess
        "read" -> pure ReadDirectory
        "write" -> pure WriteDirectory
        "read-write" -> pure ReadWriteDirectory
        _ -> fail "directory access must be none, read, write or read-write"

keysOnly :: [Text] -> Object -> Parser ()
keysOnly allowed obj = forM_ (KM.keys obj) $ \key ->
  unless
    (K.toText key `elem` allowed)
    (fail ("unknown contract field: " <> toString (K.toText key)))

validName :: Text -> Either Text ()
validName name = unless allowed (Left ("unsupported or reserved binding name: " <> name))
  where
    letter c = isAsciiLower c || isAsciiUpper c || c == '_'
    allowed = case T.uncons name of
      Just (initial, rest) ->
        letter initial
          && T.all (\c -> letter c || isDigit c) rest
          && not ("__monk_" `T.isPrefixOf` name || "fish_" `T.isPrefixOf` name)
          && name `notElem` ["argv", "status", "pipestatus", "PWD", "SHLVL", "PATH", "CDPATH", "LC_ALL", "LANG", "IFS"]
      Nothing -> False
