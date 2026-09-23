-- | Proved directory operations under the stable ancestry obligation.
module Language.Bash.Plan.Directory
  ( DirectoryOperation (..),
    proveDirectoryPath,
    directoryPermissionsFor,
    permitsDirectory,
    DirectoryLocation (..),
    DirectoryFacts (..),
    joinDirectoryFacts,
    successfulDirectory,
  )
where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Monk.Translation.Types

data DirectoryOperation
  = ChangeDirectory Text
  | ChangePreviousDirectory
  | PrintDirectory Bool
  | PushDirectory Text
  | PopDirectory
  deriving stock (Show, Eq)

-- Leading parent traversals are valid. Interior name/.. cancellation is not:
-- Fish would erase the name before cd whereas Bash must establish that it exists.
proveDirectoryPath :: Text -> Bool
proveDirectoryPath value =
  not (T.null value)
    && not (T.any (== '\0') value)
    && value /= "-"
    && B.length (encodeUtf8 value) <= 4095
    && all ((<= 255) . B.length . encodeUtf8) (T.splitOn "/" value)
    && parentsBeforeNames False (T.splitOn "/" value)
  where
    parentsBeforeNames _ [] = True
    parentsBeforeNames seen (part : rest)
      | part `elem` ["", "."] = parentsBeforeNames seen rest
      | part == ".." = not seen && parentsBeforeNames False rest
      | otherwise = parentsBeforeNames True rest

directoryPermissionsFor :: DirectoryOperation -> DirectoryPermissions
directoryPermissionsFor = \case
  PrintDirectory True -> MkDirectoryPermissions ReadDirectory NoDirectoryAccess NoDirectoryAccess NoDirectoryAccess
  PrintDirectory False -> MkDirectoryPermissions ReadDirectory ReadDirectory NoDirectoryAccess NoDirectoryAccess
  ChangeDirectory _ -> MkDirectoryPermissions ReadWriteDirectory ReadWriteDirectory WriteDirectory NoDirectoryAccess
  ChangePreviousDirectory -> MkDirectoryPermissions ReadWriteDirectory ReadWriteDirectory ReadWriteDirectory NoDirectoryAccess
  PushDirectory _ -> MkDirectoryPermissions ReadWriteDirectory ReadWriteDirectory WriteDirectory ReadWriteDirectory
  PopDirectory -> MkDirectoryPermissions ReadWriteDirectory ReadWriteDirectory WriteDirectory ReadWriteDirectory

permitsDirectory :: DirectoryPermissions -> DirectoryPermissions -> Bool
permitsDirectory allowed required = and (zipWith permits (fields allowed) (fields required))
  where
    fields permissions = [directoryCwd permissions, directoryPwd permissions, directoryOldpwd permissions, directoryStack permissions]
    permits _ NoDirectoryAccess = True
    permits ReadWriteDirectory _ = True
    permits permission requiredPermission = permission == requiredPermission

-- | InitialDirectory refers to the graph's explicitly captured execution cwd;
-- it never refers to the directory containing the source document.
data DirectoryLocation = InitialDirectory | RelativeDirectory Text | KnownDirectory Text | UnknownDirectory
  deriving stock (Show, Eq, Ord)

data DirectoryFacts = MkDirectoryFacts
  { directoryLocation :: DirectoryLocation,
    directoryPreviousProved :: Bool
  }
  deriving stock (Show, Eq, Ord)

joinDirectoryFacts :: DirectoryFacts -> DirectoryFacts -> DirectoryFacts
joinDirectoryFacts a b =
  MkDirectoryFacts
    (if directoryLocation a == directoryLocation b then directoryLocation a else UnknownDirectory)
    (directoryPreviousProved a && directoryPreviousProved b)

successfulDirectory :: DirectoryOperation -> DirectoryFacts -> DirectoryFacts
successfulDirectory operation before = case operation of
  PrintDirectory _ -> before
  ChangeDirectory path -> changed path
  PushDirectory path -> changed path
  ChangePreviousDirectory -> MkDirectoryFacts UnknownDirectory True
  PopDirectory -> MkDirectoryFacts UnknownDirectory True
  where
    changed path = MkDirectoryFacts (location path) True
    location path
      | T.isPrefixOf "/" path = KnownDirectory (logical path)
      | KnownDirectory current <- directoryLocation before = KnownDirectory (logical (current <> "/" <> path))
      | path `elem` [".", "./"] = directoryLocation before
      | RelativeDirectory current <- directoryLocation before = RelativeDirectory (current <> "/" <> path)
      | InitialDirectory <- directoryLocation before = RelativeDirectory path
      | otherwise = UnknownDirectory
    logical path = "/" <> T.intercalate "/" (reverse (foldl' component [] (T.splitOn "/" path)))
    component parts "" = parts
    component parts "." = parts
    component parts ".." = drop 1 parts
    component parts part = part : parts
