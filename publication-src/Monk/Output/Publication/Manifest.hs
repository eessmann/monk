-- | Private publication manifest boundary.
module Monk.Output.Publication.Manifest
  ( ValidatedMembers,
    validateMembers,
    membersList,
    expectedMemberDirectories,
    generationManifest,
    generationDigest,
    managedRootName,
    ownershipMarkerName,
    ownershipMarkerBytes,
    generationsDirectoryName,
    generationManifestName,
    ownerReadWriteMode,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Monk.Output.Publication.Types
import Monk.Runtime.Digest (sha256)
import System.FilePath qualified as FP
import System.Posix.Types (FileMode)

membersList :: ValidatedMembers -> [PublicationMember]
membersList (ValidatedMembers members _ _ _) = members

expectedMemberDirectories :: ValidatedMembers -> [FilePath]
expectedMemberDirectories (ValidatedMembers _ directories _ _) = directories

generationManifest :: ValidatedMembers -> BS.ByteString
generationManifest (ValidatedMembers _ _ manifest _) = manifest

generationDigest :: ValidatedMembers -> FilePath
generationDigest (ValidatedMembers _ _ _ digest) = digest

validateMembers :: [PublicationMember] -> Either PublicationFailure ValidatedMembers
validateMembers members
  | null members = invalid "managed publication has no members"
  | otherwise = do
      traverse_ (validateMemberPath . memberPath) members
      let sorted = L.sortOn memberPath members
          paths = map memberPath sorted
          pathSet = Set.fromList paths
          directories = Set.fromList (concatMap (parentDirectories . FP.takeDirectory) paths)
      when (Set.size pathSet /= length paths) (invalid "managed publication has duplicate member paths")
      unless (Set.disjoint pathSet directories) (invalid "managed publication has a file-directory member collision")
      let manifest = manifestBytes sorted
      pure (ValidatedMembers sorted (Set.toAscList directories) manifest (BS8.unpack (sha256 manifest)))

validateMemberPath :: FilePath -> Either PublicationFailure ()
validateMemberPath path
  | null path = invalid "member path is empty"
  | '\0' `elem` path = invalid "member path contains a NUL byte"
  | FP.isAbsolute path = invalid "member path must be relative"
  | FP.hasTrailingPathSeparator path = invalid "member path must name a file"
  | FP.normalise path /= path = invalid "member path is not normalized"
  | any (`elem` ["", ".", ".."]) (FP.splitDirectories path) = invalid "member path contains traversal"
  | take 1 (FP.splitDirectories path) == [generationManifestName] = invalid "member path uses the reserved manifest name"
  | otherwise = Right ()

parentDirectories :: FilePath -> [FilePath]
parentDirectories "." = []
parentDirectories directory = directory : parentDirectories (FP.takeDirectory directory)

manifestBytes :: [PublicationMember] -> BS.ByteString
manifestBytes members = BS.concat (generationManifestHeader : concatMap memberEntry members)
  where
    memberEntry member =
      [ frameBytes (Text.encodeUtf8 (toText (memberPath member))),
        frameBytes (BS8.pack (show (memberRole member))),
        frameBytes (BS8.pack (show (memberMode member))),
        frameBytes (encodeWord64 (fromIntegral (BS.length (memberBytes member)))),
        frameBytes (sha256 (memberBytes member))
      ]

frameBytes :: BS.ByteString -> BS.ByteString
frameBytes bytes = encodeWord64 (fromIntegral (BS.length bytes)) <> bytes

encodeWord64 :: Word64 -> BS.ByteString
encodeWord64 value =
  BS.pack
    [ byte 56,
      byte 48,
      byte 40,
      byte 32,
      byte 24,
      byte 16,
      byte 8,
      byte 0
    ]
  where
    byte shift = fromIntegral ((value `shiftR` shift) .&. 0xff) :: Word8

managedRootName :: FilePath -> FilePath
managedRootName destination = "." <> FP.takeFileName destination <> ".monk"

ownershipMarkerName :: FilePath
ownershipMarkerName = ".monk-owner"

ownershipMarkerBytes :: BS.ByteString
ownershipMarkerBytes = "monk-managed-output-v1\n"

generationsDirectoryName :: FilePath
generationsDirectoryName = "generations"

generationManifestName :: FilePath
generationManifestName = ".monk-manifest"

generationManifestHeader :: BS.ByteString
generationManifestHeader = "monk-generation-v2\n"

ownerReadWriteMode :: FileMode
ownerReadWriteMode = 0o600
