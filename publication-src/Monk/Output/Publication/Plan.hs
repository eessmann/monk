{-# LANGUAGE LambdaCase #-}

-- | Private publication plan boundary.
module Monk.Output.Publication.Plan
  ( Destination,
    validateDestination,
    destinationPath,
    GenerationRelative,
    generationRelativePath,
    generationFor,
    PublicationPlan,
    ManagedPublicationPlan,
    SingleFilePublicationPlan,
    generationRelativeDirectory,
    generationRelativeDirectoryMembers,
    planManagedPublication,
    planManagedPublicationMembers,
    planSingleFilePublication,
    expectedEntryBytes,
    publicationPlanDestination,
  )
where

import Data.ByteString qualified as BS
import Monk.Output.Publication.Manifest
import Monk.Output.Publication.Types
import System.FilePath qualified as FP

destinationPath :: Destination -> FilePath
destinationPath (Destination path) = path

generationRelativePath :: GenerationRelative -> FilePath
generationRelativePath (GenerationRelative path) = path

generationFor :: Destination -> ValidatedMembers -> GenerationRelative
generationFor destination members = GenerationRelative (managedRootName (destinationPath destination) FP.</> generationsDirectoryName FP.</> generationDigest members)

generationRelativeDirectory :: FilePath -> [(FilePath, BS.ByteString)] -> Either PublicationFailure FilePath
generationRelativeDirectory destination = generationRelativeDirectoryMembers destination . map (\(path, bytes) -> PublicationMember path FishSource bytes)

generationRelativeDirectoryMembers :: FilePath -> [PublicationMember] -> Either PublicationFailure FilePath
generationRelativeDirectoryMembers destination members = do
  validatedDestination <- validateDestination destination
  sortedMembers <- validateMembers members
  pure (generationRelativePath (generationFor validatedDestination sortedMembers))

planManagedPublication :: FilePath -> [(FilePath, BS.ByteString)] -> BS.ByteString -> Either PublicationFailure PublicationPlan
planManagedPublication destination members = planManagedPublicationMembers destination [PublicationMember path FishSource bytes | (path, bytes) <- members]

planManagedPublicationMembers :: FilePath -> [PublicationMember] -> BS.ByteString -> Either PublicationFailure PublicationPlan
planManagedPublicationMembers destination members loaderBytes = do
  validatedDestination <- validateDestination destination
  validated <- validateMembers members
  pure (MkManagedPublicationPlan (MkManagedPlan validatedDestination validated loaderBytes (generationFor validatedDestination validated)))

planSingleFilePublication ::
  FilePath ->
  BS.ByteString ->
  Either PublicationFailure PublicationPlan
planSingleFilePublication destination contents = do
  validatedDestination <- validateDestination destination
  pure
    ( MkSingleFilePublicationPlan
        MkSingleFilePlan
          { singleDestination = validatedDestination,
            singleFileBytes = contents
          }
    )

validateDestination :: FilePath -> Either PublicationFailure Destination
validateDestination destination
  | null destination = invalid "destination is empty"
  | '\0' `elem` destination = invalid "destination contains a NUL byte"
  | FP.hasTrailingPathSeparator destination = invalid "destination must name a file"
  | FP.normalise destination /= destination = invalid "destination path is not normalized"
  | FP.takeFileName destination `elem` ["", ".", ".."] = invalid "destination must have an entry basename"
  | otherwise = Right (Destination destination)

expectedEntryBytes :: PublicationPlan -> BS.ByteString
expectedEntryBytes = \case
  MkManagedPublicationPlan MkManagedPlan {managedLoaderBytes} -> managedLoaderBytes
  MkSingleFilePublicationPlan MkSingleFilePlan {singleFileBytes} -> singleFileBytes

publicationPlanDestination :: PublicationPlan -> Destination
publicationPlanDestination = \case
  MkManagedPublicationPlan MkManagedPlan {managedDestination} -> managedDestination
  MkSingleFilePublicationPlan MkSingleFilePlan {singleDestination} -> singleDestination
