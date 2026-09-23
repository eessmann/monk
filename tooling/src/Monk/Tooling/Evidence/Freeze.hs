-- | Shake dependency graph for immutable evidence freezes.
module Monk.Tooling.Evidence.Freeze (freezeWithShake) where

import Development.Shake (ShakeOptions (..), Verbosity (Silent), getDirectoryFiles, need, shake, shakeOptions, want, (%>))
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

-- | Every freeze has one output manifest and a fresh destination. Shake records
-- the complete admitted input inventory before the IO action writes it. A
-- second invocation refuses the existing target, including an unchanged one.
-- Fresh measurements intentionally do not use this rule.
freezeWithShake :: FilePath -> [(FilePath, [String])] -> [FilePath] -> IO () -> IO ()
freezeWithShake manifest inventories fixedInputs freezeAction = do
  exists <- doesFileExist manifest
  when exists $ fail ("immutable evidence manifest already exists: " <> manifest)
  let output = takeDirectory manifest
      cache = takeDirectory output </> ("." <> takeFileName output <> ".evidence-shake")
      options = shakeOptions {shakeFiles = cache, shakeVerbosity = Silent}
  shake options $ do
    want [manifest]
    manifest %> \_ -> do
      discovered <- fmap concat $ forM inventories $ \(root, patterns) -> do
        paths <- getDirectoryFiles root patterns
        pure (map (root </>) paths)
      need (fixedInputs <> discovered)
      liftIO freezeAction
