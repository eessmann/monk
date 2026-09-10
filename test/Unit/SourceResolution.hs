module Unit.SourceResolution (unitSourceResolutionTests) where

import Monk.Source
import Monk.Translation (Diagnostic (diagnosticCode), DiagnosticCode (..))
import Path (toFilePath)
import Path.IO qualified as PathIO
import System.Directory (canonicalizePath, createDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

unitSourceResolutionTests :: TestTree
unitSourceResolutionTests =
  testGroup
    "Source lookup contract"
    [ testCase "bare sources search PATH before cwd" $ fixture $ \cwd search -> do
        writeFileText (cwd </> "dep.sh") "printf cwd"
        writeFileText (search </> "dep.sh") "printf path"
        expected <- canonicalizePath (search </> "dep.sh")
        resolveSourcePathIn (MkSourceEnvironment cwd [search] True) "dep.sh" >>= (@?= Right expected),
      testCase "slash paths use cwd without PATH search" $ fixture $ \cwd search -> do
        writeFileText (cwd </> "dep.sh") "printf cwd"
        writeFileText (search </> "dep.sh") "printf path"
        expected <- canonicalizePath (cwd </> "dep.sh")
        resolveSourcePathIn (MkSourceEnvironment cwd [search] True) "./dep.sh" >>= (@?= Right expected),
      testCase "non-POSIX profile falls back to cwd" $ fixture $ \cwd search -> do
        writeFileText (cwd </> "dep.sh") "printf cwd"
        expected <- canonicalizePath (cwd </> "dep.sh")
        resolveSourcePathIn (MkSourceEnvironment cwd [search] True) "dep.sh" >>= (@?= Right expected),
      testCase "disabled sourcepath ignores PATH" $ fixture $ \cwd search -> do
        writeFileText (search </> "dep.sh") "printf path"
        result <- resolveSourcePathIn (MkSourceEnvironment cwd [search] False) "dep.sh"
        case result of
          Left diagnostic -> diagnosticCode diagnostic @?= MkDiagnosticCode "monk.source.not-found"
          Right path -> assertFailure ("unexpected source: " <> path),
      testCase "empty and relative environment reject" $ fixture $ \cwd _ -> do
        forM_ [(MkSourceEnvironment cwd [] True, ""), (MkSourceEnvironment "." [] True, "dep.sh")] $ \(environment, target) -> do
          result <- resolveSourcePathIn environment target
          case result of
            Left _ -> pure ()
            Right path -> assertFailure ("invalid lookup admitted: " <> path)
    ]

fixture :: (FilePath -> FilePath -> IO ()) -> IO ()
fixture action = PathIO.withSystemTempDir "monk-source-resolution" $ \directory -> do
  let cwd = toFilePath directory </> "cwd"
      search = toFilePath directory </> "search"
  createDirectory cwd
  createDirectory search
  action cwd search
