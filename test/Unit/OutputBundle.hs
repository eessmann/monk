{-# LANGUAGE OverloadedStrings #-}

module Unit.OutputBundle
  ( unitOutputBundleTests,
  )
where

import Control.Exception (bracket)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Output
  ( GeneratedFile (..),
    OutputBundle (..),
    OutputTarget (..),
    planCombinedOutputBundle,
    planSeparateOutputBundle,
    renderOutputBundle,
  )
import Monk.Source (SourceGraph (..), Translation (..))
import Monk.Translation
  ( Diagnostic (diagnosticCode),
    DiagnosticCode (MkDiagnosticCode),
    TranslationResult (..),
    defaultConfig,
    translateBashScript,
  )
import Path (Abs, Dir, Path, toFilePath)
import Path.IO qualified as PathIO
import ShellSupport
  ( Shell (ShellFish),
    ShellRunMode (ShellRunSource),
    prepareEnv,
    rrExit,
    rrStderr,
    rrStdout,
    runShellWithMode,
  )
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath qualified as FP
import System.IO qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitOutputBundleTests :: TestTree
unitOutputBundleTests =
  testGroup
    "Output bundle"
    [ H.testCase "separate recursive output shares and deduplicates generated runtime" $ do
        rootResult <- successfulTranslation "read -d : left right\nsource child.sh"
        childResult <- successfulTranslation "read -d : child rest"
        let rootPath = "/project/root.sh"
            childPath = "/project/lib/child.sh"
            rootTranslation = artifact rootPath rootResult (M.singleton "child.sh" (Just childPath))
            childTranslation = artifact childPath childResult mempty
            graph = MkSourceGraph [rootPath, childPath] (M.fromList [(rootPath, rootTranslation), (childPath, childTranslation)])
        case planSeparateOutputBundle "/bundle/root.fish" rootPath graph of
          Left diagnostic -> H.assertFailure (show diagnostic)
          Right bundle -> do
            map generatedTarget (toList (bundleUserFiles bundle))
              @?= [OutputPath "/bundle/root.fish", OutputPath "/bundle/lib/child.fish"]
            case bundleRuntimeFile bundle of
              Nothing -> H.assertFailure "expected one shared runtime file"
              Just runtimeFile -> do
                generatedTarget runtimeFile @?= OutputPath "/bundle/_monk_runtime.fish"
                let rendered = renderOutputBundle bundle
                    runtimeText = lookupTarget (OutputPath "/bundle/_monk_runtime.fish") rendered
                    rootText = lookupTarget (OutputPath "/bundle/root.fish") rendered
                    childText = lookupTarget (OutputPath "/bundle/lib/child.fish") rendered
                T.count "function __monk_read_capture_delim" runtimeText @?= 1
                H.assertBool "runtime leaked into root" (not ("function __monk_read_capture_delim" `T.isInfixOf` rootText))
                H.assertBool "runtime leaked into child" (not ("function __monk_read_capture_delim" `T.isInfixOf` childText))
                H.assertBool
                  "root runtime source is not file-relative"
                  ("(status current-filename))'/_monk_runtime.fish'" `T.isInfixOf` rootText)
                H.assertBool
                  ("child runtime source is not file-relative:\n" <> T.unpack childText)
                  ("(status current-filename))'/../_monk_runtime.fish'" `T.isInfixOf` childText)
                H.assertBool
                  "translated child source is not file-relative"
                  ("(status current-filename))'/lib/child.fish'" `T.isInfixOf` rootText),
      H.testCase "combined recursive output deduplicates helpers across sourced files" $ do
        rootResult <- successfulTranslation "read -d : left right\nsource child.sh"
        childResult <- successfulTranslation "read -d : child rest"
        let rootPath = "/project/root.sh"
            childPath = "/project/child.sh"
            rootTranslation = artifact rootPath rootResult (M.singleton "child.sh" (Just childPath))
            childTranslation = artifact childPath childResult mempty
            graph = MkSourceGraph [rootPath, childPath] (M.fromList [(rootPath, rootTranslation), (childPath, childTranslation)])
        result <- planCombinedOutputBundle OutputStdout rootPath graph
        case result of
          Left diagnostic -> H.assertFailure (show diagnostic)
          Right bundle -> do
            let combinedText = lookupTarget OutputStdout (renderOutputBundle bundle)
            T.count "function __monk_read_capture_delim" combinedText @?= 1,
      H.testCase "separate output rejects a root path reserved for the shared runtime" $ do
        rootResult <- successfulTranslation "read -d : left right"
        let rootPath = "/project/root.sh"
            graph = MkSourceGraph [rootPath] (M.singleton rootPath (artifact rootPath rootResult mempty))
        case planSeparateOutputBundle "/bundle/_monk_runtime.fish" rootPath graph of
          Left diagnostic -> diagnosticCode diagnostic @?= MkDiagnosticCode "monk.output.duplicate-target"
          Right _ -> H.assertFailure "expected reserved runtime path collision",
      H.testCase "separate output rejects a child path that collides with the shared runtime" $ do
        rootResult <- successfulTranslation "read -d : left right\nsource _monk_runtime.sh"
        childResult <- successfulTranslation "echo child"
        let rootPath = "/project/root.sh"
            childPath = "/project/_monk_runtime.sh"
            rootTranslation = artifact rootPath rootResult (M.singleton "_monk_runtime.sh" (Just childPath))
            childTranslation = artifact childPath childResult mempty
            graph = MkSourceGraph [rootPath, childPath] (M.fromList [(rootPath, rootTranslation), (childPath, childTranslation)])
        case planSeparateOutputBundle "/bundle/root.fish" rootPath graph of
          Left diagnostic -> diagnosticCode diagnostic @?= MkDiagnosticCode "monk.output.duplicate-target"
          Right _ -> H.assertFailure "expected child/runtime path collision",
      H.testCase "shared helper bundles run from an unrelated working directory" $ do
        rootResult <- successfulTranslation "read -d : root rest\nprintf 'root:%s:%s\\n' \"$root\" \"$rest\"\nsource lib/child.sh"
        childResult <- successfulTranslation "read -d : child rest\nprintf 'child:%s:%s\\n' \"$child\" \"$rest\""
        let rootSource = "/project/root.sh"
            childSource = "/project/lib/child.sh"
            rootTranslation = artifact rootSource rootResult (M.singleton "lib/child.sh" (Just childSource))
            childTranslation = artifact childSource childResult mempty
            graph = MkSourceGraph [rootSource, childSource] (M.fromList [(rootSource, rootTranslation), (childSource, childTranslation)])
        withTempDir "monk-output-bundle" $ \tmpDir -> do
          let rootOutput = FP.combine (toFilePath tmpDir) "out/root.fish"
          case planSeparateOutputBundle rootOutput rootSource graph of
            Left diagnostic -> H.assertFailure (show diagnostic)
            Right bundle -> do
              writeRenderedBundle (renderOutputBundle bundle)
              env <- prepareEnv
              result <-
                runShellWithMode
                  ShellRunSource
                  ShellFish
                  env
                  ("cd /\nsource '" <> toText rootOutput <> "'")
                  []
                  "one two:three four:"
              H.assertBool ("bundle failed:\n" <> T.unpack (rrStderr result)) (rrExit result == ExitSuccess)
              rrStdout result @?= "root:one:two\nchild:three:four\n"
              rrStderr result @?= ""
    ]

successfulTranslation :: Text -> IO TranslationResult
successfulTranslation script = do
  result <- translateBashScript defaultConfig "spec.sh" script
  case result of
    Left failure -> H.assertFailure (show failure) >> error "unreachable"
    Right translation -> pure translation

artifact :: FilePath -> TranslationResult -> M.Map Text (Maybe FilePath) -> Translation
artifact path result sourceMap =
  MkTranslation
    { trPath = path,
      trScript = translationScript result,
      trDiagnostics = translationDiagnostics result,
      trRuntimeRequirements = translationRuntimeRequirements result,
      trSourceMap = sourceMap
    }

lookupTarget :: OutputTarget -> [(OutputTarget, Text)] -> Text
lookupTarget target rendered =
  fromMaybe "" (L.lookup target rendered)

writeRenderedBundle :: [(OutputTarget, Text)] -> IO ()
writeRenderedBundle = mapM_ $ \case
  (OutputPath path, contents) -> do
    createDirectoryIfMissing True (FP.takeDirectory path)
    TIO.writeFile path contents
  (OutputStdout, _) -> H.assertFailure "separate output unexpectedly targeted stdout"

withTempDir :: String -> (Path Abs Dir -> IO a) -> IO a
withTempDir prefix action = do
  tmpDir <- PathIO.getTempDir
  let create = do
        (path, handle) <- IO.openTempFile (toFilePath tmpDir) prefix
        IO.hClose handle
        removeFile path
        createDirectory path
        pure path
  bracket create cleanup (PathIO.resolveDir' >=> action)
  where
    cleanup path = do
      exists <- doesDirectoryExist path
      when exists (removeDirectoryRecursive path)
