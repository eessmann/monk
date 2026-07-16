{-# LANGUAGE OverloadedStrings #-}

module Unit.Source
  ( unitSourceTests,
  )
where

import Control.Exception (bracket)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.AST (renderScript)
import Monk.Source
  ( SourceGraph,
    Translation (..),
    resolveSourcePath,
    rewriteSources,
    sgOrder,
    sgTranslations,
    translateSourceGraph,
  )
import Monk.Translation (defaultConfig)
import Path (Abs, Dir, File, Path, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO qualified as PathIO
import ShellSupport
  ( Shell (..),
    ShellRunMode (..),
    prepareEnv,
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
import System.FilePath qualified as FP
import System.IO qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitSourceTests :: TestTree
unitSourceTests =
  testGroup
    "Source Graph"
    [ H.testCase "Recursive source graph discovers sourced child" $ do
        rootPath <- repoFile "test/fixtures/integration/source-recursive.bash"
        childPath <- repoFile "test/fixtures/integration/source-recursive-child.bash"
        graph <- loadRecursiveGraph rootPath
        sgOrder graph @?= [toFilePath rootPath, toFilePath childPath]
        H.assertBool "expected child translation in graph" (M.member (toFilePath childPath) (sgTranslations graph)),
      H.testCase "rewriteSources rewrites literal source targets to fish files" $ do
        rootPath <- repoFile "test/fixtures/integration/source-recursive.bash"
        graph <- loadRecursiveGraph rootPath
        case M.lookup (toFilePath rootPath) (sgTranslations graph) of
          Nothing -> H.assertFailure "missing root translation"
          Just rootTranslation -> do
            let rendered = renderScript (rewriteSources (sgTranslations graph) rootTranslation)
            T.isInfixOf "source 'test/fixtures/integration/source-recursive-child.fish'" rendered
              H.@? "expected rewritten .fish source path",
      H.testCase "relocated recursive sources stay runnable from the output bundle" $ do
        rootPath <- repoFile "test/fixtures/integration/source-recursive.bash"
        childPath <- repoFile "test/fixtures/integration/source-recursive-child.bash"
        graph <- loadRecursiveGraph rootPath
        withTempDir "monk-source-bundle" $ \tmpDir -> do
          childDir <- parseRelDir "children/"
          let bundleRootPath = FP.combine (toFilePath tmpDir) "monk-root-out.fish"
              bundleChildDir = tmpDir </> childDir
              bundleChildPath = FP.combine (toFilePath bundleChildDir) "source-recursive-child.fish"
              relocated =
                M.adjust (\tr -> tr {trPath = bundleRootPath}) (toFilePath rootPath)
                  . M.adjust (\tr -> tr {trPath = bundleChildPath}) (toFilePath childPath)
                  $ sgTranslations graph
          case (M.lookup (toFilePath rootPath) relocated, M.lookup (toFilePath childPath) relocated) of
            (Just rootTranslation, Just childTranslation) -> do
              createDirectoryIfMissing True (toFilePath bundleChildDir)
              let rootRendered = renderScript (rewriteSources relocated rootTranslation)
                  childRendered = renderScript (rewriteSources relocated childTranslation)
              T.isInfixOf "source 'children/source-recursive-child.fish'" rootRendered
                H.@? "expected bundled root to source bundled child relatively"
              TIO.writeFile bundleRootPath rootRendered
              TIO.writeFile bundleChildPath childRendered
              baseEnv <- prepareEnv
              fishRes <-
                runShellWithMode
                  ShellRunSource
                  ShellFish
                  baseEnv
                  ( T.unlines
                      [ "cd '" <> toText (toFilePath tmpDir) <> "'",
                        "source '" <> toText bundleRootPath <> "'"
                      ]
                  )
                  []
                  ""
              rrStdout fishRes
                @?= "argv:left|right\nstatus:7\nargv:left|right\nexpected-failure\nafter:child\n"
            _ -> H.assertFailure "missing relocated translations",
      H.testCase "resolveSourcePath keeps missing files unresolved" $ do
        rootPath <- repoFile "test/fixtures/integration/source-recursive.bash"
        resolved <- resolveSourcePath (FP.takeDirectory (toFilePath rootPath)) "does-not-exist.bash"
        resolved @?= Nothing
    ]

repoFile :: FilePath -> IO (Path Abs File)
repoFile rel = do
  cwd <- PathIO.getCurrentDir
  relPath <- parseRelFile rel
  pure (cwd </> relPath)

loadRecursiveGraph :: Path Abs File -> IO SourceGraph
loadRecursiveGraph rootPath = do
  graphE <- translateSourceGraph defaultConfig True (toFilePath rootPath)
  case graphE of
    Left err -> H.assertFailure ("unexpected source graph failure: " <> show err) >> unreachable
    Right graph -> pure graph

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

unreachable :: IO a
unreachable = error "unreachable"
