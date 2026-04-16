{-# LANGUAGE OverloadedStrings #-}

module Unit.Source
  ( unitSourceTests,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Monk.Source
  ( SourceGraph,
    resolveSourcePath,
    rewriteSources,
    sgOrder,
    sgTranslations,
    translateSourceGraph,
  )
import Monk.Translation (defaultConfig, renderFish)
import Path (Abs, File, Path, parseRelFile, toFilePath, (</>))
import Path.IO qualified as PathIO
import System.FilePath (takeDirectory)
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
            let rendered = renderFish (rewriteSources (sgTranslations graph) rootTranslation)
            T.isInfixOf "source 'test/fixtures/integration/source-recursive-child.fish'" rendered
              H.@? "expected rewritten .fish source path",
      H.testCase "resolveSourcePath keeps missing files unresolved" $ do
        rootPath <- repoFile "test/fixtures/integration/source-recursive.bash"
        resolved <- resolveSourcePath (takeDirectory (toFilePath rootPath)) "does-not-exist.bash"
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

unreachable :: IO a
unreachable = error "unreachable"
