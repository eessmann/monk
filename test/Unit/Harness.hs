{-# LANGUAGE OverloadedStrings #-}

module Unit.Harness
  ( unitHarnessTests,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Monk.Internal.Fixture
  ( FixtureMetadata (..),
    loadFixtureMetadata,
  )
import Monk.Internal.Shell
  ( ShellRunMode (..),
    diffEnv,
    envAddedOrChanged,
    envRemoved,
    prepareEnv,
  )
import Path (Abs, File, Path, parseRelFile, (</>))
import Path.IO qualified as PathIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitHarnessTests :: TestTree
unitHarnessTests =
  testGroup
    "Harness Support"
    [ H.testCase "Fixture metadata loader reads args and mode sidecars" $ do
        fixturePath <- repoFile "test/fixtures/realworld/argparse-mini.bash"
        metadata <- loadFixtureMetadata fixturePath
        fmArgs metadata
          @?=
            [ "--name=monk",
              "--mode",
              "detail",
              "--color=always",
              "--tag",
              "alpha",
              "--tag",
              "beta",
              "--",
              "--literal",
              "-n",
              "tail"
            ]
        fmMode metadata @?= ShellRunExec
        fmRecursive metadata @?= False
        fmStdin metadata @?= "",
      H.testCase "Fixture metadata loader reads stdin sidecars" $ do
        fixturePath <- repoFile "test/fixtures/realworld/envfile-preview.bash"
        metadata <- loadFixtureMetadata fixturePath
        H.assertBool "expected stdin payload" ("background wait" `T.isInfixOf` fmStdin metadata)
        fmMode metadata @?= ShellRunSource
        fmArgs metadata @?= [],
      H.testCase "prepareEnv forces the C locale" $ do
        env0 <- prepareEnv
        let envMap = M.fromList env0
        M.lookup "LC_ALL" envMap @?= Just "C"
        M.lookup "LANG" envMap @?= Just "C",
      H.testCase "diffEnv ignores volatile shell variables" $ do
        let delta =
              diffEnv
                (M.fromList [("KEEP", "same"), ("CHANGE", "old"), ("OLDPWD", "/tmp"), ("_", "bash")])
                (M.fromList [("KEEP", "same"), ("CHANGE", "new"), ("ADD", "fresh"), ("OLDPWD", "/var"), ("_", "fish")])
        envAddedOrChanged delta @?= M.fromList [("ADD", "fresh"), ("CHANGE", "new")]
        envRemoved delta @?= Set.empty
    ]

repoFile :: FilePath -> IO (Path Abs File)
repoFile rel = do
  cwd <- PathIO.getCurrentDir
  relPath <- parseRelFile rel
  pure (cwd </> relPath)
