{-# LANGUAGE OverloadedStrings #-}

module Unit.Harness
  ( unitHarnessTests,
  )
where

import Control.Exception qualified as Exception
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Monk.Internal.Fixture
  ( FixtureMetadata (..),
    loadFixtureMetadata,
  )
import Monk.Internal.Shell
  ( RunResult (..),
    Shell (..),
    ShellRunMode (..),
    ShellRunTimeout (..),
    diffEnv,
    envAddedOrChanged,
    envRemoved,
    prepareEnv,
    readCreateProcessWithTimeout,
    runShell,
    shouldRunIntegration,
  )
import Path (Abs, File, Path, parseRelFile, (</>))
import Path.IO qualified as PathIO
import System.Exit (ExitCode)
import System.Process (proc)
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
          @?= [ "--name=monk",
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
      H.testCase "Fixture metadata loader reads portable process substitution platforms" $ do
        fixturePath <- repoFile "test/fixtures/integration/procsub-output.bash"
        metadata <- loadFixtureMetadata fixturePath
        fmPlatforms metadata @?= Just ["linux", "darwin"]
        fmPrereqs metadata @?= []
        fmRecursive metadata @?= False,
      H.testCase "Fixture metadata loader reads prereq and recursive sidecars" $ do
        prereqFixture <- repoFile "test/fixtures/realworld/taoc.bash"
        prereqMetadata <- loadFixtureMetadata prereqFixture
        fmPrereqs prereqMetadata @?= ["tac"]
        recursiveFixture <- repoFile "test/fixtures/integration/source-recursive.bash"
        recursiveMetadata <- loadFixtureMetadata recursiveFixture
        fmRecursive recursiveMetadata @?= True,
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
        envRemoved delta @?= Set.empty,
      testGroup "environment snapshots preserve framed values" (map environmentSnapshotCase [ShellBash, ShellFish]),
      H.testCase "shell process runner times out hung commands" $ do
        result <-
          Exception.try
            (readCreateProcessWithTimeout 10000 (proc "bash" ["-c", "sleep 1"]) "") ::
            IO (Either Exception.SomeException (ExitCode, String, String))
        case result of
          Left ex
            | Just (MkShellRunTimeout timeoutMicros) <- Exception.fromException ex ->
                timeoutMicros @?= 10000
            | otherwise ->
                H.assertFailure ("expected ShellRunTimeout, got " <> Exception.displayException ex)
          Right _ ->
            H.assertFailure "expected hung shell command to time out"
    ]

environmentSnapshotCase :: Shell -> TestTree
environmentSnapshotCase shell = H.testCaseSteps (show shell) $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped environment snapshot comparison: " <> reason)
    Right () -> do
      base <- prepareEnv
      let multiline = "alpha\nMONK_INTEGRATION=forged\n__MONK_ENV_BEGIN__\nomega=x=y"
          values = [("MONK_MULTILINE", multiline), ("MONK_EMPTY", ""), ("MONK_EQUALS", "x=y=z"), ("MONK_INTEGRATION", "1")]
          environment = values <> filter (\(name, _) -> name `elem` ["PATH", "HOME", "TMPDIR", "LC_ALL", "LANG", "XDG_CONFIG_HOME"]) base
      result <- runShell shell environment "printf 'ordinary\\n__MONK_ENV_BEGIN__\\nlast'"
      rrStdout result @?= "ordinary\n__MONK_ENV_BEGIN__\nlast"
      rrStderr result @?= ""
      forM_ values $ \(name, value) ->
        M.lookup (toText name) (rrEnv result) @?= Just (toText value)

repoFile :: FilePath -> IO (Path Abs File)
repoFile rel = do
  cwd <- PathIO.getCurrentDir
  relPath <- parseRelFile rel
  pure (cwd </> relPath)
