{-# LANGUAGE OverloadedStrings #-}

module RealWorld
  ( realWorldTests,
  )
where

import Data.Text.IO qualified as TIO
import FixtureSupport (loadFixtureArgs, loadFixtureStdin)
import ShellSupport
  ( RunResult (..),
    Shell (..),
    diffEnv,
    prepareEnv,
    runShell,
    runShellWith,
    shouldRunIntegration,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

data RealWorldFixture = RealWorldFixture
  { rfName :: String,
    rfBashPath :: FilePath,
    rfFishPath :: FilePath
  }

realWorldFixtures :: [RealWorldFixture]
realWorldFixtures =
  [ RealWorldFixture
      "hello-world"
      "test/fixtures/realworld/hello-world.bash"
      "test/fixtures/realworld/hello-world.fish",
    RealWorldFixture
      "pyramid-right"
      "test/fixtures/realworld/pyramid-right.bash"
      "test/fixtures/realworld/pyramid-right.fish",
    RealWorldFixture
      "pyramid-left"
      "test/fixtures/realworld/pyramid-left.bash"
      "test/fixtures/realworld/pyramid-left.fish",
    RealWorldFixture
      "version-compare"
      "test/fixtures/realworld/version-compare.bash"
      "test/fixtures/realworld/version-compare.fish"
  ]

realWorldTests :: TestTree
realWorldTests =
  testGroup "Real-world fixtures (manual fish)" (map realWorldTest realWorldFixtures)

realWorldTest :: RealWorldFixture -> TestTree
realWorldTest RealWorldFixture {rfName, rfBashPath, rfFishPath} = H.testCase rfName $ do
  runnable <- shouldRunIntegration
  case runnable of
    Left _reason -> pure ()
    Right () -> do
      bashSrc <- TIO.readFile rfBashPath
      fishSrc <- TIO.readFile rfFishPath
      args <- loadFixtureArgs rfBashPath
      stdinInput <- loadFixtureStdin rfBashPath
      baseEnv <- prepareEnv
      baseBash <- runShell ShellBash baseEnv ""
      baseFish <- runShell ShellFish baseEnv ""
      bashRes <- runShellWith ShellBash baseEnv bashSrc args stdinInput
      fishRes <- runShellWith ShellFish baseEnv fishSrc args stdinInput
      let bashDelta = diffEnv (rrEnv baseBash) (rrEnv bashRes)
          fishDelta = diffEnv (rrEnv baseFish) (rrEnv fishRes)
      rrExit bashRes @?= rrExit fishRes
      rrStdout bashRes @?= rrStdout fishRes
      rrStderr bashRes @?= rrStderr fishRes
      bashDelta @?= fishDelta
