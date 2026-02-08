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
    rfFishPath :: FilePath,
    rfSkip :: Maybe Text
  }

realWorldFixtures :: [RealWorldFixture]
realWorldFixtures =
  [ RealWorldFixture
      "hello-world"
      "test/fixtures/realworld/hello-world.bash"
      "test/fixtures/realworld/hello-world.fish"
      Nothing,
    RealWorldFixture
      "echo-args"
      "test/fixtures/realworld/echo-args.bash"
      "test/fixtures/realworld/echo-args.fish"
      Nothing,
    RealWorldFixture
      "a2l"
      "test/fixtures/realworld/a2l.bash"
      "test/fixtures/realworld/a2l.fish"
      Nothing,
    RealWorldFixture
      "coat"
      "test/fixtures/realworld/coat.bash"
      "test/fixtures/realworld/coat.fish"
      Nothing,
    RealWorldFixture
      "taoc"
      "test/fixtures/realworld/taoc.bash"
      "test/fixtures/realworld/taoc.fish"
      Nothing,
    RealWorldFixture
      "neofetch"
      "test/fixtures/realworld/neofetch.bash"
      "test/fixtures/realworld/neofetch.fish"
      (Just "manual translation too large; bake-off only"),
    RealWorldFixture
      "pyramid-right"
      "test/fixtures/realworld/pyramid-right.bash"
      "test/fixtures/realworld/pyramid-right.fish"
      Nothing,
    RealWorldFixture
      "pyramid-left"
      "test/fixtures/realworld/pyramid-left.bash"
      "test/fixtures/realworld/pyramid-left.fish"
      Nothing,
    RealWorldFixture
      "version-compare"
      "test/fixtures/realworld/version-compare.bash"
      "test/fixtures/realworld/version-compare.fish"
      Nothing
  ]

realWorldTests :: TestTree
realWorldTests =
  testGroup "Real-world fixtures (manual fish)" (map realWorldTest realWorldFixtures)

realWorldTest :: RealWorldFixture -> TestTree
realWorldTest RealWorldFixture {rfName, rfBashPath, rfFishPath, rfSkip} = H.testCase rfName $ do
  case rfSkip of
    Just _reason -> pure ()
    Nothing -> do
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
