{-# LANGUAGE OverloadedStrings #-}

module RealWorld
  ( realWorldTests,
  )
where

import Data.Text.IO qualified as TIO
import FixtureSupport (loadFixtureArgs, loadFixturePrereqs, loadFixtureStdin)
import Path.IO qualified as PathIO
import ShellSupport
  ( RunResult (..),
    Shell (..),
    diffEnv,
    prepareEnv,
    runShell,
    runShellWith,
    shouldRunIntegration,
  )
import System.Directory (findExecutable)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

data RealWorldFixture = MkRealWorldFixture
  { rfName :: String,
    rfBashPath :: FilePath,
    rfFishPath :: FilePath,
    rfSkip :: Maybe Text
  }

realWorldFixtures :: [RealWorldFixture]
realWorldFixtures =
  [ MkRealWorldFixture
      "hello-world"
      "test/fixtures/realworld/hello-world.bash"
      "test/fixtures/realworld/hello-world.fish"
      Nothing,
    MkRealWorldFixture
      "echo-args"
      "test/fixtures/realworld/echo-args.bash"
      "test/fixtures/realworld/echo-args.fish"
      Nothing,
    MkRealWorldFixture
      "a2l"
      "test/fixtures/realworld/a2l.bash"
      "test/fixtures/realworld/a2l.fish"
      Nothing,
    MkRealWorldFixture
      "coat"
      "test/fixtures/realworld/coat.bash"
      "test/fixtures/realworld/coat.fish"
      Nothing,
    MkRealWorldFixture
      "taoc"
      "test/fixtures/realworld/taoc.bash"
      "test/fixtures/realworld/taoc.fish"
      Nothing,
    MkRealWorldFixture
      "neofetch"
      "test/fixtures/realworld/neofetch.bash"
      "test/fixtures/realworld/neofetch.fish"
      (Just "manual translation too large; bake-off only"),
    MkRealWorldFixture
      "pyramid-right"
      "test/fixtures/realworld/pyramid-right.bash"
      "test/fixtures/realworld/pyramid-right.fish"
      Nothing,
    MkRealWorldFixture
      "pyramid-left"
      "test/fixtures/realworld/pyramid-left.bash"
      "test/fixtures/realworld/pyramid-left.fish"
      Nothing,
    MkRealWorldFixture
      "version-compare"
      "test/fixtures/realworld/version-compare.bash"
      "test/fixtures/realworld/version-compare.fish"
      Nothing
  ]

realWorldTests :: TestTree
realWorldTests =
  testGroup "Real-world fixtures (manual fish baseline only)" (map realWorldTest realWorldFixtures)

realWorldTest :: RealWorldFixture -> TestTree
realWorldTest MkRealWorldFixture {rfName, rfBashPath, rfFishPath, rfSkip} = H.testCaseSteps rfName $ \step -> do
  case rfSkip of
    Just reason -> step ("skipped manual baseline: " <> toString reason)
    Nothing -> do
      runnable <- shouldRunIntegration
      case runnable of
        Left reason -> step ("skipped manual baseline: " <> reason)
        Right () -> do
          bashPath <- PathIO.resolveFile' rfBashPath
          prereqs <- loadFixturePrereqs bashPath
          prereqOk <- and <$> mapM (fmap isJust . findExecutable . toString) prereqs
          unless prereqOk (step ("skipped manual baseline: missing prerequisites from " <> show prereqs))
          when prereqOk $ do
            bashSrc <- TIO.readFile rfBashPath
            fishSrc <- TIO.readFile rfFishPath
            args <- loadFixtureArgs bashPath
            stdinInput <- loadFixtureStdin bashPath
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
