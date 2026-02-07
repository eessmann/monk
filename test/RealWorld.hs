{-# LANGUAGE OverloadedStrings #-}

module RealWorld
  ( realWorldTests,
  )
where

import Data.Text.IO qualified as TIO
import ShellSupport
  ( RunResult (..),
    Shell (..),
    diffEnv,
    prepareEnv,
    runShell,
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
      "test/fixtures/realworld/pyramid-left.fish"
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
      baseEnv <- prepareEnv
      baseBash <- runShell ShellBash baseEnv ""
      baseFish <- runShell ShellFish baseEnv ""
      bashRes <- runShell ShellBash baseEnv bashSrc
      fishRes <- runShell ShellFish baseEnv fishSrc
      let bashDelta = diffEnv (rrEnv baseBash) (rrEnv bashRes)
          fishDelta = diffEnv (rrEnv baseFish) (rrEnv fishRes)
      rrExit bashRes @?= rrExit fishRes
      rrStdout bashRes @?= rrStdout fishRes
      rrStderr bashRes @?= rrStderr fishRes
      bashDelta @?= fishDelta
