{-# LANGUAGE OverloadedStrings #-}

module Integration
  ( integrationTests,
  )
where

import Data.Text.IO qualified as TIO
import Monk
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

integrationTests :: TestTree
integrationTests =
  testGroup "Integration (bash vs fish)" (map integrationTest integrationFixtures)

integrationFixtures :: [IntegrationFixture]
integrationFixtures =
  [ IntegrationFixture "pwd-cd" "test/fixtures/integration/pwd-cd.bash",
    IntegrationFixture "stdout-stderr-exit" "test/fixtures/integration/stdout-stderr-exit.bash",
    IntegrationFixture "cd-tmp" "test/fixtures/integration/cd-tmp.bash",
    IntegrationFixture "pushd-popd" "test/fixtures/integration/pushd-popd.bash",
    IntegrationFixture "time-prefix" "test/fixtures/integration/time-prefix.bash",
    IntegrationFixture "corpus/simple-echo" "test/fixtures/corpus/simple-echo.bash",
    IntegrationFixture "corpus/if-then" "test/fixtures/corpus/if-then.bash",
    IntegrationFixture "realworld/hello-world" "test/fixtures/realworld/hello-world.bash",
    IntegrationFixture "realworld/pyramid-right" "test/fixtures/realworld/pyramid-right.bash",
    IntegrationFixture "realworld/pyramid-left" "test/fixtures/realworld/pyramid-left.bash",
    IntegrationFixture "realworld/version-compare" "test/fixtures/realworld/version-compare.bash"
  ]

data IntegrationFixture = IntegrationFixture
  { ifName :: String,
    ifPath :: FilePath
  }

integrationTest :: IntegrationFixture -> TestTree
integrationTest IntegrationFixture {ifName, ifPath} = H.testCase ifName $ do
  runnable <- shouldRunIntegration
  case runnable of
    Left _reason -> pure ()
    Right () -> do
      bashSrc <- TIO.readFile ifPath
      translation <- translateScriptText ifPath bashSrc
      case translation of
        Left err -> H.assertFailure err
        Right fishSrc -> do
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

translateScriptText :: FilePath -> Text -> IO (Either String Text)
translateScriptText path script = do
  parseResult <- parseBashScript path script
  case translateParseResult defaultConfig parseResult of
    Left err -> pure (Left ("translateParseResult failed: " <> show err))
    Right (stmt, _) -> pure (Right (renderFish [stmt]))
