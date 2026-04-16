{-# LANGUAGE OverloadedStrings #-}

module Integration
  ( integrationTests,
  )
where

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import FixtureSupport
  ( loadFixtureArgs,
    loadFixtureMode,
    loadFixturePlatforms,
    loadFixturePrereqs,
    loadFixtureRecursive,
    loadFixtureStdin,
  )
import Monk.Diagnostics (renderParseComment, renderTranslateError)
import Monk.Source
  ( SourceGraph (..),
    SourceGraphFailure (..),
    translateSourceGraph,
  )
import Monk.Translation
  ( defaultConfig,
    inlineStatements,
    parseBashScript,
    renderFish,
    renderTranslation,
    translateParseResult,
  )
import Path (Abs, File, Path, toFilePath)
import Path.IO qualified as PathIO
import ShellSupport
  ( RunResult (..),
    Shell (..),
    diffEnv,
    prepareEnv,
    runShell,
    runShellWithMode,
    shouldRunIntegration,
  )
import System.Directory (canonicalizePath, findExecutable)
import System.Info qualified as SysInfo
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
    IntegrationFixture "errexit-basic" "test/fixtures/integration/errexit-basic.bash",
    IntegrationFixture "errexit-andor" "test/fixtures/integration/errexit-andor.bash",
    IntegrationFixture "errexit-conditionals" "test/fixtures/integration/errexit-conditionals.bash",
    IntegrationFixture "pipefail-basic" "test/fixtures/integration/pipefail-basic.bash",
    IntegrationFixture "pipefail-toggle" "test/fixtures/integration/pipefail-toggle.bash",
    IntegrationFixture "background-success-wait" "test/fixtures/integration/background-success-wait.bash",
    IntegrationFixture "background-fail-wait" "test/fixtures/integration/background-fail-wait.bash",
    IntegrationFixture "background-pipefail" "test/fixtures/integration/background-pipefail.bash",
    IntegrationFixture "background-jobs" "test/fixtures/integration/background-jobs.bash",
    IntegrationFixture "background-local-scope" "test/fixtures/integration/background-local-scope.bash",
    IntegrationFixture "read-flags" "test/fixtures/integration/read-flags.bash",
    IntegrationFixture "read-delimiter" "test/fixtures/integration/read-delimiter.bash",
    IntegrationFixture "read-delimiter-null-array" "test/fixtures/integration/read-delimiter-null-array.bash",
    IntegrationFixture "read-delimiter-null-vars" "test/fixtures/integration/read-delimiter-null-vars.bash",
    IntegrationFixture "read-delimiter-ifs" "test/fixtures/integration/read-delimiter-ifs.bash",
    IntegrationFixture "read-delimiter-flags" "test/fixtures/integration/read-delimiter-flags.bash",
    IntegrationFixture "here-string-basic" "test/fixtures/integration/here-string-basic.bash",
    IntegrationFixture "param-expansion-args" "test/fixtures/integration/param-expansion-args.bash",
    IntegrationFixture "param-expansion-redirection" "test/fixtures/integration/param-expansion-redirection.bash",
    IntegrationFixture "param-expansion-case" "test/fixtures/integration/param-expansion-case.bash",
    IntegrationFixture "procsub-input" "test/fixtures/integration/procsub-input.bash",
    IntegrationFixture "procsub-output" "test/fixtures/integration/procsub-output.bash",
    IntegrationFixture "procsub-output-pipeline" "test/fixtures/integration/procsub-output-pipeline.bash",
    IntegrationFixture "procsub-output-variable" "test/fixtures/integration/procsub-output-variable.bash",
    IntegrationFixture "source-recursive" "test/fixtures/integration/source-recursive.bash",
    IntegrationFixture "trap-exit" "test/fixtures/integration/trap-exit.bash",
    IntegrationFixture "trap-exit-expansion" "test/fixtures/integration/trap-exit-expansion.bash",
    IntegrationFixture "arith-short-circuit" "test/fixtures/integration/arith-short-circuit.bash",
    IntegrationFixture "time-prefix" "test/fixtures/integration/time-prefix.bash",
    IntegrationFixture "corpus/simple-echo" "test/fixtures/corpus/simple-echo.bash",
    IntegrationFixture "corpus/if-then" "test/fixtures/corpus/if-then.bash",
    IntegrationFixture "realworld/hello-world" "test/fixtures/realworld/hello-world.bash",
    IntegrationFixture "realworld/echo-args" "test/fixtures/realworld/echo-args.bash",
    IntegrationFixture "realworld/a2l" "test/fixtures/realworld/a2l.bash",
    IntegrationFixture "realworld/coat" "test/fixtures/realworld/coat.bash",
    IntegrationFixture "realworld/taoc" "test/fixtures/realworld/taoc.bash",
    IntegrationFixture "realworld/neofetch-mini" "test/fixtures/realworld/neofetch-mini.bash",
    IntegrationFixture "realworld/argparse-mini" "test/fixtures/realworld/argparse-mini.bash",
    IntegrationFixture "realworld/envfile-preview" "test/fixtures/realworld/envfile-preview.bash",
    IntegrationFixture "realworld/path-filter" "test/fixtures/realworld/path-filter.bash",
    IntegrationFixture "realworld/semver-normalize" "test/fixtures/realworld/semver-normalize.bash",
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
      fixturePath <- PathIO.resolveFile' ifPath
      supportedPlatform <- fixtureSupportedOnCurrentPlatform fixturePath
      prereqsMet <- fixturePrereqsAvailable fixturePath
      when (supportedPlatform && prereqsMet) $ do
        bashSrc <- TIO.readFile ifPath
        translation <- translateScriptText fixturePath bashSrc
        args <- loadFixtureArgs fixturePath
        runMode <- loadFixtureMode fixturePath
        stdinInput <- loadFixtureStdin fixturePath
        case translation of
          Left err -> H.assertFailure err
          Right fishSrc -> do
            baseEnv <- prepareEnv
            baseBash <- runShell ShellBash baseEnv ""
            baseFish <- runShell ShellFish baseEnv ""
            bashRes <- runShellWithMode runMode ShellBash baseEnv bashSrc args stdinInput
            fishRes <- runShellWithMode runMode ShellFish baseEnv fishSrc args stdinInput
            let bashDelta = diffEnv (rrEnv baseBash) (rrEnv bashRes)
                fishDelta = diffEnv (rrEnv baseFish) (rrEnv fishRes)
            rrExit bashRes @?= rrExit fishRes
            rrStdout bashRes @?= rrStdout fishRes
            rrStderr bashRes @?= rrStderr fishRes
            bashDelta @?= fishDelta

fixturePrereqsAvailable :: Path Abs File -> IO Bool
fixturePrereqsAvailable path = do
  prereqs <- loadFixturePrereqs path
  and <$> mapM (fmap isJust . findExecutable . toString) prereqs

fixtureSupportedOnCurrentPlatform :: Path Abs File -> IO Bool
fixtureSupportedOnCurrentPlatform path = do
  mPlatforms <- loadFixturePlatforms path
  pure $
    case mPlatforms of
      Nothing -> True
      Just platforms -> toText SysInfo.os `elem` platforms

translateScriptText :: Path Abs File -> Text -> IO (Either String Text)
translateScriptText path script = do
  recursive <- loadFixtureRecursive path
  if recursive
    then translateScriptTextRecursive path
    else do
      parseResult <- parseBashScript (toFilePath path) script
      case translateParseResult defaultConfig parseResult of
        Left err -> pure (Left ("translateParseResult failed: " <> show err))
        Right translation -> pure (Right (renderTranslation translation))

translateScriptTextRecursive :: Path Abs File -> IO (Either String Text)
translateScriptTextRecursive path = do
  rootPath <- canonicalizePath (toFilePath path)
  graphE <- translateSourceGraph defaultConfig True rootPath
  case graphE of
    Left err -> pure (Left (renderSourceGraphFailure err))
    Right graph -> do
      stmts <- inlineStatements (\_ -> pure ()) (sgTranslations graph) Set.empty rootPath
      pure (Right (renderFish stmts))

renderSourceGraphFailure :: SourceGraphFailure -> String
renderSourceGraphFailure = \case
  SourceGraphParseErrors _ errs ->
    toString (T.unlines (map renderParseComment errs))
  SourceGraphTranslateFailure _ err ->
    toString (renderTranslateError err)
