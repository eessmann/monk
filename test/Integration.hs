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
  [ MkIntegrationFixture "pwd-cd" "test/fixtures/integration/pwd-cd.bash",
    MkIntegrationFixture "stdout-stderr-exit" "test/fixtures/integration/stdout-stderr-exit.bash",
    MkIntegrationFixture "cd-tmp" "test/fixtures/integration/cd-tmp.bash",
    MkIntegrationFixture "pushd-popd" "test/fixtures/integration/pushd-popd.bash",
    MkIntegrationFixture "errexit-basic" "test/fixtures/integration/errexit-basic.bash",
    MkIntegrationFixture "errexit-andor" "test/fixtures/integration/errexit-andor.bash",
    MkIntegrationFixture "errexit-conditionals" "test/fixtures/integration/errexit-conditionals.bash",
    MkIntegrationFixture "errexit-grouped-andor" "test/fixtures/integration/errexit-grouped-andor.bash",
    MkIntegrationFixture "pipefail-basic" "test/fixtures/integration/pipefail-basic.bash",
    MkIntegrationFixture "pipefail-negated" "test/fixtures/integration/pipefail-negated.bash",
    MkIntegrationFixture "pipefail-conditional-toggle" "test/fixtures/integration/pipefail-conditional-toggle.bash",
    MkIntegrationFixture "pipefail-toggle" "test/fixtures/integration/pipefail-toggle.bash",
    MkIntegrationFixture "subshell-status-variable" "test/fixtures/integration/subshell-status-variable.bash",
    MkIntegrationFixture "subshell-status-read-delimiter" "test/fixtures/integration/subshell-status-read-delimiter.bash",
    MkIntegrationFixture "subshell-status-pipefail" "test/fixtures/integration/subshell-status-pipefail.bash",
    MkIntegrationFixture "background-success-wait" "test/fixtures/integration/background-success-wait.bash",
    MkIntegrationFixture "background-fail-wait" "test/fixtures/integration/background-fail-wait.bash",
    MkIntegrationFixture "background-pipefail" "test/fixtures/integration/background-pipefail.bash",
    MkIntegrationFixture "background-jobs" "test/fixtures/integration/background-jobs.bash",
    MkIntegrationFixture "background-local-scope" "test/fixtures/integration/background-local-scope.bash",
    MkIntegrationFixture "read-flags" "test/fixtures/integration/read-flags.bash",
    MkIntegrationFixture "read-delimiter" "test/fixtures/integration/read-delimiter.bash",
    MkIntegrationFixture "read-delimiter-null-array" "test/fixtures/integration/read-delimiter-null-array.bash",
    MkIntegrationFixture "read-delimiter-null-vars" "test/fixtures/integration/read-delimiter-null-vars.bash",
    MkIntegrationFixture "read-delimiter-ifs" "test/fixtures/integration/read-delimiter-ifs.bash",
    MkIntegrationFixture "read-delimiter-flags" "test/fixtures/integration/read-delimiter-flags.bash",
    MkIntegrationFixture "here-string-basic" "test/fixtures/integration/here-string-basic.bash",
    MkIntegrationFixture "param-expansion-args" "test/fixtures/integration/param-expansion-args.bash",
    MkIntegrationFixture "param-expansion-redirection" "test/fixtures/integration/param-expansion-redirection.bash",
    MkIntegrationFixture "param-expansion-case" "test/fixtures/integration/param-expansion-case.bash",
    MkIntegrationFixture "procsub-input" "test/fixtures/integration/procsub-input.bash",
    MkIntegrationFixture "procsub-output" "test/fixtures/integration/procsub-output.bash",
    MkIntegrationFixture "procsub-output-pipeline" "test/fixtures/integration/procsub-output-pipeline.bash",
    MkIntegrationFixture "procsub-output-variable" "test/fixtures/integration/procsub-output-variable.bash",
    MkIntegrationFixture "procsub-output-status" "test/fixtures/integration/procsub-output-status.bash",
    MkIntegrationFixture "procsub-output-errexit" "test/fixtures/integration/procsub-output-errexit.bash",
    MkIntegrationFixture "procsub-output-compound" "test/fixtures/integration/procsub-output-compound.bash",
    MkIntegrationFixture "for-underscore" "test/fixtures/integration/for-underscore.bash",
    MkIntegrationFixture "source-recursive" "test/fixtures/integration/source-recursive.bash",
    MkIntegrationFixture "trap-exit" "test/fixtures/integration/trap-exit.bash",
    MkIntegrationFixture "trap-exit-expansion" "test/fixtures/integration/trap-exit-expansion.bash",
    MkIntegrationFixture "arith-short-circuit" "test/fixtures/integration/arith-short-circuit.bash",
    MkIntegrationFixture "time-prefix" "test/fixtures/integration/time-prefix.bash",
    MkIntegrationFixture "corpus/simple-echo" "test/fixtures/corpus/simple-echo.bash",
    MkIntegrationFixture "corpus/if-then" "test/fixtures/corpus/if-then.bash",
    MkIntegrationFixture "realworld/hello-world" "test/fixtures/realworld/hello-world.bash",
    MkIntegrationFixture "realworld/echo-args" "test/fixtures/realworld/echo-args.bash",
    MkIntegrationFixture "realworld/a2l" "test/fixtures/realworld/a2l.bash",
    MkIntegrationFixture "realworld/coat" "test/fixtures/realworld/coat.bash",
    MkIntegrationFixture "realworld/taoc" "test/fixtures/realworld/taoc.bash",
    MkIntegrationFixture "realworld/neofetch-mini" "test/fixtures/realworld/neofetch-mini.bash",
    MkIntegrationFixture "realworld/argparse-mini" "test/fixtures/realworld/argparse-mini.bash",
    MkIntegrationFixture "realworld/envfile-preview" "test/fixtures/realworld/envfile-preview.bash",
    MkIntegrationFixture "realworld/path-filter" "test/fixtures/realworld/path-filter.bash",
    MkIntegrationFixture "realworld/semver-normalize" "test/fixtures/realworld/semver-normalize.bash",
    MkIntegrationFixture "realworld/pyramid-right" "test/fixtures/realworld/pyramid-right.bash",
    MkIntegrationFixture "realworld/pyramid-left" "test/fixtures/realworld/pyramid-left.bash",
    MkIntegrationFixture "realworld/version-compare" "test/fixtures/realworld/version-compare.bash"
  ]

data IntegrationFixture = MkIntegrationFixture
  { ifName :: String,
    ifPath :: FilePath
  }

integrationTest :: IntegrationFixture -> TestTree
integrationTest MkIntegrationFixture {ifName, ifPath} = H.testCaseSteps ifName $ \step -> do
  runnable <- shouldRunIntegration
  case runnable of
    Left reason -> step ("skipped: " <> reason)
    Right () -> do
      fixturePath <- PathIO.resolveFile' ifPath
      platforms <- loadFixturePlatforms fixturePath
      missingPrereqs <- fixtureMissingPrereqs fixturePath
      case fixturePlatformSkipReason (toText SysInfo.os) platforms of
        Just reason -> step (toString reason)
        Nothing
          | not (null missingPrereqs) ->
              step ("skipped: missing prerequisites: " <> toString (T.intercalate ", " missingPrereqs))
          | otherwise -> do
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

fixtureMissingPrereqs :: Path Abs File -> IO [Text]
fixtureMissingPrereqs path = do
  prereqs <- loadFixturePrereqs path
  filterM (fmap isNothing . findExecutable . toString) prereqs

fixturePlatformSkipReason :: Text -> Maybe [Text] -> Maybe Text
fixturePlatformSkipReason currentPlatform mPlatforms =
  case mPlatforms of
    Nothing -> Nothing
    Just platforms
      | currentPlatform `elem` platforms -> Nothing
      | otherwise ->
          Just
            ( "skipped: platform "
                <> currentPlatform
                <> " not in fixture platforms: "
                <> T.intercalate ", " platforms
            )

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
