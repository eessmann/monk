{-# LANGUAGE OverloadedStrings #-}

module Unit.PlannedEnvironment
  ( unitPlannedEnvironmentTests,
  )
where

import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Translation
  ( AmbientEffects (..),
    CallerContract (..),
    Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticPhase (..),
    EntryMode (..),
    TranslateConfig (..),
    TranslationFailure (..),
    TranslationResult,
    emptyCallerContract,
    renderTranslation,
    strictConfig,
    translateBashScript,
  )
import ShellSupport
  ( RunResult (..),
    Shell (..),
    prepareEnv,
    runShell,
    shouldRunIntegration,
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedEnvironmentTests :: TestTree
unitPlannedEnvironmentTests =
  testGroup
    "Planned runtime environment"
    [ testGroup
        "standalone binding guards"
        [ hostileBindingCase
            "a universal scalar read is rejected before user effects"
            "printf 'effect:%s\\n' \"$x\""
            "set --universal x value"
            "universal binding x",
          hostileBindingCase
            "a path scalar read is rejected before user effects"
            "printf 'effect:%s\\n' \"$x\""
            "set --global --path x /tmp"
            "path binding x",
          hostileBindingCase
            "a list-shaped write target is rejected before user effects"
            "x=changed; printf 'effect\\n'"
            "set --global x one two"
            "non-scalar binding x",
          hostileBindingCase
            "a private function collision is rejected before user effects"
            "printf 'effect\\n'"
            "function __monk_plan_0_collision; true; end"
            "private function namespace is occupied",
          hostileBindingCase
            "a private variable collision is rejected before user effects"
            "printf 'effect\\n'"
            "set --global __monk_plan_0_collision occupied"
            "private variable namespace is occupied",
          sourceablePrivateCollisionCase,
          ambientCaptureLimitCase,
          exportedScalarCase,
          unrelatedHostileBindingCase
        ],
      testGroup "reserved target bindings" (map reservedBindingCase ["argv", "PWD", "SHLVL", "_", "fish_read_limit"])
    ]

hostileBindingCase :: String -> Text -> Text -> Text -> TestTree
hostileBindingCase name source setup detail = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withIsolatedFish source $ \path environment -> do
      result <- runFish environment (setup <> "\nsource \"$argv[1]\"") path
      H.assertEqual "runtime contract status" (ExitFailure 125) (processExit result)
      H.assertEqual "user effect stdout" "" (processStdout result)
      H.assertBool
        ("missing stable runtime contract prefix; actual stderr: " <> show (processStderr result))
        ("monk: runtime contract failed:" `isInfixOf` processStderr result)
      H.assertBool
        ("missing runtime contract detail " <> show detail <> "; actual stderr: " <> show (processStderr result))
        (T.unpack detail `isInfixOf` processStderr result)

sourceablePrivateCollisionCase :: TestTree
sourceablePrivateCollisionCase = H.testCaseSteps "sourceable entry retains the private namespace guard" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withIsolatedFishConfig sourceableConfig "printf 'effect\\n'" $ \path environment -> do
      result <-
        runFish
          environment
          "set --global __monk_plan_0_collision occupied\nsource \"$argv[1]\"\nset --local code $status\nprintf 'caller:%s\\n' \"$code\"\nexit $code"
          path
      H.assertEqual "runtime contract status" (ExitFailure 125) (processExit result)
      H.assertEqual "guard returns control without user effects" "caller:125\n" (processStdout result)
      H.assertBool "missing stable caller contract prefix" ("monk: caller contract failed:" `isInfixOf` processStderr result)
      H.assertBool "missing private-variable detail" ("private variable namespace is occupied" `isInfixOf` processStderr result)

ambientCaptureLimitCase :: TestTree
ambientCaptureLimitCase = H.testCaseSteps "owned field capture ignores an ambient low Fish capture limit" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withIsolatedFish "x=abc; printf '%s\\n' $x" $ \path environment -> do
      result <- runFish environment "set --global fish_read_limit 1\nsource \"$argv[1]\"" path
      H.assertEqual "owned capture status/stdout/stderr" (ExitSuccess, "abc\n", "") result

exportedScalarCase :: TestTree
exportedScalarCase = H.testCaseSteps "an exported scalar input satisfies the standalone guard" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> do
      let source = "printf '<%s>\\n' \"$MONK_ENV_VALUE\""
      translation <- accepted strictConfig source
      environment <- withEnvironmentValues [("MONK_ENV_VALUE", "expected")] <$> prepareEnv
      bash <- runShell ShellBash environment source
      fish <- runShell ShellFish environment (renderTranslation translation)
      H.assertEqual "independent Bash stdout" "<expected>\n" (rrStdout bash)
      H.assertEqual "exit status" (rrExit bash) (rrExit fish)
      H.assertEqual "stdout" (rrStdout bash) (rrStdout fish)
      H.assertEqual "stderr" (rrStderr bash) (rrStderr fish)

unrelatedHostileBindingCase :: TestTree
unrelatedHostileBindingCase = H.testCaseSteps "unrelated list and path variables do not block standalone execution" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withIsolatedFish "printf 'effect\\n'" $ \path environment -> do
      result <- runFish environment "set --global --path unrelated /tmp /var\nsource \"$argv[1]\"" path
      H.assertEqual "supported script status" ExitSuccess (processExit result)
      H.assertEqual "user effect stdout" "effect\n" (processStdout result)
      H.assertEqual "stderr" "" (processStderr result)

reservedBindingCase :: Text -> TestTree
reservedBindingCase name = H.testCase ("write to " <> T.unpack name <> " is rejected") $ do
  result <- translateBashScript strictConfig "planned-environment.bash" (name <> "=changed\nprintf effect\n")
  case result of
    Left failure -> do
      let diagnostic = NonEmpty.head (failureDiagnostics failure)
      diagnosticCode diagnostic H.@?= MkDiagnosticCode "monk.semantic.reserved-binding"
      diagnosticPhase diagnostic H.@?= PhaseTranslate
      H.assertBool "reserved-binding rejection lost its source range" (isJust (diagnosticRange diagnostic))
    Right translation ->
      H.assertFailure
        ( "reserved target binding produced executable output:\n"
            <> T.unpack (renderTranslation translation)
        )

sourceableConfig :: TranslateConfig
sourceableConfig =
  strictConfig
    { entryMode = Sourceable,
      callerContract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects}
    }

withIsolatedFish :: Text -> (FilePath -> [(String, String)] -> IO a) -> IO a
withIsolatedFish = withIsolatedFishConfig strictConfig

withIsolatedFishConfig :: TranslateConfig -> Text -> (FilePath -> [(String, String)] -> IO a) -> IO a
withIsolatedFishConfig config source action =
  withSystemTempDirectory "monk-environment" $ \directory -> do
    let generatedPath = directory </> "generated.fish"
        temporaryHome = directory </> "home"
        temporaryConfig = directory </> "config"
    createDirectoryIfMissing True temporaryHome
    createDirectoryIfMissing True temporaryConfig
    translation <- accepted config source
    TIO.writeFile generatedPath (renderTranslation translation)
    environment <-
      withEnvironmentValues
        [ ("HOME", temporaryHome),
          ("XDG_CONFIG_HOME", temporaryConfig)
        ]
        <$> prepareEnv
    action generatedPath environment

accepted :: TranslateConfig -> Text -> IO TranslationResult
accepted config source = do
  result <- translateBashScript config "planned-environment.bash" source
  case result of
    Left failure -> H.assertFailure ("environment case rejected during translation: " <> show failure) >> fail "unreachable"
    Right translation -> pure translation

withEnvironmentValues :: [(String, String)] -> [(String, String)] -> [(String, String)]
withEnvironmentValues values environment = foldl' setValue environment values
  where
    setValue current (name, value) = (name, value) : filter ((/= name) . fst) current

runFish :: [(String, String)] -> Text -> FilePath -> IO (ExitCode, String, String)
runFish environment command path =
  readCreateProcessWithExitCode
    (proc "fish" ["-c", T.unpack command, path]) {env = Just environment}
    ""

processExit :: (ExitCode, String, String) -> ExitCode
processExit (code, _, _) = code

processStdout :: (ExitCode, String, String) -> String
processStdout (_, output, _) = output

processStderr :: (ExitCode, String, String) -> String
processStderr (_, _, errors) = errors
