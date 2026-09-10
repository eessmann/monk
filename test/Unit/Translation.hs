{-# LANGUAGE OverloadedStrings #-}

module Unit.Translation
  ( unitTranslationTests,
  )
where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Monk.Translation
  ( Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticPhase (..),
    TranslationFailure (..),
    TranslationStatistics (..),
    parseBashScript,
    renderTranslation,
    strictConfig,
    translateBashScript,
    translateParseResult,
    translationStatistics,
  )
import ShellCheck.AST qualified as Bash
import ShellCheck.Interface (ParseResult (..))
import ShellSupport
  ( RunResult (..),
    Shell (..),
    prepareEnv,
    runShell,
    shouldRunIntegration,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitTranslationTests :: TestTree
unitTranslationTests =
  testGroup
    "Translation admission behavior"
    [ testGroup "exact behavior" (map exactCaseTest exactCases),
      statisticsTests,
      H.testCase "a hand-built unsupported pipeline stage cannot become silent success" rawAstRejects
    ]

statisticsTests :: TestTree
statisticsTests =
  testGroup
    "materialization statistics"
    [ H.testCase "user literal python is not a native call site" $ do
        result <- admitted "printf '%s\\n' 'python3 __monk_native'"
        let stats = translationStatistics result
        H.assertEqual "literal words are data" 0 (statisticsNativeCallSites stats)
        H.assertEqual "rendered UTF-8 bytes" (BS.length (encodeUtf8 (renderTranslation result))) (statisticsRenderedFishBytes stats),
      H.testCase "repeated arithmetic shares definitions but retains helper call sites" $ do
        let branch = "if test -n \"$1\"; then x=17; else x=31; fi; "
            operation = "printf '%s\\n' \"$((x + 23))\"; "
        single <- translationStatistics <$> admitted (branch <> operation)
        repeated <- translationStatistics <$> admitted (branch <> operation <> operation)
        H.assertEqual "helper definitions interned" (statisticsHelperDefinitions single) (statisticsHelperDefinitions repeated)
        H.assertBool "call sites preserved" (statisticsHelperCallSites repeated > statisticsHelperCallSites single)
        H.assertBool "native operation dispatcher reached" (statisticsNativeCallSites repeated > 0),
      H.testCase "embedded child counts come from its pre-rendering structure" $ do
        plain <- translationStatistics <$> admitted "(printf x)"
        native <- translationStatistics <$> admitted "(echo -e 'x\\ny')"
        H.assertBool "child native operation is counted" (statisticsNativeCallSites native > statisticsNativeCallSites plain),
      H.testCase "copied functions retain nested child native sites" $ do
        direct <- translationStatistics <$> admitted "f() { (echo -e 'x\\ny'); }; f"
        copied <- translationStatistics <$> admitted "f() { (echo -e 'x\\ny'); }; f; (f)"
        H.assertBool "copied nested child definition contributes sites" (statisticsNativeCallSites copied > statisticsNativeCallSites direct)
    ]
  where
    admitted source = translateBashScript strictConfig "statistics.bash" source >>= either (\failure -> H.assertFailure (show failure) >> fail "translation rejected") pure

data ExactCase = MkExactCase
  { exactName :: String,
    exactSource :: Text,
    exactStdout :: Text
  }

exactCases :: [ExactCase]
exactCases =
  [ MkExactCase
      "echo option behavior survives structural lowering"
      "echo -e 'hi\\nthere'; echo -n end"
      "hi\nthere\nend",
    MkExactCase
      "unset exposes the default parameter value"
      "value=before; unset value; printf '<%s>\\n' \"${value-default}\""
      "<default>\n",
    MkExactCase
      "set argv preserves argument boundaries"
      "set -- one \"two three\"; printf '<%s:%s:%s>\\n' \"$#\" \"$1\" \"$2\""
      "<2:one:two three>\n",
    MkExactCase
      "until negates the complete condition"
      "n=0; until test \"$n\" = 1; do printf 'loop\\n'; n=1; done; printf 'done\\n'"
      "loop\ndone\n",
    MkExactCase
      "double bracket equality selects the matching branch"
      "x=foo; if [[ \"$x\" == foo ]]; then printf 'yes\\n'; else printf 'no\\n'; fi"
      "yes\n",
    MkExactCase
      "case glob matching keeps Bash branch selection"
      "x=foobar; case \"$x\" in foo*) printf 'glob\\n' ;; *) printf 'miss\\n' ;; esac"
      "glob\n",
    MkExactCase
      "function locals do not overwrite the caller binding"
      "v=outer; f() { local v=inner; printf '<%s>\\n' \"$v\"; }; f; printf '<%s>\\n' \"$v\""
      "<inner>\n<outer>\n",
    MkExactCase
      "exported assignments are visible to child processes"
      "export FOO=bar; sh -c 'printf \"<%s>\\n\" \"$FOO\"'"
      "<bar>\n"
  ]

exactCaseTest :: ExactCase -> TestTree
exactCaseTest MkExactCase {exactName, exactSource, exactStdout} = H.testCaseSteps exactName $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> do
      result <- translateBashScript strictConfig "legacy-exact.bash" exactSource
      case result of
        Left failure -> H.assertFailure ("strict translation rejected an exact case: " <> show failure)
        Right translation -> do
          environment <- prepareEnv
          bash <- runShell ShellBash environment exactSource
          fish <- runShell ShellFish environment (renderTranslation translation)
          H.assertEqual "independent Bash stdout" exactStdout (rrStdout bash)
          assertEquivalent bash fish

assertEquivalent :: RunResult -> RunResult -> H.Assertion
assertEquivalent bash fish = do
  H.assertEqual "exit status" (rrExit bash) (rrExit fish)
  H.assertEqual "stdout" (rrStdout bash) (rrStdout fish)
  H.assertEqual "stderr" (rrStderr bash) (rrStderr fish)

rawAstRejects :: H.Assertion
rawAstRejects = do
  parsed <- translateInput "false | wc -c"
  let literal ident = Bash.T_Literal (Bash.Id ident)
      word ident literalId value = Bash.T_NormalWord (Bash.Id ident) [literal literalId value]
      command ident nameIdent nameLiteralId name arguments =
        Bash.T_SimpleCommand
          (Bash.Id ident)
          []
          (word nameIdent nameLiteralId name : arguments)
      body = command 100 101 102 "false" []
      unsupportedStage = Bash.T_BatsTest (Bash.Id 103) "unsupported" body
      wcStage = command 104 105 106 "wc" [word 107 108 "-c"]
      root =
        Bash.T_Annotation
          (Bash.Id 109)
          []
          ( Bash.T_Script
              (Bash.Id 110)
              (literal 111 "")
              [Bash.T_Pipeline (Bash.Id 112) [] [unsupportedStage, wcStage]]
          )
  case translateParseResult strictConfig parsed {prRoot = Just root} of
    Left failure -> do
      let firstDiagnostic = NonEmpty.head (failureDiagnostics failure)
      diagnosticCode firstDiagnostic H.@?= MkDiagnosticCode "monk.semantic.unsupported-syntax"
      diagnosticPhase firstDiagnostic H.@?= PhaseTranslate
    Right translation ->
      H.assertFailure
        ( "unsupported hand-built AST produced executable output:\n"
            <> T.unpack (renderTranslation translation)
        )

translateInput :: Text -> IO ParseResult
translateInput source = do
  result <- parseBashScript "legacy-raw-ast.bash" source
  case prRoot result of
    Nothing -> H.assertFailure "positive parser control failed" >> fail "unreachable"
    Just _ -> pure result
