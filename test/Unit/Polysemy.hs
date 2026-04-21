{-# LANGUAGE OverloadedStrings #-}

module Unit.Polysemy
  ( unitPolysemyTests,
  )
where

import Monk.Translation
  ( TranslateError (..),
    TranslateState,
    Warning (..),
    WarningCode (..),
    WarningSeverity (..),
    defaultConfig,
    parseBashScript,
    renderTranslation,
    stateErrexitEnabled,
    statePipefailEnabled,
    stateWarnings,
    strictConfig,
    translationState,
    translateParseResult,
    warnMessage,
  )
import Data.Text qualified as T
import Monk.AST (SourcePos (..), SourceRange (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitPolysemyTests :: TestTree
unitPolysemyTests =
  testGroup
    "Polysemy effects"
    [ H.testCase "Unsupported warning is ranged and not duplicated" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation -> do
            let st = translationState translation
                warns = stateWarnings st
            length warns @?= 1
            case warns of
              [warning] -> do
                warnCode warning @?= UnsupportedConstruct
                warnSeverity warning @?= WarnHigh
                warnMessage warning @?= "Coprocess (coproc)"
                case warnRange warning of
                  Nothing -> H.assertFailure "expected warning range"
                  Just _ -> pure ()
              _ -> H.assertFailure "expected single warning",
      H.testCase "Strict mode raises error for unsupported" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi"
        case translateParseResult strictConfig result of
          Left (Unsupported warning) -> do
            warnCode warning @?= UnsupportedConstruct
            warnMessage warning @?= "Coprocess (coproc)"
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right _ -> H.assertFailure "expected error in strict mode",
      H.testCase "Warnings accumulate in order" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi\ncoproc echo bye"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation ->
            let st = translationState translation
             in
            fmap warnMessage (stateWarnings st)
              @?= [ "Coprocess (coproc)",
                    "Coprocess (coproc)"
                  ],
      H.testCase "Background tracking warning is emitted once across repeated wait translation" $ do
        (_out, st) <- translateWithState "sleep 1 &\nwait\nwait"
        fmap warnCode (stateWarnings st)
          @?= [BackgroundTracking],
      H.testCase "Function scope is restored after translating a function body" $ do
        (_out, st) <- translateWithState "f() { local inside=1; }\nlocal outside=1"
        fmap warnMessage (stateWarnings st)
          @?= ["local used outside a function; fish will treat it as local to the current scope"],
      H.testCase "Warnings keep their own source ranges across statements" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi\ntrap"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation -> do
            let warns = stateWarnings (translationState translation)
                starts = mapMaybe (fmap rangeStart . warnRange) warns
            map srcLine starts @?= [1, 2],
      H.testCase "Arithmetic short-circuit no longer warns on side effects" $ do
        result <- parseBashScript "spec.sh" "echo $((a++ && b++))"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation ->
            let st = translationState translation
             in
            H.assertBool
              "unexpected warning for arithmetic short-circuit"
              (not (any ((== "Arithmetic short-circuit may not preserve side effects") . warnMessage) (stateWarnings st))),
      H.testCase "Arithmetic short-circuit (||) no longer warns on side effects" $ do
        result <- parseBashScript "spec.sh" "echo $((a++ || b++))"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation ->
            let st = translationState translation
             in
            H.assertBool
              "unexpected warning for arithmetic short-circuit"
              (not (any ((== "Arithmetic short-circuit may not preserve side effects") . warnMessage) (stateWarnings st))),
      H.testCase "Arithmetic ternary no longer warns on side effects" $ do
        result <- parseBashScript "spec.sh" "echo $((a ? b++ : c++))"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation ->
            let st = translationState translation
             in
            H.assertBool
              "unexpected warning for arithmetic ternary"
              (not (any ((== "Arithmetic ternary may not preserve conditional side effects") . warnMessage) (stateWarnings st))),
      H.testCase "Read -r is treated as no-op" $ do
        result <- parseBashScript "spec.sh" "read -r name"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation ->
            let st = translationState translation
             in
            H.assertBool
              "unexpected warning for read -r"
              (not (any ((== "read -r has no fish equivalent; backslash escapes may differ") . warnMessage) (stateWarnings st))),
      H.testCase "Read array warns about IFS splitting" $ do
        result <- parseBashScript "spec.sh" "read -a arr"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation ->
            let st = translationState translation
             in
            H.assertBool
              "expected warning for read IFS splitting"
              (any ((== "read IFS splitting semantics may differ between bash and fish") . warnMessage) (stateWarnings st)),
      H.testCase "Set -euo pipefail enables errexit/pipefail and warns about nounset" $ do
        result <- parseBashScript "spec.sh" "set -euo pipefail"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation -> do
            let st = translationState translation
            let msgs = fmap warnMessage (stateWarnings st)
            H.assertBool "expected nounset warning" ("Bash set -u/nounset has no fish equivalent; manual review required" `elem` msgs)
            H.assertBool "unexpected errexit warning" ("Bash set -e/errexit has no fish equivalent; manual review required" `notElem` msgs)
            H.assertBool "unexpected pipefail warning" ("Bash set -o pipefail has no fish equivalent; manual review required" `notElem` msgs)
            H.assertBool "expected errexit enabled" (stateErrexitEnabled st)
            H.assertBool "expected pipefail enabled" (statePipefailEnabled st),
      H.testCase "shopt is warning-only and lowered to true" $ do
        (out, st) <- translateWithState "shopt -s nullglob"
        assertHasWarning "shopt has no fish equivalent; ignored" st
        H.assertBool "expected shopt note in output" (T.isInfixOf "shopt has no fish equivalent; ignored" out)
        H.assertBool "expected fallback true command" (T.isInfixOf "true '-s' 'nullglob'" out),
      H.testCase "read -d lowers to exact helper without semantic warning" $ do
        (out, st) <- translateWithState "read -d : first second"
        H.assertBool
          "unexpected warning for exact read delimiter helper"
          (not (any ((== "read delimiter semantics may differ between bash and fish") . warnMessage) (stateWarnings st)))
        H.assertBool "expected exact delimiter capture helper" (T.isInfixOf "__monk_read_capture_delim" out)
        H.assertBool "expected exact variable assignment helper" (T.isInfixOf "__monk_read_assign" out),
      H.testCase "read -d '' array path lowers without delimiter or IFS warnings" $ do
        (out, st) <- translateWithState "read -d '' -ra items"
        H.assertBool
          "unexpected delimiter warning for exact null-delimited array path"
          (not (any ((== "read delimiter semantics may differ between bash and fish") . warnMessage) (stateWarnings st)))
        H.assertBool
          "unexpected IFS warning for exact null-delimited array path"
          (not (any ((== "read IFS splitting semantics may differ between bash and fish") . warnMessage) (stateWarnings st)))
        H.assertBool "expected exact array assignment helper" (T.isInfixOf "__monk_read_assign" out),
      H.testCase "numeric fd read lowers to exact helper without semantic warnings" $ do
        (out, st) <- translateWithState "read -u 3 -r tail"
        H.assertBool
          "unexpected delimiter warning for numeric fd helper path"
          (not (any ((== "read delimiter semantics may differ between bash and fish") . warnMessage) (stateWarnings st)))
        H.assertBool
          "unexpected IFS warning for numeric fd helper path"
          (not (any ((== "read IFS splitting semantics may differ between bash and fish") . warnMessage) (stateWarnings st)))
        H.assertBool "expected exact delimiter capture helper" (T.isInfixOf "__monk_read_capture_delim" out)
        H.assertBool "expected numeric fd redirection" (T.isInfixOf "<&3" out),
      H.testCase "read -s lowers without warning" $ do
        (out, st) <- translateWithState "read -s secret"
        H.assertBool "unexpected warning for read -s" (not (any ((== "Unsupported read flag: -s") . warnMessage) (stateWarnings st)))
        H.assertBool "expected lowered silent flag" (T.isInfixOf "read --silent secret" out),
      H.testCase "clustered delimiter read flags use exact helper" $ do
        (out, st) <- translateWithState "read -rsd: -n 3 secret"
        H.assertBool
          "unexpected warning for exact clustered delimiter helper"
          (not (any ((== "read delimiter semantics may differ between bash and fish") . warnMessage) (stateWarnings st)))
        H.assertBool "expected exact delimiter capture helper" (T.isInfixOf "__monk_read_capture_delim" out),
      H.testCase "clustered read flags keep nchars and array options" $ do
        (out, st) <- translateWithState "read -n3 -a items"
        assertHasWarning "read IFS splitting semantics may differ between bash and fish" st
        H.assertBool "expected clustered nchars flag" (T.isInfixOf "read --nchars 3 --array items" out),
      H.testCase "read -d '' with no vars stays warning-driven and uses --null" $ do
        (out, st) <- translateWithState "read -d ''"
        assertHasWarning "read delimiter semantics may differ between bash and fish" st
        H.assertBool "expected ReadNull pretty-printing on fallback path" (T.isInfixOf "read --null" out),
      H.testCase "dynamic set -o warns for manual review" $ do
        (out, st) <- translateWithState "set -o $mode"
        assertHasWarningContaining "dynamic option requires manual review" st
        H.assertBool "expected set warning note in output" (T.isInfixOf "dynamic option requires manual review" out),
      H.testCase "Warnings carry stable codes and severities" $ do
        (_out, st) <- translateWithState "readonly FOO=bar\ntrap"
        let warns = stateWarnings st
        H.assertBool "expected readonly code" (any ((== ReadonlyNotEnforced) . warnCode) warns)
        H.assertBool "expected trap code" (any ((== TrapIssue) . warnCode) warns)
        H.assertBool "expected high severity warning" (any ((== WarnHigh) . warnSeverity) warns)
        H.assertBool "expected medium severity warning" (any ((== WarnMedium) . warnSeverity) warns),
      H.testCase "Command-substitution subshell warns with a stable code" $ do
        (_out, st) <- translateWithState "echo $( (echo hi) )"
        let warns = stateWarnings st
        H.assertBool "expected best-effort subshell code" (any ((== BestEffortSubshell) . warnCode) warns)
        H.assertBool "expected high-severity subshell warning" (any ((== WarnHigh) . warnSeverity) warns),
      H.testCase "trap with no arguments warns" $ do
        (out, st) <- translateWithState "trap"
        assertHasWarning "trap with no arguments is not supported" st
        out @?= "trap",
      H.testCase "trap options warn and stay raw" $ do
        (out, st) <- translateWithState "trap 'echo hi' -p"
        assertHasWarning "trap options are not supported; emitting raw trap command" st
        out @?= "trap 'echo hi' '-p'",
      H.testCase "trap pseudo-signals warn and avoid invalid fish handlers" $ do
        (out, st) <- translateWithState "trap 'echo hi' ERR"
        assertHasWarning "trap signal ERR has no fish equivalent; manual review required" st
        H.assertBool "expected warning note in output" ("manual review required" `T.isInfixOf` out)
        H.assertBool "unexpected invalid ERR signal handler" (not ("--on-signal ERR" `T.isInfixOf` out)),
      H.testCase "non-numeric read fd stays warning-driven" $ do
        (out, st) <- translateWithState "read -u fd value"
        assertHasWarning "read -u requires a numeric file descriptor; manual review required" st
        H.assertBool "expected warning note in output" ("manual review required" `T.isInfixOf` out)
        H.assertBool
          "expected best-effort fd read lowering"
          ( "read --fd 'fd' value" `T.isInfixOf` out
              || "read --fd 'fd' 'value'" `T.isInfixOf` out
              || "read -u fd value" `T.isInfixOf` out
              || "read -u 'fd' value" `T.isInfixOf` out
              || "read '-u' 'fd' 'value'" `T.isInfixOf` out
          ),
      H.testCase "unsupported clustered read flags stay raw and warning-driven" $ do
        (out, st) <- translateWithState "read -rz value"
        assertHasWarning "Unsupported read flag: -z" st
        H.assertBool "expected warning note in output" ("Unsupported read flag: -z" `T.isInfixOf` out)
        H.assertBool "expected raw read fallback" ("read '-rz' 'value'" `T.isInfixOf` out),
      H.testCase "shift negative count warns with comment" $ do
        (out, st) <- translateWithState "shift -1"
        assertHasWarning "shift count must be non-negative; emitting comment" st
        H.assertBool "expected unsupported shift count comment" (T.isInfixOf "Unsupported shift count" out),
      H.testCase "shift with multiple arguments warns with comment" $ do
        (out, st) <- translateWithState "shift 1 2"
        assertHasWarning "shift with multiple arguments is not supported; emitting comment" st
        H.assertBool "expected unsupported shift arguments comment" (T.isInfixOf "Unsupported shift arguments" out),
      H.testCase "readonly warns about missing enforcement" $ do
        (out, st) <- translateWithState "readonly FOO=bar"
        assertHasWarning "readonly/declare -r has no direct fish equivalent; emitted set without enforcing readonly" st
        H.assertBool "expected set output for readonly" (T.isInfixOf "set --global FOO 'bar'" out),
      H.testCase "declare invalid argument warns" $ do
        (_out, st) <- translateWithState "declare 1x"
        assertHasWarning "Unsupported declare argument: 1x" st,
      H.testCase "local outside function warns" $ do
        (out, st) <- translateWithState "local FOO=bar"
        assertHasWarning "local used outside a function; fish will treat it as local to the current scope" st
        H.assertBool "expected local set output" (T.isInfixOf "set --local FOO 'bar'" out),
      H.testCase "local invalid argument warns" $ do
        (_out, st) <- translateWithState "local 1x"
        assertHasWarning "Unsupported local argument: 1x" st,
      H.testCase "export invalid argument warns" $ do
        (_out, st) <- translateWithState "export 1x"
        assertHasWarning "Unsupported export argument: 1x" st,
      H.testCase "unset invalid flag warns but keeps valid arguments" $ do
        (out, st) <- translateWithState "unset -z foo"
        assertHasWarning "Unsupported unset flag: -z" st
        out @?= "set '-e' 'foo'",
      H.testCase "for arithmetic unsupported init warns" $ do
        (out, st) <- translateWithState "for ((i+=1; i<2; i++)); do echo $i; done"
        assertHasWarning "Unsupported arithmetic init; emitting comment" st
        H.assertBool "expected unsupported init comment" (T.isInfixOf "Unsupported arithmetic init" out),
      H.testCase "for arithmetic unsupported increment warns" $ do
        (out, st) <- translateWithState "for ((i=0; i<2; i*=2)); do echo $i; done"
        assertHasWarning "Unsupported arithmetic increment; emitting comment" st
        H.assertBool "expected unsupported increment comment" (T.isInfixOf "Unsupported arithmetic increment" out)
    ]

translateWithState :: T.Text -> IO (T.Text, TranslateState)
translateWithState script = do
  result <- parseBashScript "spec.sh" script
  case translateParseResult defaultConfig result of
    Left err -> H.assertFailure ("unexpected error: " <> show err) >> pure ("", error "unreachable")
    Right translation -> pure (renderTranslation translation, translationState translation)

assertHasWarning :: T.Text -> TranslateState -> Assertion
assertHasWarning msg st =
  H.assertBool
    ("expected warning: " <> toString msg)
    (any ((== msg) . warnMessage) (stateWarnings st))

assertHasWarningContaining :: T.Text -> TranslateState -> Assertion
assertHasWarningContaining needle st =
  H.assertBool
    ("expected warning containing: " <> toString needle)
    (any (T.isInfixOf needle . warnMessage) (stateWarnings st))
