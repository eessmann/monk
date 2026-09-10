{-# LANGUAGE OverloadedStrings #-}

module Unit.TranslatorMonad
  ( unitTranslatorMonadTests,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Monk.AST (SourcePos (..), SourceRange (..))
import Monk.Translation
  ( Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticPhase (..),
    DiagnosticSeverity (..),
    ReviewRisk (..),
    TranslateConfig,
    TranslationFailure (..),
    defaultConfig,
    renderTranslation,
    strictConfig,
    translateBashScript,
    translationDiagnostics,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitTranslatorMonadTests :: TestTree
unitTranslatorMonadTests =
  testGroup
    "Translation failure contract"
    [ testGroup "stable semantic rejections" (map rejectedCaseTest rejectedCases),
      H.testCase "normal and strict policy fail at the same unsupported source site" policyModesRejectTheSameSite,
      H.testCase "normalization stops at the first unsupported source site" firstUnsupportedSiteWins,
      H.testCase "an exact translation does not inherit legacy warning state" exactTranslationHasNoLegacyWarnings
    ]

data RejectedCase = MkRejectedCase
  { rejectedName :: String,
    rejectedSource :: Text,
    rejectedCode :: DiagnosticCode
  }

rejectedCases :: [RejectedCase]
rejectedCases =
  [ rejected "coprocesses" "coproc echo hi" "unsupported-syntax",
    rejected "redirected output process substitution" "printf hi > >(wc -c > out)" "redirect-file",
    rejected "argument-position output process substitution" "echo >(cat)" "word",
    rejected "input process substitution" "cat <(printf value)" "word",
    rejected "extended globs" "echo !(foo|bar)" "word",
    rejected "computed shift count" "shift \"$n\"" "shift-operand",
    rejected "declare" "declare -x FOO=bar" "builtin",
    rejected "parameter error operators" "printf '%s\\n' \"${MISSING:?nope}\"" "parameter",
    rejected "heredocs" "cat <<EOF\nvalue\nEOF\n" "redirect-shape",
    rejected "array assignments" "arr[0]=foo" "assignment-shape",
    rejected "array expansion" "printf '%s\\n' \"${arr[0]}\"" "parameter",
    rejected "substring expansion" "printf '%s\\n' \"${var:1:2}\"" "parameter",
    rejected "arithmetic for array updates" "for ((i=0; i<2; a[0]++)); do echo \"$i\"; done" "arithmetic-shape",
    rejected "command-prefix assignments" "FOO=bar echo value" "command-prefix",
    rejected "local outside a function" "local FOO=bar" "local-context",
    rejected "select loops" "select x in a b; do echo \"$x\"; break; done" "unsupported-syntax",
    rejected "read" "read -r value" "builtin",
    rejected "background jobs" "false &" "unsupported-syntax",
    rejected "time pipelines" "time sleep 1" "word",
    rejected "pipelines containing source" "printf value | source child.bash" "source-child-context",
    rejected "trap" "trap 'echo bye' EXIT" "builtin",
    rejected "shopt" "shopt -s nullglob" "builtin",
    rejected "ambiguous set operands" "set a b" "set-option",
    rejected "computed source targets" "source \"$child\"" "computed-source",
    rejected "missing source targets" "." "source-target",
    rejected "readonly" "readonly FOO=bar" "readonly",
    rejected "double-bracket regular expressions" "if [[ x =~ ^x ]]; then echo yes; fi" "condition",
    rejected "invalid local binding names" "f() { local 1x; }; f" "name",
    rejected "invalid unset flags" "unset -z value" "name"
  ]

rejected :: String -> Text -> Text -> RejectedCase
rejected name source suffix =
  MkRejectedCase
    { rejectedName = name,
      rejectedSource = source,
      rejectedCode = MkDiagnosticCode ("monk.semantic." <> suffix)
    }

rejectedCaseTest :: RejectedCase -> TestTree
rejectedCaseTest test = H.testCase (rejectedName test) $ do
  forM_ [defaultConfig, strictConfig] $ \config -> assertRejectedAt config test

assertRejectedAt :: TranslateConfig -> RejectedCase -> H.Assertion
assertRejectedAt config MkRejectedCase {rejectedName, rejectedSource, rejectedCode} = do
  result <- translateBashScript config "legacy-rejected.bash" rejectedSource
  case result of
    Left failure -> assertSemanticFailure rejectedName rejectedCode True failure
    Right translation ->
      H.assertFailure
        ( rejectedName
            <> " produced executable output:\n"
            <> toString (renderTranslation translation)
        )

assertSemanticFailure :: String -> DiagnosticCode -> Bool -> TranslationFailure -> H.Assertion
assertSemanticFailure label expectedCode requireRange failure = do
  let diagnostic = NonEmpty.head (failureDiagnostics failure)
  diagnosticCode diagnostic H.@?= expectedCode
  diagnosticPhase diagnostic H.@?= PhaseTranslate
  diagnosticSeverity diagnostic H.@?= DiagnosticError
  diagnosticRisk diagnostic H.@?= Unsafe
  when requireRange $
    H.assertBool (label <> " lost its source range") (isJust (diagnosticRange diagnostic))

policyModesRejectTheSameSite :: H.Assertion
policyModesRejectTheSameSite = do
  normal <- firstFailure defaultConfig source
  strict <- firstFailure strictConfig source
  diagnosticCode normal H.@?= MkDiagnosticCode "monk.semantic.unsupported-syntax"
  diagnosticCode strict H.@?= diagnosticCode normal
  diagnosticRange strict H.@?= diagnosticRange normal
  where
    source = "true\ncoproc echo hi\ntrap"

firstUnsupportedSiteWins :: H.Assertion
firstUnsupportedSiteWins = do
  result <- translateBashScript strictConfig "legacy-order.bash" "true\ncoproc echo hi\ntrap"
  case result of
    Left failure -> do
      NonEmpty.length (failureDiagnostics failure) H.@?= 1
      let diagnostic = NonEmpty.head (failureDiagnostics failure)
      diagnosticCode diagnostic H.@?= MkDiagnosticCode "monk.semantic.unsupported-syntax"
      fmap (srcLine . rangeStart) (diagnosticRange diagnostic) H.@?= Just 2
    Right translation ->
      H.assertFailure
        ( "unsupported input produced executable output:\n"
            <> toString (renderTranslation translation)
        )

exactTranslationHasNoLegacyWarnings :: H.Assertion
exactTranslationHasNoLegacyWarnings = do
  result <- translateBashScript strictConfig "legacy-exact.bash" "set -- a b; printf '%s:%s\\n' \"$1\" \"$2\""
  case result of
    Left failure -> H.assertFailure ("exact input was rejected: " <> show failure)
    Right translation -> translationDiagnostics translation H.@?= []

firstFailure :: TranslateConfig -> Text -> IO Diagnostic
firstFailure config source = do
  result <- translateBashScript config "legacy-policy.bash" source
  case result of
    Left failure -> pure (NonEmpty.head (failureDiagnostics failure))
    Right translation ->
      H.assertFailure
        ( "unsupported input produced executable output:\n"
            <> toString (renderTranslation translation)
        )
        >> fail "unreachable"
