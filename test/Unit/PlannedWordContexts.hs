module Unit.PlannedWordContexts (unitPlannedWordContextTests) where

import Data.Text qualified as T
import Monk.Translation
import ShellSupport
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitPlannedWordContextTests :: TestTree
unitPlannedWordContextTests =
  testGroup
    "Planned word contexts"
    [ exact "quoted case wildcard is a literal" "case x in '*') printf 'bad\\n';; *) printf 'good\\n';; esac",
      exact "quoted case variable keeps wildcard inactive" "p='*'; case x in \"$p\") printf 'bad\\n';; *) printf 'good\\n';; esac",
      exact "unquoted case variable activates admitted wildcard" "p='a*'; case abc in $p) printf 'yes\\n';; *) printf 'no\\n';; esac",
      exact "double bracket equality uses the active pattern operand" "p='a*'; if [[ abc = $p ]]; then printf 'yes\\n'; else printf 'no\\n'; fi",
      exact "quoted double bracket wildcard stays literal" "p='a*'; if [[ abc = \"$p\" ]]; then printf 'bad\\n'; else printf 'good\\n'; fi",
      exact "positional defaults distinguish unset from empty" "set -- ''; printf '<%s>|<%s>|<%s>\\n' \"${1-fallback}\" \"${1:-fallback}\" \"${2-fallback}\"",
      exact "IFS separates individual C locale bytes" "IFS='é'; x='aéb'; printf '<%s>\\n' $x",
      exact "case wildcard cardinality follows C locale bytes" "case é in ?) printf 'bad\\n';; ??) printf 'two bytes\\n';; esac",
      exact "numeric comparisons retain integer precision" "if [[ 9007199254740993 -gt 9007199254740992 ]]; then printf 'yes\\n'; fi",
      exact "numeric comparisons use Bash numeral bases" "if [[ 010 -eq 8 ]]; then printf 'yes\\n'; fi",
      exact "bare local shadows its caller while remaining unset" "x=outer; f() { local x; printf '<%s>|<%s>\\n' \"${x-fallback}\" \"$x\"; }; f; printf '%s\\n' \"$x\"",
      exact "repeated local declaration retains an initialized local" "f() { local x=value; local x; printf '%s\\n' \"$x\"; }; f",
      globCase "unmatched glob preserves its literal spelling" [] "*.txt",
      globCase "one glob match remains one field" ["two words.txt"] "*.txt",
      globCase "many glob matches sort in C order and omit dot files" ["b.txt", "a.txt", ".hidden.txt", "other"] "*.txt",
      globCase "quoted glob fragments remain literal" ["a*b.txt", "aXb.txt"] "'a*'?.txt",
      exact "trim quoted wildcard is literal" "x='*abc'; printf '<%s>' \"${x#'*'}\"",
      exact "trim mixed quoted and active fragments" "x='*abcabc'; printf '<%s><%s>' \"${x#'*'a*}\" \"${x##'*'a*}\"",
      exact "trim escaped source wildcard is literal" "x='*abc'; printf '<%s>' \"${x#\\*}\"",
      exact "trim bracket and C class syntax remains active" "x=abc123; printf '<%s><%s>' \"${x#[[:alpha:]]}\" \"${x%%[[:digit:]]*}\"",
      exact "trim quoted bracket text remains literal" "x='[ab]tail'; printf '<%s>' \"${x#'[ab]'}\"",
      exact "trim quoted variable is literal without static proof" "set -- '*'; p=$1; x='*abc'; printf '<%s>' \"${x#\"$p\"}\"",
      exact "trim proved variable activates wildcard syntax" "p='a*'; x=abcabc; printf '<%s><%s>' \"${x#$p}\" \"${x##$p}\"",
      exact "trim bracket spans quoted fragments" "x='-rest'; printf '<%s>' \"${x#[\"-\"]}\"",
      exact "trim suffix shortest and longest preserve mixed activity" "x='abcabc*'; printf '<%s><%s>' \"${x%a*'*'}\" \"${x%%a*'*'}\"",
      exact "trim empty pattern preserves one empty word" "x=; printf '<%s>' \"${x#}\"",
      exact "trim no match preserves subject" "x=abc; printf '<%s>' \"${x#z[0-9]}\"",
      exact "trim byte classes preserve C locale semantics" "x=éz; printf '<%s>' \"${x#??}\"",
      exact "trim unquoted result still follows normal field splitting" "x='prefix a b'; printf '<%s>' ${x#prefix}",
      exact "trim inactive parentheses are ordinary bytes" "x='@(a)tail'; printf '<%s>' \"${x#'@(a)'}\"",
      rejected "trim unknown active pattern stays rejected" "x=abc; printf \"%s\" \"${x#$1}\"",
      rejected "trim active extended pattern stays rejected" "x=abc; printf \"%s\" \"${x#@(a|b)}\"",
      rejected "trim extended pattern assembled dynamically stays rejected" "p=\"@(a)\"; x=abc; printf \"%s\" \"${x#$p}\"",
      rejected "numeric double bracket cannot evaluate an expression string" "if [[ '2+3' -eq 5 ]]; then printf 'yes\\n'; fi",
      rejected "unknown arithmetic comparison operand cannot bypass arithmetic admission" "if [[ $unknown_operand -eq 5 ]]; then printf 'yes\\n'; fi"
    ]

exact :: String -> Text -> TestTree
exact label source = H.testCaseSteps label (compareSource source)

compareSource :: Text -> (String -> IO ()) -> H.Assertion
compareSource source step = do
  result <- translateBashScript strictConfig "word-contexts.bash" source
  case result of
    Left failure -> H.assertFailure ("mandatory word admission failed: " <> show failure)
    Right translated -> do
      readiness <- shouldRunIntegration
      case readiness of
        Left reason -> step ("skipped: " <> reason)
        Right () -> do
          environment <- prepareEnv
          bash <- runShellWithMode ShellRunExec ShellBash environment source [] ""
          fish <- runShellWithMode ShellRunExec ShellFish environment (renderTranslation translated) [] ""
          let observations value = (rrExit value, rrStdoutBytes value, rrStderrBytes value)
          H.assertEqual
            ((if null (translationDiagnostics translated) then "ZERO_DIAGNOSTIC_MISMATCH" else "DIAGNOSED_MISMATCH") <> "\n" <> toString source)
            (observations bash)
            (observations fish)

rejected :: String -> Text -> TestTree
rejected label source = H.testCase label $ do
  result <- translateBashScript strictConfig "word-contexts.bash" source
  case result of
    Left failure -> H.assertBool "missing semantic rejection" (any (T.isPrefixOf "monk.semantic." . diagnosticCodeText . diagnosticCode) (failureDiagnostics failure))
    Right translated -> H.assertFailure ("unsupported numeric context produced executable output: " <> toString (renderTranslation translated))

globCase :: String -> [FilePath] -> Text -> TestTree
globCase label entries patternText = H.testCaseSteps label $ \step ->
  withSystemTempDirectory "monk-word-glob" $ \directory -> do
    forM_ entries $ \entry -> writeFileBS (directory </> entry) "fixture"
    let source = "printf '<%s>\\n' '" <> toText directory <> "/'" <> patternText
    compareSource source step
