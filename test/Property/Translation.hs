module Property.Translation (propertyTranslationTests) where

import Data.Text qualified as T
import Monk.Translation
import ShellSupport
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

-- These properties exercise semantic admission and execution. Private renderer
-- spellings and unsupported legacy lowering are not translation contracts.
propertyTranslationTests :: TestTree
propertyTranslationTests =
  testGroup
    "Translation properties"
    [ rejectedProperty "array index requires an array storage plan" $ do
        index <- QC.chooseInt (0, 20)
        pure ("printf '%s\\n' \"${arr[" <> show index <> "]}\""),
      rejectedProperty "array assignment requires an array storage plan" $ do
        index <- QC.chooseInt (0, 20)
        pure ("arr[" <> show index <> "]=foo"),
      rejectedProperty "substring modifier remains an explicit exclusion" $ do
        start <- QC.chooseInt (0, 20)
        pure ("x=abcdef; printf '%s\\n' \"${x:" <> show start <> ":2}\""),
      rejectedProperty "case modifiers remain an explicit exclusion" $ do
        modifier <- QC.elements ["^^", ",,", "^", ","]
        pure ("x=AbCd; printf '%s\\n' \"${x" <> modifier <> "}\""),
      QC.testProperty "compound double-bracket syntax preserves both branches" $
        QC.forAll (QC.elements ["&&", "||"]) $ \operator ->
          exactProperty ("if [[ x = x " <> operator <> " y = z ]]; then printf yes; else printf no; fi"),
      rejectedProperty "regular expression condition remains explicitly excluded" $ do
        patternText <- QC.elements ["^foo", "bar[0-9]+", "^baz$", "qux.*"]
        pure ("[[ foo =~ " <> patternText <> " ]]"),
      QC.testProperty "admitted pattern equality preserves literal and wildcard meaning" $
        QC.forAllShrink genPatternCase shrinkPatternCase $ \(subject, patternText, quoted) ->
          exactProperty
            ( "if [[ "
                <> quote subject
                <> " = "
                <> (if quoted then quote patternText else patternText)
                <> " ]]; then printf 'yes\\n'; else printf 'no\\n'; fi"
            ),
      QC.testProperty "integer command preserves its zero and nonzero status" $
        QC.forAllShrink (QC.chooseInt (-100, 100)) QC.shrink $ \number ->
          exactProperty ("((" <> show number <> ")); printf '%s\\n' \"$?\"")
    ]

rejectedProperty :: String -> QC.Gen Text -> TestTree
rejectedProperty name generator = QC.testProperty name $ QC.forAll generator $ \source -> QC.ioProperty $ do
  result <- translateBashScript strictConfig "generated-exclusion.bash" source
  pure $ case result of
    Left failure -> QC.counterexample (show failure) (any semanticError (failureDiagnostics failure))
    Right translated -> QC.counterexample ("excluded syntax produced executable output:\n" <> toString (renderTranslation translated)) False
  where
    semanticError diagnostic =
      diagnosticSeverity diagnostic == DiagnosticError
        && diagnosticPhase diagnostic == PhaseTranslate
        && isJust (diagnosticRange diagnostic)
        && T.isPrefixOf "monk.semantic." (diagnosticCodeText (diagnosticCode diagnostic))

exactProperty :: Text -> QC.Property
exactProperty source = QCM.monadicIO $ do
  QCM.monitor (QC.counterexample ("source:\n" <> toString source))
  result <- QCM.run (translateBashScript strictConfig "generated-core.bash" source)
  case result of
    Left failure -> do
      QCM.monitor (QC.counterexample ("ADMISSION_REGRESSION: " <> show failure))
      QCM.assert False
    Right translated -> do
      readiness <- QCM.run shouldRunIntegration
      case readiness of
        Left reason -> QCM.monitor (QC.label ("SKIPPED runtime: " <> reason))
        Right () -> do
          environment <- QCM.run prepareEnv
          bash <- QCM.run (runShellWithMode ShellRunExec ShellBash environment source [] "")
          fish <- QCM.run (runShellWithMode ShellRunExec ShellFish environment (renderTranslation translated) [] "")
          let observation value = (rrExit value, rrStdout value, rrStderr value)
          QCM.monitor
            ( QC.counterexample
                ( (if null (translationDiagnostics translated) then "ZERO_DIAGNOSTIC_MISMATCH" else "DIAGNOSED_MISMATCH")
                    <> "\nBash: "
                    <> show (observation bash)
                    <> "\nFish: "
                    <> show (observation fish)
                )
            )
          QCM.assert (observation bash == observation fish)

genPatternCase :: QC.Gen (Text, Text, Bool)
genPatternCase = (,,) <$> QC.elements ["", "x", "abc", "a*c", "two words", "é"] <*> QC.elements ["*", "?", "a*", "a?c", "abc", "??"] <*> QC.arbitrary

shrinkPatternCase :: (Text, Text, Bool) -> [(Text, Text, Bool)]
shrinkPatternCase (subject, patternText, quoted) = [("", patternText, quoted) | not (T.null subject)]

quote :: Text -> Text
quote value = "'" <> T.replace "'" "'\\''" value <> "'"
