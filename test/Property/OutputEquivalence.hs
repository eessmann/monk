{-# LANGUAGE OverloadedStrings #-}

module Property.OutputEquivalence
  ( propertyOutputEquivalenceTests,
  )
where

import Data.Text qualified as T
import Monk.Translation
import ShellSupport
  ( RunResult (..),
    Shell (..),
    ShellRunMode (..),
    prepareEnv,
    runShellWithMode,
    shouldRunIntegration,
  )
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

propertyOutputEquivalenceTests :: TestTree
propertyOutputEquivalenceTests =
  testGroup
    "Output equivalence properties"
    [ exactCases "scalar assignment and quoted output" genEchoVar,
      exactCases "integer addition" genArithmetic,
      exactCases "draining transform pipeline" genPipelineUpper,
      exactCases "positional argument round trip" genArgvRoundTrip,
      exactCases "lazy case glob selection" genCaseGlob,
      excludedCases "indexed arrays require storage plans" genArrayIndex,
      excludedCases "read requires a byte and binding primitive" genReadSplit,
      excludedCases "temporary environment assignments require binding lifetime" genTempEnv,
      excludedCases "here strings require owned input redirection" genHereString
    ]

exactCases :: String -> QC.Gen ScriptCase -> TestTree
exactCases testName generator = QC.testProperty testName $ QC.withMaxSuccess 30 $ QC.forAllShrink generator shrinkCase $ \scriptCase -> QCM.monadicIO $ do
  let source = scScript scriptCase
  QCM.monitor (QC.counterexample ("case: " <> toString (scLabel scriptCase) <> "\nscript:\n" <> toString source))
  result <- QCM.run (translateBashScript strictConfig "generated-equivalence.bash" source)
  case result of
    Left failure -> do
      QCM.monitor (QC.counterexample ("ADMISSION_REGRESSION: " <> show failure))
      QCM.assert False
    Right translated -> do
      ready <- QCM.run shouldRunIntegration
      case ready of
        Left reason -> QCM.monitor (QC.label ("SKIPPED runtime: " <> reason))
        Right () -> do
          environment <- QCM.run prepareEnv
          bash <- QCM.run (runShellWithMode ShellRunExec ShellBash environment source (scArgs scriptCase) (scStdin scriptCase))
          fish <- QCM.run (runShellWithMode ShellRunExec ShellFish environment (renderTranslation translated) (scArgs scriptCase) (scStdin scriptCase))
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

excludedCases :: String -> QC.Gen ScriptCase -> TestTree
excludedCases testName generator = QC.testProperty testName $ QC.withMaxSuccess 30 $ QC.forAll generator $ \scriptCase -> QC.ioProperty $ do
  result <- translateBashScript strictConfig "generated-exclusion.bash" (scScript scriptCase)
  pure $ case result of
    Left failure -> QC.counterexample (show failure) (any semanticError (failureDiagnostics failure))
    Right translated -> QC.counterexample ("excluded form produced executable output:\n" <> toString (renderTranslation translated)) False
  where
    semanticError diagnostic = diagnosticSeverity diagnostic == DiagnosticError && diagnosticPhase diagnostic == PhaseTranslate && isJust (diagnosticRange diagnostic)

shrinkCase :: ScriptCase -> [ScriptCase]
shrinkCase value = [value {scArgs = args} | args <- QC.shrinkList (const []) (scArgs value)]

data ScriptCase = MkScriptCase
  { scLabel :: Text,
    scScript :: Text,
    scArgs :: [Text],
    scStdin :: Text
  }
  deriving stock (Show, Eq)

genEchoVar :: QC.Gen ScriptCase
genEchoVar = do
  val <- genWord
  pure (mkCase "echo-var" ("x=" <> val <> "\n" <> "echo \"$x\""))

genArithmetic :: QC.Gen ScriptCase
genArithmetic = do
  a <- genSmallInt
  b <- genSmallInt
  let script =
        "a="
          <> T.pack (show a)
          <> "\n"
          <> "b="
          <> T.pack (show b)
          <> "\n"
          <> "echo $((a + b))"
  pure (mkCase "arithmetic" script)

genArrayIndex :: QC.Gen ScriptCase
genArrayIndex = do
  vals <- QC.vectorOf 3 genWord
  idx <- QC.chooseInt (0, 2)
  let arr = T.intercalate " " vals
      script =
        "arr=("
          <> arr
          <> ")\n"
          <> "i="
          <> T.pack (show idx)
          <> "\n"
          <> "echo ${arr[$i]}"
  pure (mkCase "array-index" script)

genPipelineUpper :: QC.Gen ScriptCase
genPipelineUpper = do
  val <- genLowerWord
  let script = "echo " <> val <> " | tr a-z A-Z"
  pure (mkCase "pipeline-upper" script)

genArgvRoundTrip :: QC.Gen ScriptCase
genArgvRoundTrip = do
  count <- QC.chooseInt (0, 6)
  args <- QC.vectorOf count (QC.oneof [genWord, QC.elements ["", "two words", "line\nbreak", "*", "-n"]])
  let script =
        "printf 'argc:%s\\n' \"$#\"\n"
          <> "for arg in \"$@\"; do\n"
          <> "  printf 'arg:%s\\n' \"$arg\"\n"
          <> "done"
  pure (MkScriptCase "argv-roundtrip" script args "")

genReadSplit :: QC.Gen ScriptCase
genReadSplit = do
  lhs <- genWord
  rhs <- genWord
  let script =
        "IFS=:\n"
          <> "read -r left right\n"
          <> "printf 'read:%s|%s\\n' \"$left\" \"$right\""
      stdinInput = lhs <> ":" <> rhs <> "\n"
  pure (MkScriptCase "read-split" script [] stdinInput)

genTempEnv :: QC.Gen ScriptCase
genTempEnv = do
  val <- genWord
  let script = "FOO=" <> val <> " sh -c 'printf \"%s\\n\" \"$FOO\"'"
  pure (mkCase "temp-env" script)

genCaseGlob :: QC.Gen ScriptCase
genCaseGlob = do
  stem <- genLowerWord
  matches <- QC.arbitrary
  let pat = stem <> "*"
      value =
        if matches
          then stem <> "tail"
          else "other" <> stem
      script =
        "x="
          <> value
          <> "\n"
          <> "case \"$x\" in\n"
          <> "  "
          <> pat
          <> ") echo match ;;\n"
          <> "  *) echo miss ;;\n"
          <> "esac"
  pure (mkCase "case-glob" script)

genHereString :: QC.Gen ScriptCase
genHereString = do
  val <- genWord
  let script =
        "read value <<< \""
          <> val
          <> "\"\n"
          <> "printf 'here:%s\\n' \"$value\""
  pure (mkCase "here-string" script)

mkCase :: Text -> Text -> ScriptCase
mkCase caseName script = MkScriptCase caseName script [] ""

genWord :: QC.Gen Text
genWord = T.pack <$> QC.resize 12 (QC.listOf1 (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_'])))

genLowerWord :: QC.Gen Text
genLowerWord = T.pack <$> QC.resize 12 (QC.listOf1 (QC.elements ['a' .. 'z']))

genSmallInt :: QC.Gen Int
genSmallInt = QC.chooseInt (0, 20)
