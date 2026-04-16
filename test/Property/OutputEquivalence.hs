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
    prepareEnv,
    runShellWith,
    shouldRunIntegration,
  )
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

propertyOutputEquivalenceTests :: TestTree
propertyOutputEquivalenceTests =
  testGroup
    "Output equivalence properties"
    [ QC.testProperty "Translated output matches bash output (simple scripts)" $
        QC.withMaxSuccess 30 $
          QC.forAll genScriptCase $ \scriptCase ->
            QCM.monadicIO $ do
              enabled <- QCM.run shouldRunIntegration
              case enabled of
                Left _ -> QCM.assert True
                Right () -> do
                  let script = scScript scriptCase
                      caseLabel = scLabel scriptCase
                      args = scArgs scriptCase
                      stdinInput = scStdin scriptCase
                  QCM.monitor (QC.counterexample ("case: " <> T.unpack caseLabel <> "\nscript:\n" <> T.unpack script))
                  translated <- QCM.run (translateScriptText "prop.sh" script)
                  case translated of
                    Left err -> do
                      QCM.monitor (QC.counterexample ("translation error: " <> err))
                      QCM.assert False
                    Right fishSrc -> do
                      env <- QCM.run prepareEnv
                      bashRes <- QCM.run (runShellWith ShellBash env script args stdinInput)
                      fishRes <- QCM.run (runShellWith ShellFish env fishSrc args stdinInput)
                      QCM.assert (rrExit bashRes == rrExit fishRes)
                      QCM.assert (rrStdout bashRes == rrStdout fishRes)
                      QCM.assert (rrStderr bashRes == rrStderr fishRes)
    ]

data ScriptCase = ScriptCase
  { scLabel :: Text,
    scScript :: Text,
    scArgs :: [Text],
    scStdin :: Text
  }
  deriving stock (Show, Eq)

genScriptCase :: QC.Gen ScriptCase
genScriptCase =
  QC.oneof
    [ genEchoVar,
      genArithmetic,
      genArrayIndex,
      genPipelineUpper,
      genArgvRoundTrip,
      genReadSplit,
      genTempEnv,
      genCaseGlob,
      genHereString
    ]

genEchoVar :: QC.Gen ScriptCase
genEchoVar = do
  val <- genWord
  pure (mkCase "echo-var" ("x=" <> val <> "\n" <> "echo \"$x\""))

genArithmetic :: QC.Gen ScriptCase
genArithmetic = do
  a <- genSmallInt
  b <- genSmallInt
  let script =
        "a=" <> T.pack (show a) <> "\n"
          <> "b=" <> T.pack (show b) <> "\n"
          <> "echo $((a + b))"
  pure (mkCase "arithmetic" script)

genArrayIndex :: QC.Gen ScriptCase
genArrayIndex = do
  vals <- QC.vectorOf 3 genWord
  idx <- QC.chooseInt (0, 2)
  let arr = T.intercalate " " vals
      script =
        "arr=(" <> arr <> ")\n"
          <> "i=" <> T.pack (show idx) <> "\n"
          <> "echo ${arr[$i]}"
  pure (mkCase "array-index" script)

genPipelineUpper :: QC.Gen ScriptCase
genPipelineUpper = do
  val <- genLowerWord
  let script = "echo " <> val <> " | tr a-z A-Z"
  pure (mkCase "pipeline-upper" script)

genArgvRoundTrip :: QC.Gen ScriptCase
genArgvRoundTrip = do
  args <- QC.listOf genWord
  let script =
        "printf 'argc:%s\\n' \"$#\"\n"
          <> "for arg in \"$@\"; do\n"
          <> "  printf 'arg:%s\\n' \"$arg\"\n"
          <> "done"
  pure (ScriptCase "argv-roundtrip" script args "")

genReadSplit :: QC.Gen ScriptCase
genReadSplit = do
  lhs <- genWord
  rhs <- genWord
  let script =
        "IFS=:\n"
          <> "read -r left right\n"
          <> "printf 'read:%s|%s\\n' \"$left\" \"$right\""
      stdinInput = lhs <> ":" <> rhs <> "\n"
  pure (ScriptCase "read-split" script [] stdinInput)

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
        "x=" <> value <> "\n"
          <> "case \"$x\" in\n"
          <> "  " <> pat <> ") echo match ;;\n"
          <> "  *) echo miss ;;\n"
          <> "esac"
  pure (mkCase "case-glob" script)

genHereString :: QC.Gen ScriptCase
genHereString = do
  val <- genWord
  let script =
        "read value <<< \"" <> val <> "\"\n"
          <> "printf 'here:%s\\n' \"$value\""
  pure (mkCase "here-string" script)

mkCase :: Text -> Text -> ScriptCase
mkCase caseName script = ScriptCase caseName script [] ""

genWord :: QC.Gen Text
genWord = T.pack <$> QC.listOf1 (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_']))

genLowerWord :: QC.Gen Text
genLowerWord = T.pack <$> QC.listOf1 (QC.elements ['a' .. 'z'])

genSmallInt :: QC.Gen Int
genSmallInt = QC.chooseInt (0, 20)

translateScriptText :: FilePath -> Text -> IO (Either String Text)
translateScriptText path script = do
  parseResult <- parseBashScript path script
  case translateParseResult defaultConfig parseResult of
    Left err -> pure (Left ("translateParseResult failed: " <> show err))
    Right translation -> pure (Right (renderTranslation translation))
