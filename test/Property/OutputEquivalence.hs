{-# LANGUAGE OverloadedStrings #-}

module Property.OutputEquivalence
  ( propertyOutputEquivalenceTests,
  )
where

import Data.Text qualified as T
import Monk
import ShellSupport
  ( RunResult (..),
    Shell (..),
    prepareEnv,
    runShell,
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
        QC.withMaxSuccess 20 $
          QC.forAll genScriptCase $ \scriptCase ->
            QCM.monadicIO $ do
              enabled <- QCM.run shouldRunIntegration
              case enabled of
                Left _ -> QCM.assert True
                Right () -> do
                  let script = scScript scriptCase
                      caseLabel = scLabel scriptCase
                  QCM.monitor (QC.counterexample ("case: " <> T.unpack caseLabel <> "\nscript:\n" <> T.unpack script))
                  translated <- QCM.run (translateScriptText "prop.sh" script)
                  case translated of
                    Left err -> do
                      QCM.monitor (QC.counterexample ("translation error: " <> err))
                      QCM.assert False
                    Right fishSrc -> do
                      env <- QCM.run prepareEnv
                      bashRes <- QCM.run (runShell ShellBash env script)
                      fishRes <- QCM.run (runShell ShellFish env fishSrc)
                      QCM.assert (rrExit bashRes == rrExit fishRes)
                      QCM.assert (rrStdout bashRes == rrStdout fishRes)
                      QCM.assert (rrStderr bashRes == rrStderr fishRes)
    ]

data ScriptCase = ScriptCase
  { scLabel :: Text,
    scScript :: Text
  }
  deriving stock (Show, Eq)

genScriptCase :: QC.Gen ScriptCase
genScriptCase =
  QC.oneof
    [ genEchoVar,
      genArithmetic,
      genArrayIndex,
      genPipelineUpper
    ]

genEchoVar :: QC.Gen ScriptCase
genEchoVar = do
  val <- genWord
  pure (ScriptCase "echo-var" ("x=" <> val <> "\n" <> "echo \"$x\""))

genArithmetic :: QC.Gen ScriptCase
genArithmetic = do
  a <- genSmallInt
  b <- genSmallInt
  let script =
        "a=" <> T.pack (show a) <> "\n"
          <> "b=" <> T.pack (show b) <> "\n"
          <> "echo $((a + b))"
  pure (ScriptCase "arithmetic" script)

genArrayIndex :: QC.Gen ScriptCase
genArrayIndex = do
  vals <- QC.vectorOf 3 genWord
  idx <- QC.chooseInt (0, 2)
  let arr = T.intercalate " " vals
      script =
        "arr=(" <> arr <> ")\n"
          <> "i=" <> T.pack (show idx) <> "\n"
          <> "echo ${arr[$i]}"
  pure (ScriptCase "array-index" script)

genPipelineUpper :: QC.Gen ScriptCase
genPipelineUpper = do
  val <- genLowerWord
  let script = "echo " <> val <> " | tr a-z A-Z"
  pure (ScriptCase "pipeline-upper" script)

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
    Right (stmt, _) -> pure (Right (renderFish [stmt]))
