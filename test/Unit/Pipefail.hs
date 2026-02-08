{-# LANGUAGE OverloadedStrings #-}

module Unit.Pipefail
  ( unitPipefailTests,
  )
where

import Data.Text qualified as T
import Monk
  ( defaultConfig,
    parseBashScript,
    renderTranslation,
    translateParseResult,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitPipefailTests :: TestTree
unitPipefailTests =
  testGroup
    "Pipefail"
    [ H.testCase "Pipefail does not wrap single-command pipelines" $ do
        result <- parseBashScript "spec.sh" "set -o pipefail\necho hi\n"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right translation -> do
            let out = renderTranslation translation
            T.isInfixOf "echo 'hi'" out H.@? "expected echo preserved"
            H.assertBool
              "unexpected __monk_pipefail in single command"
              (not (T.isInfixOf "__monk_pipefail" out))
    ]
