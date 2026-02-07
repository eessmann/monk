{-# LANGUAGE OverloadedStrings #-}

module Unit.Polysemy
  ( unitPolysemyTests,
  )
where

import Monk
  ( TranslateError (..),
    TranslateState (..),
    Warning (..),
    defaultConfig,
    parseBashScript,
    strictConfig,
    translateParseResult,
  )
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
          Right (_, st) -> do
            length (warnings st) @?= 1
            case warnings st of
              [Warning msg mRange] -> do
                msg @?= "Coprocess (coproc)"
                case mRange of
                  Nothing -> H.assertFailure "expected warning range"
                  Just _ -> pure ()
              _ -> H.assertFailure "expected single warning"
            rangeStack st @?= [],
      H.testCase "Strict mode raises error for unsupported" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi"
        case translateParseResult strictConfig result of
          Left (Unsupported msg _) -> msg @?= "Coprocess (coproc)"
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right _ -> H.assertFailure "expected error in strict mode",
      H.testCase "Warnings accumulate in order" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi\ncoproc echo bye"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) ->
            fmap warnMessage (warnings st)
              @?= [ "Coprocess (coproc)",
                    "Coprocess (coproc)"
                  ],
      H.testCase "Arithmetic short-circuit no longer warns on side effects" $ do
        result <- parseBashScript "spec.sh" "echo $((a++ && b++))"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) ->
            H.assertBool
              "unexpected warning for arithmetic short-circuit"
              (not (any ((== "Arithmetic short-circuit may not preserve side effects") . warnMessage) (warnings st))),
      H.testCase "Arithmetic short-circuit (||) no longer warns on side effects" $ do
        result <- parseBashScript "spec.sh" "echo $((a++ || b++))"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) ->
            H.assertBool
              "unexpected warning for arithmetic short-circuit"
              (not (any ((== "Arithmetic short-circuit may not preserve side effects") . warnMessage) (warnings st))),
      H.testCase "Arithmetic ternary no longer warns on side effects" $ do
        result <- parseBashScript "spec.sh" "echo $((a ? b++ : c++))"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) ->
            H.assertBool
              "unexpected warning for arithmetic ternary"
              (not (any ((== "Arithmetic ternary may not preserve conditional side effects") . warnMessage) (warnings st))),
      H.testCase "Read -r warns about escape handling" $ do
        result <- parseBashScript "spec.sh" "read -r name"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) ->
            H.assertBool
              "expected warning for read -r"
              (any ((== "read -r has no fish equivalent; backslash escapes may differ") . warnMessage) (warnings st)),
      H.testCase "Read array warns about IFS splitting" $ do
        result <- parseBashScript "spec.sh" "read -a arr"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) ->
            H.assertBool
              "expected warning for read IFS splitting"
              (any ((== "read IFS splitting semantics may differ between bash and fish") . warnMessage) (warnings st)),
      H.testCase "Set -euo pipefail warns about errexit/nounset/pipefail" $ do
        result <- parseBashScript "spec.sh" "set -euo pipefail"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("unexpected error: " <> show err)
          Right (_, st) -> do
            let msgs = fmap warnMessage (warnings st)
            H.assertBool "expected errexit warning" ("Bash set -e/errexit has no fish equivalent; manual review required" `elem` msgs)
            H.assertBool "expected nounset warning" ("Bash set -u/nounset has no fish equivalent; manual review required" `elem` msgs)
            H.assertBool "expected pipefail warning" ("Bash set -o pipefail has no fish equivalent; manual review required" `elem` msgs)
    ]
