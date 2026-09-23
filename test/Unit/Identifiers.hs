module Unit.Identifiers (unitIdentifierTests) where

import Language.Fish.DSL (Identifier, identifier, identifierText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitIdentifierTests :: TestTree
unitIdentifierTests =
  testGroup
    "Canonical variable identifiers"
    [ H.testCase "portable and generated names preserve spelling" $
        forM_ ["a", "_", "x9", "__monk_plan_0_value_12", "argv", "status", "pipestatus", "PWD"] $ \name ->
          fmap identifierText (identifier name) H.@?= Right name,
      H.testCase "dynamic names cannot inject expansion or shell grammar" $
        forM_ ["", "9x", "x; echo bad", "x\ny", "$argv", "x[1]", "x y", "a'b", "a\"b", "a\\b", "a\0b", "é", "#"] $ \name ->
          H.assertBool ("reject " <> show name) (isLeft (identifier name)),
      H.testCase "static literals and safe concatenation retain canonical names" $ do
        identifierText ("__monk_" <> "value" :: Identifier) H.@?= "__monk_value"
    ]
