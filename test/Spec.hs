module Main (main) where

import Golden
import Integration
import Property.Pretty
import Property.Translation
import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.Pretty
import Unit.Translation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Monk"
    [ unitPrettyTests,
      unitTranslationTests,
      propertyPrettyTests,
      propertyTranslationTests,
      goldenTests,
      integrationTests
    ]
