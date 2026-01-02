module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.Pretty
import Unit.Translation
import Property.Pretty
import Property.Translation
import Golden
import Integration

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Monk"
  [ unitPrettyTests
  , unitTranslationTests
  , propertyPrettyTests
  , propertyTranslationTests
  , goldenTests
  , integrationTests
  ]
