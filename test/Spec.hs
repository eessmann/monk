module Main (main) where

import Golden
import Integration
import Property.OutputEquivalence
import Property.Pretty
import Property.Translation
import RealWorld
import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.Polysemy
import Unit.Pretty
import Unit.Translation
import Unit.Inline

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Monk"
    [ unitPrettyTests,
      unitTranslationTests,
      unitInlineTests,
      unitPolysemyTests,
      propertyPrettyTests,
      propertyTranslationTests,
      propertyOutputEquivalenceTests,
      goldenTests,
      integrationTests,
      realWorldTests
    ]
