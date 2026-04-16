module Main (main) where

import Golden
import Integration
import Property.OutputEquivalence
import Property.Pretty
import Property.Translation
import RealWorld
import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.Bakeoff
import Unit.Harness
import Unit.Inline
import Unit.Polysemy
import Unit.Pipefail
import Unit.Pretty
import Unit.Refactor
import Unit.Source
import Unit.Translation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Monk"
    [ unitPrettyTests,
      unitTranslationTests,
      unitInlineTests,
      unitSourceTests,
      unitHarnessTests,
      unitBakeoffTests,
      unitRefactorTests,
      unitPolysemyTests,
      unitPipefailTests,
      propertyPrettyTests,
      propertyTranslationTests,
      propertyOutputEquivalenceTests,
      goldenTests,
      integrationTests,
      realWorldTests
    ]
