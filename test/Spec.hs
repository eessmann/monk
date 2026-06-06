module Main (main) where

import Golden
import Integration
import Property.OutputEquivalence
import Property.Pretty
import Property.Translation
import RealWorld
import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.Bakeoff
import Unit.DSL
import Unit.Diagnostics
import Unit.Harness
import Unit.Inline
import Unit.Pipefail
import Unit.Pretty
import Unit.Refactor
import Unit.Source
import Unit.Translation
import Unit.TranslatorMonad

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Monk"
    [ unitPrettyTests,
      unitTranslationTests,
      unitDiagnosticsTests,
      unitDslTests,
      unitInlineTests,
      unitSourceTests,
      unitHarnessTests,
      unitBakeoffTests,
      unitRefactorTests,
      unitTranslatorMonadTests,
      unitPipefailTests,
      propertyPrettyTests,
      propertyTranslationTests,
      propertyOutputEquivalenceTests,
      goldenTests,
      integrationTests,
      realWorldTests
    ]
