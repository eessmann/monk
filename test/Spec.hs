module Main (main) where

import FixtureAdmission
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import Golden
import Integration
import Property.AdmittedCompositions
import Property.OutputEquivalence
import Property.Pretty
import Property.Translation
import RealWorld
import System.IO (hSetEncoding)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit.API04
import Unit.Bakeoff
import Unit.Contract
import Unit.DSL
import Unit.Diagnostics
import Unit.Harness
import Unit.Inline
import Unit.OutputBundle
import Unit.Pipefail
import Unit.PlannedArithmetic
import Unit.PlannedCommonCoverage
import Unit.PlannedDirectory
import Unit.PlannedEnvironment
import Unit.PlannedFlowFacts
import Unit.PlannedIsolation
import Unit.PlannedPrimitives
import Unit.PlannedRedirects
import Unit.PlannedSourceable
import Unit.PlannedWordContexts
import Unit.Pretty
import Unit.Refactor
import Unit.SemanticAdmission
import Unit.Source
import Unit.SourceResolution
import Unit.Translation
import Unit.TranslatorMonad

main :: IO ()
main = do
  -- Fixture files and generated sources use UTF-8 independently of the
  -- inherited host locale; ShellSupport still gives child shells LC_ALL=C.
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  traverse_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Monk"
    [ fixtureAdmissionTests,
      unitPrettyTests,
      unitTranslationTests,
      unitDiagnosticsTests,
      unitDslTests,
      unitInlineTests,
      unitOutputBundleTests,
      unitSourceTests,
      unitSourceResolutionTests,
      unitHarnessTests,
      unitBakeoffTests,
      unitApi04Tests,
      unitRefactorTests,
      unitSemanticAdmissionTests,
      unitPlannedWordContextTests,
      unitPlannedPrimitivesTests,
      unitPlannedRedirectTests,
      unitPlannedArithmeticTests,
      unitPlannedCommonCoverageTests,
      unitPlannedDirectoryTests,
      unitPlannedEnvironmentTests,
      unitPlannedFlowFactTests,
      unitPlannedIsolationTests,
      plannedSourceableTests,
      unitContractTests,
      unitTranslatorMonadTests,
      unitPipefailTests,
      propertyPrettyTests,
      propertyTranslationTests,
      propertyOutputEquivalenceTests,
      admittedCompositionTests,
      goldenTests,
      integrationTests,
      realWorldTests
    ]
