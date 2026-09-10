{-# LANGUAGE OverloadedStrings #-}

module Unit.Diagnostics
  ( unitDiagnosticsTests,
  )
where

import Monk.AST (SourcePos (..), SourceRange (..))
import Monk.Diagnostics
  ( DiagnosticCounts (..),
    renderDiagnostic,
    renderRuntimeRequirement,
    renderTranslationNotes,
    reviewRisk,
    summarizeDiagnostics,
    translationNoteCount,
  )
import Monk.Translation (translateBashScript)
import Monk.Translation.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitDiagnosticsTests :: TestTree
unitDiagnosticsTests =
  testGroup
    "Diagnostics"
    [ H.testCase "translation config defaults are exported from the public types module" $ do
        strictMode defaultConfig @?= False
        strictMode strictConfig @?= True,
      H.testCase "normal translation does not opt into readonly approximation" $ do
        result <- translateBashScript defaultConfig "spec.sh" "readonly value=one"
        case result of
          Left _ -> pure ()
          Right _ -> H.assertFailure "an approximation requires a named opt-in",
      H.testCase "renderDiagnostic includes stable code, risk, and source range" $
        renderDiagnostic (sampleDiagnostic Review)
          @?= "spec.sh:3:5: warning[monk.read][review]: read semantics require review",
      H.testCase "diagnostic summaries and review risk stay aligned" $ do
        let diagnostics =
              [ sampleDiagnostic Review,
                (sampleDiagnostic Unsafe) {diagnosticSeverity = DiagnosticError}
              ]
        summarizeDiagnostics diagnostics @?= MkDiagnosticCounts {dcErrors = 1, dcWarnings = 1, dcNotes = 0}
        reviewRisk diagnostics @?= Unsafe
        renderTranslationNotes "spec.sh" diagnostics
          @?= [ "note: spec.sh: 2 diagnostic(s) (1 error, 1 warning, 0 note)",
                "note: review risk unsafe"
              ]
        translationNoteCount diagnostics @?= 2,
      H.testCase "runtime requirements render stable program names and use counts" $
        renderRuntimeRequirement
          ( MkRuntimeRequirement
              (RequiresCommand "python3")
              ( MkRequirementUse "exact delimiter read" sampleRangeMaybe
                  :| [MkRequirementUse "array read" Nothing]
              )
          )
          @?= "note: runtime requirement: python3 (2 uses)"
    ]

sampleDiagnostic :: ReviewRisk -> Diagnostic
sampleDiagnostic risk =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode "monk.read",
      diagnosticPhase = PhaseTranslate,
      diagnosticSeverity = DiagnosticWarning,
      diagnosticRisk = risk,
      diagnosticMessage = "read semantics require review",
      diagnosticRange = Just sampleRange
    }

sampleRange :: SourceRange
sampleRange =
  MkSourceRange
    { rangeStart = MkSourcePos {srcFile = "spec.sh", srcLine = 3, srcColumn = 5},
      rangeEnd = MkSourcePos {srcFile = "spec.sh", srcLine = 3, srcColumn = 12}
    }

sampleRangeMaybe :: Maybe SourceRange
sampleRangeMaybe = Just sampleRange
