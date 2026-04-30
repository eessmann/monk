{-# LANGUAGE OverloadedStrings #-}

module Unit.Diagnostics
  ( unitDiagnosticsTests,
  )
where

import Data.Text qualified as T
import Monk.AST (SourcePos (..), SourceRange (..))
import Monk.Diagnostics
  ( WarningCounts (..),
    confidenceScore,
    renderTranslateError,
    renderTranslationNotes,
    renderWarning,
    summarizeWarnings,
    translationNoteCount,
  )
import Monk.Translation.Types
  ( TranslateConfig (..),
    TranslateError (..),
    Warning (..),
    WarningCode (..),
    WarningSeverity (..),
    allWarningCodes,
    defaultConfig,
    strictConfig,
    warnMessage,
    warningCodeSeverity,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitDiagnosticsTests :: TestTree
unitDiagnosticsTests =
  testGroup
    "Diagnostics"
    [ H.testCase "translation config defaults are exported from the public types module" $ do
        strictMode defaultConfig @?= False
        strictMode strictConfig @?= True,
      H.testCase "warning code severities are owned by the public types module" $ do
        warningCodeSeverity BestEffortSubshell @?= WarnHigh
        warningCodeSeverity ReadIssue @?= WarnMedium
        warningCodeSeverity TrapIssue @?= WarnMedium,
      H.testCase "all warning codes are listed for contract checks" $ do
        allWarningCodes
          @?= [ UnsupportedConstruct,
                BestEffortSubshell,
                ExecFdRedirect,
                BackgroundTracking,
                SetOptionIssue,
                ReadIssue,
                ShoptIgnored,
                TrapIssue,
                ShiftIssue,
                ReadonlyNotEnforced,
                DeclareIssue,
                ScopeIssue,
                UnsetIssue,
                ForArithmeticIssue,
                ArithmeticIssue
              ]
        forM_ allWarningCodes $ \code ->
          H.assertBool
            ("missing default warning message for " <> show code)
            (not (T.null (warnMessage (MkWarning code (warningCodeSeverity code) Nothing Nothing)))),
      H.testCase "warnMessage keeps stable defaults and detail overrides" $ do
        warnMessage (MkWarning ReadIssue WarnMedium Nothing Nothing)
          @?= "read semantics may differ between bash and fish"
        warnMessage (MkWarning ReadIssue WarnMedium (Just "custom read detail") Nothing)
          @?= "custom read detail"
        warnMessage (MkWarning ShoptIgnored WarnHigh Nothing Nothing)
          @?= "shopt has no fish equivalent; ignored",
      H.testCase "renderWarning includes the source range prefix" $
        renderWarning (sampleWarning ReadIssue Nothing)
          @?= "spec.sh:3:5: warning: read semantics may differ between bash and fish",
      H.testCase "renderTranslateError includes the source range prefix" $
        renderTranslateError (Unsupported (sampleWarning UnsupportedConstruct (Just "Coprocess (coproc)")))
          @?= "spec.sh:3:5: error: Coprocess (coproc)",
      H.testCase "warning summaries and notes stay aligned" $ do
        let warns = [sampleWarning BestEffortSubshell Nothing, sampleWarning ReadIssue Nothing]
        summarizeWarnings warns @?= MkWarningCounts {wcHigh = 1, wcMedium = 1, wcLow = 0}
        confidenceScore warns @?= 70
        renderTranslationNotes "spec.sh" warns
          @?= [ "note: spec.sh: translation confidence 70/100",
                "note: 2 warning(s) (1 high, 1 medium, 0 low)",
                "note: high-risk translations present; review recommended"
              ]
        translationNoteCount warns @?= 3
    ]

sampleWarning :: WarningCode -> Maybe Text -> Warning
sampleWarning code detail =
  MkWarning
    { warnCode = code,
      warnSeverity = warningCodeSeverity code,
      warnDetail = detail,
      warnRange = Just sampleRange
    }

sampleRange :: SourceRange
sampleRange =
  MkSourceRange
    { rangeStart = MkSourcePos {srcFile = "spec.sh", srcLine = 3, srcColumn = 5},
      rangeEnd = MkSourcePos {srcFile = "spec.sh", srcLine = 3, srcColumn = 12}
    }
