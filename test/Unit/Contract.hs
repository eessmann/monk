{-# LANGUAGE OverloadedStrings #-}

module Unit.Contract (unitContractTests) where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Source qualified as Source
import Monk.Translation
import Path (toFilePath)
import Path.IO qualified as PathIO
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitContractTests :: TestTree
unitContractTests =
  testGroup
    "Execution contract"
    [ testGroup
        "programmatic contracts share admission validation"
        [ invalidProgrammatic "standalone imports cannot redirect dispatch" "caller-contract-mode" (strictConfig {callerContract = imported}) "visit",
          invalidProgrammatic "undeclared function reads reject" "caller-contract" (sourceable (withEffects (Set.singleton "missing") mempty)) ":",
          invalidProgrammatic "undeclared function writes reject" "caller-contract" (sourceable (withEffects mempty (Set.singleton "missing"))) ":",
          invalidProgrammatic "input-only function writes reject" "caller-contract" (sourceable ((withEffects mempty (Set.singleton "x")) {callerVariables = M.singleton "x" (ScalarBinding InputBinding VisibleBinding UnexportedBinding)})) ":",
          invalidProgrammatic "output-only function reads reject" "caller-contract" (sourceable ((withEffects (Set.singleton "x") mempty) {callerVariables = M.singleton "x" (ScalarBinding OutputBinding GlobalBinding UnexportedBinding)})) ":",
          invalidProgrammatic "reserved scalar name rejects even when unused" "caller-contract" (sourceable (base {callerVariables = M.singleton "__monk_x" (ScalarBinding InputBinding VisibleBinding UnexportedBinding)})) ":",
          invalidProgrammatic "reserved Fish import target rejects" "caller-contract" (sourceable (base {callerFunctions = M.singleton "visit" (MkFunctionContract "end" mempty mempty)})) ":",
          invalidProgrammatic "invalid imported source name rejects" "caller-contract" (sourceable (base {callerFunctions = M.singleton "bad-name" (MkFunctionContract "host_visit" mempty mempty)})) ":",
          invalidProgrammatic "import target cannot also be exported" "caller-contract" (sourceable (imported {callerExportedFunctions = Set.singleton "host_visit"})) ":",
          H.testCase "valid programmatic sourceable import remains admitted" $ do
            result <- translateBashScript (sourceable imported) "contract.bash" "visit"
            either (H.assertFailure . show) (const (pure ())) result
        ],
      H.testCase "named approximations require individual selection" $ do
        allowsApproximation defaultConfig ReadonlyUnchecked @?= False
        allowsApproximation strictConfig ReadonlyUnchecked @?= False
        let cfg = defaultConfig {translationPolicy = Migration (Set.singleton ReadonlyUnchecked)}
        allowsApproximation cfg ReadonlyUnchecked @?= True
        parseApproximation "subshell-sharing" @?= Nothing
        parseApproximation "here-string-printf" @?= Nothing,
      H.testCase "caller JSON preserves scalar binding and function effect declarations" $
        parseCallerContract "{\"version\":1,\"ambientEffects\":\"none\",\"variables\":{\"x\":{\"access\":\"read-write\",\"scope\":\"visible\"}},\"functions\":{\"visit\":{\"target\":\"host_visit\",\"reads\":[\"x\"],\"writes\":[\"x\"]}}}"
          @?= Right
            ( MkCallerContract
                (M.singleton "x" (ScalarBinding InputOutputBinding VisibleBinding UnexportedBinding))
                (M.singleton "visit" (MkFunctionContract "host_visit" (Set.singleton "x") (Set.singleton "x")))
                mempty
                NoRelevantAmbientEffects
                Nothing
                mempty
            ),
      H.testCase "caller JSON does not invent an ambient guarantee" $
        parseCallerContract "{\"version\":1}" @?= Right emptyCallerContract,
      H.testCase "export attributes and installed function names are explicit" $
        parseCallerContract "{\"version\":1,\"variables\":{\"x\":{\"access\":\"write\",\"scope\":\"global\",\"exported\":true}},\"exportedFunctions\":[\"visit\"]}"
          @?= Right
            emptyCallerContract
              { callerVariables = M.singleton "x" (ScalarBinding OutputBinding GlobalBinding ExportedBinding),
                callerExportedFunctions = Set.singleton "visit"
              },
      H.testCase "unknown versions and fields reject" $ do
        assertRejected "{\"version\":3}"
        assertRejected "{\"version\":1,\"trustEverything\":true}",
      H.testCase "reserved names and undeclared effects reject" $ do
        assertRejected "{\"version\":1,\"variables\":{\"__monk_x\":{\"access\":\"read\",\"scope\":\"global\"}}}"
        assertRejected "{\"version\":1,\"functions\":{\"f\":{\"target\":\"host_f\",\"writes\":[\"missing\"]}}}"
    ]
  where
    base = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects}
    imported = base {callerFunctions = M.singleton "visit" (MkFunctionContract "host_visit" mempty mempty)}
    withEffects readNames writeNames = base {callerFunctions = M.singleton "visit" (MkFunctionContract "host_visit" readNames writeNames)}
    sourceable caller = strictConfig {entryMode = Sourceable, callerContract = caller}

invalidProgrammatic :: String -> Text -> TranslateConfig -> Text -> TestTree
invalidProgrammatic name code config source = H.testCase name $ do
  parsed <- parseBashScript "contract.bash" source
  scriptResult <- translateBashScript config "contract.bash" source
  forM_ [scriptResult, translateParseResult config parsed] check
  PathIO.withSystemTempDir "monk-contract" $ \directory -> do
    let sourcePath = toFilePath directory </> "contract.bash"
    TIO.writeFile sourcePath source
    graphResult <- Source.translateSourceGraph config False sourcePath
    case graphResult of
      Left (Source.MkSourceGraphFailure _ failure) -> check (Left failure)
      Right _ -> H.assertFailure "source graph admitted an invalid programmatic contract"
  where
    check result = case result of
      Left failure -> H.assertBool ("wrong contract diagnostic: " <> show failure) (MkDiagnosticCode ("monk.semantic." <> code) `elem` map diagnosticCode (toList (failureDiagnostics failure)))
      Right _ -> H.assertFailure "invalid programmatic contract produced executable output"

assertRejected :: Text -> H.Assertion
assertRejected text = case parseCallerContract text of
  Left message -> H.assertBool "missing explanation" (not (T.null message))
  Right value -> H.assertFailure ("invalid contract accepted: " <> show value)
