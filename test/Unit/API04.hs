module Unit.API04 (unitApi04Tests) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Monk.Diagnostics (renderDiagnostic, renderRuntimeRequirement, reviewRisk)
import Monk.Translation
import ShellCheck.AST qualified as Bash
import ShellCheck.Interface (ParseResult (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitApi04Tests :: TestTree
unitApi04Tests =
  testGroup
    "Monk translation product API"
    [ H.testCase "diagnostics render stable codes and aggregate review risk" $ do
        let diagnostic = MkDiagnostic (MkDiagnosticCode "monk.example") PhaseOutput DiagnosticWarning Review "review required" Nothing
        renderDiagnostic diagnostic @?= "warning[monk.example][review]: review required"
        reviewRisk [diagnostic] @?= Review
        reviewRisk [diagnostic {diagnosticRisk = Unsafe}] @?= Unsafe,
      H.testCase "an exact primitive declares its bounded native runtime" $ do
        translated <- accepted strictConfig "x=value; echo \"$x\""
        H.assertBool "missing native operation requirement" (RequiresNativeRuntime 1 Bash53Signed64Fish46 (Set.singleton NativeEcho) `elem` programs translated),
      H.testCase "deduplicated requirements retain every operation and range" $ do
        translated <- accepted strictConfig "x=one; echo \"$x\"\necho \"$x\""
        H.assertBool "missing native provider pathname capability producer" (RequiresFishFeature NulDelimitedCapture `elem` programs translated)
        case find ((== RequiresNativeRuntime 1 Bash53Signed64Fish46 (Set.singleton NativeEcho)) . requirementProgram) (translationRuntimeRequirements translated) of
          Nothing -> H.assertFailure "missing native operation requirement"
          Just requirement -> do
            let uses = toList (requirementUses requirement)
            length uses @?= 2
            H.assertBool "operation reason is missing" (not (any (T.null . requirementReason) uses))
            H.assertBool "operation lost its original source range" (all (isJust . requirementRange) uses)
            H.assertBool "two source occurrences collapsed into one" (length (Set.fromList (map requirementRange uses)) == 2),
      H.testCase "every materialized capability is supported by the selected profile" $ do
        translated <- accepted strictConfig "f() { local x=one; printf '%s\\n' \"$x\"; }; f"
        H.assertBool "missing base Fish capability" (RequiresFishFeature Fish46 `elem` programs translated)
        H.assertBool "missing scope-sharing capability producer" (RequiresFishFeature FunctionScopeSharing `elem` programs translated)
        forM_ (programs translated) $ \case
          RequiresFishFeature feature -> H.assertBool "unsupported materialization capability" (profileSupportsFishFeature Bash53Signed64Fish46 feature)
          RequiresCommand _ -> pure ()
          RequiresNativeRuntime abi profile _ -> do
            abi @?= 1
            profile @?= Bash53Signed64Fish46
          RequiresPlatformCapability capability -> H.assertBool "unsupported platform capability" (profileSupportsPlatformCapability Bash53Signed64Fish46 capability),
      H.testCase "NUL capture capability has an actual materialized producer" $ do
        translated <- accepted strictConfig "printf '<%s>\\n' \"$(printf 'x\\n')\""
        H.assertBool "capture capability missing" (RequiresFishFeature NulDelimitedCapture `elem` programs translated),
      H.testCase "owned child transport declares its descriptor platform contract" $ do
        translated <- accepted strictConfig "x=\"$(printf child)\""
        H.assertBool "missing typed platform producer" (RequiresPlatformCapability Linux64DescriptorFilesystem `elem` programs translated)
        H.assertBool "missing descriptor platform requirement" (any (T.isInfixOf "platform:linux-64-descriptor-filesystem" . renderRuntimeRequirement) (translationRuntimeRequirements translated)),
      H.testCase "normal and strict defaults reject unsupported semantics without a script" $
        forM_ [defaultConfig, strictConfig] $
          \config -> forM_ ["coproc echo hi", "eval 'echo unsafe'", "echo +([ab])"] $ \source -> assertRejected config source,
      H.testCase "readonly approximation is never selected implicitly" $
        forM_ [defaultConfig, strictConfig] $
          \config -> assertRejected config "readonly x=one; x=two; printf '%s\\n' \"$x\"",
      H.testCase "selected readonly approximation records the affected occurrence" $ do
        translated <- accepted (defaultConfig {translationPolicy = Migration (Set.singleton ReadonlyUnchecked)}) "readonly x=one; x=two; printf '%s\\n' \"$x\""
        let diagnostics = filter ((== MkDiagnosticCode "monk.approximation.readonly-unchecked") . diagnosticCode) (translationDiagnostics translated)
        length diagnostics @?= 1
        map diagnosticSeverity diagnostics @?= [DiagnosticWarning]
        map diagnosticRisk diagnostics @?= [Review]
        H.assertBool "approximation lost its source occurrence" (all (isJust . diagnosticRange) diagnostics),
      H.testCase "readonly opt-in does not change multiple-operand evaluation order" $
        assertRejected (defaultConfig {translationPolicy = Migration (Set.singleton ReadonlyUnchecked)}) "x=outer; readonly x=inner y=\"$x\"",
      H.testCase "an approximation selection does not permit unrelated eval" $
        assertRejected (defaultConfig {translationPolicy = Migration (Set.singleton ReadonlyUnchecked)}) "eval 'echo unsafe'",
      H.testCase "syntax-only arithmetic cannot claim original spelling evidence" $ do
        parsed <- parseBashScript "spec.bash" "printf '%s\\n' \"$((1/0))\""
        case translateParseResult strictConfig parsed of
          Left failure -> H.assertBool "wrong rejection" (MkDiagnosticCode "monk.semantic.arithmetic-source" `elem` map diagnosticCode (toList (failureDiagnostics failure)))
          Right _ -> H.assertFailure "syntax-only input fabricated arithmetic source evidence",
      H.testCase "parser inclusion metadata cannot create a certified source dependency" $ do
        parsed <- parseBashScript "spec.bash" "echo original"
        case prRoot parsed of
          Nothing -> H.assertFailure "positive parser control failed"
          Just root -> forM_ [Bash.T_Include (Bash.Id 9001) root, Bash.T_SourceCommand (Bash.Id 9002) root (Bash.T_Include (Bash.Id 9003) root)] $ \wrapped ->
            case translateParseResult strictConfig parsed {prRoot = Just wrapped} of
              Left _ -> pure ()
              Right _ -> H.assertFailure "parser source metadata bypassed owned source discovery",
      H.testCase "translation failures contain at least one diagnostic" $ assertRejected strictConfig "if"
    ]

accepted :: TranslateConfig -> Text -> IO TranslationResult
accepted config source = do
  result <- translateBashScript config "spec.bash" source
  case result of
    Right translation -> pure translation
    Left failure -> H.assertFailure (show failure) >> fail "unreachable"

assertRejected :: TranslateConfig -> Text -> H.Assertion
assertRejected config source = do
  result <- translateBashScript config "spec.bash" source
  case result of
    Left failure -> H.assertBool "empty diagnostic failure" (not (null (failureDiagnostics failure)))
    Right translation -> H.assertFailure ("unsupported input produced executable output: " <> toString (renderTranslation translation))

programs :: TranslationResult -> [RuntimeProgram]
programs = map requirementProgram . translationRuntimeRequirements
