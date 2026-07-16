{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.API04
  ( unitApi04Tests,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Monk.Diagnostics (renderDiagnostic, reviewRisk)
import Monk.Translation
  ( Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticPhase (..),
    DiagnosticSeverity (..),
    RequirementUse (..),
    ReviewRisk (..),
    RuntimeProgram (..),
    RuntimeRequirement (..),
    TranslationFailure (..),
    TranslationResult,
    defaultConfig,
    parseBashScript,
    renderTranslation,
    strictConfig,
    translateBashScript,
    translateParseResult,
    translationDiagnostics,
    translationRuntimeRequirements,
  )
import ShellCheck.AST qualified as Bash
import ShellCheck.Interface (ParseResult (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitApi04Tests :: TestTree
unitApi04Tests =
  testGroup
    "Monk 0.4 API"
    [ H.testCase "diagnostics render stable codes and aggregate review risk" $ do
        let diagnostic =
              MkDiagnostic
                { diagnosticCode = MkDiagnosticCode "monk.read",
                  diagnosticPhase = PhaseTranslate,
                  diagnosticSeverity = DiagnosticWarning,
                  diagnosticRisk = Review,
                  diagnosticMessage = "read fallback requires review",
                  diagnosticRange = Nothing
                }
        renderDiagnostic diagnostic @?= "warning[monk.read][review]: read fallback requires review"
        reviewRisk [diagnostic] @?= Review
        reviewRisk [diagnostic {diagnosticRisk = Unsafe}] @?= Unsafe,
      H.testCase "exact delimiter read declares its Python runtime" $ do
        result <- translateBashScript defaultConfig "spec.sh" "read -d : left right"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            translationDiagnostics translation @?= []
            H.assertBool
              "missing python3 runtime requirement"
              (RequiresCommand "python3" `elem` requiredPrograms translation),
      H.testCase "deduplicated requirements retain every operation and range" $ do
        result <- translateBashScript defaultConfig "spec.sh" "read -d : left right\nread -d : child rest"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation ->
            case find ((== RequiresCommand "python3") . requirementProgram) (translationRuntimeRequirements translation) of
              Nothing -> H.assertFailure "missing python3 runtime requirement"
              Just requirement -> do
                let uses = toList (requirementUses requirement)
                length uses @?= 2
                H.assertBool "requirement reason is generic" (all ((== "perform exact delimiter read") . requirementReason) uses)
                H.assertBool "requirement use lost its source range" (all (isJust . requirementRange) uses),
      H.testCase "exact delimiter read uses one Python process and no nested Fish" $ do
        result <- translateBashScript defaultConfig "spec.sh" "read -d : left right"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            let rendered = renderTranslation translation
            T.count "python3" rendered @?= 1
            H.assertBool "legacy assignment helper is still emitted" (not ("__monk_read_assign" `T.isInfixOf` rendered))
            H.assertBool "nested Fish status restoration is still emitted" (not ("fish '--no-config'" `T.isInfixOf` rendered))
            H.assertBool "generated status-return function is missing" ("__monk_return_status" `T.isInfixOf` rendered),
      H.testCase "standalone negation lowers through Fish not" $ do
        result <- translateBashScript defaultConfig "spec.sh" "! false"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            renderTranslation translation @?= "not false"
            translationDiagnostics translation @?= [],
      H.testCase "compound commands retain status in conjunctions and conditions" $ do
        translations <-
          mapM
            (translateBashScript defaultConfig "spec.sh")
            [ "case x in x) false ;; esac && echo bad",
              "if case x in x) false ;; esac; then echo bad; else echo ok; fi",
              "for x in one; do false; done && echo bad",
              "while false; do true; done && echo ok",
              "select x in one; do break; done && echo ok",
              "f() { false; } && echo defined",
              "{ false; } & echo launched"
            ]
        forM_ translations $ \case
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            H.assertBool
              "compound status emitted an unsupported diagnostic"
              (MkDiagnosticCode "monk.unsupported" `notElem` map diagnosticCode (translationDiagnostics translation))
            H.assertBool "compound command rendered no output" (not (T.null (renderTranslation translation))),
      H.testCase "errexit guards only the final AND-OR operand" $ do
        result <- translateBashScript defaultConfig "spec.sh" "set -e\nfalse && echo no"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation ->
            T.count "status 'is-command-substitution'" (renderTranslation translation) @?= 1,
      H.testCase "parameter-operator path separators stay literal" $ do
        result <- translateBashScript defaultConfig "spec.sh" "X=${X:-${HOME}/.config}"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            let rendered = renderTranslation translation
            H.assertBool "path separator was rendered as an invalid Fish variable" (not ("$/" `T.isInfixOf` rendered))
            H.assertBool
              "literal path suffix is missing"
              ("(string join ' ' -- '/' ; or printf '')'.config'" `T.isInfixOf` rendered),
      H.testCase "here-strings have dedicated diagnostics and strict rejection" $ do
        normal <- translateBashScript defaultConfig "spec.sh" "cat <<< value"
        strictResult <- translateBashScript strictConfig "spec.sh" "cat <<< value"
        case normal of
          Left failure -> H.assertFailure (show failure)
          Right translation ->
            H.assertBool
              "missing stable here-string diagnostic"
              (MkDiagnosticCode "monk.here-string" `elem` map diagnosticCode (translationDiagnostics translation))
        case strictResult of
          Left _ -> pure ()
          Right _ -> H.assertFailure "strict mode accepted a best-effort here-string",
      H.testCase "extglob compatibility fallback declares Bash and is unsafe" $ do
        result <- translateBashScript defaultConfig "spec.sh" "echo +([ab])"
        strictResult <- translateBashScript strictConfig "spec.sh" "echo +([ab])"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            H.assertBool
              "missing bash runtime requirement"
              (RequiresCommand "bash" `elem` requiredPrograms translation)
            reviewRisk (translationDiagnostics translation) @?= Unsafe
        case strictResult of
          Left _ -> pure ()
          Right _ -> H.assertFailure "strict mode accepted a Bash extglob fallback",
      H.testCase "unsupported standalone statements fail closed or reject" $ do
        normal <- translateBashScript defaultConfig "spec.sh" "coproc echo hi"
        strictResult <- translateBashScript strictConfig "spec.sh" "coproc echo hi"
        case normal of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            renderTranslation translation @?= "#Unsupported: Coprocess (coproc)\nfalse"
            map diagnosticCode (translationDiagnostics translation) @?= [MkDiagnosticCode "monk.unsupported"]
        case strictResult of
          Left _ -> pure ()
          Right _ -> H.assertFailure "strict mode accepted an unsupported standalone statement",
      H.testCase "source expansion wrappers are defensively unwrapped" $ do
        originalParsed <- parseBashScript "spec.sh" "echo original"
        includedParsed <- parseBashScript "child.sh" "echo included"
        case (prRoot originalParsed, prRoot includedParsed, translateParseResult defaultConfig originalParsed) of
          (Just originalRoot, Just includedRoot, Right originalTranslation) -> do
            let includeOnly = originalParsed {prRoot = Just (Bash.T_Include (Bash.Id 9001) includedRoot)}
                sourceWrapped =
                  originalParsed
                    { prRoot =
                        Just
                          ( Bash.T_SourceCommand
                              (Bash.Id 9002)
                              originalRoot
                              (Bash.T_Include (Bash.Id 9003) includedRoot)
                          )
                    }
            case translateParseResult defaultConfig includeOnly of
              Left failure -> H.assertFailure (show failure)
              Right translation -> renderTranslation translation @?= "echo 'included'"
            case translateParseResult defaultConfig sourceWrapped of
              Left failure -> H.assertFailure (show failure)
              Right translation -> renderTranslation translation @?= renderTranslation originalTranslation
          other -> H.assertFailure ("unable to construct source-wrapper fixture: " <> show other),
      H.testCase "translation failures contain at least one diagnostic" $ do
        result <- translateBashScript defaultConfig "broken.sh" "if"
        case result of
          Left failure -> H.assertBool "empty translation failure" (not (null (NE.toList (failureDiagnostics failure))))
          Right _ -> H.assertFailure "expected invalid Bash to fail"
    ]

requiredPrograms :: TranslationResult -> [RuntimeProgram]
requiredPrograms translation =
  map requirementProgram (translationRuntimeRequirements translation)
