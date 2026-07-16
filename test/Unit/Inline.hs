{-# LANGUAGE OverloadedStrings #-}

module Unit.Inline
  ( unitInlineTests,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Monk.AST (renderScript)
import Monk.Source
  ( SourceGraph (..),
    Translation (..),
    inlineSourceGraph,
  )
import Monk.Translation
  ( Diagnostic (..),
    TranslationResult (..),
    defaultConfig,
    parseBashScript,
    translateParseResult,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitInlineTests :: TestTree
unitInlineTests =
  testGroup
    "Inlining"
    [ H.testCase "Inline source preserves argv" $ do
        let rootPath = "root.sh"
            subPath = "sub.sh"
            rootScript = "source sub.sh a b\n"
            subScript = "echo $1\n"
        rootParse <- parseBashScript rootPath rootScript
        subParse <- parseBashScript subPath subScript
        case (translateParseResult defaultConfig rootParse, translateParseResult defaultConfig subParse) of
          (Right rootResult, Right subResult) -> do
            let rootTr = translationArtifact rootPath rootResult (M.fromList [("sub.sh", Just subPath)])
                subTr = translationArtifact subPath subResult mempty
                graph = MkSourceGraph [rootPath, subPath] (M.fromList [(rootPath, rootTr), (subPath, subTr)])
            (inlined, diagnostics) <- inlineSourceGraph graph rootPath
            diagnostics @?= []
            let out = renderScript inlined
            T.isInfixOf "set '--local' '__monk_saved_argv_" out H.@? "expected argv save"
            T.isInfixOf "set 'argv' 'a' 'b'" out H.@? "expected argv override"
            T.isInfixOf "set 'argv' $__monk_saved_argv_" out H.@? "expected argv restore"
            T.isInfixOf "set '--local' '__monk_source_status_" out H.@? "expected sourced status capture"
            T.isInfixOf "__monk_source_return_status $__monk_source_status_" out H.@? "expected exact sourced status restoration"
          (Left err, _) -> H.assertFailure (show err)
          (_, Left err) -> H.assertFailure (show err),
      H.testCase "Inline source preserves argv and attaches source redirections to the inlined block" $ do
        let rootPath = "root.sh"
            childPath = "child.sh"
            rootScript = "source child.sh left right >capture.txt\n"
            childScript = "printf '%s:%s\\n' \"$1\" \"$2\"\n"
        rootParse <- parseBashScript rootPath rootScript
        childParse <- parseBashScript childPath childScript
        case (translateParseResult defaultConfig rootParse, translateParseResult defaultConfig childParse) of
          (Right rootResult, Right childResult) -> do
            let rootTr = translationArtifact rootPath rootResult (M.singleton "child.sh" (Just childPath))
                childTr = translationArtifact childPath childResult mempty
                graph = MkSourceGraph [rootPath, childPath] (M.fromList [(rootPath, rootTr), (childPath, childTr)])
            (inlined, diagnostics) <- inlineSourceGraph graph rootPath
            diagnostics @?= []
            let out = renderScript inlined
            H.assertBool ("source survived argv/redirection inlining:\n" <> T.unpack out) (not ("source 'child.sh'" `T.isInfixOf` out))
            H.assertBool "expected argv override" ("set 'argv' 'left' 'right'" `T.isInfixOf` out)
            H.assertBool ("redirection was not attached to inlined block:\n" <> T.unpack out) ("end > 'capture.txt'" `T.isInfixOf` out)
          (Left err, _) -> H.assertFailure (show err)
          (_, Left err) -> H.assertFailure (show err),
      H.testCase "Inline source reports structured diagnostics for non-literal paths" $ do
        rootParse <- parseBashScript "root.sh" "source \"$child\"\n"
        case translateParseResult defaultConfig rootParse of
          Left err -> H.assertFailure (show err)
          Right rootResult -> do
            let rootTr = translationArtifact "root.sh" rootResult mempty
                graph = MkSourceGraph ["root.sh"] (M.fromList [("root.sh", rootTr)])
            (_, diagnostics) <- inlineSourceGraph graph "root.sh"
            H.assertBool
              "expected non-literal source diagnostic"
              (any (T.isInfixOf "non-literal source path" . diagnosticMessage) diagnostics),
      H.testCase "Inline source traverses conditions pipelines conjunctions wrappers and substitutions" $ do
        let rootPath = "root.sh"
            childPath = "child.sh"
            rootScript =
              T.unlines
                [ "if source child.sh; then echo condition; fi",
                  "source child.sh | cat",
                  "source child.sh && echo conjunction",
                  "! source child.sh",
                  "captured=$(source child.sh)"
                ]
            childScript = "echo child\n"
        rootParse <- parseBashScript rootPath rootScript
        childParse <- parseBashScript childPath childScript
        case (translateParseResult defaultConfig rootParse, translateParseResult defaultConfig childParse) of
          (Right rootResult, Right childResult) -> do
            let rootTr = translationArtifact rootPath rootResult (M.singleton "child.sh" (Just childPath))
                childTr = translationArtifact childPath childResult mempty
                graph = MkSourceGraph [rootPath, childPath] (M.fromList [(rootPath, rootTr), (childPath, childTr)])
            (inlined, diagnostics) <- inlineSourceGraph graph rootPath
            diagnostics @?= []
            let out = renderScript inlined
            H.assertBool ("nested source survived inlining:\n" <> T.unpack out) (not ("source 'child.sh'" `T.isInfixOf` out))
            H.assertBool "child body was not inserted" (T.count "echo 'child'" out >= 5)
          (Left err, _) -> H.assertFailure (show err)
          (_, Left err) -> H.assertFailure (show err)
    ]

translationArtifact :: FilePath -> TranslationResult -> Map Text (Maybe FilePath) -> Translation
translationArtifact path result sourceMap =
  MkTranslation
    { trPath = path,
      trScript = translationScript result,
      trDiagnostics = translationDiagnostics result,
      trRuntimeRequirements = translationRuntimeRequirements result,
      trSourceMap = sourceMap
    }
