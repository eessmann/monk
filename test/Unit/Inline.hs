{-# LANGUAGE OverloadedStrings #-}

module Unit.Inline
  ( unitInlineTests,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Monk
  ( Translation (..),
    defaultConfig,
    inlineStatements,
    parseBashScript,
    renderFish,
    translationStatements,
    translationState,
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
            let rootTr =
                  Translation
                    { trPath = rootPath,
                      trStatements = translationStatements rootResult,
                      trState = translationState rootResult,
                      trSourceMap = M.fromList [("sub.sh", Just subPath)]
                    }
                subTr =
                  Translation
                    { trPath = subPath,
                      trStatements = translationStatements subResult,
                      trState = translationState subResult,
                      trSourceMap = mempty
                    }
                translations = M.fromList [(rootPath, rootTr), (subPath, subTr)]
            inlined <- inlineStatements (\_ -> pure ()) translations Set.empty rootPath
            let out = renderFish inlined
            T.isInfixOf "set '--local' '__monk_saved_argv_" out H.@? "expected argv save"
            T.isInfixOf "set 'argv' 'a' 'b'" out H.@? "expected argv override"
            T.isInfixOf "set 'argv' $__monk_saved_argv_" out H.@? "expected argv restore"
          (Left err, _) -> H.assertFailure (show err)
          (_, Left err) -> H.assertFailure (show err)
    ]
