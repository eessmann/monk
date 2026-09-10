{-# LANGUAGE OverloadedStrings #-}

module Property.Pretty
  ( propertyPrettyTests,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Gen
import Monk.AST
import ShellSupport
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC
import TestSupport

propertyPrettyTests :: TestTree
propertyPrettyTests =
  testGroup
    "Pretty properties"
    [ QC.testProperty "Literal rendering preserves the value through Fish parsing" $
        QC.withMaxSuccess 50 $
          QC.forAllShrink genShellScalar (map T.pack . QC.shrink . T.unpack) $ \value -> QCM.monadicIO $ do
            readiness <- QCM.run shouldRunIntegration
            case readiness of
              Left reason -> QCM.monitor (QC.label ("SKIPPED: " <> reason)) >> QCM.assert True
              Right () -> do
                environment <- QCM.run prepareEnv
                let output = renderDsl (script [stmt (command "printf" [arg (str "%s"), arg (str value)])])
                result <- QCM.run (runShellWithMode ShellRunExec ShellFish environment output [] "")
                QCM.monitor (QC.counterexample ("rendered: " <> toString output))
                QCM.assert (rrStdout result == value && T.null (rrStderr result)),
      QC.testProperty "Pipeline renders N pipes for N continuations" $ \(QC.NonNegative n) ->
        let stages = NE.fromList (replicate (n + 1) (stage (command "true" [])))
            out = renderDsl (script [stmt (pipeline stages)])
         in T.count " | " out == n,
      QC.testProperty "Job conjunction counts and/or tokens" $ \(xs :: [Bool]) ->
        let mkPipe = pipelineValue (stage (command "true" []) NE.:| [])
            cont = map (\b -> if b then andThen mkPipe else orElse mkPipe) xs
            conj = jobConjunction Nothing mkPipe cont
            out = renderDsl (script [stmt (job conj)])
            ands = length (filter id xs)
            ors = length (filter not xs)
         in T.count "and " out == ands && T.count "or " out == ors,
      QC.testProperty "Rendered pipeline is non-empty" $ QC.forAll genPipeline $ \p ->
        not (T.null (renderDsl (script [stmt p]))),
      QC.testProperty "Rendered job conjunction has at least head job" $ QC.forAll genConjunction $ \jc ->
        let out = renderDsl (script [stmt (job jc)]) in not (T.null out),
      QC.testProperty "If pretty has one end and proper indentation" $
        QC.forAll genNonEmptyStmts $ \thn ->
          QC.forAll (QC.listOf (stmt <$> genStatusCommand)) $ \els ->
            let cmd = stmt (if_ trueCond (block thn) els [])
                out = renderDsl (script [cmd])
                ls = T.lines out
                ends = length (filter (== "end") ls)
                indented = all (\l -> T.null l || T.isPrefixOf "  " l || l `elem` ["if true", "else", "end"]) ls
             in ends == 1 QC..&&. indented,
      QC.testProperty "Switch pretty has N case lines and one end" $
        let genCase = do
              pat <- str <$> genTextNoQuote
              caseItem (pat NE.:| []) . block <$> genNonEmptyStmts
         in QC.forAll (QC.listOf1 genCase) $ \items ->
              let switchCmd = stmt (switch (str "x") (NE.fromList items) [])
                  out = renderDsl (script [switchCmd])
                  ls = T.lines out
                  caseCount = length (filter (T.isPrefixOf "  case ") ls)
                  ends = length (filter (== "end") ls)
               in caseCount == length items QC..&&. ends == 1,
      QC.testProperty "Function pretty begins with function name and has one end" $
        QC.forAll (fmap T.pack (QC.listOf1 (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "_")))) $ \nameTxt ->
          QC.forAll genNonEmptyStmts $ \body ->
            let out = renderDsl (script [stmt (function nameTxt [] [] (block body))])
                ls = T.lines out
                headerOK = case ls of
                  (h : _) -> T.isPrefixOf ("function " <> nameTxt) h
                  _ -> False
                ends = length (filter (== "end") ls)
             in headerOK QC..&&. ends == 1
    ]

renderDsl :: Script -> Text
renderDsl = renderScript

-- An operating-system argument cannot contain NUL. Include quotes, controls,
-- whitespace and UTF-8 explicitly while staying inside that scalar grammar.
genShellScalar :: QC.Gen Text
genShellScalar = T.pack <$> QC.listOf (QC.elements ([' ' .. '~'] <> "\t\r\néλ😀"))
