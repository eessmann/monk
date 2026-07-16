{-# LANGUAGE OverloadedStrings #-}

module Property.Pretty
  ( propertyPrettyTests,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Gen
import Monk.AST
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC
import TestSupport

propertyPrettyTests :: TestTree
propertyPrettyTests =
  testGroup
    "Pretty properties"
    [ QC.testProperty "Echo literals choose a safe quote style" $
        QC.forAll genTextNoQuote $ \t ->
          let out = renderDsl (script [stmt (command "echo" [arg (str t)])])
           in if "\\" `T.isInfixOf` t
                then T.isPrefixOf "echo \"" out
                else T.isInfixOf ("'" <> t <> "'") out,
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
