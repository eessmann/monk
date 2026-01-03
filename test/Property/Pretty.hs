{-# LANGUAGE OverloadedStrings #-}

module Property.Pretty
  ( propertyPrettyTests,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Gen
import Monk
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
import TestSupport

propertyPrettyTests :: TestTree
propertyPrettyTests =
  testGroup
    "Pretty properties"
    [ QC.testProperty "Echo literal without single quotes is single-quoted" $
        QC.forAll genTextNoQuote $ \t ->
          let out = renderFish [Stmt (Command "echo" [ExprVal (ExprLiteral t)])]
           in T.isInfixOf ("'" <> t <> "'") out,
      QC.testProperty "Pipeline renders N pipes for N continuations" $ \(QC.NonNegative n) ->
        let conts = replicate n (PipeTo [] (Stmt (Command "true" [])))
            pipe = FishJobPipeline False [] (Stmt (Command "true" [])) conts False
            out = renderFish [Stmt (Pipeline pipe)]
         in T.count " | " out == n,
      QC.testProperty "Job conjunction counts and/or tokens" $ \(xs :: [Bool]) ->
        let mkPipe = FishJobPipeline False [] (Stmt (Command "true" [])) [] False
            cont = map (\b -> if b then JCAnd mkPipe else JCOr mkPipe) xs
            conj = FishJobConjunction Nothing mkPipe cont
            out = renderFish [Stmt (JobConj conj)]
            ands = length (filter id xs)
            ors = length (filter not xs)
         in T.count "and " out == ands && T.count "or " out == ors,
      QC.testProperty "Rendered pipeline is non-empty" $ QC.forAll genPipeline $ \p ->
        not (T.null (renderFish [Stmt (Pipeline p)])),
      QC.testProperty "Rendered job conjunction has at least head job" $ QC.forAll genConjunction $ \jc ->
        let out = renderFish [Stmt (JobConj jc)] in not (T.null out),
      QC.testProperty "If pretty has one end and proper indentation" $
        QC.forAll genNonEmptyStmts $ \thn ->
          QC.forAll (QC.listOf (Stmt <$> genStatusCommand)) $ \els ->
            let cmd = Stmt (If trueCond thn els [])
                out = renderFish [cmd]
                ls = T.lines out
                ends = length (filter (== "end") ls)
                indented = all (\l -> T.null l || T.isPrefixOf "  " l || l `elem` ["if true", "else", "end"]) ls
             in ends == 1 QC..&&. indented,
      QC.testProperty "Switch pretty has N case lines and one end" $
        let genCase = do
              pat <- ExprLiteral <$> genTextNoQuote
              body <- genNonEmptyStmts
              pure (CaseItem (pat NE.:| []) body)
         in QC.forAll (QC.listOf1 genCase) $ \items ->
              let switchCmd = Stmt (Switch (ExprLiteral "x") (NE.fromList items) [])
                  out = renderFish [switchCmd]
                  ls = T.lines out
                  caseCount = length (filter (T.isPrefixOf "  case ") ls)
                  ends = length (filter (== "end") ls)
               in caseCount == length items QC..&&. ends == 1,
      QC.testProperty "Function pretty begins with function name and has one end" $
        QC.forAll (fmap T.pack (QC.listOf1 (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "_")))) $ \nameTxt ->
          QC.forAll genNonEmptyStmts $ \body ->
            let fn = FishFunction {funcName = nameTxt, funcFlags = [], funcParams = [], funcBody = body}
                out = renderFish [Stmt (Function fn)]
                ls = T.lines out
                headerOK = case ls of
                  (h : _) -> T.isPrefixOf ("function " <> nameTxt) h
                  _ -> False
                ends = length (filter (== "end") ls)
             in headerOK QC..&&. ends == 1
    ]
