{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Monk
import Test.Tasty
import Test.Tasty.HUnit as H
import Test.Tasty.QuickCheck as QC
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Prelude hiding (print)
import Gen

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "AST"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Pretty printing"
  [ H.testCase "Redirection embedding" $ do
      let script =
            [ Stmt (Command "echo"
                [ ExprVal (ExprLiteral "Hello")
                , RedirectVal RedirectOut (ExprLiteral "/dev/null")
                ])
            ]
          actual = renderFish script
          expected = "echo 'Hello' > '/dev/null'"
      actual @?= expected

  , H.testCase "Simple pipeline" $ do
      let pipe = FishJobPipeline
                  { jpTime = False
                  , jpVariables = []
                  , jpStatement = Stmt (Command "grep" [ExprVal (ExprLiteral "foo")])
                  , jpCont = [ PipeTo { jpcVariables = []
                                      , jpcStatement = Stmt (Command "wc" [ExprVal (ExprLiteral "-l")])
                                      }
                             ]
                  , jpBackgrounded = False
                  }
          script = [ Stmt (Pipeline pipe) ]
          actual = renderFish script
          expected = "grep 'foo' | wc '-l'"
      actual @?= expected

  , H.testCase "Job conjunction (or)" $ do
      let job1 = FishJobPipeline False [] (Stmt (Command "false" [])) [] False
          job2 = FishJobPipeline False [] (Stmt (Command "echo" [ExprVal (ExprLiteral "ok")])) [] False
          conj = FishJobConjunction Nothing job1 [JCOr job2] False
          script = [ Stmt (JobConj conj) ]
          actual = renderFish script
          expected = "false\nor echo 'ok'"
      actual @?= expected

  , H.testCase "Begin block" $ do
      let body = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "A")])]
          script = [Stmt (Begin body)]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "begin"
            , "  echo 'A'"
            , "end"
            ]
      actual @?= expected

  , H.testCase "If then else" $ do
      let thn = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "then")])]
          els = [Stmt (Command "echo" [ExprVal (ExprLiteral "else")])]
          script = [Stmt (If (ExprBoolLiteral True) thn els)]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "if true"
            , "  echo 'then'"
            , "else"
            , "  echo 'else'"
            , "end"
            ]
      actual @?= expected

  , H.testCase "Switch with two cases" $ do
      let case1 = CaseItem (ExprLiteral "foo" NE.:| []) (NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "a")])])
          case2 = CaseItem (ExprLiteral "bar" NE.:| []) (NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "b")])])
          script = [Stmt (Switch (ExprLiteral "x") (case1 NE.:| [case2]))]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "switch 'x'"
            , "  case 'foo'"
            , "    echo 'a'"
            , "  case 'bar'"
            , "    echo 'b'"
            , "end"
            ]
      actual @?= expected

  , H.testCase "Function printing (no params)" $ do
      let fn = FishFunction { funcName = "greet", funcFlags = [], funcParams = [], funcBody = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "hi")])] }
          script = [Stmt (Function fn)]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "function greet"
            , "  echo 'hi'"
            , "end"
            ]
      actual @?= expected
  ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ QC.testProperty "Echo literal without single quotes is single-quoted" $ 
      QC.forAll (QC.suchThat QC.arbitrary (not . T.any (== '\''))) $ \t ->
        let out = renderFish [Stmt (Command "echo" [ExprVal (ExprLiteral t)])]
        in T.isInfixOf ("'" <> t <> "'") out

  , QC.testProperty "Pipeline renders N pipes for N continuations" $ \(QC.NonNegative n) ->
      let conts = replicate n (PipeTo [] (Stmt (Command "true" [])))
          pipe = FishJobPipeline False [] (Stmt (Command "true" [])) conts False
          out = renderFish [Stmt (Pipeline pipe)]
      in T.count " | " out == fromIntegral n

  , QC.testProperty "Job conjunction counts and/or tokens" $ \(xs :: [Bool]) ->
      let mkPipe = FishJobPipeline False [] (Stmt (Command "true" [])) [] False
          cont = map (\b -> if b then JCAnd mkPipe else JCOr mkPipe) xs
          conj = FishJobConjunction Nothing mkPipe cont False
          out = renderFish [Stmt (JobConj conj)]
          ands = length (filter id xs)
          ors  = length (filter not xs)
      in T.count "and " out == fromIntegral ands && T.count "or " out == fromIntegral ors

  , QC.testProperty "Rendered pipeline is non-empty" $ QC.forAll genPipeline $ \p ->
      not (T.null (renderFish [Stmt (Pipeline p)]))

  , QC.testProperty "Rendered job conjunction has at least head job" $ QC.forAll genConjunction $ \jc ->
      let out = renderFish [Stmt (JobConj jc)] in not (T.null out)

  , QC.testProperty "If pretty has one end and proper indentation" $ do
      QC.forAll genNonEmptyStmts $ \thn ->
        QC.forAll (QC.listOf (Stmt <$> genStatusCommand)) $ \els ->
          let cmd = Stmt (If (ExprBoolLiteral True) thn els)
              out = renderFish [cmd]
              ls  = T.lines out
              ends = length (filter (== "end") ls)
              indented = all (\l -> T.null l || T.isPrefixOf "  " l || l `elem` ["if true","else","end"]) ls
          in ends == 1 QC..&&. indented

  , QC.testProperty "Switch pretty has N case lines and one end" $ do
      let genCase = do
            pat <- ExprLiteral <$> genTextNoQuote
            body <- genNonEmptyStmts
            pure (CaseItem (pat NE.:| []) body)
      QC.forAll (QC.listOf1 genCase) $ \items ->
        let switchCmd = Stmt (Switch (ExprLiteral "x") (NE.fromList items))
            out = renderFish [switchCmd]
            ls  = T.lines out
            caseCount = length (filter (T.isPrefixOf "  case ") ls)
            ends = length (filter (== "end") ls)
        in caseCount == length items QC..&&. ends == 1

  , QC.testProperty "Function pretty begins with function name and has one end" $ do
      QC.forAll genTextNoQuote $ \nameTxt ->
        QC.forAll genNonEmptyStmts $ \body ->
          let fn = FishFunction { funcName = nameTxt, funcFlags = [], funcParams = [], funcBody = body }
              out = renderFish [Stmt (Function fn)]
              ls  = T.lines out
              headerOK = case ls of
                           (h:_) -> T.isPrefixOf ("function " <> nameTxt) h
                           _     -> False
              ends = length (filter (== "end") ls)
          in headerOK QC..&&. ends == 1
  ]

--------------------------------------------------------------------------------
-- Generators for a subset of the AST (no deep recursion)
--------------------------------------------------------------------------------

genTextNoQuote :: QC.Gen Text
genTextNoQuote = do
  chars <- QC.listOf (QC.suchThat QC.arbitrary (/= '\''))
  pure (T.pack (take 8 chars))

genExprStr :: QC.Gen (FishExpr 'TStr)
genExprStr = ExprLiteral <$> genTextNoQuote

genStatusCommand :: QC.Gen (FishCommand 'TStatus)
genStatusCommand = QC.oneof
  [ pure (Command "true" [])
  , pure (Command "false" [])
  , do t <- genTextNoQuote
       pure (Command "echo" [ExprVal (ExprLiteral t)])
  ]

genPipeline :: QC.Gen FishJobPipeline
genPipeline = do
  headCmd <- genStatusCommand
  k <- QC.chooseInt (0, 3)
  contCmds <- QC.vectorOf k genStatusCommand
  bg <- QC.arbitrary
  let toCont c = PipeTo { jpcVariables = [], jpcStatement = Stmt c }
  pure FishJobPipeline
        { jpTime = False
        , jpVariables = []
        , jpStatement = Stmt headCmd
        , jpCont = map toCont contCmds
        , jpBackgrounded = bg
        }

genConjunction :: QC.Gen FishJobConjunction
genConjunction = do
  headP <- genPipeline
  k <- QC.chooseInt (0, 3)
  bools <- QC.vectorOf k QC.arbitrary
  tails <- QC.vectorOf k genPipeline
  let mk b p = if b then JCAnd p else JCOr p
  semi <- QC.arbitrary
  pure FishJobConjunction
        { jcDecorator = Nothing
        , jcJob = headP
        , jcContinuations = zipWith mk bools tails
        , jcSemiNl = semi
        }
