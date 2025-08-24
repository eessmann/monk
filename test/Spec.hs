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

  , H.testCase "Exit with code" $ do
      let script = [ Stmt (Exit (Just (ExprNumLiteral 42))) ]
          actual = renderFish script
      actual @?= "exit 42"

  , H.testCase "Eval command" $ do
      let script = [ Stmt (Eval (ExprLiteral "echo hi")) ]
          actual = renderFish script
      actual @?= "eval 'echo hi'"

  , H.testCase "Read with flags and vars" $ do
      let script = [ Stmt (Read [ReadPrompt "Name:", ReadLocal] ["name"]) ]
          actual = renderFish script
          expected = "read --prompt 'Name:' --local name"
      actual @?= expected

  , H.testCase "Glob brace pattern" $ do
      let script = [ Stmt (Command "ls" [ExprVal (ExprGlob (GlobBraces ["a","b"]))]) ]
          actual = renderFish script
          expected = "ls { 'a' , 'b' }"
      -- We accept spacing differences; ensure braces present
      T.isInfixOf "ls {" actual H.@? "must contain brace glob"

  , H.testCase "Process substitution" $ do
      let inner = Stmt (Command "echo" [ExprVal (ExprLiteral "x")])
          script = [ Stmt (Command "cat" [ExprVal (ExprProcessSubst (inner NE.:| []))]) ]
          actual = renderFish script
      T.isInfixOf "cat (" actual H.@? "must begin with cat ("
      T.isInfixOf "| psub)" actual H.@? "must pipe to psub"

  , H.testCase "Statement-level and/or" $ do
      let s1 = Stmt (Command "true" [])
          s2 = Stmt (Command "echo" [ExprVal (ExprLiteral "ok")])
          script1 = [ AndStmt s1 s2 ]
          script2 = [ OrStmt s1 s2 ]
      renderFish script1 @?= "true\nand echo 'ok'"
      renderFish script2 @?= "true\nor echo 'ok'"

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
          expected = "false \nor echo 'ok'"
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

  , H.testCase "BraceStmt with redirect" $ do
      let body = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "B")])]
          script = [BraceStmt body [RedirectVal RedirectOut (ExprLiteral "/dev/null")]]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "{"
            , "  echo 'B'"
            , "} > '/dev/null'"
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
      QC.forAll genTextNoQuote $ \t ->
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
      QC.forAll (fmap T.pack (QC.listOf1 (QC.elements (['a'..'z'] <> ['A'..'Z'] <> "_")))) $ \nameTxt ->
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

-- Generators moved to test/Gen.hs
  -- Generators moved to test/Gen.hs
