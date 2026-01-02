{-# LANGUAGE OverloadedStrings #-}

module Unit.Pretty
  ( unitPrettyTests
  ) where

import Monk
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import TestSupport

unitPrettyTests :: TestTree
unitPrettyTests = testGroup "Pretty printing"
  [ H.testCase "Redirection embedding" $ do
      let script =
            [ Stmt (Command "echo"
                [ ExprVal (ExprLiteral "Hello")
                , RedirectVal (Redirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null")))
                ])
            ]
          actual = renderFish script
          expected = "echo 'Hello' > '/dev/null'"
      actual @?= expected

  , H.testCase "Redirect stdout+stderr to file" $ do
      let script =
            [ Stmt (Command "echo"
                [ ExprVal (ExprLiteral "Hello")
                , RedirectVal (Redirect RedirectBoth RedirectOut (RedirectFile (ExprLiteral "/tmp/out")))
                ])
            ]
          actual = renderFish script
          expected = "echo 'Hello' > '/tmp/out' 2>&1"
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
      let script = [ Stmt (Command "ls" [ExprVal (ExprGlob (GlobPattern [GlobBraces ("a" NE.:| ["b"])]))]) ]
          actual = renderFish script
      T.isInfixOf "ls {" actual H.@? "must contain brace glob"

  , H.testCase "Process substitution" $ do
      let inner = Stmt (Command "echo" [ExprVal (ExprLiteral "x")])
          script = [ Stmt (Command "cat" [ExprVal (ExprProcessSubst (inner NE.:| []))]) ]
          actual = renderFish script
      T.isInfixOf "cat (" actual H.@? "must begin with cat ("
      T.isInfixOf "| psub)" actual H.@? "must pipe to psub"

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
          conj = FishJobConjunction Nothing job1 [JCOr job2]
          script = [ Stmt (JobConj conj) ]
          actual = renderFish script
          expected = "false \nor echo 'ok'"
      actual @?= expected

  , H.testCase "Begin block" $ do
      let body = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "A")])]
          script = [Stmt (Begin body [])]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "begin"
            , "  echo 'A'"
            , "end"
            ]
      actual @?= expected

  , H.testCase "Begin block with redirect" $ do
      let body = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "B")])]
          script =
            [Stmt (Begin body [RedirectVal (Redirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null")))])]
          actual = renderFish script
          expected = T.intercalate "\n"
            [ "begin"
            , "  echo 'B'"
            , "end > '/dev/null'"
            ]
      actual @?= expected

  , H.testCase "If then else" $ do
      let thn = NE.fromList [Stmt (Command "echo" [ExprVal (ExprLiteral "then")])]
          els = [Stmt (Command "echo" [ExprVal (ExprLiteral "else")])]
          script = [Stmt (If trueCond thn els [])]
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
          script = [Stmt (Switch (ExprLiteral "x") (case1 NE.:| [case2]) [])]
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
