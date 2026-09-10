{-# LANGUAGE OverloadedStrings #-}

module Unit.Pretty
  ( unitPrettyTests,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Monk.AST hiding (stdout)
import Monk.AST qualified as AST
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import TestSupport

unitPrettyTests :: TestTree
unitPrettyTests =
  testGroup
    "Pretty printing"
    [ H.testCase "Redirection embedding" $ do
        let fishScript =
              script
                [ stmt
                    ( command
                        "echo"
                        [ arg (str "Hello"),
                          redirect AST.stdout overwrite (fileTarget (str "/dev/null"))
                        ]
                    )
                ]
            actual = renderDsl fishScript
            expected = "echo 'Hello' > '/dev/null'"
        actual @?= expected,
      H.testCase "Redirect stdout+stderr to file" $ do
        let fishScript =
              script
                [ stmt
                    ( command
                        "echo"
                        [ arg (str "Hello"),
                          redirect both overwrite (fileTarget (str "/tmp/out"))
                        ]
                    )
                ]
            actual = renderDsl fishScript
            expected = "echo 'Hello' > '/tmp/out' 2>&1"
        actual @?= expected,
      H.testCase "Exit with code" $ do
        let fishScript = script [stmt (exit (Just (int 42)))]
            actual = renderDsl fishScript
        actual @?= "exit 42",
      H.testCase "Eval command" $ do
        let fishScript = script [stmt (eval (str "echo hi"))]
            actual = renderDsl fishScript
        actual @?= "eval 'echo hi'",
      H.testCase "Backslash-terminated literals remain valid Fish" $ do
        let fishScript = script [stmt (command "echo" [arg (str "\\\\[AMD\\")])]
            actual = renderDsl fishScript
        actual @?= "echo \"\\\\\\\\[AMD\\\\\"",
      H.testCase "Multiline command substitutions use bounded indentation" $ do
        let inner =
              stmt (command "first" [])
                NE.:| [stmt (command "second" [])]
            fishScript = script [stmt (command "very_long_command_name" [arg (commandSubst inner)])]
            actual = renderDsl fishScript
        actual @?= "very_long_command_name (first\n  second)",
      H.testCase "Read with flags and vars" $ do
        let fishScript = script [stmt (read_ [ReadPrompt "Name:", ReadLocal] ["name"])]
            actual = renderDsl fishScript
            expected = "read --prompt 'Name:' --local name"
        actual @?= expected,
      H.testCase "Glob brace pattern" $ do
        let fishScript =
              script
                [ stmt
                    ( command
                        "ls"
                        [arg (glob (MkGlobPattern [GlobBraces ("a" NE.:| ["b"])]))]
                    )
                ]
            actual = renderDsl fishScript
        T.isInfixOf "ls {" actual H.@? "must contain brace glob",
      H.testCase "Process substitution" $ do
        let inner = stmt (command "echo" [arg (str "x")])
            fishScript = script [stmt (command "cat" [arg (processSubst (inner NE.:| []))])]
            actual = renderDsl fishScript
        T.isInfixOf "cat (" actual H.@? "must begin with cat ("
        T.isInfixOf "| psub)" actual H.@? "must pipe to psub",
      H.testCase "Simple pipeline" $ do
        let fishScript =
              script
                [ stmt
                    ( pipeline
                        ( stage (command "grep" [arg (str "foo")])
                            NE.:| [stage (command "wc" [arg (str "-l")])]
                        )
                    )
                ]
            actual = renderDsl fishScript
            expected = "grep 'foo' | wc '-l'"
        actual @?= expected,
      H.testCase "Job conjunction (or)" $ do
        let job1 = pipelineValue (stage (command "false" []) NE.:| [])
            job2 = pipelineValue (stage (command "echo" [arg (str "ok")]) NE.:| [])
            conj = jobConjunction Nothing job1 [orElse job2]
            fishScript = script [stmt (job conj)]
            actual = renderDsl fishScript
            expected = "false\nor echo 'ok'"
        actual @?= expected,
      H.testCase "Begin block" $ do
        let body = NE.fromList [stmt (command "echo" [arg (str "A")])]
            fishScript = script [stmt (begin body)]
            actual = renderDsl fishScript
            expected =
              T.intercalate
                "\n"
                [ "begin",
                  "  echo 'A'",
                  "end"
                ]
        actual @?= expected,
      H.testCase "Begin block with redirect" $ do
        let body = NE.fromList [stmt (command "echo" [arg (str "B")])]
            fishScript =
              script
                [ stmt
                    ( beginWithRedirects
                        body
                        [redirect AST.stdout overwrite (fileTarget (str "/dev/null"))]
                    )
                ]
            actual = renderDsl fishScript
            expected =
              T.intercalate
                "\n"
                [ "begin",
                  "  echo 'B'",
                  "end > '/dev/null'"
                ]
        actual @?= expected,
      H.testCase "If then else" $ do
        let thn = NE.fromList [stmt (command "echo" [arg (str "then")])]
            els = [stmt (command "echo" [arg (str "else")])]
            fishScript = script [stmt (if_ trueCond (block thn) els [])]
            actual = renderDsl fishScript
            expected =
              T.intercalate
                "\n"
                [ "if true",
                  "  echo 'then'",
                  "else",
                  "  echo 'else'",
                  "end"
                ]
        actual @?= expected,
      H.testCase "Switch with two cases" $ do
        let case1 = caseItem (str "foo" NE.:| []) (block (NE.fromList [stmt (command "echo" [arg (str "a")])]))
            case2 = caseItem (str "bar" NE.:| []) (block (NE.fromList [stmt (command "echo" [arg (str "b")])]))
            fishScript = script [stmt (switch (str "x") (case1 NE.:| [case2]) [])]
            actual = renderDsl fishScript
            expected =
              T.intercalate
                "\n"
                [ "switch 'x'",
                  "  case 'foo'",
                  "    echo 'a'",
                  "  case 'bar'",
                  "    echo 'b'",
                  "end"
                ]
        actual @?= expected,
      H.testCase "Join list fallback" $ do
        let fishScript = script [stmt (command "echo" [arg (joinList (vars "x"))])]
            actual = renderDsl fishScript
            expected = "echo (string join ' ' -- $x ; or printf '')"
        actual @?= expected,
      H.testCase "Function printing (no params)" $ do
        let body = block (NE.fromList [stmt (command "echo" [arg (str "hi")])])
            fishScript = script [stmt (function "greet" [] [] body)]
            actual = renderDsl fishScript
            expected =
              T.intercalate
                "\n"
                [ "function greet",
                  "  echo 'hi'",
                  "end"
                ]
        actual @?= expected
    ]

renderDsl :: Script -> Text
renderDsl = renderScript
