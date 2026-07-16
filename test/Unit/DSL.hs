{-# LANGUAGE NoImplicitPrelude #-}

module Unit.DSL
  ( unitDslTests,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.DSL
import Relude hiding (stdout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

unitDslTests :: TestTree
unitDslTests =
  testGroup
    "Fish DSL"
    [ testCase "lowers command args and redirects through the raw renderer"
        $ renderDsl
          ( script
              [ stmt (command "echo" [arg (str "hello"), redirect stdout overwrite (fileTarget (str "out.txt"))])
              ]
          )
        @?= "echo 'hello' > 'out.txt'",
      testCase "lowers non-empty pipelines without admitting empty stage lists"
        $ renderDsl
          ( script
              [ stmt
                  $ pipeline
                    ( stage (command "printf" [arg (str "%s\\n"), arg (str "hello")])
                        NE.:| [stage (command "string" [arg (str "upper")])]
                    )
              ]
          )
        @?= "printf \"%s\\\\n\" 'hello' | string 'upper'",
      testCase "lowers blocks with non-empty bodies"
        $ renderDsl
          ( script
              [ stmt
                  $ begin
                    ( stmt (command "echo" [arg (str "inside")])
                        NE.:| []
                    )
              ]
          )
        @?= "begin\n  echo 'inside'\nend",
      testCase "lowers typed conditionals and redirects"
        $ renderDsl
          ( script
              [ stmt
                  $ if_
                    (condition (command "test" [arg (str "-n"), arg (var "name")]))
                    (block (stmt (command "echo" [arg (str "then")]) NE.:| []))
                    [stmt (command "echo" [arg (str "else")])]
                    [redirect stdout overwrite (fileTarget (str "out.txt"))]
              ]
          )
        @?= "if test '-n' $name\n  echo 'then'\nelse\n  echo 'else'\nend > 'out.txt'",
      testCase "lowers typed loops, switch cases, and functions"
        $ renderDsl
          ( script
              [ stmt
                  $ for
                    "item"
                    (list [str "a", str "b"])
                    (block (stmt (command "echo" [arg (var "item")]) NE.:| []))
                    [],
                stmt
                  $ switch
                    (var "item")
                    ( caseItem
                        (str "a" NE.:| [])
                        (block (stmt (command "echo" [arg (str "alpha")]) NE.:| []))
                        NE.:| []
                    )
                    [],
                stmt
                  $ function
                    "say_hi"
                    []
                    []
                    (block (stmt (command "echo" [arg (str "hi")]) NE.:| []))
              ]
          )
        @?= "for item in 'a' 'b'\n  echo $item\nend\nswitch $item\n  case 'a'\n    echo 'alpha'\nend\nfunction say_hi\n  echo 'hi'\nend",
      testCase "lowers typed job conjunctions"
        $ renderDsl
          ( script
              [ stmt
                  $ job
                    ( jobConjunction
                        Nothing
                        (pipelineValue (stage (command "false" []) NE.:| []))
                        [orElse (pipelineValue (stage (command "echo" [arg (str "ok")]) NE.:| []))]
                    )
              ]
          )
        @?= "false \nor echo 'ok'"
    ]

renderDsl :: Script -> Text
renderDsl = renderScript
