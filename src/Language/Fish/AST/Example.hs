module Language.Fish.AST.Example
  ( exampleAST,
  )
where

import Language.Fish.AST.Types

-- | Example AST.
exampleAST :: [FishStatement]
exampleAST =
  [ Comment "Example test script",
    Stmt (Command "echo" [ExprVal (ExprLiteral "Hello Fish!")]),
    Stmt
      ( If
          (FishJobList (FishJobConjunction Nothing (FishJobPipeline False [] (Stmt (Command "true" [])) [] False) [] :| []))
          (Stmt (Command "echo" [ExprVal (ExprLiteral "In the if!")]) :| [])
          [Stmt (Command "echo" [ExprVal (ExprLiteral "In the else!")])]
          []
      ),
    Stmt
      ( Function
          FishFunction
            { funcName = "greet",
              funcFlags = [],
              funcParams = ["name"],
              funcBody =
                Stmt
                  ( Command
                      "echo"
                      [ ExprVal (ExprLiteral "Hello,"),
                        ExprVal (ExprVariable (VarIndex "name" (IndexSingle (ExprNumLiteral 1))))
                      ]
                  )
                  :| []
            }
      ),
    Stmt
      ( For
          "x"
          (ExprListLiteral [ExprLiteral "1", ExprLiteral "2", ExprLiteral "3"])
          ( Stmt
              ( Command
                  "echo"
                  [ ExprVal (ExprLiteral "Number:"),
                    ExprVal (ExprVariable (VarAll "x"))
                  ]
              )
              :| []
          )
          []
      ),
    Stmt
      ( For
          "x"
          ( ExprListConcat
              (ExprVariable (VarAll "var1"))
              (ExprListConcat (ExprVariable (VarAll "var2")) (ExprVariable (VarAll "var3")))
          )
          ( Stmt
              ( Command
                  "echo"
                  [ ExprVal (ExprLiteral "Variable:"),
                    ExprVal (ExprVariable (VarAll "x"))
                  ]
              )
              :| []
          )
          []
      ),
    Stmt
      ( Pipeline
          FishJobPipeline
            { jpTime = False,
              jpVariables = [],
              jpStatement = Stmt (Command "grep" [ExprVal (ExprLiteral "something")]),
              jpCont = [PipeTo {jpcVariables = [], jpcStatement = Stmt (Command "wc" [ExprVal (ExprLiteral "-l")])}],
              jpBackgrounded = False
            }
      ),
    Stmt
      ( Switch
          (ExprJoinList (ExprVariable (VarAll "myvar")))
          ( CaseItem
              { casePatterns = ExprLiteral "foo" :| [],
                caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was foo")]) :| []
              }
              :| [ CaseItem
                     { casePatterns = ExprLiteral "bar" :| [ExprLiteral "baz"],
                       caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was bar or baz")]) :| []
                     }
                 ]
          )
          []
      ),
    Stmt
      ( Begin
          (Stmt (Command "echo" [ExprVal (ExprLiteral "brace body")]) :| [])
          [RedirectVal (Redirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null")))]
      )
  ]
