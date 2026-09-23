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
          (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] (Stmt (Command "true" [])) []) [] :| []))
          (Stmt (Command "echo" [ExprVal (ExprLiteral "In the if!")]) :| [])
          [Stmt (Command "echo" [ExprVal (ExprLiteral "In the else!")])]
          []
      ),
    Stmt
      ( Function
          MkFishFunction
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
          JobPipeline
            { jpTime = False,
              jpVariables = [],
              jpStatement = MkStage (Command "grep" [ExprVal (ExprLiteral "something")]),
              jpCont = [PipeToStage {jpcVariables = [], jpcStatement = MkStage (Command "wc" [ExprVal (ExprLiteral "-l")])}]
            }
      ),
    Stmt
      ( Switch
          (ExprJoinList (ExprVariable (VarAll "myvar")))
          ( MkCaseItem
              { casePatterns = ExprLiteral "foo" :| [],
                caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was foo")]) :| []
              }
              :| [ MkCaseItem
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
          [FileRedirect 1 OverwriteFile (ExprLiteral "/dev/null")]
      )
  ]
