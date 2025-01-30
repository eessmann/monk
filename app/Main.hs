module Main (main) where

import Clam (prettyFish, projectName, renderFish, parseBashFile, translateRoot, translateToken)
import ShellCheck.Interface
import FishAST

-- | A small AST demonstrating multiple Fish constructs.
exampleAST :: [FishStatement]
exampleAST =
  [ -- 1) A comment
    Comment "Example test script",
    -- 2) A simple command: echo
    Stmt $
      Command "echo" [Arg (ArgLiteral "Hello Fish!")],
    -- 3) An if-statement
    Stmt $
      If
        (ExprBool (BoolLiteral True))
        [Stmt (Command "echo" [Arg (ArgLiteral "In the if!")])]
        [Stmt (Command "echo" [Arg (ArgLiteral "In the else!")])],
    -- 4) A function definition
    Stmt $
      Function $
        FishFunction
          { funcName = "greet",
            funcFlags = [],
            funcParams = [ArgLiteral "name"], -- function greet name
            funcBody =
              [ Stmt $
                  Command
                    "echo"
                    [Arg (ArgLiteral "Hello,"), Arg (ArgVariable @'TStr "name")]
              ]
          },
    -- 5a) A for-loop
    Stmt $
      For @'TStr -- We say For expects TList TStr
        "x"
        (ArgList [ArgLiteral "1", ArgLiteral "2", ArgLiteral "3"])
        [ Stmt
            ( Command
                "echo"
                [ Arg (ArgLiteral "Number:"),
                  Arg (ArgVariable @'TStr "x")
                ]
            )
        ],
    SemiNl,
    -- 5b) A for-loop with variables
    Stmt $
      For @'TStr -- We say For expects TList TStr
        "x"
        (ArgList [ArgVariable @'TStr "var1", ArgVariable @'TStr "var2", ArgVariable @'TStr "var3"])
        [ Stmt
            ( Command
                "echo"
                [ Arg (ArgLiteral "Number:"),
                  Arg (ArgVariable @'TStr "x")
                ]
            )
        ],
    -- 6) A pipeline of commands returning TStatus
    --    We have to wrap "Command" in something returning TStatus, e.g. "Not Command" yields TStatus.
    Stmt $
      Pipeline $
        Not (Command "grep" [Arg (ArgLiteral "something")])
          :| [Not (Command "wc" [Arg (ArgLiteral "-l")])],
    -- 7) A switch statement as another example
    Stmt $
      Switch
        (ArgVariable @'TStr "myvar")
        [ CaseItem
            { casePatterns = [ArgLiteral "foo"],
              caseBody =
                [ Stmt $ Command "echo" [Arg (ArgLiteral "It was foo")]
                ]
            },
          CaseItem
            { casePatterns = [ArgLiteral "bar"],
              caseBody =
                [ Stmt $ Command "echo" [Arg (ArgLiteral "It was bar")]
                ]
            }
        ]
  ]

main :: IO ()
main = do
  putTextLn ("Executable for " <> projectName)
  putTextLn "Reading script:"
  script <- readFileBS "test.sh"
  putBSLn script
  putTextLn "Parsing script:"
  resultE <- parseBashFile "test.sh"
  case resultE of
    Left errs -> putStrLn ("Parse failed: " <> show errs)
    Right parseRes -> do
      print (prRoot parseRes)            -- The AST root
      print (prComments parseRes)        -- Any warnings/comments
      putTextLn "Translating to Fish:"
      let fishAST = do
            bashAST <- prRoot parseRes
            pure (translateToken bashAST)
      putTextLn $ show fishAST

  putTextLn "\nDone!"