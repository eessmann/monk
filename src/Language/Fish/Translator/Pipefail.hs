{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Pipefail
  ( ensurePipefailHelper,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.Translator.Monad
  ( HelperId (..),
    TranslateM,
    ensureHelper,
  )
import Language.Fish.Translator.Syntax

ensurePipefailHelper :: TranslateM ()
ensurePipefailHelper =
  ensureHelper HelperPipefail [pipefailHelper]

pipefailHelper :: FishStatement
pipefailHelper =
  let statusVar = "__monk_pipe_status"
      statusInit =
        Stmt
          ( Set
              [SetLocal]
              statusVar
              (ExprListLiteral [ExprLiteral "0"])
          )
      testCmd =
        Command
          "test"
          [ ExprVal (ExprVariable (VarAll "s")),
            ExprVal (ExprLiteral "-ne"),
            ExprVal (ExprLiteral "0")
          ]
      cond =
        MkFishJobList
          ( MkFishJobConjunction
              Nothing
              (MkFishJobPipeline False [] (Stmt testCmd) [] False)
              []
              NE.:| []
          )
      setStatus =
        Stmt
          ( Set
              []
              statusVar
              (ExprVariable (VarAll "s"))
          )
      ifStmt = Stmt (If cond (setStatus NE.:| []) [] [])
      forStmt =
        Stmt
          ( For
              "s"
              (ExprVariable (VarAll "argv"))
              (ifStmt NE.:| [])
              []
          )
      returnStmt =
        Stmt
          ( Command
              "return"
              [ExprVal (ExprVariable (VarAll statusVar))]
          )
      body = statusInit NE.:| [forStmt, returnStmt]
   in Stmt
        ( Function
            MkFishFunction
              { funcName = "__monk_pipefail",
                funcFlags = [],
                funcParams = [],
                funcBody = body
              }
        )
