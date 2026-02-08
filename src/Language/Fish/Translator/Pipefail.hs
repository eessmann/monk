{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Pipefail
  ( ensurePipefailHelper,
  )
where

import Prelude hiding (get, modify)
import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslateM, TranslateState (..))
import Polysemy.State (get, modify)

ensurePipefailHelper :: TranslateM ()
ensurePipefailHelper = do
  st <- get
  if pipefailHelperAdded st
    then pure ()
    else modify (\s -> s {pipefailHelperAdded = True, preamble = preamble s <> [pipefailHelper]})

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
        FishJobList
          ( FishJobConjunction
              Nothing
              (FishJobPipeline False [] (Stmt testCmd) [] False)
              []
              NE.:| []
          )
      setStatus =
        Stmt
          ( Set
              [SetLocal]
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
          ( Return
              (Just (ExprMath (ExprVariable (VarScalar statusVar) NE.:| [])))
          )
      body = statusInit NE.:| [forStmt, returnStmt]
   in Stmt
        ( Function
            FishFunction
              { funcName = "__monk_pipefail",
                funcFlags = [],
                funcParams = [],
                funcBody = body
              }
        )
