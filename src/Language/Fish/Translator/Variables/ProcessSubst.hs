{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.ProcessSubst
  ( procSubExpr,
    procSubListExpr,
    procSubOutList,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST

procSubExpr :: String -> NonEmpty FishStatement -> FishExpr TStr
procSubExpr dir body =
  case dir of
    "<" -> ExprProcessSubst body
    ">" -> ExprJoinList (procSubOutList body)
    _ -> ExprProcessSubst body

procSubListExpr :: String -> NonEmpty FishStatement -> FishExpr (TList TStr)
procSubListExpr dir body =
  case dir of
    "<" -> ExprListLiteral [ExprProcessSubst body]
    ">" -> procSubOutList body
    _ -> ExprListLiteral [ExprProcessSubst body]

procSubOutList :: NonEmpty FishStatement -> FishExpr (TList TStr)
procSubOutList body =
  let dirVar = "__monk_psub_dir"
      fifoVar = "__monk_psub_fifo"
      mktempStmt =
        Stmt
          ( Set
              [SetLocal]
              dirVar
              ( ExprCommandSubst
                  ( Stmt
                      ( Command
                          "mktemp"
                          [ExprVal (ExprLiteral "-d")]
                      )
                      NE.:| []
                  )
              )
          )
      fifoPath =
        ExprListLiteral
          [ ExprStringConcat
              (ExprVariable (VarScalar dirVar))
              (ExprLiteral "/fifo")
          ]
      setFifoStmt =
        Stmt
          ( Set
              [SetLocal]
              fifoVar
              fifoPath
          )
      rmFifoStmt =
        Stmt
          ( Command
              "rm"
              [ ExprVal (ExprLiteral "-f"),
                ExprVal (ExprVariable (VarScalar fifoVar))
              ]
          )
      rmdirStmt =
        Stmt
          ( Command
              "rmdir"
              [ExprVal (ExprVariable (VarScalar dirVar))]
          )
      mkfifoStmt =
        Stmt
          ( Command
              "mkfifo"
              [ExprVal (ExprVariable (VarScalar fifoVar))]
          )
      catStmt =
        Stmt
          ( Command
              "cat"
              [ExprVal (ExprVariable (VarScalar fifoVar))]
          )
      rhsStmt = case NE.toList body of
        [s] -> s
        xs -> Stmt (Begin (NE.fromList xs) [])
      pipe = FishJobPipeline False [] catStmt [PipeTo [] rhsStmt] False
      pipeStmt = Stmt (Pipeline pipe)
      consumerBody =
        pipeStmt NE.:| [rmFifoStmt, rmdirStmt]
      bgStmt =
        Stmt
          ( Background
              ( Begin consumerBody []
              )
          )
      echoStmt =
        Stmt
          ( Command
              "echo"
              [ExprVal (ExprVariable (VarScalar fifoVar))]
          )
   in ExprCommandSubst (mktempStmt NE.:| [setFifoStmt, mkfifoStmt, bgStmt, echoStmt])
