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
  let fifoVar = "__monk_psub_fifo"
      mktempStmt =
        Stmt
          ( Set
              [SetLocal]
              fifoVar
              ( ExprCommandSubst
                  ( Stmt
                      ( Command
                          "mktemp"
                          [ ExprVal (ExprLiteral "-t"),
                            ExprVal (ExprLiteral "monk_psub")
                          ]
                      )
                      NE.:| []
                  )
              )
          )
      rmStmt = Stmt (Command "rm" [ExprVal (ExprVariable (VarAll fifoVar))])
      mkfifoStmt = Stmt (Command "mkfifo" [ExprVal (ExprVariable (VarAll fifoVar))])
      catStmt = Stmt (Command "cat" [ExprVal (ExprVariable (VarAll fifoVar))])
      rhsStmt = case NE.toList body of
        [s] -> s
        xs -> Stmt (Begin (NE.fromList xs) [])
      pipe = FishJobPipeline False [] catStmt [PipeTo [] rhsStmt] False
      pipeStmt = Stmt (Pipeline pipe)
      bgBody = pipeStmt NE.:| [rmStmt]
      bgStmt = Stmt (Background (Begin bgBody []))
      echoStmt = Stmt (Command "echo" [ExprVal (ExprVariable (VarAll fifoVar))])
   in ExprCommandSubst (mktempStmt NE.:| [rmStmt, mkfifoStmt, bgStmt, echoStmt])
