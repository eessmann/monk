{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.ProcessSubst
  ( procSubExpr,
    procSubExprM,
    procSubListExpr,
    procSubListExprM,
    procSubOutList,
    procSubOutRedirectCommand,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.Pretty (renderFish)
import Language.Fish.Translator.Monad
  ( HelperId (..),
    TranslateM,
    WarningCode (..),
    ensureHelper,
    unsupported,
  )
import Language.Fish.Translator.Pipeline (jobPipelineFromList)
import Language.Fish.Translator.Syntax

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

procSubExprM :: String -> NonEmpty FishStatement -> TranslateM (FishExpr TStr)
procSubExprM dir body =
  case dir of
    "<" -> pure (ExprProcessSubst body)
    ">" -> do
      unsupported ProcessSubstitutionIssue (Just outputProcessSubstitutionWarning)
      ensureProcSubOutHelper $> ExprJoinList (procSubOutHelperList body)
    _ -> pure (ExprProcessSubst body)

procSubListExprM :: String -> NonEmpty FishStatement -> TranslateM (FishExpr (TList TStr))
procSubListExprM dir body =
  case dir of
    "<" -> pure (ExprListLiteral [ExprProcessSubst body])
    ">" -> do
      unsupported ProcessSubstitutionIssue (Just outputProcessSubstitutionWarning)
      ensureProcSubOutHelper $> procSubOutHelperList body
    _ -> pure (ExprListLiteral [ExprProcessSubst body])

outputProcessSubstitutionWarning :: Text
outputProcessSubstitutionWarning =
  "output process substitution in argument position requires manual review"

procSubDirVar :: Text
procSubDirVar = "__monk_psub_dir"

procSubFifoVar :: Text
procSubFifoVar = "__monk_psub_fifo"

procSubFileVar :: Text
procSubFileVar = "__monk_psub_file"

procSubStatusVar :: Text
procSubStatusVar = "__monk_psub_status"

procSubOutList :: NonEmpty FishStatement -> FishExpr (TList TStr)
procSubOutList body =
  ExprCommandSubst
    ( procSubSetDirStmt
        NE.:| [ procSubSetFifoStmt,
                procSubMkfifoStmt,
                procSubBackgroundStmt (procSubBodyStmt body) [procSubRmFifoStmt, procSubRmdirStmt],
                procSubEchoFifoStmt
              ]
    )

procSubOutHelperList :: NonEmpty FishStatement -> FishExpr (TList TStr)
procSubOutHelperList body =
  ExprCommandSubst
    ( Stmt
        ( Command
            "__monk_procsub_out"
            [ExprVal (ExprLiteral (renderFish (NE.toList body)))]
        )
        NE.:| []
    )

procSubOutRedirectCommand :: FishCommand TStatus -> FishStatement -> FishCommand TStatus
procSubOutRedirectCommand producer consumer =
  Begin
    ( procSubSetDirStmt
        NE.:| [ procSubSetFileStmt,
                procSubProducerFileStmt producer,
                procSubCaptureStatusStmt,
                procSubPipelineFileStmt consumer,
                procSubRmFileStmt,
                procSubRmdirStmt,
                procSubReturnStatusStmt
              ]
    )
    []

ensureProcSubOutHelper :: TranslateM ()
ensureProcSubOutHelper =
  ensureHelper HelperProcSubOut [procSubOutHelperStmt]

procSubOutHelperStmt :: FishStatement
procSubOutHelperStmt =
  Stmt
    ( Function
        MkFishFunction
          { funcName = "__monk_procsub_out",
            funcFlags = [],
            funcParams = ["body"],
            funcBody =
              procSubSetDirStmt
                NE.:| [ procSubSetFifoStmt,
                        procSubRmFifoStmt,
                        procSubMkfifoStmt,
                        procSubBackgroundStmt
                          procSubEvalBodyStmt
                          [ procSubCaptureStatusStmt,
                            procSubRmFifoStmt,
                            procSubRmdirStmt,
                            procSubReturnStatusStmt
                          ],
                        procSubPrintFifoStmt
                      ]
          }
    )

procSubSetDirStmt :: FishStatement
procSubSetDirStmt =
  Stmt
    ( Set
        [SetLocal]
        procSubDirVar
        (ExprCommandSubst (Stmt (Command "mktemp" [ExprVal (ExprLiteral "-d")]) NE.:| []))
    )

procSubSetFifoStmt :: FishStatement
procSubSetFifoStmt =
  Stmt
    ( Set
        [SetLocal]
        procSubFifoVar
        procSubFifoPathExpr
    )

procSubFifoPathExpr :: FishExpr (TList TStr)
procSubFifoPathExpr =
  ExprListLiteral
    [ ExprStringConcat
        (ExprVariable (VarScalar procSubDirVar))
        (ExprLiteral "/fifo")
    ]

procSubSetFileStmt :: FishStatement
procSubSetFileStmt =
  Stmt
    ( Set
        [SetLocal]
        procSubFileVar
        procSubFilePathExpr
    )

procSubFilePathExpr :: FishExpr (TList TStr)
procSubFilePathExpr =
  ExprListLiteral
    [ ExprStringConcat
        (ExprVariable (VarScalar procSubDirVar))
        (ExprLiteral "/stdout")
    ]

procSubRmFifoStmt :: FishStatement
procSubRmFifoStmt =
  Stmt
    ( Command
        "rm"
        [ ExprVal (ExprLiteral "-f"),
          ExprVal (ExprVariable (VarScalar procSubFifoVar))
        ]
    )

procSubRmFileStmt :: FishStatement
procSubRmFileStmt =
  Stmt
    ( Command
        "rm"
        [ ExprVal (ExprLiteral "-f"),
          ExprVal (ExprVariable (VarScalar procSubFileVar))
        ]
    )

procSubRmdirStmt :: FishStatement
procSubRmdirStmt =
  Stmt
    ( Command
        "rmdir"
        [ExprVal (ExprVariable (VarScalar procSubDirVar))]
    )

procSubMkfifoStmt :: FishStatement
procSubMkfifoStmt =
  Stmt
    ( Command
        "mkfifo"
        [ExprVal (ExprVariable (VarScalar procSubFifoVar))]
    )

procSubBodyStmt :: NonEmpty FishStatement -> FishStatement
procSubBodyStmt body =
  case NE.toList body of
    [stmt] -> stmt
    stmts -> Stmt (Begin (NE.fromList stmts) [])

procSubInputRedirect :: ExprOrRedirect
procSubInputRedirect =
  RedirectVal
    ( MkRedirect
        RedirectStdin
        RedirectIn
        (RedirectFile (ExprVariable (VarScalar procSubFifoVar)))
    )

procSubFileOutputRedirect :: ExprOrRedirect
procSubFileOutputRedirect =
  RedirectVal
    ( MkRedirect
        RedirectStdout
        RedirectOut
        (RedirectFile (ExprVariable (VarScalar procSubFileVar)))
    )

procSubProducerFileStmt :: FishCommand TStatus -> FishStatement
procSubProducerFileStmt =
  Stmt . attachRedirectsToCommand [procSubFileOutputRedirect]

procSubCatFileCommand :: FishCommand TStatus
procSubCatFileCommand =
  Command
    "cat"
    [ExprVal (ExprVariable (VarScalar procSubFileVar))]

procSubPipelineFileStmt :: FishStatement -> FishStatement
procSubPipelineFileStmt consumer =
  let pipe =
        (jobPipelineFromList (procSubCatFileCommand NE.:| []))
          { jpCont = [PipeTo [] consumer]
          }
   in Stmt (Pipeline pipe)

procSubConsumerStmt :: FishStatement -> FishStatement
procSubConsumerStmt = attachRedirectsToStatement [procSubInputRedirect]

procSubBackgroundStmt :: FishStatement -> [FishStatement] -> FishStatement
procSubBackgroundStmt rhsStmt cleanupStmts =
  Stmt
    ( Background
        ( Begin
            (procSubConsumerStmt rhsStmt NE.:| cleanupStmts)
            []
        )
    )

procSubEvalBodyStmt :: FishStatement
procSubEvalBodyStmt =
  Stmt (Command "eval" [ExprVal (ExprVariable (VarScalar "body"))])

procSubCaptureStatusStmt :: FishStatement
procSubCaptureStatusStmt =
  Stmt
    ( Set
        [SetLocal]
        procSubStatusVar
        ( ExprCommandSubst
            ( Stmt
                ( Command
                    "printf"
                    [ ExprVal (ExprLiteral "%s"),
                      ExprVal (ExprSpecialVar SVStatus)
                    ]
                )
                NE.:| []
            )
        )
    )

procSubReturnStatusStmt :: FishStatement
procSubReturnStatusStmt =
  Stmt
    ( Command
        "fish"
        [ ExprVal (ExprLiteral "--no-config"),
          ExprVal (ExprLiteral "-c"),
          ExprVal (ExprLiteral "exit $argv[1]"),
          ExprVal (ExprVariable (VarScalar procSubStatusVar))
        ]
    )

procSubEchoFifoStmt :: FishStatement
procSubEchoFifoStmt =
  Stmt
    ( Command
        "echo"
        [ExprVal (ExprVariable (VarScalar procSubFifoVar))]
    )

procSubPrintFifoStmt :: FishStatement
procSubPrintFifoStmt =
  Stmt
    ( Printf
        (ExprLiteral "%s\n")
        [ExprVariable (VarScalar procSubFifoVar)]
    )
