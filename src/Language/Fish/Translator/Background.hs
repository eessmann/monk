{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Background
  ( ensureBackgroundRuntime,
    noteBackgroundTracking,
    instrumentBackgroundStatusCmd,
    translateWaitArgs,
    translateWaitArgsM,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Args (Arg, renderArgs)
import Language.Fish.Translator.Monad
  ( TranslateM,
    HelperId (..),
    WarningCode (..),
    addWarningOnce,
    ensureHelper,
  )

ensureBackgroundRuntime :: TranslateM ()
ensureBackgroundRuntime =
  ensureHelper HelperBackground backgroundRuntimeStatements

noteBackgroundTracking :: TranslateM ()
noteBackgroundTracking = do
  ensureBackgroundRuntime
  addWarningOnce BackgroundTracking Nothing

instrumentBackgroundStatusCmd :: FishCommand TStatus -> TranslateM FishStatement
instrumentBackgroundStatusCmd cmd = do
  ensureBackgroundRuntime
  pure (Stmt (Begin backgroundPrelude []))
  where
    backgroundPrelude =
      incSeqStmt
        NE.:| [ setLastJobStmt,
                appendTrackedJobStmt,
                setTokenStmt,
                setStatusFileStmt,
                removeStaleStatusStmt,
                Stmt (Background (Begin backgroundBody []))
              ]

    backgroundBody =
      Stmt cmd
        NE.:| [ captureStatusStmt,
                writeStatusStmt
              ]

translateWaitArgs :: [Arg] -> FishCommand TStatus
translateWaitArgs args = Command "__monk_wait" (renderArgs args)

translateWaitArgsM :: [Arg] -> TranslateM (FishCommand TStatus)
translateWaitArgsM args = do
  noteBackgroundTracking
  pure (translateWaitArgs args)

backgroundRuntimeStatements :: [FishStatement]
backgroundRuntimeStatements =
  [ initBgDirStmt,
    initBgSeqStmt,
    initBgJobsStmt,
    initLastJobStmt,
    statusPathHelperStmt,
    waitHelperStmt
  ]

initBgDirStmt :: FishStatement
initBgDirStmt =
  Stmt
    ( Set
        [SetGlobal]
        "__monk_bg_dir"
        (ExprCommandSubst (Stmt mktempCmd NE.:| []))
    )
  where
    mktempCmd = Command "mktemp" [ExprVal (ExprLiteral "-d")]

initBgSeqStmt :: FishStatement
initBgSeqStmt =
  Stmt
    ( Set
        [SetGlobal]
        "__monk_bg_seq"
        (ExprListLiteral [ExprLiteral "0"])
    )

initBgJobsStmt :: FishStatement
initBgJobsStmt = Stmt (Set [SetGlobal] "__monk_bg_jobs" (ExprListLiteral []))

initLastJobStmt :: FishStatement
initLastJobStmt = Stmt (Set [SetGlobal] "__monk_last_job" (ExprListLiteral []))

statusPathHelperStmt :: FishStatement
statusPathHelperStmt =
  Stmt
    ( Function
        MkFishFunction
          { funcName = "__monk_bg_status_path",
            funcFlags = [],
            funcParams = ["token"],
            funcBody =
              Stmt
                ( Printf
                    (ExprLiteral "%s/job-%s.status\\n")
                    [ExprVariable (VarScalar "__monk_bg_dir"), ExprVariable (VarScalar "token")]
                )
                NE.:| []
          }
    )

waitHelperStmt :: FishStatement
waitHelperStmt =
  Stmt
    ( Function
        MkFishFunction
          { funcName = "__monk_wait",
            funcFlags = [],
            funcParams = [],
            funcBody =
              initTargetsStmt
                NE.:| [ setTargetsStmt,
                        initWaitStatusStmt,
                        waitForTargetsStmt,
                        finalizeNoArgWaitStmt,
                        returnWaitStatusStmt
                      ]
          }
    )
  where
    argvCountCmd =
      Command
        "count"
        [ExprVal (ExprVariable (VarAll "argv"))]

    argvCountExpr = ExprCommandSubst (Stmt argvCountCmd NE.:| [])
    hasArgsCond =
      jobListFromCommand
        ( Command
            "test"
            [ ExprVal argvCountExpr,
              ExprVal (ExprLiteral "-gt"),
              ExprVal (ExprLiteral "0")
            ]
        )
    noArgsCond =
      jobListFromCommand
        ( Command
            "test"
            [ ExprVal argvCountExpr,
              ExprVal (ExprLiteral "-eq"),
              ExprVal (ExprLiteral "0")
            ]
        )
    initTargetsStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_wait_targets"
            (ExprListLiteral [])
        )
    setTargetsStmt =
      Stmt
        ( If
            hasArgsCond
            (Stmt (Set [] "__monk_wait_targets" (ExprVariable (VarAll "argv"))) NE.:| [])
            [Stmt (Set [] "__monk_wait_targets" (ExprVariable (VarAll "__monk_bg_jobs")))]
            []
        )
    initWaitStatusStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_wait_status"
            (ExprListLiteral [ExprLiteral "0"])
        )
    waitForTargetsStmt =
      Stmt
        ( For
            "__monk_wait_token"
            (ExprVariable (VarAll "__monk_wait_targets"))
            (waitTargetBody NE.:| [])
            []
        )
    waitTargetBody =
      Stmt
        ( If
            trackedTokenCond
            trackedTokenBody
            [fallbackWaitStmt, captureFallbackStatusStmt]
            []
        )
    trackedTokenCond =
      jobListFromCommand
        ( Command
            "contains"
            [ ExprVal (ExprLiteral "--"),
              ExprVal (ExprVariable (VarScalar "__monk_wait_token")),
              ExprVal (ExprVariable (VarAll "__monk_bg_jobs"))
            ]
        )
    trackedTokenBody =
      setHelperStatusFileStmt
        NE.:| [ waitForStatusFileStmt,
                loadTrackedStatusStmt,
                removeTrackedStatusFileStmt,
                setTrackedIndexStmt,
                removeTrackedIndexStmt
              ]
    setHelperStatusFileStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_bg_status_file"
            ( ExprCommandSubst
                ( Stmt
                    ( Command
                        "__monk_bg_status_path"
                        [ExprVal (ExprVariable (VarScalar "__monk_wait_token"))]
                    )
                    NE.:| []
                )
            )
        )
    waitForStatusFileStmt =
      Stmt
        ( While
            ( jobListFromCommand
                ( Not
                    ( Command
                        "test"
                        [ ExprVal (ExprLiteral "-e"),
                          ExprVal (ExprVariable (VarScalar "__monk_bg_status_file"))
                        ]
                    )
                )
            )
            (Stmt (Command "sleep" [ExprVal (ExprLiteral "0.01")]) NE.:| [])
            []
        )
    loadTrackedStatusStmt =
      Stmt
        ( Set
            []
            "__monk_wait_status"
            ( ExprCommandSubst
                ( Stmt
                    ( Command
                        "cat"
                        [ExprVal (ExprVariable (VarScalar "__monk_bg_status_file"))]
                    )
                    NE.:| []
                )
            )
        )
    removeTrackedStatusFileStmt =
      Stmt
        ( Command
            "rm"
            [ ExprVal (ExprLiteral "-f"),
              ExprVal (ExprVariable (VarScalar "__monk_bg_status_file"))
            ]
        )
    setTrackedIndexStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_bg_idx"
            ( ExprCommandSubst
                ( Stmt
                    ( Command
                        "contains"
                        [ ExprVal (ExprLiteral "-i"),
                          ExprVal (ExprLiteral "--"),
                          ExprVal (ExprVariable (VarScalar "__monk_wait_token")),
                          ExprVal (ExprVariable (VarAll "__monk_bg_jobs"))
                        ]
                    )
                    NE.:| []
                )
            )
        )
    removeTrackedIndexStmt =
      Stmt
        ( If
            trackedIndexCond
            ( eraseTrackedIndexStmt
                NE.:| []
            )
            []
            []
        )
    trackedIndexCond =
      jobListFromCommand
        ( Command
            "test"
            [ ExprVal (ExprLiteral "-n"),
              ExprVal (ExprJoinList (ExprVariable (VarAll "__monk_bg_idx")))
            ]
        )
    eraseTrackedIndexStmt =
      Stmt
        ( Command
            "set"
            [ ExprVal (ExprLiteral "--erase"),
              ExprVal
                ( ExprStringConcat
                    (ExprLiteral "__monk_bg_jobs[")
                    ( ExprStringConcat
                        (ExprVariable (VarScalar "__monk_bg_idx"))
                        (ExprLiteral "]")
                    )
                )
            ]
        )
    fallbackWaitStmt =
      Stmt
        ( Command
            "wait"
            [ExprVal (ExprVariable (VarScalar "__monk_wait_token"))]
        )
    captureFallbackStatusStmt =
      Stmt
        ( Command
            "set"
            [ ExprVal (ExprLiteral "__monk_wait_status"),
              ExprVal (ExprSpecialVar SVStatus)
            ]
        )
    finalizeNoArgWaitStmt =
      Stmt
        ( If
            noArgsCond
            ( Stmt (Command "wait" []) NE.:| [Stmt (Command "return" [ExprVal (ExprLiteral "0")])]
            )
            []
            []
        )
    returnWaitStatusStmt =
      Stmt
        ( Command
            "return"
            [ExprVal (ExprVariable (VarAll "__monk_wait_status"))]
        )

jobListFromCommand :: FishCommand TStatus -> FishJobList
jobListFromCommand cmd =
  MkFishJobList ( MkFishJobConjunction Nothing
        (MkFishJobPipeline False [] (Stmt cmd) [] False)
        []
        NE.:| []
    )

incSeqStmt :: FishStatement
incSeqStmt =
  Stmt
    ( Command
        "set"
        [ ExprVal (ExprLiteral "--global"),
          ExprVal (ExprLiteral "__monk_bg_seq"),
          ExprVal
            ( ExprMath
                ( ExprVariable (VarScalar "__monk_bg_seq")
                    NE.:| [ExprLiteral "+", ExprLiteral "1"]
                )
            )
        ]
    )

setLastJobStmt :: FishStatement
setLastJobStmt =
  Stmt
    ( Set
        [SetGlobal]
        "__monk_last_job"
        (ExprListLiteral [ExprVariable (VarScalar "__monk_bg_seq")])
    )

appendTrackedJobStmt :: FishStatement
appendTrackedJobStmt =
  Stmt
    ( Set
        [SetGlobal, SetAppend]
        "__monk_bg_jobs"
        (ExprListLiteral [ExprVariable (VarScalar "__monk_last_job")])
    )

setTokenStmt :: FishStatement
setTokenStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_bg_token"
        (ExprListLiteral [ExprVariable (VarScalar "__monk_last_job")])
    )

setStatusFileStmt :: FishStatement
setStatusFileStmt =
  Stmt
    ( Set
        [SetLocal]
        "__monk_bg_status_file"
        ( ExprCommandSubst
            ( Stmt
                ( Command
                    "__monk_bg_status_path"
                    [ExprVal (ExprVariable (VarScalar "__monk_bg_token"))]
                )
                NE.:| []
            )
        )
    )

removeStaleStatusStmt :: FishStatement
removeStaleStatusStmt =
  Stmt
    ( Command
        "rm"
        [ ExprVal (ExprLiteral "-f"),
          ExprVal (ExprVariable (VarScalar "__monk_bg_status_file"))
        ]
    )

captureStatusStmt :: FishStatement
captureStatusStmt =
  Stmt
    ( Command
        "set"
        [ ExprVal (ExprLiteral "--local"),
          ExprVal (ExprLiteral "__monk_bg_status"),
          ExprVal (ExprSpecialVar SVStatus)
        ]
    )

writeStatusStmt :: FishStatement
writeStatusStmt =
  Stmt
        ( Command
            "printf"
        [ ExprVal (ExprLiteral "%s\\n"),
          ExprVal (ExprVariable (VarScalar "__monk_bg_status")),
          RedirectVal
            ( MkRedirect
                RedirectStdout
                RedirectOut
                (RedirectFile (ExprVariable (VarScalar "__monk_bg_status_file")))
            )
        ]
    )
