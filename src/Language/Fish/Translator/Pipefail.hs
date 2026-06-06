{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Pipefail
  ( ensurePipefailHelper,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.DSL qualified as DSL
import Language.Fish.Translator.Monad
  ( HelperId (..),
    TranslateM,
    ensureHelperScript,
  )

ensurePipefailHelper :: TranslateM ()
ensurePipefailHelper =
  ensureHelperScript HelperPipefail (DSL.script [pipefailHelper])

pipefailHelper :: DSL.Stmt
pipefailHelper =
  let statusVar = "__monk_pipe_status"
      statusInit =
        DSL.stmt
          ( DSL.set
              [DSL.SetLocal]
              statusVar
              (DSL.list [DSL.str "0"])
          )
      testCmd =
        DSL.command
          "test"
          [ DSL.arg (DSL.vars "s"),
            DSL.arg (DSL.str "-ne"),
            DSL.arg (DSL.str "0")
          ]
      setStatus =
        DSL.stmt
          ( DSL.set
              []
              statusVar
              (DSL.vars "s")
          )
      ifStmt = DSL.stmt (DSL.if_ (DSL.condition testCmd) (DSL.block (setStatus NE.:| [])) [] [])
      forStmt =
        DSL.stmt
          ( DSL.for
              "s"
              (DSL.vars "argv")
              (DSL.block (ifStmt NE.:| []))
              []
          )
      returnStmt =
        DSL.stmt
          ( DSL.command
              "return"
              [DSL.arg (DSL.vars statusVar)]
          )
      body = statusInit NE.:| [forStmt, returnStmt]
   in DSL.stmt (DSL.function "__monk_pipefail" [] [] (DSL.block body))
