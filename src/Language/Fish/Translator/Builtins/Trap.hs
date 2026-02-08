{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Trap
  ( translateTrapCommand,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Builtins.Common (wrapStmtList)
import Language.Fish.Translator.Monad (TranslateM, addWarning)
import Language.Fish.Translator.Variables
  ( tokenToLiteralText,
    translateTokenToExpr,
    translateTokenToExprOrRedirect,
  )
import ShellCheck.AST

translateTrapCommand :: [Token] -> TranslateM FishStatement
translateTrapCommand args =
  case args of
    [] -> do
      addWarning "trap with no arguments is not supported"
      pure (Stmt (Command "trap" []))
    (cmdTok : signalToks) -> do
      let cmdExpr = translateTokenToExpr cmdTok
          rawSignals = map tokenToLiteralText signalToks
      if any isTrapOption rawSignals
        then do
          addWarning "trap options are not supported; emitting raw trap command"
          pure (Stmt (Command "trap" (map translateTokenToExprOrRedirect args)))
        else do
          let signals = if null rawSignals then ["EXIT"] else rawSignals
              trapStmts = map (trapForSignal cmdExpr) signals
          pure (wrapStmtList (map Stmt trapStmts))
  where
    isTrapOption sig = sig `elem` ["-p", "-l", "--"]

    trapForSignal :: FishExpr TStr -> Text -> FishCommand TStatus
    trapForSignal cmd sig
      | isExitSignal sig =
          Command "trap" [ExprVal (ExprLiteral "--on-exit"), ExprVal cmd]
      | otherwise =
          Command
            "trap"
            [ ExprVal (ExprLiteral "--on-signal"),
              ExprVal (ExprLiteral (normalizeSignal sig)),
              ExprVal cmd
            ]

    isExitSignal sig =
      let upper = T.toUpper sig
       in upper == "EXIT" || upper == "0"

    normalizeSignal sig =
      let upper = T.toUpper sig
       in fromMaybe upper (T.stripPrefix "SIG" upper)
