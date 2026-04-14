{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Trap
  ( translateTrapCommand,
  )
where

import Data.Text qualified as T
import Data.List.NonEmpty qualified as NE
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
              trapStmts = concatMap (trapStatementsForSignal cmdExpr) signals
          pure (wrapStmtList trapStmts)
  where
    isTrapOption sig = sig `elem` ["-p", "-l", "--"]

    trapStatementsForSignal :: FishExpr TStr -> Text -> [FishStatement]
    trapStatementsForSignal cmd sig =
      [ Stmt (Set [SetGlobal] (trapBodyVar sig) (ExprListLiteral [cmd])),
        Stmt (trapForSignal sig)
      ]

    trapForSignal :: Text -> FishCommand TUnit
    trapForSignal sig
      | isExitSignal sig =
          Function
            FishFunction
              { funcName = "__monk_trap_exit",
                funcFlags = [FuncOnProcessExit "%self"],
                funcParams = [],
                funcBody = trapHandlerBody sig
              }
      | otherwise =
          Function
            FishFunction
              { funcName = "__monk_trap_sig_" <> normalizeSignal sig,
                funcFlags =
                  [ FuncUnknownFlag "--on-signal",
                    FuncUnknownFlag (normalizeSignal sig)
                  ],
                funcParams = [],
                funcBody = trapHandlerBody sig
              }

    trapHandlerBody :: Text -> NonEmpty FishStatement
    trapHandlerBody sig =
      Stmt (Eval (ExprVariable (VarScalar (trapBodyVar sig)))) NE.:| []

    trapBodyVar :: Text -> Text
    trapBodyVar sig = "__monk_trap_body_" <> signalKey sig

    signalKey :: Text -> Text
    signalKey sig
      | isExitSignal sig = "exit"
      | otherwise = T.toLower (normalizeSignal sig)

    isExitSignal sig =
      let upper = T.toUpper sig
       in upper == "EXIT" || upper == "0"

    normalizeSignal sig =
      let upper = T.toUpper sig
       in fromMaybe upper (T.stripPrefix "SIG" upper)
