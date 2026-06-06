{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Trap
  ( translateTrapCommand,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Builtins.Common (wrapStmtList)
import Language.Fish.Translator.Monad
  ( TranslateM,
    WarningCode (..),
    addWarning,
    noteUnsupported,
  )
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
      addWarning TrapIssue (Just "trap with no arguments is not supported")
      pure (Stmt (Command "trap" []))
    [singleTok]
      | isTrapQueryForm [tokenToLiteralText singleTok] -> emitRawOptions
    (cmdTok : signalToks)
      | tokenToLiteralText cmdTok == "-" ->
          wrapTrapStatements =<< traverse clearTrapForSignal (defaultSignals signalToks)
      | isTrapQueryForm (map tokenToLiteralText args) ->
          emitRawOptions
      | isTrapOption (tokenToLiteralText cmdTok) || any (isTrapOption . tokenToLiteralText) signalToks ->
          emitRawOptions
      | otherwise -> do
          let cmdExpr = translateTokenToExpr cmdTok
          wrapTrapStatements =<< traverse (setTrapForSignal cmdExpr) (defaultSignals signalToks)
  where
    isTrapOption sig = sig `elem` ["-p", "-l", "--"]

    emitRawOptions = do
      addWarning TrapIssue (Just "trap options are not supported; emitting raw trap command")
      pure (Stmt (Command "trap" (map translateTokenToExprOrRedirect args)))

    isTrapQueryForm = \case
      ["-p"] -> True
      ["-l"] -> True
      _ -> False

    defaultSignals signalToks =
      let rawSignals = map tokenToLiteralText signalToks
       in if null rawSignals then ["EXIT"] else rawSignals

    wrapTrapStatements stmtLists =
      pure (wrapStmtList (concat stmtLists))

    setTrapForSignal :: FishExpr TStr -> Text -> TranslateM [FishStatement]
    setTrapForSignal cmd sig =
      case classifySignal sig of
        TrapSupported trapSignal ->
          pure
            [ Stmt (Set [SetGlobal] (trapBodyVar trapSignal) (ExprListLiteral [cmd])),
              Stmt (trapFunction trapSignal)
            ]
        TrapUnsupported detail -> do
          note <- noteUnsupported TrapIssue (Just detail)
          pure [note]

    clearTrapForSignal :: Text -> TranslateM [FishStatement]
    clearTrapForSignal sig =
      case classifySignal sig of
        TrapSupported trapSignal ->
          pure
            [ Stmt
                ( Command
                    "functions"
                    [ ExprVal (ExprLiteral "-e"),
                      ExprVal (ExprLiteral (trapFunctionName trapSignal))
                    ]
                ),
              Stmt
                ( Command
                    "set"
                    [ ExprVal (ExprLiteral "-e"),
                      ExprVal (ExprLiteral (trapBodyVar trapSignal))
                    ]
                )
            ]
        TrapUnsupported detail -> do
          note <- noteUnsupported TrapIssue (Just detail)
          pure [note]

    trapFunction :: TrapSignal -> FishCommand TUnit
    trapFunction trapSignal
      | trapSignal == TrapExit =
          Function
            MkFishFunction
              { funcName = trapFunctionName trapSignal,
                funcFlags = [FuncOnProcessExit "%self"],
                funcParams = [],
                funcBody = trapHandlerBody trapSignal
              }
      | otherwise =
          Function
            MkFishFunction
              { funcName = trapFunctionName trapSignal,
                funcFlags =
                  [ FuncUnknownFlag "--on-signal",
                    FuncUnknownFlag (trapSignalValue trapSignal)
                  ],
                funcParams = [],
                funcBody = trapHandlerBody trapSignal
              }

    trapHandlerBody :: TrapSignal -> NonEmpty FishStatement
    trapHandlerBody trapSignal =
      Stmt (Eval (ExprVariable (VarScalar (trapBodyVar trapSignal)))) NE.:| []

    trapFunctionName :: TrapSignal -> Text
    trapFunctionName = \case
      TrapExit -> "__monk_trap_exit"
      TrapSignal sig -> "__monk_trap_sig_" <> sig

    trapBodyVar :: TrapSignal -> Text
    trapBodyVar trapSignal = "__monk_trap_body_" <> trapSignalKey trapSignal

    trapSignalKey :: TrapSignal -> Text
    trapSignalKey = \case
      TrapExit -> "exit"
      TrapSignal sig -> T.toLower sig

    trapSignalValue :: TrapSignal -> Text
    trapSignalValue = \case
      TrapExit -> "EXIT"
      TrapSignal sig -> sig

    classifySignal :: Text -> TrapSignalClassification
    classifySignal sig
      | isExitSignal sig = TrapSupported TrapExit
      | isNumericSignal normalized =
          TrapSupported (TrapSignal normalized)
      | otherwise = classifyNamedSignal normalized
      where
        normalized = normalizeSignal sig

    classifyNamedSignal sig
      | isPseudoSignal sig =
          TrapUnsupported ("trap signal " <> sig <> " has no fish equivalent; manual review required")
      | sig `elem` uncatchableSignals =
          TrapUnsupported ("trap signal " <> sig <> " cannot be caught; manual review required")
      | sig `elem` supportedSignals = TrapSupported (TrapSignal sig)
      | otherwise =
          TrapUnsupported ("trap signal " <> sig <> " has no fish equivalent; manual review required")

    isExitSignal sig =
      let upper = T.toUpper sig
       in upper == "EXIT" || upper == "0"

    isPseudoSignal sig = sig `elem` ["ERR", "DEBUG", "RETURN"]

    isNumericSignal sig = not (T.null sig) && T.all isDigit sig

    normalizeSignal sig =
      let upper = T.toUpper sig
       in fromMaybe upper (T.stripPrefix "SIG" upper)

    supportedSignals =
      [ "ABRT",
        "ALRM",
        "BUS",
        "CHLD",
        "CONT",
        "FPE",
        "HUP",
        "ILL",
        "INFO",
        "INT",
        "IO",
        "PIPE",
        "PROF",
        "QUIT",
        "SEGV",
        "SYS",
        "TERM",
        "TRAP",
        "TSTP",
        "TTIN",
        "TTOU",
        "URG",
        "USR1",
        "USR2",
        "VTALRM",
        "WINCH",
        "XCPU",
        "XFSZ"
      ]

    uncatchableSignals = ["KILL", "STOP"]

data TrapSignal
  = TrapExit
  | TrapSignal Text
  deriving stock (Eq, Show)

data TrapSignalClassification
  = TrapSupported TrapSignal
  | TrapUnsupported Text
