{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands.CommandTokens.Core
  ( translateCommandTokensWithoutTime,
    translateCommandTokensWithoutTimeM,
  )
where

import Prelude hiding (gets)
import Data.List (nub)
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Commands.Args (translateArgsM, translateEvalM)
import Language.Fish.Translator.Commands.CommandTokens.Builtins
  ( translateEval,
    translateExec,
    translateExit,
    translateExitFromExprs,
    translateSource,
  )
import Language.Fish.Translator.Commands.Echo (translateEcho, translateEchoM)
import Language.Fish.Translator.Commands.Read
  ( ReadParseResult (..),
    parseReadArgsDetailed,
    translateRead,
  )
import Language.Fish.Translator.Commands.SetOptions (SetOptionParse (..), parseSetOptions)
import Language.Fish.Translator.Commands.Tests
  ( isDoubleBracketTest,
    isDoubleBracketTokens,
    isSingleBracketTest,
    isSingleBracketTokens,
    normalizeTestExprs,
    translateDoubleBracketArgsM,
  )
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad
  ( noteUnsupported,
    setErrexitEnabled,
    setPipefailEnabled,
  )
import Language.Fish.Translator.Redirections (parseRedirectTokens, parseRedirectTokensM)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables (translateTokenToArg)
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Command translation (no time handling)
--------------------------------------------------------------------------------

translateCommandTokensWithoutTime :: [Token] -> Maybe (FishCommand TStatus)
translateCommandTokensWithoutTime cmdTokens =
  case cmdTokens of
    [] -> Nothing
    (c : args) ->
      let name = tokenToLiteralText c
          (redirs, plainArgs) = parseRedirectTokens args
          argExprs = map translateTokenToArg plainArgs
          renderedArgs = renderArgs (argExprs ++ redirs)
          testArgs = normalizeTestExprs renderedArgs
          fallback = Command name renderedArgs
       in if T.null name
            then Nothing
            else
              Just $
                case T.unpack name of
                  "echo" -> translateEcho plainArgs redirs
                  "test" -> Command "test" testArgs
                  _ ->
                    if null redirs
                      then case isSingleBracketTest c plainArgs of
                        Just testCmd -> testCmd
                        Nothing -> case isDoubleBracketTest c plainArgs of
                          Just testCmd -> testCmd
                          Nothing -> case T.unpack name of
                            "exit" -> translateExit plainArgs
                            "source" -> translateSource plainArgs
                            "." -> translateSource plainArgs
                            "eval" -> translateEval plainArgs
                            "exec" -> translateExec plainArgs
                            "read" -> translateRead plainArgs
                            "shopt" -> Command "true" renderedArgs
                            _ -> Command name (renderArgs argExprs)
                      else fallback

translateCommandTokensWithoutTimeM :: [Token] -> HoistedM (Maybe (FishCommand TStatus))
translateCommandTokensWithoutTimeM = translateCommandTokensWithoutTimeHoistedM

translateCommandTokensWithoutTimeHoistedM :: [Token] -> HoistedM (Maybe (FishCommand TStatus))
translateCommandTokensWithoutTimeHoistedM cmdTokens =
  case cmdTokens of
    [] -> hoistM [] Nothing
    (c : args) -> do
      let name = tokenToLiteralText c
      Hoisted preRedirs (redirs, plainArgs) <- parseRedirectTokensM args
      if T.null name
        then do
          Hoisted preArgs _ <- translateArgsM plainArgs
          hoistM (preRedirs <> preArgs) Nothing
        else
          case T.unpack name of
            "echo" -> do
              Hoisted pre cmd <- translateEchoM plainArgs redirs
              hoistM (preRedirs <> pre) (Just cmd)
            _ -> do
              Hoisted preArgs argExprs <- translateArgsM plainArgs
              let renderedArgs = renderArgs (argExprs ++ redirs)
                  testArgs = normalizeTestExprs renderedArgs
                  fallback = Command name renderedArgs
                  pre0 = preRedirs <> preArgs
              case T.unpack name of
                "test" ->
                  hoistM pre0 (Just (Command "test" testArgs))
                _ ->
                  if null redirs
                    then case isSingleBracketTokens c plainArgs of
                      Just middle -> do
                        Hoisted pre bracketArgs <- translateArgsM middle
                        hoistM (preRedirs <> pre) (Just (Command "test" (normalizeTestExprs (renderArgs bracketArgs))))
                      Nothing ->
                        case isDoubleBracketTokens c plainArgs of
                          Just middle -> do
                            Hoisted pre cmd <- translateDoubleBracketArgsM middle
                            hoistM (preRedirs <> pre) (Just cmd)
                          Nothing ->
                            case T.unpack name of
                              "exit" ->
                                hoistM pre0 (Just (translateExitFromExprs plainArgs argExprs))
                              "source" ->
                                hoistM pre0 (Just (Command "source" (renderArgs argExprs)))
                              "." ->
                                hoistM pre0 (Just (Command "source" (renderArgs argExprs)))
                              "eval" -> do
                                Hoisted pre expr <- translateEvalM plainArgs
                                hoistM (preRedirs <> pre) (Just (Eval expr))
                              "exec" ->
                                hoistM pre0 (Just (Command "exec" (renderArgs argExprs)))
                              "set" -> do
                                let opts = parseSetOptions plainArgs
                                forM_ (setErrexit opts) setErrexitEnabled
                                forM_ (setPipefail opts) setPipefailEnabled
                                notes <- mapM noteUnsupported (setIssues opts)
                                let preAll = pre0 <> notes
                                if setSawOptions opts || not (null (setIssues opts))
                                  then hoistM preAll Nothing
                                  else hoistM preAll (Just (Command "set" (renderArgs argExprs)))
                              "read" -> do
                                let ReadParseResult {readFlags, readVars, readIssues, readUnsupported} =
                                      parseReadArgsDetailed plainArgs [] [] [] False
                                notes <- mapM noteUnsupported (nub readIssues)
                                let preAll = pre0 <> notes
                                if readUnsupported
                                  then hoistM preAll (Just (Command "read" (renderArgs argExprs)))
                                  else hoistM preAll (Just (Read readFlags readVars))
                              "shopt" -> do
                                note <- noteUnsupported "shopt has no fish equivalent; ignored"
                                hoistM (pre0 <> [note]) (Just (Command "true" (renderArgs (argExprs ++ redirs))))
                              _ ->
                                hoistM pre0 (Just (Command name (renderArgs argExprs)))
                    else hoistM pre0 (Just fallback)
