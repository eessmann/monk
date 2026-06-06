{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands.CommandTokens.Core
  ( translateCommandTokensWithoutTime,
    translateCommandTokensWithoutTimeM,
  )
where

import Data.List (nub)
import Data.Text qualified as T
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Background
  ( translateWaitArgs,
    translateWaitArgsM,
  )
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
    translateReadM,
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
  ( WarningCode (..),
    noteUnsupported,
    setErrexitEnabled,
    setPipefailEnabled,
    unsupported,
  )
import Language.Fish.Translator.Redirections (parseRedirectTokens, parseRedirectTokensM)
import Language.Fish.Translator.Syntax hiding (Arg, argExpr, argRedirect, renderArg, renderArgs)
import Language.Fish.Translator.Token (tokenHasExpansion, tokenRawText, tokenToLiteralText)
import Language.Fish.Translator.Variables (translateTokenToArg)
import ShellCheck.AST
import Prelude hiding (gets)

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
            else Just $
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
                          "wait" -> translateWaitArgs argExprs
                          "read" -> translateRead plainArgs
                          "shopt" -> Command "true" renderedArgs
                          _ -> Command name (renderArgs argExprs)
                    else fallback

translateCommandTokensWithoutTimeM :: [Token] -> HoistedM (Maybe (FishCommand TStatus))
translateCommandTokensWithoutTimeM = translateCommandTokensWithoutTimePlanM

translateCommandTokensWithoutTimePlanM :: [Token] -> HoistedM (Maybe (FishCommand TStatus))
translateCommandTokensWithoutTimePlanM cmdTokens =
  case cmdTokens of
    [] -> hoistM [] Nothing
    (c : args) -> do
      let name = tokenToLiteralText c
      MkHoisted preRedirs (redirs, plainArgs) <- parseRedirectTokensM args
      if T.null name
        then do
          MkHoisted preArgs _ <- translateArgsM plainArgs
          hoistM (preRedirs <> preArgs) Nothing
        else case T.unpack name of
          "echo" -> do
            MkHoisted pre cmd <- translateEchoM plainArgs redirs
            hoistM (preRedirs <> pre) (Just cmd)
          _ -> do
            MkHoisted preArgs argExprs <- translateArgsM plainArgs
            let renderedArgs = renderArgs (argExprs ++ redirs)
                testArgs = normalizeTestExprs renderedArgs
                fallback = Command name renderedArgs
                pre0 = preRedirs <> preArgs
            case T.unpack name of
              "test" ->
                hoistM pre0 (Just (Command "test" testArgs))
              "source" ->
                translateSourceCommandM pre0 plainArgs renderedArgs
              "." ->
                translateSourceCommandM pre0 plainArgs renderedArgs
              "shopt" -> do
                unsupported ShoptIgnored Nothing
                hoistM pre0 (Just (Command "true" renderedArgs))
              _ ->
                if null redirs
                  then case isSingleBracketTokens c plainArgs of
                    Just middle -> do
                      MkHoisted pre bracketArgs <- translateArgsM middle
                      hoistM (preRedirs <> pre) (Just (Command "test" (normalizeTestExprs (renderArgs bracketArgs))))
                    Nothing ->
                      case isDoubleBracketTokens c plainArgs of
                        Just middle -> do
                          MkHoisted pre cmd <- translateDoubleBracketArgsM middle
                          hoistM (preRedirs <> pre) (Just cmd)
                        Nothing ->
                          case T.unpack name of
                            "exit" ->
                              hoistM pre0 (Just (translateExitFromExprs plainArgs argExprs))
                            "eval" -> do
                              MkHoisted pre expr <- translateEvalM plainArgs
                              hoistM (preRedirs <> pre) (Just (Eval expr))
                            "exec" ->
                              hoistM pre0 (Just (Command "exec" (renderArgs argExprs)))
                            "wait" -> do
                              waitCmd <- translateWaitArgsM argExprs
                              hoistM pre0 (Just waitCmd)
                            "set" -> do
                              let opts = parseSetOptions plainArgs
                              forM_ (setErrexit opts) setErrexitEnabled
                              forM_ (setPipefail opts) setPipefailEnabled
                              notes <- mapM (noteUnsupported SetOptionIssue . Just) (setIssues opts)
                              let preAll = pre0 <> notes
                              case setPositionalArgs opts of
                                Just positional -> do
                                  MkHoisted prePos cmd <- translateSetPositionalM positional
                                  hoistM (preRedirs <> prePos <> notes) (Just cmd)
                                Nothing
                                  | setSawOptions opts ->
                                      hoistM preAll Nothing
                                  | not (null (setIssues opts)) ->
                                      hoistM preAll (Just (Command "set" (renderArgs argExprs)))
                                  | otherwise ->
                                      hoistM preAll (Just (Command "set" (renderArgs argExprs)))
                            "read" -> do
                              let MkReadParseResult {readIssues} =
                                    parseReadArgsDetailed plainArgs [] [] [] False False
                              notes <- mapM (noteUnsupported ReadIssue . Just) (nub readIssues)
                              let preAll = pre0 <> notes
                              readCmd <- translateReadM plainArgs
                              hoistM preAll (Just readCmd)
                            "shopt" -> do
                              unsupported ShoptIgnored Nothing
                              hoistM pre0 (Just (Command "true" (renderArgs (argExprs ++ redirs))))
                            _ ->
                              hoistM pre0 (Just (Command name (renderArgs argExprs)))
                  else hoistM pre0 (Just fallback)

translateSetPositionalM :: [Token] -> HoistedM (FishCommand TStatus)
translateSetPositionalM positional = do
  MkHoisted pre args <- translateArgsM positional
  hoistM pre (Command "set argv" (renderArgs args))

translateSourceCommandM :: [FishStatement] -> [Token] -> [ExprOrRedirect] -> HoistedM (Maybe (FishCommand TStatus))
translateSourceCommandM pre plainArgs renderedArgs = do
  mapM_ (unsupported SourceIssue . Just) (sourceIssues plainArgs)
  hoistM pre (Just (Command "source" renderedArgs))

sourceIssues :: [Token] -> [Text]
sourceIssues [] = ["source command missing path argument; manual review required"]
sourceIssues (pathTok : _)
  | sourcePathIsNonLiteral pathTok = ["non-literal source path requires manual review"]
  | otherwise = []

sourcePathIsNonLiteral :: Token -> Bool
sourcePathIsNonLiteral tok =
  tokenHasExpansion tok
    || T.null (tokenToLiteralText tok)
    || T.isInfixOf "$" (tokenRawText tok)
