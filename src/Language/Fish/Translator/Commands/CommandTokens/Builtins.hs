{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands.CommandTokens.Builtins
  ( translateExit,
    translateExitFromExprs,
    translateSource,
    translateEval,
    translateExec,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Args (Arg, renderArgs)
import Language.Fish.Translator.Commands.Args (concatWithSpaces)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables
  ( translateTokenToExpr,
    translateTokenToExprOrRedirect,
  )
import ShellCheck.AST

-- exit [n]
translateExit :: [Token] -> FishCommand TStatus
translateExit [] = Exit Nothing
translateExit [t] =
  case tokenToLiteralText t of
    txt
      | T.all isDigit txt && not (T.null txt),
        Just n <- readMaybe (T.unpack txt) ->
          Exit (Just (ExprNumLiteral n))
      | otherwise -> Command "exit" [translateTokenToExprOrRedirect t]
translateExit ts = Command "exit" (map translateTokenToExprOrRedirect ts)

translateExitFromExprs :: [Token] -> [Arg] -> FishCommand TStatus
translateExitFromExprs [] _ = Exit Nothing
translateExitFromExprs [t] [arg] =
  case tokenToLiteralText t of
    txt
      | T.all isDigit txt && not (T.null txt),
        Just n <- readMaybe (T.unpack txt) ->
          Exit (Just (ExprNumLiteral n))
      | otherwise -> Command "exit" (renderArgs [arg])
translateExitFromExprs _ args = Command "exit" (renderArgs args)

-- source FILE
translateSource :: [Token] -> FishCommand TStatus
translateSource [] = Command "source" []
translateSource (t : rest) =
  if null rest
    then Source (translateTokenToExpr t)
    else Command "source" (map translateTokenToExprOrRedirect (t : rest))

-- eval STRING (join args by space)
translateEval :: [Token] -> FishCommand TStatus
translateEval ts = Eval (concatWithSpaces (map translateTokenToExpr ts))

-- exec CMD [ARGS|REDIRS]
translateExec :: [Token] -> FishCommand TStatus
translateExec [] = Command "exec" []
translateExec (t : rest) = Exec (translateTokenToExpr t) (map translateTokenToExprOrRedirect rest)
