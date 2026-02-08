module Language.Fish.Translator.Commands.Time
  ( stripTimePrefix,
  )
where

import Data.Text qualified as T
import Language.Fish.Translator.Token (tokenToLiteralText)
import ShellCheck.AST

stripTimePrefix :: [Token] -> (Bool, [Token])
stripTimePrefix cmds =
  case cmds of
    (T_SimpleCommand tokId assignments (cmdTok : rest) : xs)
      | tokenToLiteralText cmdTok == "time",
        not (null rest) ->
          case rest of
            (arg1 : _)
              | isTimeOption arg1 -> (False, cmds)
            _ -> (True, T_SimpleCommand tokId assignments rest : xs)
    _ -> (False, cmds)
  where
    isTimeOption tok =
      let txt = tokenToLiteralText tok
       in T.isPrefixOf "-" txt
