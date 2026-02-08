{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Shift
  ( translateShiftCommand,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslateM, addWarning)
import Language.Fish.Translator.Variables (tokenToLiteralText)
import ShellCheck.AST

translateShiftCommand :: [Token] -> TranslateM FishStatement
translateShiftCommand args =
  case args of
    [] -> pure (shiftByCount 1)
    [tok] ->
      case parseShiftCount tok of
        Just n | n >= 0 -> pure (shiftByCount n)
        Just _ -> do
          addWarning "shift count must be non-negative; emitting comment"
          pure (Comment "Unsupported shift count")
        Nothing -> do
          addWarning "Unsupported shift argument; emitting comment"
          pure (Comment "Unsupported shift argument")
    _ -> do
      addWarning "shift with multiple arguments is not supported; emitting comment"
      pure (Comment "Unsupported shift arguments")

shiftByCount :: Int -> FishStatement
shiftByCount n =
  let start = ExprNumLiteral (n + 1)
      range = IndexRange (Just start) (Just (ExprNumLiteral (-1)))
      expr = ExprVariable (VarIndex "argv" range)
   in Stmt (Set [] "argv" expr)

parseShiftCount :: Token -> Maybe Int
parseShiftCount tok =
  case tok of
    T_Literal _ s -> readMaybe (toString (toText s))
    _ -> readMaybe (toString (tokenToLiteralText tok))
