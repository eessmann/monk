-- | Executable tokens cannot introduce Fish control grammar. Dynamic command
-- selection is restricted to a quoted scalar variable, never shell source or
-- a command substitution (which Fish rejects in command position).
module Language.Fish.DSL.Executable
  ( CommandName,
    commandName,
    commandNameText,
    Executable,
    literalExecutable,
    variableExecutable,
    foldExecutable,
  )
where

import Data.Text qualified as T
import Language.Fish.DSL.Name (Identifier)

newtype CommandName = CommandName Text
  deriving stock (Show, Eq, Ord)

commandName :: Text -> Either Text CommandName
commandName value
  | T.null value = Left "An executable name cannot be empty"
  | T.any (== '\0') value = Left "An executable name cannot contain NUL"
  | value `elem` reserved = Left "Fish control words require their dedicated structural command"
  | otherwise = Right (CommandName value)
  where
    reserved = ["!", "and", "begin", "break", "builtin", "case", "command", "continue", "else", "end", "exec", "for", "function", "if", "in", "not", "or", "return", "switch", "then", "time", "while"]

commandNameText :: CommandName -> Text
commandNameText (CommandName value) = value

instance IsString CommandName where
  fromString value = either error id (commandName (toText value))

data Executable
  = LiteralExecutable CommandName
  | VariableExecutable Identifier
  deriving stock (Show, Eq)

literalExecutable :: CommandName -> Executable
literalExecutable = LiteralExecutable

variableExecutable :: Identifier -> Executable
variableExecutable = VariableExecutable

foldExecutable :: (CommandName -> result) -> (Identifier -> result) -> Executable -> result
foldExecutable literal variable value = case value of
  LiteralExecutable name -> literal name
  VariableExecutable name -> variable name
