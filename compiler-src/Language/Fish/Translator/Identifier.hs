-- | Checked boundary for names already validated by normalization or generated
-- from the compiler's reserved ASCII prefix. Failure is an internal invariant
-- violation, not an unchecked route into the canonical syntax tree.
module Language.Fish.Translator.Identifier (compilerIdentifier, compilerCommandName) where

import Language.Fish.DSL.Executable (CommandName, commandName)
import Language.Fish.DSL.Name (Identifier, identifier)

compilerIdentifier :: Text -> Identifier
compilerIdentifier name = either (error . ("Invalid compiler-generated identifier: " <>)) id (identifier name)

compilerCommandName :: Text -> CommandName
compilerCommandName name = either (error . ("Invalid compiler-generated command: " <>)) id (commandName name)
