-- | Pure ShellCheck parsing over immutable source text. Filesystem ownership
-- belongs to the public source orchestration layer.
module Language.Bash.Parser (parseBashFragment) where

import ShellCheck.Interface (ParseResult, ParseSpec (..), Shell (Bash), newParseSpec, newSystemInterface)
import ShellCheck.Parser (parseScript)

-- | Parse proved program text without filesystem or configuration access.
parseBashFragment :: FilePath -> Text -> ParseResult
parseBashFragment fileName scriptText = runIdentity $ do
  -- ShellCheck's default interface supplies no filesystem/config callbacks.
  let si = newSystemInterface

      ps =
        newParseSpec
          { psFilename = fileName,
            psScript = toString scriptText,
            psShellTypeOverride = Just Bash,
            -- Monk discovers and translates literal sources through its own
            -- typed source graph. Letting ShellCheck expand them here creates
            -- parser wrapper nodes and duplicates ownership of source loading.
            psCheckSourced = False,
            psIgnoreRC = True -- ignore .shellcheckrc
          }

  parseScript si ps
