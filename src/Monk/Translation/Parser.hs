-- | Public parser orchestration. The compiler consumes only immutable text.
module Monk.Translation.Parser (parseBashFile, parseBashScript) where

import Control.Exception (IOException, try)
import Language.Bash.Parser (parseBashFragment)
import ShellCheck.Interface
  ( Comment (..),
    ParseResult (..),
    PositionedComment (..),
    Severity (..),
    newComment,
    newPosition,
    newPositionedComment,
  )

parseBashScript :: FilePath -> Text -> IO ParseResult
parseBashScript fileName scriptText = pure (parseBashFragment fileName scriptText)

-- | Read a source once and return its parser diagnostics. Cancellation remains
-- an exception; it is never converted into an ordinary source diagnostic.
parseBashFile :: FilePath -> IO (Either [PositionedComment] ParseResult)
parseBashFile filePath = do
  source <- try @IOException (readFileBS filePath)
  pure $ case source of
    Left exception -> Left [ioComment (displayException exception)]
    Right bytes ->
      let parsed = parseBashFragment filePath (decodeUtf8 bytes)
       in if isJust (prRoot parsed) then Right parsed else Left (prComments parsed)
  where
    ioComment message =
      newPositionedComment
        { pcComment = newComment {cMessage = message, cSeverity = ErrorC},
          pcStartPos = newPosition,
          pcEndPos = newPosition
        }
