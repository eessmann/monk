module BashParser
  ( parseBashScript,
    parseBashFile
  )
where

import Control.Exception.Safe (tryAny)
-- ShellCheck modules
import ShellCheck.Interface
  ( Comment (..),
    ParseResult (..),
    ParseSpec (..),
    PositionedComment (..),
    Severity (..),
    Shell (Bash),
    newComment,
    newParseSpec,
    newPosition,
    newPositionedComment,
    newSystemInterface,
  )
import ShellCheck.Parser (parseScript)

-------------------------------------------------------------------------------

-- | Parse a Bash script from (filename, text) into a ShellCheck 'ParseResult'.
--   The 'filename' is used by ShellCheck for diagnostics and source lookups.
parseBashScript ::
  -- | Script filename
  FilePath ->
  -- | Script contents
  Text ->
  IO ParseResult
parseBashScript fileName scriptText = do
  -- Prepare a default system interface. In real code you might want
  -- to customize how 'source' lookups behave or pass '-x' style logic.
  let si = newSystemInterface

      ps =
        newParseSpec
          { psFilename = fileName,
            psScript = toString scriptText,
            psShellTypeOverride = Just Bash,
            psCheckSourced = True, -- or False, depending on your needs
            psIgnoreRC = True -- ignore .shellcheckrc
          }

  parseScript si ps -- returns IO ParseResult

--------------------------------------------------------------------------------

-- | Parse a Bash file from disk. Returns either parse warnings/errors
--   or the successful 'ParseResult'.
parseBashFile :: FilePath -> IO (Either [PositionedComment] ParseResult)
parseBashFile filePath = do
  scriptTextOrErr <- tryAny (readFile filePath)
  case scriptTextOrErr of
    Left ex ->
      -- I/O error: wrap it in a synthetic "PositionedComment".
      pure $ Left [fakeComment (toText (displayException ex))]
    Right scriptText -> do
      parseResult <- parseBashScript filePath (toText scriptText)
      if isJust (prRoot parseResult)
        then pure (Right parseResult)
        else pure (Left (prComments parseResult))
  where
    -- Build a synthetic PositionedComment for I/O exceptions
    fakeComment :: Text -> PositionedComment
    fakeComment errMsg =
      let c =
            newComment
              { cMessage = toString errMsg,
                cSeverity = ErrorC
              }
       in newPositionedComment
            { pcComment = c,
              pcStartPos = newPosition,
              pcEndPos = newPosition
            } -- zero-length range

--------------------------------------------------------------------------------
-- Example usage in GHCi or from another module:
--
--   > resultE <- parseBashFile "myscript.sh"
--   > case resultE of
--   >   Left errs -> putStrLn ("Parse failed: " <> show errs)
--   >   Right parseRes -> do
--   >     print (prRoot parseRes)            -- The AST root
--   >     print (prComments parseRes)        -- Any warnings/comments
--   >     let sources = gatherSourcedFiles parseRes
--   >     putStrLn ("Sourced files: " <> show sources)
--------------------------------------------------------------------------------
