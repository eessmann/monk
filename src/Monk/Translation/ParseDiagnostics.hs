-- | One conversion of ShellCheck locations/severities for every input path.
module Monk.Translation.ParseDiagnostics (positionedCommentDiagnostic, genericParseDiagnostic) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Language.Fish.DSL (SourcePos (..), SourceRange (..))
import Monk.Translation.Types
import ShellCheck.Interface (Comment (..), Position (..), PositionedComment (..))

positionedCommentDiagnostic :: PositionedComment -> Diagnostic
positionedCommentDiagnostic comment =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode ("shellcheck." <> shellCheckCodeText (cCode payload)),
      diagnosticPhase = PhaseParse,
      diagnosticSeverity = shellCheckSeverity (show (cSeverity payload)),
      diagnosticRisk = shellCheckRisk (show (cSeverity payload)),
      diagnosticMessage = toText (cMessage payload),
      diagnosticRange = Just (positionRange (pcStartPos comment) (pcEndPos comment))
    }
  where
    payload = pcComment comment

shellCheckSeverity :: Text -> DiagnosticSeverity
shellCheckSeverity rendered
  | "Error" `T.isInfixOf` rendered = DiagnosticError
  | "Info" `T.isInfixOf` rendered || "Style" `T.isInfixOf` rendered = DiagnosticNote
  | otherwise = DiagnosticWarning

shellCheckRisk :: Text -> ReviewRisk
shellCheckRisk rendered
  | "Error" `T.isInfixOf` rendered = Unsafe
  | otherwise = Review

shellCheckCodeText :: (Show code) => code -> Text
shellCheckCodeText = toText . filter isDigit . show

positionRange :: Position -> Position -> SourceRange
positionRange start end = MkSourceRange (position start) (position end)
  where
    position source =
      MkSourcePos
        { srcFile = toText (posFile source),
          srcLine = fromInteger (posLine source),
          srcColumn = fromInteger (posColumn source)
        }

genericParseDiagnostic :: Diagnostic
genericParseDiagnostic =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode "shellcheck.parse",
      diagnosticPhase = PhaseParse,
      diagnosticSeverity = DiagnosticError,
      diagnosticRisk = Unsafe,
      diagnosticMessage = "Unable to parse Bash input",
      diagnosticRange = Nothing
    }
