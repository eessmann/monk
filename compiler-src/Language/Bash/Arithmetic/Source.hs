-- | Immutable source evidence for arithmetic diagnostics. No parser token is
-- retained and no source text is interpreted at runtime.
module Language.Bash.Arithmetic.Source
  ( ArithmeticSite,
    arithmeticSite,
    arithmeticSiteMessages,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan
import ShellCheck.AST
import ShellCheck.Interface (Position (..))

data ArithmeticSite = MkArithmeticSite Text Int Int Text [(ArithmeticOrigin, Text, Text)]
  deriving stock (Eq, Show)

arithmeticSite :: Int -> Text -> Map Id (Position, Position) -> Token -> ArithmeticExpr -> Either Text ArithmeticSite
arithmeticSite commandLine document positions wrapper expression = do
  (startPosition, endPosition) <- positionPair wrapper
  beginOffset <- offset startPosition
  endOffset <- offset endPosition
  opening <- case wrapper of
    T_Arithmetic {} -> pure "(("
    T_DollarArithmetic {} -> pure "$(("
    T_DollarBracket {} -> pure "$["
    TA_Sequence {} -> pure ""
    _ -> Left "Arithmetic source evidence requires its original expansion or command wrapper"
  let closing
        | opening == "$[" = "]"
        | T.null opening = ""
        | otherwise = "))"
      original = slice beginOffset endOffset
  contents <- maybe (Left "Arithmetic source range does not match its parser wrapper") Right (T.stripPrefix opening original >>= T.stripSuffix closing)
  let expressionBegin = beginOffset + T.length opening
      expressionEnd = endOffset - T.length closing
      spelling = T.dropWhile (`elem` [' ', '\t']) contents
  cases <- concat <$> traverse (diagnosticCase expressionBegin expressionEnd) (tokens wrapper)
  let expectedOrigins = failingOrigins Nothing expression
  unless (S.fromList [origin | (origin, _, _) <- cases] == expectedOrigins) (Left "Arithmetic error origins do not match the normalized operation tree")
  when (not (null cases) && T.any (`elem` ['$', '\\', '\'', '"', '`']) contents) (Left "Arithmetic error spelling with shell expansion preprocessing is not yet supported")
  unless (commandLine > 0) (Left "Arithmetic expansion requires its enclosing command line")
  pure (MkArithmeticSite (toText (posFile startPosition)) commandLine (fromInteger (posLine endPosition)) spelling cases)
  where
    positionPair token = maybe (Left "Missing original arithmetic source position") Right (M.lookup (getId token) positions)
    offset = sourceOffset document
    slice begin end = T.take (end - begin) (T.drop begin document)
    subtreeRange token = do
      (startPosition, endPosition) <- positionPair token
      initialStart <- offset startPosition
      initialEnd <- offset endPosition
      ranges <- traverse positionPair (tokens token)
      starts <- traverse (offset . fst) ranges
      ends <- traverse (offset . snd) ranges
      pure (foldl' min initialStart starts, foldl' max initialEnd ends)
    diagnosticCase begin end token = case token of
      TA_Binary _ operator _ right | operator `elem` ["/", "%", "**"] -> do
        (rightStart, rightEnd) <- subtreeRange right
        let isPower = operator == "**"
        cursor <- if isPower then afterOperand begin end rightStart rightEnd else pure rightStart
        pure [(originOf token, if isPower then "negative-exponent" else "division-by-zero", slice cursor end)]
      TA_Assignment _ operator _ right | operator `elem` ["/=", "%="] -> do
        (rightStart, rightEnd) <- subtreeRange right
        cursor <- afterOperand begin end rightStart rightEnd
        pure [(originOf token, "division-by-zero", slice cursor end)]
      _ -> pure []
    afterOperand begin end rightStart rightEnd = do
      unless (begin <= rightStart && rightStart < rightEnd && rightEnd <= end) (Left "Arithmetic operand source range is inconsistent")
      let following = slice rightEnd end
          skipped = T.length (T.takeWhile arithmeticSpace following)
      if rightEnd + skipped < end
        then pure (rightEnd + skipped)
        else maybe (Left "Missing final arithmetic operand token") (pure . (rightStart +)) (lastLexemeOffset (slice rightStart rightEnd))

arithmeticSiteMessages :: Bool -> ArithmeticSite -> [(ArithmeticOrigin, Text, Text)]
arithmeticSiteMessages command (MkArithmeticSite file commandLine closingLine spelling cases) =
  [ (origin, code, prefix <> spelling <> ": " <> reason code <> " (error token is \"" <> suffix <> "\")\n")
  | (origin, code, suffix) <- cases
  ]
  where
    prefix = file <> ": line " <> show (if command then closingLine else commandLine) <> ": " <> if command then "((: " else ""
    reason "division-by-zero" = "division by 0"
    reason _ = "exponent less than 0"

originOf :: Token -> ArithmeticOrigin
originOf token = let Id tokenIdentity = getId token in MkArithmeticOrigin tokenIdentity

tokens :: Token -> [Token]
tokens token@(OuterToken _ children) = token : concatMap tokens (toList children)

failingOrigins :: Maybe ArithmeticOrigin -> ArithmeticExpr -> Set ArithmeticOrigin
failingOrigins origin = \case
  ArithmeticLocated location inner -> failingOrigins (Just location) inner
  ArithmeticBinary operator left right -> own (operator `elem` [Divide, Remainder, Power]) <> failingOrigins origin left <> failingOrigins origin right
  ArithmeticAssign _ operator right -> own (operator `elem` [Just Divide, Just Remainder]) <> failingOrigins origin right
  ArithmeticUnary _ inner -> failingOrigins origin inner
  ArithmeticConditional condition yes no -> foldMap (failingOrigins origin) [condition, yes, no]
  ArithmeticSequence values -> foldMap (failingOrigins origin) values
  _ -> mempty
  where
    own required = if required then maybe mempty S.singleton origin else mempty

sourceOffset :: Text -> Position -> Either Text Int
sourceOffset document position = walk 0 1 1 (T.unpack document)
  where
    targetLine = posLine position
    targetColumn = posColumn position
    walk consumed line column remaining
      | line == targetLine && column == targetColumn = pure consumed
      | line > targetLine || (line == targetLine && column > targetColumn) = Left "Arithmetic source position does not identify a character boundary"
      | otherwise = case remaining of
          [] -> Left "Arithmetic source position extends beyond the original document"
          character : rest -> case character of
            '\n' -> walk (consumed + 1) (line + 1) 1 rest
            '\t' -> walk (consumed + 1) line (((column - 1) `div` 8 + 1) * 8 + 1) rest
            _ -> walk (consumed + 1) line (column + 1) rest

arithmeticSpace :: Char -> Bool
arithmeticSpace character = character `elem` [' ', '\t', '\n', '\r']

-- The Bash error cursor retains the final lexical token when lookahead reaches
-- the expression end. This scanner only locates that token in admitted syntax.
lastLexemeOffset :: Text -> Maybe Int
lastLexemeOffset = scan 0 Nothing
  where
    scan consumed latest remaining = case T.uncons remaining of
      Nothing -> latest
      Just (character, rest)
        | arithmeticSpace character -> scan (consumed + 1) latest rest
        | wordCharacter character ->
            let count = T.length (T.takeWhile wordCharacter remaining)
             in scan (consumed + count) (Just consumed) (T.drop count remaining)
        | otherwise ->
            let count = if T.take 2 remaining `elem` ["++", "--", "**", "<<", ">>", "<=", ">=", "==", "!=", "&&", "||"] then 2 else 1
             in scan (consumed + count) (Just consumed) (T.drop count remaining)
    wordCharacter character = isAsciiLower character || isAsciiUpper character || isDigit character || character `elem` ['_', '#', '@']
