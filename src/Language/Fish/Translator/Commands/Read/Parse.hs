{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Read.Parse
  ( parseReadArgs,
    parseReadArgsDetailed,
    exactReadDelim,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Commands.Read.Types
import Language.Fish.Translator.Token (tokenToLiteralText)
import ShellCheck.AST

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> Bool -> ([ReadFlag], [Text], Bool)
parseReadArgs ts fs vs unsupported =
  let MkReadParseResult {readFlags, readVars, readUnsupported} =
        parseReadArgsDetailed ts fs vs [] unsupported False
   in (readFlags, readVars, readUnsupported)

parseReadArgsDetailed :: [Token] -> [ReadFlag] -> [Text] -> [Text] -> Bool -> Bool -> ReadParseResult
parseReadArgsDetailed [] fs vs issues unsupported raw =
  let parsed = MkReadParseResult fs vs [] unsupported raw
      exact = isJust (exactReadDelim parsed)
      needsSplitNote = not exact && (ReadArray `elem` fs || length vs > 1)
      delimiterNote =
        [ "read delimiter semantics may differ between bash and fish"
          | any isDelimiterFlag fs && not exact
        ]
      issues' =
        if needsSplitNote
          then issues <> ["read IFS splitting semantics may differ between bash and fish"] <> delimiterNote
          else issues <> delimiterNote
   in parsed {readIssues = issues'}
  where
    isDelimiterFlag = \case
      ReadDelimiter {} -> True
      ReadNull -> True
      _ -> False
parseReadArgsDetailed (x : xs) fs vs issues unsupported raw =
  case tokenToLiteralText x of
    "-p" -> case xs of
      (p : rest) ->
        parseReadArgsDetailed rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs issues unsupported raw
      [] -> parseReadArgsDetailed xs fs vs issues True raw
    "--prompt" -> case xs of
      (p : rest) ->
        parseReadArgsDetailed rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs issues unsupported raw
      [] -> parseReadArgsDetailed xs fs vs issues True raw
    "-d" -> parseReadArgValueAllowEmpty True xs fs vs issues unsupported raw delimiterFlagFromValue "read -d requires a value"
    "--delimiter" -> parseReadArgValueAllowEmpty True xs fs vs issues unsupported raw delimiterFlagFromValue "read --delimiter requires a value"
    "-n" -> parseReadArgValue xs fs vs issues unsupported raw ReadNChars "read -n requires a value"
    "--nchars" -> parseReadArgValue xs fs vs issues unsupported raw ReadNChars "read --nchars requires a value"
    "-t" -> parseReadArgValue xs fs vs issues unsupported raw ReadTimeout "read -t requires a value"
    "--timeout" -> parseReadArgValue xs fs vs issues unsupported raw ReadTimeout "read --timeout requires a value"
    "-u" -> parseReadArgValue xs fs vs issues unsupported raw ReadFD "read -u requires a value"
    "--fd" -> parseReadArgValue xs fs vs issues unsupported raw ReadFD "read --fd requires a value"
    "-s" ->
      parseReadArgsDetailed xs (fs ++ [ReadSilent]) vs issues unsupported raw
    "--silent" ->
      parseReadArgsDetailed xs (fs ++ [ReadSilent]) vs issues unsupported raw
    "-a" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported raw
    "--array" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported raw
    "-r" ->
      parseReadArgsDetailed xs fs vs issues unsupported True
    tok
      | T.isPrefixOf "-" tok ->
          parseReadShortFlags tok xs fs vs issues unsupported raw
      | otherwise ->
          parseReadArgsDetailed xs fs (vs ++ [tok]) issues unsupported raw

parseReadArgValue ::
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  Bool ->
  (Text -> ReadFlag) ->
  Text ->
  ReadParseResult
parseReadArgValue =
  parseReadArgValueAllowEmpty False

parseReadArgValueAllowEmpty ::
  Bool ->
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  Bool ->
  (Text -> ReadFlag) ->
  Text ->
  ReadParseResult
parseReadArgValueAllowEmpty allowEmpty xs fs vs issues unsupported raw mkFlag errMsg =
  case xs of
    (p : rest) ->
      let val = tokenToLiteralText p
       in if T.null val && not allowEmpty
            then parseReadArgsDetailed rest fs vs (issues <> [errMsg]) True raw
            else parseReadArgsDetailed rest (fs ++ [mkFlag val]) vs issues unsupported raw
    [] -> parseReadArgsDetailed xs fs vs (issues <> [errMsg]) True raw

parseReadShortFlags ::
  Text ->
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  Bool ->
  ReadParseResult
parseReadShortFlags tok xs fs vs issues unsupported raw =
  case consumeShortFlags (T.unpack (T.drop 1 tok)) fs raw of
    Left msg ->
      parseReadArgsDetailed xs fs vs (issues <> [msg]) True raw
    Right (fs', raw', Nothing) ->
      parseReadArgsDetailed xs fs' vs issues unsupported raw'
    Right (fs', raw', Just (allowEmpty, mkFlag, errMsg)) ->
      parseReadArgValueAllowEmpty allowEmpty xs fs' vs issues unsupported raw' mkFlag errMsg

consumeShortFlags ::
  String ->
  [ReadFlag] ->
  Bool ->
  Either Text ([ReadFlag], Bool, Maybe (Bool, Text -> ReadFlag, Text))
consumeShortFlags = go
  where
    go [] fs raw = Right (fs, raw, Nothing)
    go (c : cs) fs raw =
      case c of
        'r' -> go cs fs True
        's' -> go cs (fs ++ [ReadSilent]) raw
        'a' -> go cs (fs ++ [ReadArray]) raw
        'd' -> consumeArg True delimiterFlagFromValue "read -d requires a value" cs fs raw
        'n' -> consumeArg False ReadNChars "read -n requires a value" cs fs raw
        't' -> consumeArg False ReadTimeout "read -t requires a value" cs fs raw
        'u' -> consumeArg False ReadFD "read -u requires a value" cs fs raw
        'p' -> consumeArg True ReadPrompt "read -p requires a value" cs fs raw
        _ -> Left ("Unsupported read flag: -" <> T.singleton c)

    consumeArg allowEmpty mkFlag errMsg cs fs raw =
      case cs of
        [] -> Right (fs, raw, Just (allowEmpty, mkFlag, errMsg))
        _ -> Right (fs ++ [mkFlag (toText cs)], raw, Nothing)

delimiterFlagFromValue :: Text -> ReadFlag
delimiterFlagFromValue val
  | T.null val = ReadNull
  | otherwise = ReadDelimiter (T.take 1 val)

exactReadDelim :: ReadParseResult -> Maybe ExactReadDelim
exactReadDelim MkReadParseResult {readFlags, readVars, readUnsupported, readRaw}
  | readUnsupported = Nothing
  | otherwise = do
      guard (all exactSupportedFlag readFlags)
      delimiter <- exactDelimiter
      prompt <- optionalSingleValue [p | ReadPrompt p <- readFlags]
      timeout <- optionalSingleValue [t | ReadTimeout t <- readFlags]
      nChars <- optionalSingleValue [n | ReadNChars n <- readFlags]
      fd <- traverse parseFdValue =<< optionalSingleValue [fdVal | ReadFD fdVal <- readFlags]
      guard (maybe True validTimeout timeout)
      guard (maybe True validNChars nChars)
      guard (countFlags isSilentFlag readFlags <= 1)
      target <- exactReadTarget readFlags readVars
      pure
        MkExactReadDelim
          { erdDelimiter = delimiter,
            erdPrompt = prompt,
            erdSilent = any isSilentFlag readFlags,
            erdTimeout = timeout,
            erdNChars = nChars,
            erdFD = fd,
            erdRaw = readRaw,
            erdTarget = target
          }
  where
    delimiterValues =
      [ExactReadDelimited d | ReadDelimiter d <- readFlags]
        <> [ExactReadNull | ReadNull <- readFlags]

    exactDelimiter =
      case delimiterValues of
        [] | any isFdFlag readFlags -> Just (ExactReadDelimited "\n")
        [value] -> Just value
        _ -> Nothing

    exactSupportedFlag = \case
      ReadPrompt {} -> True
      ReadSilent -> True
      ReadArray -> True
      ReadNull -> True
      ReadDelimiter {} -> True
      ReadNChars {} -> True
      ReadTimeout {} -> True
      ReadFD {} -> True
      _ -> False

    isSilentFlag = \case
      ReadSilent -> True
      _ -> False

    isFdFlag = \case
      ReadFD {} -> True
      _ -> False

exactReadTarget :: [ReadFlag] -> [Text] -> Maybe ExactReadTarget
exactReadTarget flags vars =
  case any isArrayFlag flags of
    True ->
      case vars of
        [var] -> Just (ExactReadArray var)
        _ -> Nothing
    False ->
      case vars of
        [] -> Nothing
        _ -> Just (ExactReadVars vars)
  where
    isArrayFlag = \case
      ReadArray -> True
      _ -> False

countFlags :: (ReadFlag -> Bool) -> [ReadFlag] -> Int
countFlags pred' = length . filter pred'

optionalSingleValue :: [a] -> Maybe (Maybe a)
optionalSingleValue = \case
  [] -> Just Nothing
  [value] -> Just (Just value)
  _ -> Nothing

parseFdValue :: Text -> Maybe Int
parseFdValue txt
  | T.null txt = Nothing
  | T.all isDigit txt = readMaybe (toString txt)
  | otherwise = Nothing

validTimeout :: Text -> Bool
validTimeout txt =
  case readMaybe (toString txt) :: Maybe Double of
    Just n -> n >= 0
    Nothing -> False

validNChars :: Text -> Bool
validNChars txt =
  case readMaybe (toString txt) :: Maybe Int of
    Just n -> n >= 0
    Nothing -> False
