module Language.Fish.Translator.Commands.Read
  ( translateRead,
    translateReadM,
    parseReadArgs,
    parseReadArgsDetailed,
    ReadParseResult (..),
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables (translateTokenToExprOrRedirect)
import ShellCheck.AST

-- | read [flags] VARS...
translateRead :: [Token] -> FishCommand TStatus
translateRead ts =
  let (flags, vars, unsupported) = parseReadArgs ts [] [] False
   in if unsupported
        then Command "read" (map translateTokenToExprOrRedirect ts)
        else Read flags vars

translateReadM :: [Token] -> TranslateM (FishCommand TStatus)
translateReadM = pure . translateRead

data ReadParseResult = ReadParseResult
  { readFlags :: [ReadFlag],
    readVars :: [Text],
    readIssues :: [Text],
    readUnsupported :: Bool
  }
  deriving stock (Show, Eq)

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> Bool -> ([ReadFlag], [Text], Bool)
parseReadArgs ts fs vs unsupported =
  let ReadParseResult {readFlags, readVars, readUnsupported} = parseReadArgsDetailed ts fs vs [] unsupported
   in (readFlags, readVars, readUnsupported)

parseReadArgsDetailed :: [Token] -> [ReadFlag] -> [Text] -> [Text] -> Bool -> ReadParseResult
parseReadArgsDetailed [] fs vs issues unsupported =
  let needsSplitNote = ReadArray `elem` fs || length vs > 1
      delimiterNote =
        if any isDelimiterFlag fs
          then ["read delimiter semantics may differ between bash and fish"]
          else []
      issues' =
        if needsSplitNote
          then issues <> ["read IFS splitting semantics may differ between bash and fish"] <> delimiterNote
          else issues <> delimiterNote
   in ReadParseResult fs vs issues' unsupported
  where
    isDelimiterFlag = \case
      ReadDelimiter {} -> True
      _ -> False
parseReadArgsDetailed (x : xs) fs vs issues unsupported =
  case tokenToLiteralText x of
    "-p" -> case xs of
      (p : rest) ->
        parseReadArgsDetailed rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs issues unsupported
      [] -> parseReadArgsDetailed xs fs vs issues True
    "--prompt" -> case xs of
      (p : rest) ->
        parseReadArgsDetailed rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs issues unsupported
      [] -> parseReadArgsDetailed xs fs vs issues True
    "-d" -> parseReadArgValueAllowEmpty True xs fs vs issues unsupported ReadDelimiter "read -d requires a value"
    "--delimiter" -> parseReadArgValueAllowEmpty True xs fs vs issues unsupported ReadDelimiter "read --delimiter requires a value"
    "-n" -> parseReadArgValue xs fs vs issues unsupported ReadNChars "read -n requires a value"
    "--nchars" -> parseReadArgValue xs fs vs issues unsupported ReadNChars "read --nchars requires a value"
    "-t" -> parseReadArgValue xs fs vs issues unsupported ReadTimeout "read -t requires a value"
    "--timeout" -> parseReadArgValue xs fs vs issues unsupported ReadTimeout "read --timeout requires a value"
    "-u" -> parseReadArgValue xs fs vs issues unsupported ReadFD "read -u requires a value"
    "--fd" -> parseReadArgValue xs fs vs issues unsupported ReadFD "read --fd requires a value"
    "-s" ->
      parseReadArgsDetailed xs (fs ++ [ReadSilent]) vs issues unsupported
    "--silent" ->
      parseReadArgsDetailed xs (fs ++ [ReadSilent]) vs issues unsupported
    "-a" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported
    "--array" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported
    "-r" ->
      parseReadArgsDetailed xs fs vs issues unsupported
    tok
      | T.isPrefixOf "-" tok ->
          parseReadShortFlags tok xs fs vs issues unsupported
      | otherwise ->
          parseReadArgsDetailed xs fs (vs ++ [tok]) issues unsupported

parseReadArgValue ::
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  (Text -> ReadFlag) ->
  Text ->
  ReadParseResult
parseReadArgValue xs fs vs issues unsupported mkFlag errMsg =
  parseReadArgValueAllowEmpty False xs fs vs issues unsupported mkFlag errMsg

parseReadArgValueAllowEmpty ::
  Bool ->
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  (Text -> ReadFlag) ->
  Text ->
  ReadParseResult
parseReadArgValueAllowEmpty allowEmpty xs fs vs issues unsupported mkFlag errMsg =
  case xs of
    (p : rest) ->
      let val = tokenToLiteralText p
       in if T.null val && not allowEmpty
            then parseReadArgsDetailed rest fs vs (issues <> [errMsg]) True
            else parseReadArgsDetailed rest (fs ++ [mkFlag val]) vs issues unsupported
    [] -> parseReadArgsDetailed xs fs vs (issues <> [errMsg]) True

parseReadShortFlags ::
  Text ->
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  ReadParseResult
parseReadShortFlags tok xs fs vs issues unsupported =
  case consumeShortFlags (T.unpack (T.drop 1 tok)) fs of
    Left msg ->
      parseReadArgsDetailed xs fs vs (issues <> [msg]) True
    Right (fs', Nothing) ->
      parseReadArgsDetailed xs fs' vs issues unsupported
    Right (fs', Just (allowEmpty, mkFlag, errMsg)) ->
      parseReadArgValueAllowEmpty allowEmpty xs fs' vs issues unsupported mkFlag errMsg

consumeShortFlags ::
  String ->
  [ReadFlag] ->
  Either Text ([ReadFlag], Maybe (Bool, Text -> ReadFlag, Text))
consumeShortFlags chars0 fs0 = go chars0 fs0
  where
    go [] fs = Right (fs, Nothing)
    go (c : cs) fs =
      case c of
        'r' -> go cs fs
        's' -> go cs (fs ++ [ReadSilent])
        'a' -> go cs (fs ++ [ReadArray])
        'd' -> consumeArg True ReadDelimiter "read -d requires a value" cs fs
        'n' -> consumeArg False ReadNChars "read -n requires a value" cs fs
        't' -> consumeArg False ReadTimeout "read -t requires a value" cs fs
        'u' -> consumeArg False ReadFD "read -u requires a value" cs fs
        'p' -> consumeArg True ReadPrompt "read -p requires a value" cs fs
        _ -> Left ("Unsupported read flag: -" <> T.singleton c)

    consumeArg allowEmpty mkFlag errMsg cs fs =
      case cs of
        [] -> Right (fs, Just (allowEmpty, mkFlag, errMsg))
        _ -> Right (fs ++ [mkFlag (toText cs)], Nothing)
