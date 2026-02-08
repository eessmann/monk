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
      issues' =
        if needsSplitNote
          then issues <> ["read IFS splitting semantics may differ between bash and fish"]
          else issues
   in ReadParseResult fs vs issues' unsupported
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
    "-n" -> parseReadArgValue xs fs vs issues unsupported ReadNChars "read -n requires a value"
    "--nchars" -> parseReadArgValue xs fs vs issues unsupported ReadNChars "read --nchars requires a value"
    "-t" -> parseReadArgValue xs fs vs issues unsupported ReadTimeout "read -t requires a value"
    "--timeout" -> parseReadArgValue xs fs vs issues unsupported ReadTimeout "read --timeout requires a value"
    "-u" -> parseReadArgValue xs fs vs issues unsupported ReadFD "read -u requires a value"
    "--fd" -> parseReadArgValue xs fs vs issues unsupported ReadFD "read --fd requires a value"
    "-a" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported
    "--array" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported
    "-r" ->
      parseReadArgsDetailed xs fs vs issues unsupported
    tok
      | Just val <- readShortArg "n" tok -> parseReadArgsDetailed xs (fs ++ [ReadNChars val]) vs issues unsupported
      | Just val <- readShortArg "t" tok -> parseReadArgsDetailed xs (fs ++ [ReadTimeout val]) vs issues unsupported
      | Just val <- readShortArg "u" tok -> parseReadArgsDetailed xs (fs ++ [ReadFD val]) vs issues unsupported
      | T.isPrefixOf "-" tok ->
          parseReadArgsDetailed xs fs vs (issues <> ["Unsupported read flag: " <> tok]) True
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
  case xs of
    (p : rest) ->
      let val = tokenToLiteralText p
       in if T.null val
            then parseReadArgsDetailed rest fs vs (issues <> [errMsg]) True
            else parseReadArgsDetailed rest (fs ++ [mkFlag val]) vs issues unsupported
    [] -> parseReadArgsDetailed xs fs vs (issues <> [errMsg]) True

readShortArg :: Text -> Text -> Maybe Text
readShortArg flag tok =
  if T.isPrefixOf ("-" <> flag) tok && T.length tok > 2
    then Just (T.drop 2 tok)
    else Nothing
