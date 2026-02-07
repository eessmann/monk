{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Monad (foldM)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Show qualified as GHC
import Monk
import Options.Applicative
import ShellCheck.AST (Token (..), pattern T_SimpleCommand, pattern T_SourceCommand)
import ShellCheck.ASTLib (getLiteralStringDef)
import ShellCheck.Interface (ParseResult (..), Position (..), PositionedComment (..))
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isRelative, replaceExtension, takeDirectory, (</>))
import System.IO (hPutStrLn)

data SourceMode
  = SourceInline
  | SourceSeparate
  deriving stock (Show, Eq)

data Options = Options
  { optInput :: FilePath,
    optOutput :: Maybe FilePath,
    optStrict :: Bool,
    optQuietWarnings :: Bool,
    optRecursive :: Bool,
    optSourceMode :: SourceMode
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Translate bash scripts to fish"))
  runWithOptions opts

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument (metavar "FILE" <> help "Bash script to translate")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE" <> help "Write output to file"))
    <*> switch (long "strict" <> help "Fail on unsupported constructs")
    <*> switch (short 'q' <> long "quiet-warnings" <> help "Suppress warnings")
    <*> switch (long "recursive" <> help "Recursively translate sourced scripts")
    <*> option
      (eitherReader parseSourceMode)
      ( long "sources"
          <> metavar "MODE"
          <> value SourceSeparate
          <> showDefaultWith renderSourceMode
          <> help "Source handling mode when --recursive is set (inline|separate)"
      )

parseSourceMode :: String -> Either String SourceMode
parseSourceMode = \case
  "inline" -> Right SourceInline
  "separate" -> Right SourceSeparate
  other -> Left ("invalid source mode: " <> other)

renderSourceMode :: SourceMode -> String
renderSourceMode = \case
  SourceInline -> "inline"
  SourceSeparate -> "separate"

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let cfg = if optStrict opts then strictConfig else defaultConfig
  rootPath <- canonicalizePath (optInput opts)
  translations <- translateAll opts cfg rootPath
  case optSourceMode opts of
    SourceInline ->
      outputInline opts translations rootPath
    SourceSeparate ->
      outputSeparate opts translations rootPath

translateAll :: Options -> TranslateConfig -> FilePath -> IO (M.Map FilePath Translation)
translateAll opts cfg rootPath =
  go Set.empty M.empty [rootPath]
  where
    go _ acc [] = pure acc
    go seen acc (path : rest)
      | Set.member path seen = go seen acc rest
      | otherwise = do
          translation <- translateFile opts cfg path
          let next =
                if optRecursive opts
                  then catMaybes (M.elems (trSourceMap translation))
                  else []
          go (Set.insert path seen) (M.insert path translation acc) (rest <> next)

translateFile :: Options -> TranslateConfig -> FilePath -> IO Translation
translateFile opts cfg path = do
  parseResE <- parseBashFile path
  case parseResE of
    Left errs -> do
      emitParseErrors errs
      exitFailure
    Right parseRes -> do
      unless (optQuietWarnings opts) $
        emitParseWarnings (prComments parseRes)
      case translateParseResult cfg parseRes of
        Left err -> do
          emitTranslateError err
          exitFailure
        Right (stmt, st) -> do
          unless (optQuietWarnings opts) $
            emitTranslateWarnings (warnings st)
          unless (optQuietWarnings opts) $
            emitTranslateNotes path (warnings st)
          sourceMap <-
            if optRecursive opts
              then collectSourceMap opts path (prRoot parseRes)
              else pure mempty
          let stmts = flattenStatements stmt
          pure
            Translation
              { trPath = path,
                trStatements = stmts,
                trState = st,
                trSourceMap = sourceMap
              }

flattenStatements :: FishStatement -> [FishStatement]
flattenStatements = \case
  StmtList xs -> xs
  stmt -> [stmt]

collectSourceMap :: Options -> FilePath -> Maybe Token -> IO (M.Map Text (Maybe FilePath))
collectSourceMap opts path mRoot = do
  let sources = maybe [] collectSourceArgs mRoot
      baseDir = takeDirectory path
      literals = map tokenToLiteralText sources
  resolved <- foldM (resolveSource baseDir) M.empty literals
  pure resolved
  where
    resolveSource base acc txt
      | T.null txt = do
          emitWarn opts "warning: non-literal source path; skipping"
          pure acc
      | M.member txt acc = pure acc
      | otherwise = do
          mPath <- resolveSourcePath base txt
          when (isNothing mPath) $
            emitWarn opts ("warning: source file not found: " <> txt)
          pure (M.insert txt mPath acc)

resolveSourcePath :: FilePath -> Text -> IO (Maybe FilePath)
resolveSourcePath base txt = do
  let raw = toString txt
      candidate = if isRelative raw then base </> raw else raw
  exists <- doesFileExist candidate
  if exists
    then Just <$> canonicalizePath candidate
    else pure Nothing

collectSourceArgs :: Token -> [Token]
collectSourceArgs tok =
  let direct =
        case tok of
          T_SourceCommand _ _ pathTok -> [pathTok]
          T_SimpleCommand _ _ (cmdTok : argTok : _)
            | isSourceCmd cmdTok -> [argTok]
          _ -> []
   in direct <> concatMap collectSourceArgs (children tok)

children :: Token -> [Token]
children (OuterToken _ inner) = toList inner

isSourceCmd :: Token -> Bool
isSourceCmd tok =
  let name = tokenToLiteralText tok
   in name == "source" || name == "."

outputInline :: Options -> M.Map FilePath Translation -> FilePath -> IO ()
outputInline opts translations rootPath =
  case M.lookup rootPath translations of
    Nothing -> emitWarn opts "warning: no translation output"
    Just _ -> do
      stmts <- inlineStatements (emitWarn opts) translations Set.empty rootPath
      writeOutput opts (renderFish stmts)

outputSeparate :: Options -> M.Map FilePath Translation -> FilePath -> IO ()
outputSeparate opts translations rootPath = do
  case M.lookup rootPath translations of
    Nothing -> emitWarn opts "warning: no translation output"
    Just rootTr -> do
      let rewritten = rewriteSources translations rootTr
      writeOutput opts (renderFish rewritten)
      when (optRecursive opts) $ do
        let writeRoot = isNothing (optOutput opts)
        forM_ (M.elems translations) $ \tr -> do
          let outPath = replaceExtension (trPath tr) "fish"
              rendered = renderFish (rewriteSources translations tr)
              shouldWrite = writeRoot || trPath tr /= rootPath
          when shouldWrite $
            writeFileText outPath rendered

writeOutput :: Options -> Text -> IO ()
writeOutput opts output =
  case optOutput opts of
    Nothing -> putText output
    Just path -> writeFileText path output

rewriteSources :: M.Map FilePath Translation -> Translation -> [FishStatement]
rewriteSources translations tr =
  map (rewriteStatement sourceRewrite) (trStatements tr)
  where
    sourceRewrite txt =
      case M.lookup txt (trSourceMap tr) >>= id of
        Just resolved
          | M.member resolved translations ->
              toText (replaceExtension (toString txt) "fish")
        _ -> txt

rewriteStatement :: (Text -> Text) -> FishStatement -> FishStatement
rewriteStatement f = \case
  Stmt cmd -> Stmt (rewriteCommand f cmd)
  StmtList xs -> StmtList (map (rewriteStatement f) xs)
  other -> other

rewriteCommand :: (Text -> Text) -> FishCommand t -> FishCommand t
rewriteCommand f = \case
  Source expr -> Source (rewriteSourceExpr f expr)
  Begin body suffix -> Begin (NE.map (rewriteStatement f) body) suffix
  If cond thn els suffix ->
    If
      (rewriteJobList f cond)
      (NE.map (rewriteStatement f) thn)
      (map (rewriteStatement f) els)
      suffix
  While cond body suffix ->
    While (rewriteJobList f cond) (NE.map (rewriteStatement f) body) suffix
  For var listExpr body suffix ->
    For var listExpr (NE.map (rewriteStatement f) body) suffix
  Switch expr cases suffix ->
    Switch expr (NE.map (rewriteCaseItem f) cases) suffix
  Function func ->
    Function func {funcBody = NE.map (rewriteStatement f) (funcBody func)}
  Pipeline pipe -> Pipeline (rewritePipeline f pipe)
  JobConj jc -> JobConj (rewriteConjunction f jc)
  Semicolon c1 c2 -> Semicolon (rewriteCommand f c1) (rewriteCommand f c2)
  Not cmd -> Not (rewriteCommand f cmd)
  Background cmd -> Background (rewriteCommand f cmd)
  Decorated dec cmd -> Decorated dec (rewriteCommand f cmd)
  other -> other

rewriteSourceExpr :: (Text -> Text) -> FishExpr TStr -> FishExpr TStr
rewriteSourceExpr f = \case
  ExprLiteral txt -> ExprLiteral (f txt)
  other -> other

rewriteCaseItem :: (Text -> Text) -> CaseItem -> CaseItem
rewriteCaseItem f (CaseItem pats body) =
  CaseItem pats (NE.map (rewriteStatement f) body)

rewriteJobList :: (Text -> Text) -> FishJobList -> FishJobList
rewriteJobList f (FishJobList conj) =
  FishJobList (NE.map (rewriteConjunction f) conj)

rewriteConjunction :: (Text -> Text) -> FishJobConjunction -> FishJobConjunction
rewriteConjunction f jc =
  jc
    { jcJob = rewritePipeline f (jcJob jc),
      jcContinuations = map (rewriteConjCont f) (jcContinuations jc)
    }

rewriteConjCont :: (Text -> Text) -> FishJobConjCont -> FishJobConjCont
rewriteConjCont f = \case
  JCAnd pipe -> JCAnd (rewritePipeline f pipe)
  JCOr pipe -> JCOr (rewritePipeline f pipe)

rewritePipeline :: (Text -> Text) -> FishJobPipeline -> FishJobPipeline
rewritePipeline f pipe =
  pipe
    { jpStatement = rewriteStatement f (jpStatement pipe),
      jpCont = map rewritePipeCont (jpCont pipe)
    }
  where
    rewritePipeCont cont =
      cont {jpcStatement = rewriteStatement f (jpcStatement cont)}

emitParseWarnings :: [PositionedComment] -> IO ()
emitParseWarnings = mapM_ (hPutStrLn stderr . toString . renderParseComment)

emitParseErrors :: [PositionedComment] -> IO ()
emitParseErrors = mapM_ (hPutStrLn stderr . toString . renderParseComment)

renderParseComment :: PositionedComment -> Text
renderParseComment pc =
  let pos = pcStartPos pc
      loc = formatPosition pos
   in loc <> ": " <> toText (GHC.show (pcComment pc))

formatPosition :: Position -> Text
formatPosition pos =
  toText (posFile pos) <> ":" <> show (posLine pos) <> ":" <> show (posColumn pos)

emitTranslateWarnings :: [Warning] -> IO ()
emitTranslateWarnings = mapM_ (hPutStrLn stderr . toString . renderWarning)

renderWarning :: Warning -> Text
renderWarning Warning {warnMessage = msg, warnRange = mRange} =
  case mRange of
    Nothing -> "warning: " <> msg
    Just range -> formatRange range <> ": warning: " <> msg

emitTranslateError :: TranslateError -> IO ()
emitTranslateError err =
  hPutStrLn stderr (toString (renderTranslateError err))

renderTranslateError :: TranslateError -> Text
renderTranslateError = \case
  Unsupported msg mRange ->
    case mRange of
      Nothing -> "error: " <> msg
      Just range -> formatRange range <> ": error: " <> msg
  InternalError msg -> "error: " <> msg

formatRange :: SourceRange -> Text
formatRange SourceRange {rangeStart = SourcePos {..}} =
  srcFile <> ":" <> show srcLine <> ":" <> show srcColumn

emitTranslateNotes :: FilePath -> [Warning] -> IO ()
emitTranslateNotes path warns = do
  let (highCount, mediumCount, lowCount) = summarizeWarnings warns
      total = highCount + mediumCount + lowCount
      score = confidenceScore warns
      noteHeader = "note: " <> toText path <> ": translation confidence " <> T.pack (show score) <> "/100"
      details =
        "note: "
          <> T.pack (show total)
          <> " warning(s) ("
          <> T.pack (show highCount)
          <> " high, "
          <> T.pack (show mediumCount)
          <> " medium, "
          <> T.pack (show lowCount)
          <> " low)"
  hPutStrLn stderr (toString noteHeader)
  when (total > 0) $
    hPutStrLn stderr (toString details)
  when (highCount > 0) $
    hPutStrLn stderr "note: high-risk translations present; review recommended"

data WarningSeverity
  = WarnHigh
  | WarnMedium
  | WarnLow
  deriving stock (Eq, Show)

summarizeWarnings :: [Warning] -> (Int, Int, Int)
summarizeWarnings warns =
  foldl' tally (0, 0, 0) warns
  where
    tally (highC, medC, lowC) warn =
      case warningSeverity warn of
        WarnHigh -> (highC + 1, medC, lowC)
        WarnMedium -> (highC, medC + 1, lowC)
        WarnLow -> (highC, medC, lowC + 1)

confidenceScore :: [Warning] -> Int
confidenceScore warns =
  let (highC, medC, lowC) = summarizeWarnings warns
      raw = 100 - (highC * 20) - (medC * 10) - (lowC * 4)
   in max 0 (min 100 raw)

warningSeverity :: Warning -> WarningSeverity
warningSeverity Warning {warnMessage = msg} =
  let msgLower = T.toLower msg
   in if any (`T.isInfixOf` msgLower) highKeywords
        then WarnHigh
        else if any (`T.isInfixOf` msgLower) mediumKeywords
          then WarnMedium
          else WarnLow
  where
    highKeywords =
      [ "unsupported",
        "not supported",
        "no direct fish equivalent",
        "manual",
        "skipped"
      ]
    mediumKeywords =
      [ "emitting comment",
        "outside a function",
        "may require manual",
        "may need"
      ]

emitWarn :: Options -> Text -> IO ()
emitWarn opts msg =
  unless (optQuietWarnings opts) $
    hPutStrLn stderr (toString msg)


tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""
