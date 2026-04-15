{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Read
  ( translateRead,
    translateReadM,
    parseReadArgs,
    parseReadArgsDetailed,
    ReadParseResult (..),
  )
where

import Prelude hiding (get, gets, modify)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Monad
  ( TranslateM,
    TranslateState (..),
    TranslationContext (..),
  )
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables (translateTokenToExprOrRedirect)
import Polysemy.State (get, gets, modify)
import ShellCheck.AST

-- | read [flags] VARS...
translateRead :: [Token] -> FishCommand TStatus
translateRead ts =
  let (flags, vars, unsupported) = parseReadArgs ts [] [] False
   in if unsupported
        then Command "read" (map translateTokenToExprOrRedirect ts)
        else Read flags vars

translateReadM :: [Token] -> TranslateM (FishCommand TStatus)
translateReadM ts =
  let parsed = parseReadArgsDetailed ts [] [] [] False False
   in if readUnsupported parsed
        then pure (Command "read" (map translateTokenToExprOrRedirect ts))
        else case simpleReadDelim parsed of
          Just spec -> translateReadDelimHelperM spec
          Nothing -> pure (Read (readFlags parsed) (readVars parsed))

data ReadParseResult = ReadParseResult
  { readFlags :: [ReadFlag],
    readVars :: [Text],
    readIssues :: [Text],
    readUnsupported :: Bool,
    readRaw :: Bool
  }
  deriving stock (Show, Eq)

data SimpleReadDelim = SimpleReadDelim
  { srdDelimiter :: Text,
    srdPrompt :: Maybe Text,
    srdRaw :: Bool,
    srdVar :: Text
  }
  deriving stock (Show, Eq)

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> Bool -> ([ReadFlag], [Text], Bool)
parseReadArgs ts fs vs unsupported =
  let ReadParseResult {readFlags, readVars, readUnsupported} =
        parseReadArgsDetailed ts fs vs [] unsupported False
   in (readFlags, readVars, readUnsupported)

parseReadArgsDetailed :: [Token] -> [ReadFlag] -> [Text] -> [Text] -> Bool -> Bool -> ReadParseResult
parseReadArgsDetailed [] fs vs issues unsupported raw =
  let needsSplitNote = ReadArray `elem` fs || length vs > 1
      delimiterNote =
        if any isDelimiterFlag fs && not (isSimpleReadDelimFlags fs vs unsupported)
          then ["read delimiter semantics may differ between bash and fish"]
          else []
      issues' =
        if needsSplitNote
          then issues <> ["read IFS splitting semantics may differ between bash and fish"] <> delimiterNote
          else issues <> delimiterNote
   in ReadParseResult fs vs issues' unsupported raw
  where
    isDelimiterFlag = \case
      ReadDelimiter {} -> True
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
    "-d" -> parseReadArgValueAllowEmpty True xs fs vs issues unsupported raw ReadDelimiter "read -d requires a value"
    "--delimiter" -> parseReadArgValueAllowEmpty True xs fs vs issues unsupported raw ReadDelimiter "read --delimiter requires a value"
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
parseReadArgValue xs fs vs issues unsupported raw mkFlag errMsg =
  parseReadArgValueAllowEmpty False xs fs vs issues unsupported raw mkFlag errMsg

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
consumeShortFlags chars0 fs0 raw0 = go chars0 fs0 raw0
  where
    go [] fs raw = Right (fs, raw, Nothing)
    go (c : cs) fs raw =
      case c of
        'r' -> go cs fs True
        's' -> go cs (fs ++ [ReadSilent]) raw
        'a' -> go cs (fs ++ [ReadArray]) raw
        'd' -> consumeArg True ReadDelimiter "read -d requires a value" cs fs raw
        'n' -> consumeArg False ReadNChars "read -n requires a value" cs fs raw
        't' -> consumeArg False ReadTimeout "read -t requires a value" cs fs raw
        'u' -> consumeArg False ReadFD "read -u requires a value" cs fs raw
        'p' -> consumeArg True ReadPrompt "read -p requires a value" cs fs raw
        _ -> Left ("Unsupported read flag: -" <> T.singleton c)

    consumeArg allowEmpty mkFlag errMsg cs fs raw =
      case cs of
        [] -> Right (fs, raw, Just (allowEmpty, mkFlag, errMsg))
        _ -> Right (fs ++ [mkFlag (toText cs)], raw, Nothing)

simpleReadDelim :: ReadParseResult -> Maybe SimpleReadDelim
simpleReadDelim ReadParseResult {readFlags, readVars, readUnsupported, readRaw}
  | readUnsupported = Nothing
  | otherwise =
      case (readVars, delimiters, prompts) of
        ([var], [delim], []) | isAllowedFlags readFlags && not (T.null delim) ->
          Just
            SimpleReadDelim
              { srdDelimiter = T.take 1 delim,
                srdPrompt = Nothing,
                srdRaw = readRaw,
                srdVar = var
              }
        ([var], [delim], [prompt]) | isAllowedFlags readFlags && not (T.null delim) ->
          Just
            SimpleReadDelim
              { srdDelimiter = T.take 1 delim,
                srdPrompt = Just prompt,
                srdRaw = readRaw,
                srdVar = var
              }
        _ -> Nothing
  where
    prompts = [p | ReadPrompt p <- readFlags]
    delimiters = [d | ReadDelimiter d <- readFlags]
    isAllowedFlags =
      all
        ( \case
            ReadPrompt {} -> True
            ReadDelimiter {} -> True
            _ -> False
        )

isSimpleReadDelimFlags :: [ReadFlag] -> [Text] -> Bool -> Bool
isSimpleReadDelimFlags flags vars unsupported =
  case simpleReadDelim (ReadParseResult flags vars [] unsupported False) of
    Just _ -> True
    Nothing -> False

translateReadDelimHelperM :: SimpleReadDelim -> TranslateM (FishCommand TStatus)
translateReadDelimHelperM spec = do
  ensureReadDelimHelper
  scopeFlags <- readScopeFlags (srdVar spec)
  pure (Begin (captureBody scopeFlags spec) [])
  where
    captureBody scopeFlags spec' =
      setCapturedValueStmt spec'
        NE.:| [captureHelperStatusStmt, assignReadVarStmt scopeFlags spec', finishReadStatusStmt]

    setCapturedValueStmt spec' =
      Stmt
        ( Set
            [SetLocal]
            "__monk_read_delim_value"
            (captureHelperExpr spec')
        )

    captureHelperStatusStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_read_delim_status"
            (ExprListLiteral [helperPipelineStatusExpr])
        )

    assignReadVarStmt scopeFlags spec' =
      Stmt
        ( Set
            scopeFlags
            (srdVar spec')
            (ExprVariable (VarAll "__monk_read_delim_value"))
        )

    finishReadStatusStmt =
      Stmt
        ( Command
            "test"
            [ ExprVal (ExprVariable (VarScalar "__monk_read_delim_status")),
              ExprVal (ExprLiteral "-eq"),
              ExprVal (ExprLiteral "0")
            ]
        )

readScopeFlags :: Text -> TranslateM [SetFlag]
readScopeFlags name = do
  ctx <- gets context
  let localFlag = if inFunction ctx then SetFunction else SetLocal
  pure $
    if Set.member name (localVars ctx)
      then [localFlag]
      else [SetGlobal]

ensureReadDelimHelper :: TranslateM ()
ensureReadDelimHelper = do
  st <- get
  if readDelimHelperAdded st
    then pure ()
    else
      modify
        ( \s ->
            s
              { readDelimHelperAdded = True,
                preamble = preamble s <> [readDelimHelperStmt]
              }
        )

readDelimHelperStmt :: FishStatement
readDelimHelperStmt =
  Stmt
    ( Function
        FishFunction
          { funcName = "__monk_read_delim",
            funcFlags = [],
            funcParams = ["delimiter", "raw", "prompt"],
            funcBody =
              maybePrintPromptStmt
                NE.:| [ setDelimiterHexStmt,
                        initEscapeStmt,
                        readLoopStmt
                      ]
          }
    )
  where
    maybePrintPromptStmt =
      Stmt
        ( If
            (testCmd [ExprVal (ExprLiteral "-n"), ExprVal (ExprVariable (VarScalar "prompt"))])
            ( printPromptStmt
                NE.:| []
            )
            []
            []
        )

    printPromptStmt =
      Stmt
        ( Command
            "printf"
            [ ExprVal (ExprLiteral "%s"),
              ExprVal (ExprVariable (VarScalar "prompt")),
              RedirectVal (Redirect RedirectStdout RedirectOut (RedirectTargetFD 2))
            ]
        )

    setDelimiterHexStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_read_delim_delim_hex"
            delimiterHexExpr
        )

    initEscapeStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_read_delim_escape"
            (ExprListLiteral [ExprLiteral "0"])
        )

    readLoopStmt =
      Stmt
        ( While
            (jobListFromCommand (Command "true" []))
            ( loopSetHexStmt
                NE.:| [ handleEofStmt,
                        handleRawStmt
                      ]
            )
            []
        )

    loopSetHexStmt =
      Stmt
        ( Set
            [SetLocal]
            "__monk_read_delim_hex"
            readByteHexExpr
        )

    handleEofStmt =
      Stmt
        ( If
            (testCmd [ExprVal (ExprLiteral "-z"), ExprVal (hexValueExpr "__monk_read_delim_hex")])
            ( Stmt (Return (Just (ExprNumLiteral 1)))
                NE.:| []
            )
            []
            []
        )

    handleRawStmt =
      Stmt
        ( If
            (testCmd [ExprVal (ExprVariable (VarScalar "raw")), ExprVal (ExprLiteral "-eq"), ExprVal (ExprLiteral "1")])
            rawBody
            [nonRawBodyStmt]
            []
        )

    rawBody =
      checkDelimiterStmt
        NE.:| [emitByteStmt]

    checkDelimiterStmt =
      Stmt
        ( If
            delimiterMatchCond
            (Stmt (Return (Just (ExprNumLiteral 0))) NE.:| [])
            []
            []
        )

    nonRawBodyStmt =
      Stmt
        ( If
            escapeSetCond
            escapedByteBody
            [handleBackslashOrDelimiterStmt]
            []
        )

    escapedByteBody =
      emitByteStmt
        NE.:| [clearEscapeStmt]

    clearEscapeStmt =
      Stmt
        ( Set
            []
            "__monk_read_delim_escape"
            (ExprListLiteral [ExprLiteral "0"])
        )

    handleBackslashOrDelimiterStmt =
      Stmt
        ( If
            backslashCond
            (setEscapeStmt NE.:| [])
            [handleDelimiterOrEmitStmt]
            []
        )

    setEscapeStmt =
      Stmt
        ( Set
            []
            "__monk_read_delim_escape"
            (ExprListLiteral [ExprLiteral "1"])
        )

    handleDelimiterOrEmitStmt =
      Stmt
        ( If
            delimiterMatchCond
            (Stmt (Return (Just (ExprNumLiteral 0))) NE.:| [])
            [emitByteStmt]
            []
        )

    emitByteStmt =
      Stmt
        ( Command
            "printf"
            [ ExprVal (ExprLiteral "%b"),
              ExprVal
                ( ExprStringConcat
                    (ExprLiteral "\\x")
                    (hexValueExpr "__monk_read_delim_hex")
                )
            ]
        )

    delimiterMatchCond =
      testCmd
        [ ExprVal (hexValueExpr "__monk_read_delim_hex"),
          ExprVal (ExprLiteral "="),
          ExprVal (hexValueExpr "__monk_read_delim_delim_hex")
        ]

    escapeSetCond =
      testCmd
        [ ExprVal (ExprVariable (VarScalar "__monk_read_delim_escape")),
          ExprVal (ExprLiteral "-eq"),
          ExprVal (ExprLiteral "1")
        ]

    backslashCond =
      testCmd
        [ ExprVal (hexValueExpr "__monk_read_delim_hex"),
          ExprVal (ExprLiteral "="),
          ExprVal (ExprLiteral "5c")
        ]

captureHelperExpr :: SimpleReadDelim -> FishExpr (TList TStr)
captureHelperExpr spec =
  ExprCommandSubst
    ( Stmt (Pipeline helperCapturePipe) NE.:| []
    )
  where
    helperCapturePipe =
      pipelineFromCommands
        ( Command
            "__monk_read_delim"
            [ ExprVal (ExprLiteral (srdDelimiter spec)),
              ExprVal (ExprLiteral (if srdRaw spec then "1" else "0")),
              ExprVal (ExprLiteral (maybe "" id (srdPrompt spec)))
            ]
        )
        [Command "string" [ExprVal (ExprLiteral "collect")]]

helperPipelineStatusExpr :: FishExpr TStr
helperPipelineStatusExpr =
  ExprVariable
    ( VarIndex
        "pipestatus"
        (IndexSingle (ExprNumLiteral 1))
    )

delimiterHexExpr :: FishExpr (TList TStr)
delimiterHexExpr =
  ExprListLiteral [ExprJoinList delimiterHexValueExpr]
  where
    delimiterHexValueExpr =
      ExprCommandSubst
        ( Stmt (Pipeline delimiterHexPipe) NE.:| []
        )
    delimiterHexPipe =
      pipelineFromCommands
        ( Command
            "printf"
            [ExprVal (ExprLiteral "%s"), ExprVal (ExprVariable (VarScalar "delimiter"))]
        )
        [ Command "head" [ExprVal (ExprLiteral "-c"), ExprVal (ExprLiteral "1")],
          Command "od" [ExprVal (ExprLiteral "-An"), ExprVal (ExprLiteral "-v"), ExprVal (ExprLiteral "-t"), ExprVal (ExprLiteral "x1")],
          Command "string" [ExprVal (ExprLiteral "trim")],
          Command "string" [ExprVal (ExprLiteral "collect")]
        ]

readByteHexExpr :: FishExpr (TList TStr)
readByteHexExpr =
  ExprListLiteral [ExprJoinList readByteHexValueExpr]
  where
    readByteHexValueExpr =
      ExprCommandSubst
        ( Stmt (Pipeline readByteHexPipe) NE.:| []
        )
    readByteHexPipe =
      pipelineFromCommands
        ( Command
            "dd"
            [ ExprVal (ExprLiteral "bs=1"),
              ExprVal (ExprLiteral "count=1"),
              RedirectVal (Redirect RedirectStderr RedirectOut (RedirectFile (ExprLiteral "/dev/null")))
            ]
        )
        [ Command "od" [ExprVal (ExprLiteral "-An"), ExprVal (ExprLiteral "-v"), ExprVal (ExprLiteral "-t"), ExprVal (ExprLiteral "x1")],
          Command "string" [ExprVal (ExprLiteral "trim")],
          Command "string" [ExprVal (ExprLiteral "collect")]
        ]

pipelineFromCommands :: FishCommand TStatus -> [FishCommand TStatus] -> FishJobPipeline
pipelineFromCommands firstCmd rest =
  FishJobPipeline
    { jpTime = False,
      jpVariables = [],
      jpStatement = Stmt firstCmd,
      jpCont = map (PipeTo [] . Stmt) rest,
      jpBackgrounded = False
    }

jobListFromCommand :: FishCommand TStatus -> FishJobList
jobListFromCommand cmd =
  FishJobList
    ( FishJobConjunction
        Nothing
        (FishJobPipeline False [] (Stmt cmd) [] False)
        []
        NE.:| []
    )

testCmd :: [ExprOrRedirect] -> FishJobList
testCmd = jobListFromCommand . Command "test"

hexValueExpr :: Text -> FishExpr TStr
hexValueExpr name = ExprJoinList (ExprVariable (VarAll name))
