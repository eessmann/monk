{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Read.Runtime
  ( ensureReadDelimHelper,
    currentIfsExpr,
    captureHelperExpr,
    captureHelperCommandToFile,
    helperPipelineStatusExpr,
    statusFromVarCommand,
    collectCommandExpr,
    pipelineFromCommands,
    jobListFromCommand,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.DSL qualified as DSL
import Language.Fish.Translator.Commands.Read.Types
import Language.Fish.Translator.Monad
  ( HelperId (..),
    TranslateM,
    ensureHelperScript,
  )
import Language.Fish.Translator.Pipeline (jobPipelineFromList, pipelineOf)
import Language.Fish.Translator.Types

ensureReadDelimHelper :: TranslateM ()
ensureReadDelimHelper =
  ensureHelperScript HelperReadRuntime "perform exact delimiter read" (DSL.script readRuntimeHelperStatements)

readRuntimeHelperStatements :: [DSL.Stmt]
readRuntimeHelperStatements =
  [ readCaptureHelperStmt,
    returnStatusHelperStmt
  ]

readCaptureHelperStmt :: DSL.Stmt
readCaptureHelperStmt =
  DSL.stmt
    ( DSL.function
        "__monk_read_capture_delim"
        []
        ["mode", "delimiter", "raw", "prompt", "silent", "timeout", "nchars", "ifs", "assign_mode", "count"]
        ( DSL.block
            ( pythonStmt
                [ DSL.str ("\n" <> readCapturePythonScript),
                  DSL.var "mode",
                  DSL.var "delimiter",
                  DSL.var "raw",
                  DSL.var "prompt",
                  DSL.var "silent",
                  DSL.var "timeout",
                  DSL.var "nchars",
                  DSL.var "ifs",
                  DSL.var "assign_mode",
                  DSL.var "count"
                ]
                NE.:| []
            )
        )
    )

returnStatusHelperStmt :: DSL.Stmt
returnStatusHelperStmt =
  DSL.stmt
    ( DSL.function
        "__monk_return_status"
        []
        ["code"]
        ( DSL.block
            ( DSL.stmt
                (DSL.return_ (Just (DSL.math (DSL.var "code" NE.:| []))))
                NE.:| []
            )
        )
    )

pythonStmt :: [DSL.Expr 'TStr] -> DSL.Stmt
pythonStmt args =
  DSL.stmt
    ( DSL.command
        "python3"
        ( map
            DSL.arg
            ( [ DSL.str "-S",
                DSL.str "-c",
                DSL.str pythonDedentExecScript
              ]
                <> args
            )
        )
    )

currentIfsExpr :: FishExpr (TList TStr)
currentIfsExpr = collectCommandExpr ifsOutputCommand
  where
    ifsOutputCommand =
      If
        (jobListFromCommand (Command "set" [ExprVal (ExprLiteral "-q"), ExprVal (ExprLiteral "IFS")]))
        ( Stmt
            ( Command
                "string"
                [ ExprVal (ExprLiteral "join"),
                  ExprVal (ExprLiteral ""),
                  ExprVal (ExprLiteral "--"),
                  ExprVal (ExprVariable (VarAll "IFS"))
                ]
            )
            NE.:| []
        )
        [ Stmt
            ( Command
                "string"
                [ ExprVal (ExprLiteral "unescape"),
                  ExprVal (ExprLiteral "--"),
                  ExprVal (ExprLiteral " \\t\\n")
                ]
            )
        ]
        []

captureHelperExpr :: ExactReadDelim -> FishExpr (TList TStr)
captureHelperExpr spec =
  ExprCommandSubst
    ( Stmt
        ( Pipeline
            ( pipelineFromCommands
                (captureHelperCommand spec)
                [split0Command]
            )
        )
        NE.:| []
    )

collectCommandExpr :: FishCommand TStatus -> FishExpr (TList TStr)
collectCommandExpr cmd =
  ExprCommandSubst
    ( Stmt
        ( Pipeline
            ( pipelineFromCommands
                cmd
                [collectNoTrimCommand]
            )
        )
        NE.:| []
    )

collectNoTrimCommand :: FishCommand TStatus
collectNoTrimCommand =
  Command
    "string"
    [ ExprVal (ExprLiteral "collect"),
      ExprVal (ExprLiteral "--allow-empty"),
      ExprVal (ExprLiteral "--no-trim-newlines")
    ]

captureHelperCommand :: ExactReadDelim -> FishCommand TStatus
captureHelperCommand spec =
  Command
    "__monk_read_capture_delim"
    ( map ExprVal (captureHelperArgs spec)
        <> captureHelperInputRedirects spec
    )

captureHelperCommandToFile :: ExactReadDelim -> FishCommand TStatus
captureHelperCommandToFile spec =
  Command
    "__monk_read_capture_delim"
    ( map ExprVal (captureHelperArgs spec)
        <> captureHelperInputRedirects spec
        <> [ RedirectVal
               ( MkRedirect
                   RedirectStdout
                   RedirectOut
                   (RedirectFile (ExprVariable (VarScalar "__monk_read_capture_file")))
               )
           ]
    )

captureHelperArgs :: ExactReadDelim -> [FishExpr TStr]
captureHelperArgs spec =
  [ ExprLiteral (delimiterModeArg (erdDelimiter spec)),
    ExprLiteral (delimiterValueArg (erdDelimiter spec)),
    ExprLiteral (if erdRaw spec then "1" else "0"),
    ExprLiteral (fromMaybe "" (erdPrompt spec)),
    ExprLiteral (if erdSilent spec then "1" else "0"),
    ExprLiteral (fromMaybe "" (erdTimeout spec)),
    ExprLiteral (fromMaybe "" (erdNChars spec)),
    ExprVariable (VarScalar "__monk_read_ifs"),
    ExprLiteral assignmentMode,
    ExprLiteral assignmentCount
  ]
  where
    (assignmentMode, assignmentCount) =
      case erdTarget spec of
        ExactReadArray _ -> ("array", "")
        ExactReadVars names -> ("vars", T.pack (show (length names)))

captureHelperInputRedirects :: ExactReadDelim -> [ExprOrRedirect]
captureHelperInputRedirects spec =
  maybe
    []
    (\fd -> [RedirectVal (MkRedirect RedirectStdin RedirectIn (RedirectTargetFD fd))])
    (erdFD spec)

delimiterModeArg :: ExactReadDelimiter -> Text
delimiterModeArg = \case
  ExactReadDelimited {} -> "char"
  ExactReadNull -> "null"

delimiterValueArg :: ExactReadDelimiter -> Text
delimiterValueArg = \case
  ExactReadDelimited txt -> txt
  ExactReadNull -> ""

split0Command :: FishCommand TStatus
split0Command = Command "string" [ExprVal (ExprLiteral "split0")]

helperPipelineStatusExpr :: FishExpr TStr
helperPipelineStatusExpr =
  ExprVariable
    ( VarIndex
        "pipestatus"
        (IndexSingle (ExprNumLiteral 1))
    )

statusFromVarCommand :: Text -> FishCommand TStatus
statusFromVarCommand name =
  Command
    "__monk_return_status"
    [ ExprVal (ExprVariable (VarScalar name))
    ]

pipelineFromCommands :: FishCommand TStatus -> [FishCommand TStatus] -> FishJobPipeline
pipelineFromCommands firstCmd rest =
  jobPipelineFromList (firstCmd NE.:| rest)

jobListFromCommand :: FishCommand TStatus -> FishJobList
jobListFromCommand cmd =
  MkFishJobList
    ( MkFishJobConjunction
        Nothing
        (pipelineOf cmd)
        []
        NE.:| []
    )

pythonDedentExecScript :: Text
pythonDedentExecScript =
  "import sys, textwrap; script = textwrap.dedent(sys.argv[1]); sys.argv = [sys.argv[0]] + sys.argv[2:]; exec(script)"

readCapturePythonScript :: Text
readCapturePythonScript =
  T.unlines
    [ "import os",
      "import select",
      "import sys",
      "import termios",
      "import time",
      "",
      "mode, delimiter, raw_s, prompt, silent_s, timeout_s, nchars_s, ifs, assign_mode, count_s = sys.argv[1:11]",
      "raw = raw_s == '1'",
      "silent = silent_s == '1'",
      "timeout = None if timeout_s == '' else float(timeout_s)",
      "nchars = None if nchars_s == '' else int(nchars_s)",
      "delim = b'\\0' if mode == 'null' else delimiter.encode('utf-8')[:1]",
      "fd = sys.stdin.fileno()",
      "deadline = None if timeout is None else time.monotonic() + timeout",
      "buf = bytearray()",
      "escaped = False",
      "emitted = 0",
      "status = 1",
      "orig = None",
      "if prompt:",
      "    sys.stderr.write(prompt)",
      "    sys.stderr.flush()",
      "if silent and os.isatty(fd):",
      "    try:",
      "        orig = termios.tcgetattr(fd)",
      "        hidden = termios.tcgetattr(fd)",
      "        hidden[3] &= ~termios.ECHO",
      "        termios.tcsetattr(fd, termios.TCSADRAIN, hidden)",
      "    except Exception:",
      "        orig = None",
      "try:",
      "    while True:",
      "        if deadline is not None:",
      "            remaining = deadline - time.monotonic()",
      "            if remaining <= 0:",
      "                status = 142",
      "                break",
      "            ready, _, _ = select.select([fd], [], [], remaining)",
      "            if not ready:",
      "                status = 142",
      "                break",
      "        ch = os.read(fd, 1)",
      "        if ch == b'':",
      "            status = 1",
      "            break",
      "        if not raw and not escaped and ch == b'\\\\':",
      "            escaped = True",
      "            continue",
      "        if not raw and escaped:",
      "            buf.extend(ch)",
      "            escaped = False",
      "            emitted += 1",
      "        else:",
      "            if ch == delim:",
      "                status = 0",
      "                break",
      "            buf.extend(ch)",
      "            emitted += 1",
      "        if nchars is not None and emitted >= nchars:",
      "            status = 0",
      "            break",
      "finally:",
      "    if orig is not None:",
      "        try:",
      "            termios.tcsetattr(fd, termios.TCSADRAIN, orig)",
      "        except Exception:",
      "            pass",
      "record = bytes(buf).decode('utf-8', errors='surrogateescape')",
      "count = int(count_s) if assign_mode == 'vars' else 0",
      "",
      "def is_ws(ch):",
      "    return ch in ifs and ch in ' \\t\\n'",
      "",
      "def is_nonws(ch):",
      "    return ch in ifs and ch not in ' \\t\\n'",
      "",
      "def parse_fields():",
      "    if ifs == '':",
      "        return ([], []) if record == '' else ([record], [0])",
      "    fields = []",
      "    starts = []",
      "    pos = 0",
      "    length = len(record)",
      "    while pos < length and is_ws(record[pos]):",
      "        pos += 1",
      "    while pos < length:",
      "        starts.append(pos)",
      "        if is_nonws(record[pos]):",
      "            fields.append('')",
      "            pos += 1",
      "            while pos < length and is_ws(record[pos]):",
      "                pos += 1",
      "            continue",
      "        start = pos",
      "        while pos < length and not is_ws(record[pos]) and not is_nonws(record[pos]):",
      "            pos += 1",
      "        fields.append(record[start:pos])",
      "        if pos >= length:",
      "            break",
      "        if is_nonws(record[pos]):",
      "            pos += 1",
      "        while pos < length and is_ws(record[pos]):",
      "            pos += 1",
      "    return fields, starts",
      "",
      "fields, starts = parse_fields()",
      "if assign_mode == 'array':",
      "    values = fields",
      "elif len(fields) > count and count > 0:",
      "    values = fields[:count - 1]",
      "    end = len(record)",
      "    while end > 0 and is_ws(record[end - 1]):",
      "        end -= 1",
      "    start = starts[count - 1]",
      "    values.append(record[start:end] if start < end else '')",
      "else:",
      "    values = fields[:count] + [''] * max(0, count - len(fields))",
      "for value in values:",
      "    sys.stdout.buffer.write(value.encode('utf-8', errors='surrogateescape') + b'\\0')",
      "sys.exit(status)"
    ]
