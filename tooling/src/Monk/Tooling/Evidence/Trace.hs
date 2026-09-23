-- | Interpret observed strace process events and collect per-fixture traces.
module Monk.Tooling.Evidence.Trace (parseTrace, runTrace) where

import Data.Aeson (Value (..), eitherDecodeStrict', object, toJSON, (.=))
import Data.ByteString qualified as B
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, cleanEnvironment, digestFile, field, nativeRecord, numberField, readJson, runObservation, textField, unbase64, writeJson)
import System.Directory (canonicalizePath, createDirectory)
import System.FilePath ((</>))

parseTrace :: Text -> Value
parseTrace raw =
  let (executions, creations, _) = foldl' consume ([], [], []) (T.lines raw)
   in object
        [ "successful_execs" .= reverse executions,
          "process_creations" .= reverse creations,
          "successful_exec_count_including_entry_shell" .= length executions,
          "child_process_creation_count_excluding_threads" .= length creations
        ]
  where
    consume :: ([Value], [Value], [(Int, Text)]) -> Text -> ([Value], [Value], [(Int, Text)])
    consume (execs, creates, pending) line = case T.words (T.strip line) of
      pidText : rest
        | Just pid <- readMaybe (T.unpack pidText) ->
            let call = T.unwords rest
             in if "<unfinished ...>" `T.isInfixOf` call
                  then (execs, creates, (pid, T.replace "<unfinished ...>" "" call) : filter ((/= pid) . fst) pending)
                  else
                    let resumed = "<... " `T.isPrefixOf` call && " resumed>" `T.isInfixOf` call
                        suffix = T.drop 1 (snd (T.breakOn ">" call))
                        combined = if resumed then fromMaybe "" (lookup pid pending) <> suffix else call
                        nextPending = if resumed then filter ((/= pid) . fst) pending else pending
                     in classify pid combined execs creates nextPending
      _ -> (execs, creates, pending)
    classify pid call execs creates pending =
      let syscall = T.takeWhile (/= '(') call
          success = "= 0" `T.isSuffixOf` call
          child = case reverse (T.words call) of
            result : "=" : _ -> result
            _ -> ""
          path = case T.breakOn "\"" call of
            (_, quoted)
              | not (T.null quoted) ->
                  let body = T.takeWhile (/= '"') (T.drop 1 quoted)
                   in case eitherDecodeStrict' (TE.encodeUtf8 ("\"" <> body <> "\"")) of
                        Right (String decoded) -> Just decoded
                        _ -> Nothing
            _ -> Nothing
          execs' =
            if syscall `elem` ["execve", "execveat"] && success
              then object ["pid" .= pid, "syscall" .= syscall, "executable" .= path] : execs
              else execs
          creates' =
            if syscall `elem` ["clone", "clone3", "fork", "vfork"] && not ("CLONE_THREAD" `T.isInfixOf` call)
              then case readMaybe (T.unpack child) of
                Just childPid | childPid > (0 :: Int) -> object ["parent_pid" .= pid, "child_pid" .= childPid, "syscall" .= syscall] : creates
                _ -> creates
              else creates
       in (execs', creates', pending)

runTrace :: FilePath -> FilePath -> String -> Maybe FilePath -> IO Value
runTrace output fish variant candidateDirectory = do
  unless (variant `elem` ["baseline", "candidate"]) $ fail "variant must be baseline or candidate"
  cohort <- readJson (output </> "baseline-cohort.json")
  root <- either fail pure (textField "cwd" cohort)
  fixtures <- either fail pure (arrayField "fixtures" cohort)
  common <- either fail pure (arrayField "common16" cohort >>= traverse fixtureString)
  env <- cleanEnvironment output
  fishPath <- canonicalizePath fish
  let traceDirectory = output </> "process-traces-" <> variant
  createDirectory traceDirectory
  rows <- forM fixtures $ \fixture -> do
    fixtureName <- either fail pure (textField "fixture" fixture)
    let arithmetic = field "extra_arithmetic" fixture == Right (Bool True)
    if fixtureName `notElem` common && not arithmetic
      then pure Nothing
      else do
        index <- either fail pure (numberField "index" fixture)
        metadata <- either fail pure (field "metadata" fixture)
        argv <- either fail pure (arrayField "fixtureMetaArgs" metadata >>= traverse fixtureString)
        stdinText <- either fail pure (textField "stdin_base64" fixture)
        inputBytes <- either fail pure (unbase64 stdinText)
        generated <- case variant of
          "baseline" -> pure (output </> "baseline" </> show index <> ".fish")
          _ -> maybe (fail "candidate trace requires --candidate-dir") (\directory -> pure (directory </> show index <> "-default.fish")) candidateDirectory
        let logPath = traceDirectory </> show index <> ".strace"
            command = ["strace", "-f", "-qq", "-s", "4096", "-e", "trace=process", "-o", logPath, fishPath, "--no-config", generated] <> map T.unpack argv
        observed <- runObservation command inputBytes (T.unpack root) env 60
        unless (status observed == "completed") $ fail ("trace command failed for " <> T.unpack fixtureName <> ": " <> T.unpack (status observed))
        parsed <- parseTrace . TE.decodeUtf8 <$> B.readFile logPath
        executions <- either fail pure (arrayField "successful_execs" parsed)
        let entryObserved = any ((== Right (String (T.pack fishPath))) . field "executable") executions
        unless entryObserved $ fail ("trace lacks successful Fish entry exec: " <> T.unpack fixtureName)
        direct <- runObservation ([fishPath, "--no-config", generated] <> map T.unpack argv) inputBytes (T.unpack root) env 60
        unless (status direct == "completed" && exit observed == exit direct && observedStdout observed == observedStdout direct && observedStderr observed == observedStderr direct) $
          fail ("traced execution differs from untraced fixture: " <> T.unpack fixtureName)
        generatedHash <- digestFile generated
        let eventFields = case parsed of Object values -> values; _ -> mempty
            row = case object ["fixture" .= fixtureName, "generated_sha256" .= generatedHash, "command" .= command, "execution" .= nativeRecord observed] of
              Object base -> Object (base <> eventFields)
              value -> value
        pure (Just row)
  let report = catMaybes rows
  writeJson (traceDirectory </> "report.json") (toJSON report)
  pure $
    object
      [ "variant" .= variant,
        "fixtures" .= length report,
        "successful_execs" .= sum (map (fromRight 0 . numberField "successful_exec_count_including_entry_shell") report),
        "child_process_creations" .= sum (map (fromRight 0 . numberField "child_process_creation_count_excluding_threads") report)
      ]
  where
    fixtureString (String value) = Right value
    fixtureString _ = Left "expected fixture string"
