{-# LANGUAGE OverloadedStrings #-}

module Monk.Tooling.Runtime.Semantics
  ( runDigest,
    runPrintf,
    runProtocol,
    runPatternParts,
    runRead,
    runExpansion,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List qualified as List
import Monk.Tooling.Process (ProcessResult (..))
import Monk.Tooling.Runtime.Common
import System.Directory (createDirectory)
import System.FilePath ((</>))

b :: String -> B.ByteString
b = C.pack

str :: B.ByteString -> String
str = C.unpack

runDigest :: Context -> IO ()
runDigest ctx = forM_ sizes $ \size -> do
  let payload = B.pack $ take size $ map (fromIntegral . (`mod` 256)) $ iterate next (20260910 :: Int)
  actual <- invoke (runtime ctx) ["--digest"] payload
  oracle <- invoke "sha256sum" [] payload
  check ("digest size " <> show size) $
    code actual == 0 && processStdout actual == B.takeWhile (/= 32) (processStdout oracle)
  where
    sizes = [0, 1, 3, 55, 56, 57, 63, 64, 65, 127, 128, 129, 1024, 65536, 1000000]
    next x = (x * 1664525 + 1013904223) `xor` (x `div` 65536)

runPrintf :: Context -> IO ()
runPrintf ctx = do
  env <- referenceEnvironment
  forM_ (zip [0 :: Int ..] cases) $ \(index, values) -> do
    oracle <- invokeWith "bash" ["--noprofile", "--norc", "-c", "mapfile -d '' -t args; printf \"${args[@]}\"", "printf-case"] (frames values) Nothing (Just env) 10000000
    actual <- invokeWith (runtime ctx) ["--abi", "2", "printf"] (frames values) Nothing (Just env) 10000000
    checkResult ("printf case " <> show index) (code oracle, processStdout oracle, processStderr oracle) actual
  let value = B.replicate 1048576 120
  actual <- invoke (runtime ctx) ["--abi", "2", "printf"] (frames ["%s", value])
  checkResult "printf million-byte payload" (0, value, "") actual
  where
    cases =
      [ ["%s", ""],
        ["<%s>", "a b", "", B.pack [255]],
        ["%s:%d:%%\n", "hello", "-9223372036854775808", "last"],
        ["%d:%d", "9223372036854775807"],
        ["literal", "ignored"],
        ["--", "-x:%s", "ok"],
        ["\\a\\b\\e\\f\\n\\r\\t\\v\\\\"],
        ["\\0\\000\\1\\12\\123\\777\\x1\\x12", "unused"],
        [""]
      ]

runtimeCall :: Context -> String -> [B.ByteString] -> IO ProcessResult
runtimeCall ctx operation values = invoke (runtime ctx) ["--abi", "2", operation] (frames values)

expectRuntime :: Context -> String -> [B.ByteString] -> Int -> B.ByteString -> IO ()
expectRuntime ctx operation values status output = do
  result <- runtimeCall ctx operation values
  check (operation <> " " <> show values <> " -> " <> show result) $ code result == status && processStdout result == output

runProtocol :: Context -> IO ()
runProtocol ctx = do
  expectRuntime ctx "integer" ["add", "9223372036854775807", "1"] 0 "ok\n-9223372036854775808\n-\n"
  forM_ operations $ \(operation, left, right, expression) -> do
    oracle <- invoke "bash" ["-c", "printf \"%s\" \"$(( " <> expression <> " ))\""] ""
    expectRuntime ctx "integer" (map b [operation, left, right]) 0 ("ok\n" <> processStdout oracle <> "\n-\n")
  expectRuntime ctx "integer" ["div", "1", "0"] 0 "error\n-\ndivision-by-zero\n"
  expectRuntime ctx "split" [" :", " :a:: b: "] 0 "\0a\0\0b\0"
  expectRuntime ctx "argv" ["p", "s", "0", "", "z"] 0 "p\0zs\0"
  expectRuntime ctx "argv" ["p", "s", "1"] 0 "ps\0"
  expectRuntime ctx "echo" ["-e", "a\\0b\\xFF\\cignored"] 0 (B.pack [97, 0, 98, 255])
  expectRuntime ctx "echo" [""] 0 "\n"
  expectRuntime ctx "directory-initial-oldpwd" [""] 1 ""
  expectRuntime ctx "pattern" ["match", B.pack [97, 98, 255], "1", "a?", "0", B.singleton 255] 0 ""
  expectRuntime ctx "pattern" ["match", "a", "0", "*"] 1 ""
  expectRuntime ctx "glob" ["0", "/definitely-absent-monk/*"] 0 "/definitely-absent-monk/*\0"
  forM_ [("integer", "add\0"), ("split", "a"), ("pattern", "a\0x\0b\0"), ("argv", "a\0b\0bad\0")] $ \(operation, raw) -> do
    result <- invoke (runtime ctx) ["--abi", "2", operation] raw
    check ("malformed " <> operation) (code result == 125)
  badAbi <- invoke (runtime ctx) ["--abi", "1", "echo"] ""
  check "old ABI rejected" (code badAbi == 125)
  env <- referenceEnvironment
  forM_ ifses $ \ifs -> forM_ splitValues $ \value -> do
    oracle <- invokeWith "bash" ["-c", "mapfile -d '' -t args; IFS=${args[0]}; value=${args[1]}; set -- $value; if (( $# )); then printf \"%s\\0\" \"$@\"; fi", "bash"] (frames [ifs, value]) Nothing (Just env) 10000000
    expectRuntime ctx "split" [ifs, value] 0 (processStdout oracle)
  forM_ echoCases $ \values -> do
    oracle <- invokeWith "bash" ["-c", "mapfile -d '' -t args; echo \"${args[@]}\"", "bash"] (frames values) Nothing (Just env) 10000000
    expectRuntime ctx "echo" values 0 (processStdout oracle)
  withWorkspace $ \directory -> do
    createDirectory (directory </> "dir")
    createDirectory (directory </> "dir" </> "inner")
    forM_ ["a", "b", "star*", ".hidden", "é"] $ \name -> B.writeFile (directory </> name) ""
    forM_ [directory <> "/*", directory <> "//*", "/" <> directory <> "/*", directory <> "//*/", directory <> "/*//", directory <> "/*///", directory <> "/*//inner//", directory <> "///none*"] $ \spelling -> do
      oracle <- invokeWith "bash" ["-c", "printf \"%s\\0\" " <> spelling] "" Nothing (Just env) 10000000
      result <- invokeWith (runtime ctx) ["--abi", "2", "glob"] (frames ["1", b spelling]) (Just directory) (Just env) 10000000
      checkResult ("glob " <> spelling) (code oracle, processStdout oracle, processStderr oracle) result
  expectRuntime ctx "glob" [] 0 "\0"
  expectRuntime ctx "glob" ["0", ""] 0 "\0"
  forM_ [("trim-prefix-short", "${x#a*}", "a*"), ("trim-prefix-long", "${x##a*}", "a*"), ("trim-suffix-short", "${x%*c}", "*c"), ("trim-suffix-long", "${x%%*c}", "*c")] $ \(operation, expression, patternValue) -> do
    oracle <- invoke "bash" ["-c", "x=$1; printf \"%s\" \"" <> expression <> "\"", "bash", "abcabc"] ""
    expectRuntime ctx "pattern" [b operation, "abcabc", b patternValue] 0 (processStdout oracle <> "\0")
  expectRuntime ctx "pattern" ["replace-all", "ababa", "aba", "&"] 0 "&ba\0"
  expectRuntime ctx "pattern" ["replace-first", "abc", "", "x"] 0 "abc\0"
  forM_ ([0 .. 7] :: [Int]) $ \mask -> do
    let closed = concat ["exec " <> show descriptor <> (if descriptor == 0 then "<&-; " else ">&-; ") | descriptor <- ([0 .. 2] :: [Int]), mask `mod` (2 ^ (descriptor + 1)) < 2 ^ descriptor]
    descriptorState <- invoke "bash" ["-c", closed <> "exec \"$1\" --abi 2 descriptor-state", "bash", runtime ctx] ""
    check ("descriptor-state mask " <> show mask <> ": " <> show descriptorState) (code descriptorState == mask)
  platform <- invoke "uname" ["-s"] ""
  let selected = if processStdout platform == "Darwin\n" then B.pack [255, 97, 0] else B.pack [254, 98, 0]
  expectRuntime ctx "bytes-platform" ["ff61", "fe62"] 0 selected
  expectRuntime ctx "bytes-platform" ["", ""] 0 "\0"
  forM_ [["0", ""], ["gg", ""], ["", "xy"], ["00"]] $ \values -> do
    actual <- runtimeCall ctx "bytes-platform" values
    check "bad byte hex rejected" (code actual == 125)
  where
    operations = [("div", "-7", "3", "-7/3"), ("rem", "-7", "3", "-7%3"), ("pow", "3", "41", "3**41"), ("shr", "-7", "65", "-7>>65"), ("add", "64#_", "1", "64#_+1")]
    ifses = ["", " ", ":", " :\t\n", B.singleton 255]
    splitValues = ["", " ", ":", "::", " a : b: ", B.pack [255, 97, 255, 255, 122], "\na\tb  c\n"]
    echoCases = [[""], ["-n", "x"], ["-e", "\\u1234 \\U0001F600"], ["-e", "\\x4a\\0777\\0\\cxxx"], ["-e", "\\\\c"], ["-eE", "\\n"], ["-e", "\\uD800 \\U7FFFFFFF \\U80000000 \\UFFFFFFFF"], [B.singleton 255]]

runPatternParts :: Context -> IO ()
runPatternParts ctx = do
  env <- referenceEnvironment
  let cases = [(operation, spelling, subject, parts) | (operation, spelling) <- operations, subject <- subjects, parts <- patterns] <> randomCases
  forM_ (zip [0 :: Int ..] cases) $ \(index, (operation, spelling, subject, parts)) -> do
    let assignments = "mapfile -d '' -t args; subject=${args[0]}; " <> B.concat [b ("p" <> show n <> "=${args[" <> show (n + 1) <> "]}; ") | n <- [0 .. length parts - 1]]
        expression = B.concat [if active then b ("${p" <> show n <> "}") else b ("\"${p" <> show n <> "}\"") | (n, (active, _)) <- zip [0 :: Int ..] parts]
        script = assignments <> "printf \"%s\\0\" \"${subject" <> spelling <> expression <> "}\""
        values = subject : map snd parts
    oracle <- invokeWith "bash" ["--noprofile", "--norc", "-c", str script, "parameter-oracle"] (frames values) Nothing (Just env) 10000000
    let request = [operation, subject] <> concat [[if active then "1" else "0", value] | (active, value) <- parts]
    actual <- invokeWith (runtime ctx) ["--abi", "2", "pattern-parts"] (frames request) Nothing (Just env) 10000000
    checkResult ("pattern parts " <> show index) (code oracle, processStdout oracle, processStderr oracle) actual
  forM_ [B.concat ["trim-prefix-short\0", "a\0", "bad\0", "a\0"], B.concat ["trim-prefix-short\0", "a\0", "1\0"], B.concat ["unknown\0", "a\0", "1\0", "a\0"]] $ \payload -> do
    actual <- invoke (runtime ctx) ["--abi", "2", "pattern-parts"] payload
    check "malformed pattern parts rejected" (code actual == 125)
  where
    operations = [("trim-prefix-short", "#"), ("trim-prefix-long", "##"), ("trim-suffix-short", "%"), ("trim-suffix-long", "%%")]
    subjects = ["", "a", "ababa", "a*b?x", "[ab]", "a-]b", "\\abc\\", B.pack [254, 255, 97], B.pack [97, 255, 254], "abc123", "a\n"]
    activePatterns = ["", "*", "?", "a*", "*a", "[ab]*", "[!a-z]", "[[:alpha:]]*", "[[:digit:]]", B.pack [91, 254, 45, 255, 93, 42], "\\*"]
    quoted = ["*", "?", "[ab]", "\\", B.singleton 255]
    patterns =
      map (\v -> [(True, v)]) activePatterns
        <> map (\v -> [(False, v)]) quoted
        <> [[(False, "a*"), (True, "?*")], [(True, "*"), (False, "?x")], [(True, "["), (False, "a-z"), (True, "]*")], [(True, "[[:"), (False, "alpha"), (True, ":]]*")], [(False, "["), (True, "*"), (False, "]")], [(True, "a"), (False, "\\"), (True, "*")]]
    randomCases =
      [ let (operation, spelling) = operations `at` (seed `mod` 4)
            subject = randomBytes seed (seed `mod` 9)
            partCount = 1 + seed `mod` 3
            parts = [(odd (seed `div` (n + 1)), randomBytes (seed + 17 * n) ((seed `div` (n + 2)) `mod` 5)) | n <- [0 .. partCount - 1]]
         in (operation, spelling, subject, parts)
      | seed <- take 120 (iterate next 431)
      ]
    at values index = case drop index values of value : _ -> value; [] -> error "random case index"
    next value = (value * 1103515245 + 12345) `mod` 2147483647
    randomBytes seed count = B.pack [B.index (B.pack [97, 42, 63, 91, 93, 45, 92, 255]) ((seed `div` (n + 1)) `mod` 8) | n <- [0 .. count - 1]]

runRead :: Context -> IO ()
runRead ctx = do
  forM_ initial $ \datum -> forM_ modes $ \mode -> compareRead ctx datum True "\n" (-1) " \t\n" mode 2
  forM_ special $ \datum -> forM_ [True, False] $ \raw -> forM_ modes $ \mode -> compareRead ctx (datum <> "\n") raw "\n" (-1) " :" mode 2
  forM_ delimiters $ \datum -> forM_ ["\n", "", ":"] $ \delimiter -> forM_ [0, 1, 3, -1] $ \count -> compareRead ctx datum False delimiter count " \t\n" "reply" 2
  where
    modes = ["scalar", "array", "reply"]
    initial = ["a b c\n", " a  b  c  \n", "a::b:\n", "\n", "", "partial", B.pack [255, 32, 254, 10], "a\0b c\n"]
    special = ["a:", "a::", ":a:", "::", "a : b: ", " a b\\ c  ", "a\\:b:c"]
    delimiters = ["abc\ndef", "a\\\nbc\n", "\\ a b\n", "ab\0cd\n", "\0tail"]

compareRead :: Context -> B.ByteString -> Bool -> String -> Int -> String -> String -> Int -> IO ()
compareRead ctx datum raw delimiter count ifs mode names = do
  env <- referenceEnvironment
  let flags = (["-r" | raw]) <> ["-d", delimiter] <> (if count >= 0 then ["-n", show count] else [])
      (arguments, source) = case mode of
        "array" -> (flags <> ["-a", "values"], "IFS=$1; shift; read \"$@\"; code=$?; printf \"%s\\0\" \"$code\"; if (( ${#values[@]} )); then printf \"%s\\0\" \"${values[@]}\"; fi")
        "reply" -> (flags, "IFS=$1; shift; read \"$@\"; code=$?; printf \"%s\\0%s\\0\" \"$code\" \"$REPLY\"")
        _ -> (flags <> ["a" <> show n | n <- [0 .. names - 1]], "IFS=$1; shift; read \"$@\"; code=$?; printf \"%s\\0\" \"$code\" " <> List.unwords ["\"$a" <> show n <> "\"" | n <- [0 .. names - 1]])
      request = ["read", "7", "oracle.sh", "1", "0", if raw then "1" else "0", delimiter, show count, ifs, mode, show names]
      fish = rpcHeader <> "rpc " <> List.unwords (map quoteFish request) <> "\nprintf '%s\\0' \"$response[2]\"\nif test (count $response) -gt 3\n printf '%s\\0' $response[4..-1]\nend\n"
  oracle <- invokeWith "bash" (["-c", source, "oracle.sh", ifs] <> arguments) datum Nothing (Just env) 10000000
  withWorkspace $ \directory -> do
    path <- writeScript (directory </> "read.fish") fish
    sessionEnv <- sessionEnvironment (runtime ctx)
    actual <- invokeWith (runtime ctx) ["--abi", "2", "session-run", path] datum Nothing (Just sessionEnv) 10000000
    checkResult ("read " <> show (datum, raw, delimiter, count, ifs, mode)) (0, processStdout oracle, processStderr oracle) actual

runExpansion :: Context -> IO ()
runExpansion ctx = withWorkspace $ \directory -> do
  env <- referenceEnvironment
  let values = ["", " ", "a", " a ", ":", "::", "a:", ":b", " : a:: b : ", "\t\na\t", B.pack [255, 58, 97, 255]]
      ifses = ["", " \t\n", ":", " :\t\n", B.singleton 255]
  forM_ ifses $ \ifs -> do
    forM_ values $ \left -> forM_ values $ \right -> do
      checkExpansion ctx env directory ifs [("e", left), ("e", right)]
      checkExpansion ctx env directory ifs [("e", left), ("q", ""), ("e", right)]
    forM_ values $ \value -> checkExpansion ctx env directory ifs [("q", ""), ("e", value), ("q", "")]
  forM_ (["a", "b", "z", "A", "0", "9", "-", "]", "^", "!", ":", "*", "?", "[a]", "a.txt", "b.txt", "star*", ".hidden", "\\a", "\\*", "\\abc", B.singleton 255, B.pack [254, 46, 116, 120, 116]] :: [B.ByteString]) $ \name -> do
    created <- invokeWith "bash" ["-c", "mapfile -d '' -t names; : > \"${names[0]}\""] (frames [name]) (Just directory) (Just env) 10000000
    when (code created /= 0) $ putStrLn ("platform pathname gap: filesystem rejected " <> show name)
  createDirectory (directory </> "dir")
  B.writeFile (directory </> "dir" </> "leaf") ""
  forM_ (take 250 (iterate next 220926)) $ \seed -> do
    let partCount = 1 + seed `mod` 5
        part n =
          let modes = ["e", "q", "l"]
              mode = case drop ((seed `div` (n + 2)) `mod` 3) modes of selectedMode : _ -> selectedMode; [] -> "e"
              value = case drop ((seed `div` (n + 3)) `mod` 9) values of selected : _ -> selected; [] -> ""
           in (mode, value)
        ifs = case drop (seed `mod` 4) ifses of value : _ -> value; [] -> ""
    checkExpansion ctx env directory ifs [part n | n <- [0 .. partCount - 1]]
  forM_ globPatterns $ \patternValue -> do
    checkExpansion ctx env directory " \t\n" [("e", patternValue)]
    checkExpansion ctx env directory " \t\n" [("q", patternValue)]
  forM_ ["alnum", "alpha", "ascii", "blank", "cntrl", "digit", "graph", "lower", "print", "punct", "space", "upper", "word", "xdigit"] $ \className ->
    checkExpansion ctx env directory "" [("e", b ("[[:" <> className <> ":]]"))]
  forM_ mixed $ checkExpansion ctx env directory " \t\n"
  forM_ bracketPatterns $ \patternValue -> forM_ bracketSubjects $ \subject -> do
    oracle <- invokeWith "bash" ["--noprofile", "--norc", "-c", "mapfile -d '' -t args; [[ ${args[0]} == ${args[1]} ]]", "pattern-oracle"] (frames [subject, patternValue]) Nothing (Just env) 10000000
    actual <- invoke (runtime ctx) ["--abi", "2", "pattern"] (frames ["match", subject, "1", patternValue])
    checkResult "byte bracket pattern" (code oracle, "", "") actual
  forM_ ["", " \0q\0", " \0invalid\0x\0"] $ \payload -> do
    actual <- invoke (runtime ctx) ["--abi", "2", "expansion"] payload
    check "malformed expansion rejected" (code actual == 125)
  where
    globPatterns = ["*", "?", "[ab]", "[a-z]", "[!a-z]", "[^a-z]", "[]a]", "[-a]", "[a-]", "[a/]", "[a-[:digit:]]", "[!a-[:digit:]]", "[a[:digit:]-z]", "[0-9]", "[[:alpha:]]", "[[:digit:]]", "[[:punct:]]", "[[:space:]]", "[[:print:]]", "[[:xdigit:]]", "[[:upper:]]", "[[:unknown:]]", "[[=a=]]", "[[.a.]]", "[", "[abc", "[.]hidden", ".*", "*.txt", "dir/*", "dir//*", "*///", "missing*", "\\a*", "\\*", "\\*?", B.pack [91, 254, 45, 255, 93]]
    next value = (value * 1664525 + 1013904223) `mod` 2147483647
    bracketPatterns = ["[a/]", "[a-z]", "[!a-z]", B.pack [91, 254, 45, 255, 93], "[[:alpha:]]", "[[:punct:]]", "[![:bogus:]]", "[a-[:digit:]]", "[!a-[:digit:]]", "[a[:digit:]-z]"]
    bracketSubjects = ["a", "Z", "0", "/", "-", B.singleton 254, B.singleton 255]
    mixed = [[("l", "*"), ("q", ".txt")], [("q", "*"), ("l", "*")], [("l", "["), ("q", "a-z"), ("l", "]")], [("e", "["), ("q", "a-z]")], [("e", "\\"), ("q", "*"), ("l", "*")], [("e", "\\"), ("q", "a"), ("l", "*")], [("e", "a* b*"), ("q", ".txt")], [("q", "a"), ("e", "* b*")], [("e", " * "), ("q", ""), ("e", " ? ")]]

checkExpansion :: Context -> [(String, String)] -> FilePath -> B.ByteString -> [(B.ByteString, B.ByteString)] -> IO ()
checkExpansion ctx env directory ifs parts = do
  let assignment = "mapfile -d '' -t args; IFS=${args[0]}; " <> B.concat [b ("v" <> show n <> "=${args[" <> show (1 + length (filter ((/= "l") . fst) (take n parts))) <> "]}; ") | (n, (mode, _)) <- zip [0 :: Int ..] parts, mode /= "l"]
      expression =
        B.concat
          [ case mode of
              "l" -> literal value
              "q" -> "\"${v" <> b (show n) <> "}\""
              _ -> "${v" <> b (show n) <> "}"
          | (n, (mode, value)) <- zip [0 :: Int ..] parts
          ]
      script = assignment <> "set -- " <> expression <> "; if (( $# )); then printf \"%s\\0\" \"$@\"; fi"
      values = ifs : [value | (mode, value) <- parts, mode /= "l"]
      request = ifs : concat [[mode, value] | (mode, value) <- parts]
  oracle <- invokeWith "bash" ["--noprofile", "--norc", "-c", str script, "word-oracle"] (frames values) (Just directory) (Just env) 10000000
  actual <- invokeWith (runtime ctx) ["--abi", "2", "expansion"] (frames request) (Just directory) (Just env) 10000000
  checkResult ("expansion " <> show (ifs, parts)) (code oracle, processStdout oracle, processStderr oracle) actual
  where
    literal = B.concatMap (\byte -> if byte `B.elem` "*?[]!-^" then B.singleton byte else B.pack [92, byte])
