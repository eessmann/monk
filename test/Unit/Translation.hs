{-# LANGUAGE OverloadedStrings #-}

module Unit.Translation
  ( unitTranslationTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import qualified Data.Text as T
import TestSupport
import Monk (parseBashScript, translateParseResult, strictConfig)

unitTranslationTests :: TestTree
unitTranslationTests = testGroup "Translation"
  [ H.testCase "Process substitution output redirect uses FIFO workaround" $ do
      out <- translateScript "echo hi > >(cat)"
      T.isInfixOf "mkfifo" out H.@? "expected mkfifo in translation"
      T.isInfixOf "__monk_psub_fifo" out H.@? "expected fifo var name in translation"

  , H.testCase "Process substitution input uses psub" $ do
      out <- translateScript "cat <(echo 123)"
      T.isInfixOf "psub" out H.@? "expected psub for input process substitution"

  , H.testCase "Unsupported extglob uses bash shim" $ do
      out <- translateScript "echo !(foo|bar)"
      T.isInfixOf "bash" out H.@? "expected bash shim"
      T.isInfixOf "extglob" out H.@? "expected extglob enabled"
      T.isInfixOf "!(foo|bar)" out H.@? "expected extglob pattern passed through"

  , H.testCase "Shift uses argv slice" $ do
      out <- translateScript "shift"
      out @?= "set argv $argv[2..-1]"

  , H.testCase "Declare export maps to set --global --export" $ do
      out <- translateScript "declare -x FOO=bar"
      out @?= "set --global --export FOO 'bar'"

  , H.testCase "Default expansion uses set -q for unset" $ do
      out <- translateScript "echo ${JAVA_HOME-}"
      T.isInfixOf "set '-q' 'JAVA_HOME'" out H.@? "expected set -q for default expansion"

  , H.testCase "Alternate expansion uses test -n" $ do
      out <- translateScript "echo ${NIX_PATH:+:$NIX_PATH}"
      T.isInfixOf "set '-q' 'NIX_PATH'" out H.@? "expected set -q for alternate expansion"
      T.isInfixOf "test '-n'" out H.@? "expected test -n for non-empty check"

  , H.testCase "Length expansion for argv uses count" $ do
      out <- translateScript "echo ${#@}"
      T.isInfixOf "count $argv" out H.@? "expected count for argv length"

  , H.testCase "Length expansion for arrays uses count" $ do
      out <- translateScript "echo ${#arr[@]}"
      T.isInfixOf "count $arr" out H.@? "expected count for array length"

  , H.testCase "Unset functions and variables translate to functions -e / set -e" $ do
      out <- translateScript "unset -f foo -v bar"
      out @?= "functions '-e' 'foo'\nset '-e' 'bar'"

  , H.testCase "Unset variable without flags maps to set -e" $ do
      out <- translateScript "unset ASPELL_CONF"
      out @?= "set '-e' 'ASPELL_CONF'"

  , H.testCase "Hash in word is preserved" $ do
      out <- translateScript "a=nixpkgs\nnix run $a#hello"
      T.isInfixOf "set --global a 'nixpkgs'" out H.@? "expected assignment translation"
      T.isInfixOf "string join ' ' $a" out H.@? "expected variable join in word"
      T.isInfixOf "#hello" out H.@? "expected hash in word preserved"

  , H.testCase "Pushd and popd pass through" $ do
      outPushd <- translateScript "pushd /tmp"
      outPopd <- translateScript "popd"
      outPushd @?= "pushd '/tmp'"
      outPopd @?= "popd"

  , H.testCase "Nested command substitution translates inner" $ do
      out <- translateScript "echo $(echo $(echo hi))"
      out @?= "echo (string join ' ' (echo (string join ' ' (echo 'hi'))))"

  , H.testCase "Strict mode fails on unsupported coproc" $ do
      result <- parseBashScript "spec.sh" "coproc echo hi"
      case translateParseResult strictConfig result of
        Left _ -> pure ()
        Right _ -> H.assertFailure "expected translation failure in strict mode"

  , H.testCase "Array index assignment is 1-based" $ do
      out <- translateScript "arr[0]=foo"
      out @?= "set --global arr[1] 'foo'"

  , H.testCase "Array index expansion is 1-based" $ do
      out <- translateScript "echo ${arr[0]}"
      out @?= "echo $arr[1]"

  , H.testCase "Substring expansion uses string sub" $ do
      out <- translateScript "echo ${var:1:2}"
      T.isInfixOf "string 'sub' '--start' 2 '--length' 2 '--' $var" out H.@? "expected string sub with 1-based start"

  , H.testCase "Pattern removal uses string replace" $ do
      out <- translateScript "echo ${var#foo}"
      T.isInfixOf "string 'replace' '-r' '--' '^foo' '' $var" out H.@? "expected anchored replace for prefix removal"

  , H.testCase "Pattern replacement // uses -a" $ do
      out <- translateScript "echo ${var//foo/bar}"
      T.isInfixOf "string 'replace' '-r' '-a' '--' 'foo' 'bar' $var" out H.@? "expected global replacement with -a"

  , H.testCase "Case modification uses string upper/lower" $ do
      outUpper <- translateScript "echo ${var^^}"
      outLower <- translateScript "echo ${var,,}"
      T.isInfixOf "string upper" outUpper H.@? "expected upper-case conversion"
      T.isInfixOf "string lower" outLower H.@? "expected lower-case conversion"

  , H.testCase "Length expansion uses string length" $ do
      out <- translateScript "echo ${#var}"
      T.isInfixOf "string length" out H.@? "expected string length for ${#var}"

  , H.testCase "Arithmetic command uses math with redirect" $ do
      out <- translateScript "((1 + 2))"
      T.isInfixOf "math" out H.@? "expected math command for arithmetic statement"
      T.isInfixOf "/dev/null" out H.@? "expected stdout redirect to /dev/null"

  , H.testCase "Arithmetic for loop lowers to begin/while and increment" $ do
      out <- translateScript "for ((i=0; i<2; i++)); do echo $i; done"
      T.isInfixOf "set --global i" out H.@? "expected init set"
      T.isInfixOf "while test" out H.@? "expected while test condition"
      T.isInfixOf "math $i" out H.@? "expected increment math"

  , H.testCase "Until loop negates condition" $ do
      out <- translateScript "until true; do echo 1; done"
      T.isInfixOf "while not true" out H.@? "expected while not for until loop"

  , H.testCase "Time prefix is preserved in pipelines" $ do
      out <- translateScript "time sleep 1"
      T.isInfixOf "time sleep" out H.@? "expected time prefix in output"

  , H.testCase "Pipeline to source stays piped" $ do
      out <- translateScript "echo 123 | source"
      T.isInfixOf "| source" out H.@? "expected pipeline to source"

  , H.testCase "Double bracket pattern match uses string match -q" $ do
      out <- translateScript "if [[ $x == foo* ]]; then echo ok; fi"
      H.assertBool ("expected glob match for [[ == ]], got: " <> T.unpack out)
        (T.isInfixOf "string 'match' '-q' '--'" out)

  , H.testCase "Double bracket regex uses string match -qr" $ do
      out <- translateScript "if [[ $x =~ ^foo ]]; then echo ok; fi"
      H.assertBool ("expected regex match for [[ =~ ]], got: " <> T.unpack out)
        (T.isInfixOf "string 'match' '-qr' '--'" out)

  , H.testCase "Double bracket negation uses not" $ do
      out <- translateScript "if [[ ! $x == foo ]]; then echo ok; fi"
      H.assertBool ("expected not for [[ ! ]], got: " <> T.unpack out)
        (T.isInfixOf "not string 'match' '-q' '--'" out)

  , H.testCase "Double bracket with && and || uses conjunctions" $ do
      out <- translateScript "if [[ $x == foo && $y != bar || $z == baz ]]; then echo ok; fi"
      H.assertBool ("expected and/or conjunctions, got: " <> T.unpack out)
        (T.isInfixOf "and " out && T.isInfixOf "or " out)

  , H.testCase "Double bracket parentheses preserve nested conjunctions" $ do
      out <- translateScript "if [[ ( $x == foo || $y == bar ) && ! $z == baz ]]; then echo ok; fi"
      H.assertBool ("expected nested conjunctions, got: " <> T.unpack out)
        (T.isInfixOf "and " out && T.isInfixOf "or " out && T.isInfixOf "not " out)

  , H.testCase "Env prefix uses local export block" $ do
      out <- translateScript "FOO=bar echo hi"
      T.isInfixOf "set --local --export FOO 'bar'" out H.@? "expected local export set"
      T.isInfixOf "echo 'hi'" out H.@? "expected command in block"

  , H.testCase "Export command uses set --global --export" $ do
      out <- translateScript "export FOO=bar"
      T.isInfixOf "set --global --export FOO 'bar'" out H.@? "expected global export set"

  , H.testCase "Local command uses set --local" $ do
      out <- translateScript "local FOO=bar"
      T.isInfixOf "set --local FOO 'bar'" out H.@? "expected local set"

  , H.testCase "Select loop uses read prompt and items list" $ do
      out <- translateScript "select x in a b; do echo $x; break; done"
      T.isInfixOf "set --local __monk_select_items" out H.@? "expected select items list"
      T.isInfixOf "while true" out H.@? "expected select while loop"
      T.isInfixOf "read --prompt '> '" out H.@? "expected select prompt read"

  , H.testCase "Trap translates to fish trap syntax" $ do
      out <- translateScript "trap 'echo bye' EXIT"
      T.isInfixOf "trap '--on-exit' 'echo bye'" out H.@? "expected fish trap on exit"
  ]
