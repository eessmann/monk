{-# LANGUAGE OverloadedStrings #-}

module Unit.Translation
  ( unitTranslationTests,
  )
where

import Data.Text qualified as T
import Monk (parseBashScript, strictConfig, translateParseResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import TestSupport

unitTranslationTests :: TestTree
unitTranslationTests =
  testGroup
    "Translation"
    [ H.testCase "Process substitution output redirect uses FIFO workaround" $ do
        out <- translateScript "echo hi > >(cat)"
        T.isInfixOf "mkfifo" out H.@? "expected mkfifo in translation"
        T.isInfixOf "__monk_psub_fifo" out H.@? "expected fifo var name in translation",
      H.testCase "Process substitution input uses psub" $ do
        out <- translateScript "cat <(echo 123)"
        T.isInfixOf "psub" out H.@? "expected psub for input process substitution",
      H.testCase "Unsupported extglob uses bash shim" $ do
        out <- translateScript "echo !(foo|bar)"
        T.isInfixOf "bash" out H.@? "expected bash shim"
        T.isInfixOf "extglob" out H.@? "expected extglob enabled"
        T.isInfixOf "!(foo|bar)" out H.@? "expected extglob pattern passed through",
      H.testCase "Shift uses argv slice" $ do
        out <- translateScript "shift"
        out @?= "set argv $argv[2..-1]",
      H.testCase "Declare export maps to set --global --export" $ do
        out <- translateScript "declare -x FOO=bar"
        out @?= "set --global --export FOO 'bar'",
      H.testCase "Default expansion uses set -q for unset" $ do
        out <- translateScript "echo ${JAVA_HOME-}"
        T.isInfixOf "set '-q' 'JAVA_HOME'" out H.@? "expected set -q for default expansion",
      H.testCase "Alternate expansion uses test -n" $ do
        out <- translateScript "echo ${NIX_PATH:+:$NIX_PATH}"
        T.isInfixOf "set '-q' 'NIX_PATH'" out H.@? "expected set -q for alternate expansion"
        T.isInfixOf "test '-n'" out H.@? "expected test -n for non-empty check",
      H.testCase "Assigning expansion hoists side effects" $ do
        out <- translateScript "echo ${HOME:=/tmp}"
        T.isInfixOf "set --global HOME '/tmp'" out H.@? "expected assignment in prelude"
        T.isInfixOf "string 'split' '--' $IFS" out H.@? "expected IFS split for unquoted expansion"
        T.isInfixOf "$HOME" out H.@? "expected variable use after assignment",
      H.testCase "Error expansion hoists exit" $ do
        out <- translateScript "echo ${MISSING:?nope}"
        T.isInfixOf "printf" out H.@? "expected error printf"
        T.isInfixOf "exit 1" out H.@? "expected exit in outer scope",
      H.testCase "Redirection expansion hoists side effects" $ do
        out <- translateScript "echo hi > ${OUT:=/tmp/out}"
        T.isInfixOf "set --global OUT '/tmp/out'" out H.@? "expected assignment before redirection"
        T.isInfixOf "> (string join ' ' $OUT ; or printf '')" out H.@? "expected redirection to use OUT",
      H.testCase "Heredoc expansion hoists side effects" $ do
        let script = "cat <<EOF\n${VAL:=ok}\nEOF\n"
        out <- translateScript script
        T.isInfixOf "set --global VAL 'ok'" out H.@? "expected assignment before heredoc"
        T.isInfixOf "string join ' ' $VAL ; or printf ''" out H.@? "expected heredoc to use VAL",
      H.testCase "Length expansion for argv uses count" $ do
        out <- translateScript "echo ${#@}"
        T.isInfixOf "count $argv" out H.@? "expected count for argv length",
      H.testCase "Length expansion for arrays uses count" $ do
        out <- translateScript "echo ${#arr[@]}"
        T.isInfixOf "count $arr" out H.@? "expected count for array length",
      H.testCase "Unset functions and variables translate to functions -e / set -e" $ do
        out <- translateScript "unset -f foo -v bar"
        out @?= "functions '-e' 'foo'\nset '-e' 'bar'",
      H.testCase "Unset variable without flags maps to set -e" $ do
        out <- translateScript "unset ASPELL_CONF"
        out @?= "set '-e' 'ASPELL_CONF'",
      H.testCase "Hash in word is preserved" $ do
        out <- translateScript "a=nixpkgs\nnix run $a#hello"
        T.isInfixOf "set --global a 'nixpkgs'" out H.@? "expected assignment translation"
        T.isInfixOf "string join ' ' $a ; or printf ''" out H.@? "expected variable join in word"
        T.isInfixOf "#hello" out H.@? "expected hash in word preserved",
      H.testCase "Pushd and popd pass through" $ do
        outPushd <- translateScript "pushd /tmp"
        outPopd <- translateScript "popd"
        outPushd @?= "pushd '/tmp'"
        outPopd @?= "popd",
      H.testCase "Nested command substitution translates inner" $ do
        out <- translateScript "echo $(echo $(echo hi))"
        T.isInfixOf "string 'split' '--' $IFS" out H.@? "expected IFS split in command substitution"
        T.isInfixOf "(echo" out H.@? "expected command substitution structure"
        T.isInfixOf "echo 'hi'" out H.@? "expected innermost echo",
      H.testCase "Strict mode fails on unsupported coproc" $ do
        result <- parseBashScript "spec.sh" "coproc echo hi"
        case translateParseResult strictConfig result of
          Left _ -> pure ()
          Right _ -> H.assertFailure "expected translation failure in strict mode",
      H.testCase "Strict mode fails on subshell" $ do
        result <- parseBashScript "spec.sh" "(echo hi)"
        case translateParseResult strictConfig result of
          Left _ -> pure ()
          Right _ -> H.assertFailure "expected translation failure in strict mode",
      H.testCase "Array index assignment is 1-based" $ do
        out <- translateScript "arr[0]=foo"
        out @?= "set --global arr[1] 'foo'",
      H.testCase "Array index expansion is 1-based" $ do
        out <- translateScript "echo ${arr[0]}"
        T.isInfixOf "$arr[1]" out H.@? "expected 1-based index"
        T.isInfixOf "string 'split' '--' $IFS" out H.@? "expected IFS split for unquoted expansion",
      H.testCase "Substring expansion uses string sub" $ do
        out <- translateScript "echo ${var:1:2}"
        T.isInfixOf "string 'sub' '--start' 2 '--length' 2 '--' $var" out H.@? "expected string sub with 1-based start",
      H.testCase "Pattern removal uses string replace" $ do
        out <- translateScript "echo ${var#foo}"
        T.isInfixOf "string 'replace' '-r' '--' '^foo' '' $var" out H.@? "expected anchored replace for prefix removal",
      H.testCase "Pattern replacement // uses -a" $ do
        out <- translateScript "echo ${var//foo/bar}"
        T.isInfixOf "string 'replace' '-r' '-a' '--' 'foo' 'bar' $var" out H.@? "expected global replacement with -a",
      H.testCase "Case modification uses string upper/lower" $ do
        outUpper <- translateScript "echo ${var^^}"
        outLower <- translateScript "echo ${var,,}"
        T.isInfixOf "string upper" outUpper H.@? "expected upper-case conversion"
        T.isInfixOf "string lower" outLower H.@? "expected lower-case conversion",
      H.testCase "Length expansion uses string length" $ do
        out <- translateScript "echo ${#var}"
        T.isInfixOf "string length" out H.@? "expected string length for ${#var}",
      H.testCase "Arithmetic command sets status from math" $ do
        out <- translateScript "((1 + 2))"
        T.isInfixOf "math" out H.@? "expected math command for arithmetic statement"
        T.isInfixOf "test" out H.@? "expected test for arithmetic status"
        T.isInfixOf "-ne" out H.@? "expected numeric comparison",
      H.testCase "Arithmetic postfix increment hoists temp" $ do
        out <- translateScript "echo $((i++))"
        T.isInfixOf "__monk_arith_tmp_" out H.@? "expected temp var for postfix increment"
        T.isInfixOf "set --global i" out H.@? "expected increment side effect",
      H.testCase "Arithmetic prefix increment updates variable" $ do
        out <- translateScript "echo $((++i))"
        T.isInfixOf "set --global i" out H.@? "expected increment side effect",
      H.testCase "Arithmetic assignment in expression hoists set" $ do
        out <- translateScript "echo $((x = y + 1))"
        T.isInfixOf "set --global x" out H.@? "expected assignment before math expression",
      H.testCase "Arithmetic short-circuit lowers to conditional evaluation" $ do
        out <- translateScript "echo $((a++ && b++))"
        T.isInfixOf "if test" out H.@? "expected conditional evaluation for &&"
        T.isInfixOf "__monk_arith_tmp_" out H.@? "expected temp vars for short-circuit",
      H.testCase "Arithmetic ternary lowers to conditional evaluation" $ do
        out <- translateScript "echo $((a ? b++ : c++))"
        T.isInfixOf "if test" out H.@? "expected conditional evaluation for ternary"
        T.isInfixOf "__monk_arith_tmp_" out H.@? "expected temp vars for ternary",
      H.testCase "Arithmetic for loop lowers to begin/while and increment" $ do
        out <- translateScript "for ((i=0; i<2; i++)); do echo $i; done"
        T.isInfixOf "set --global i" out H.@? "expected init set"
        T.isInfixOf "while test" out H.@? "expected while test condition"
        T.isInfixOf "math $i" out H.@? "expected increment math",
      H.testCase "Until loop negates condition" $ do
        out <- translateScript "until true; do echo 1; done"
        T.isInfixOf "while not" out H.@? "expected while not for until loop",
      H.testCase "Until loop negates compound condition" $ do
        out <- translateScript "until false && true; do echo ok; done"
        T.isInfixOf "while not begin" out H.@? "expected negation of full condition list"
        T.isInfixOf "and" out H.@? "expected compound condition inside negated block",
      H.testCase "Time prefix is preserved in pipelines" $ do
        out <- translateScript "time sleep 1"
        T.isInfixOf "time sleep" out H.@? "expected time prefix in output",
      H.testCase "Pipeline to source stays piped" $ do
        out <- translateScript "echo 123 | source"
        T.isInfixOf "| source" out H.@? "expected pipeline to source",
      H.testCase "Double bracket pattern match uses string match -q" $ do
        out <- translateScript "if [[ $x == foo* ]]; then echo ok; fi"
        H.assertBool
          ("expected glob match for [[ == ]], got: " <> T.unpack out)
          (T.isInfixOf "string 'match' '-q' '--'" out),
      H.testCase "Double bracket regex uses string match -qr" $ do
        out <- translateScript "if [[ $x =~ ^foo ]]; then echo ok; fi"
        H.assertBool
          ("expected regex match for [[ =~ ]], got: " <> T.unpack out)
          (T.isInfixOf "string 'match' '-qr' '--'" out),
      H.testCase "Double bracket negation uses not" $ do
        out <- translateScript "if [[ ! $x == foo ]]; then echo ok; fi"
        H.assertBool
          ("expected not for [[ ! ]], got: " <> T.unpack out)
          (T.isInfixOf "not string 'match' '-q' '--'" out),
      H.testCase "Double bracket with && and || uses conjunctions" $ do
        out <- translateScript "if [[ $x == foo && $y != bar || $z == baz ]]; then echo ok; fi"
        H.assertBool
          ("expected and/or conjunctions, got: " <> T.unpack out)
          (T.isInfixOf "and " out && T.isInfixOf "or " out),
      H.testCase "Double bracket parentheses preserve nested conjunctions" $ do
        out <- translateScript "if [[ ( $x == foo || $y == bar ) && ! $z == baz ]]; then echo ok; fi"
        H.assertBool
          ("expected nested conjunctions, got: " <> T.unpack out)
          (T.isInfixOf "and " out && T.isInfixOf "or " out && T.isInfixOf "not " out),
      H.testCase "Env prefix uses local export block" $ do
        out <- translateScript "FOO=bar echo hi"
        T.isInfixOf "set --local --export FOO 'bar'" out H.@? "expected local export set"
        T.isInfixOf "echo 'hi'" out H.@? "expected command in block",
      H.testCase "Export command uses set --global --export" $ do
        out <- translateScript "export FOO=bar"
        T.isInfixOf "set --global --export FOO 'bar'" out H.@? "expected global export set",
      H.testCase "Local command uses set --local" $ do
        out <- translateScript "local FOO=bar"
        T.isInfixOf "set --local FOO 'bar'" out H.@? "expected local set",
      H.testCase "Select loop uses read prompt and items list" $ do
        out <- translateScript "select x in a b; do echo $x; break; done"
        T.isInfixOf "set --local __monk_select_items" out H.@? "expected select items list"
        T.isInfixOf "while true" out H.@? "expected select while loop"
        T.isInfixOf "read --prompt '> '" out H.@? "expected select prompt read",
      H.testCase "Case patterns preserve globs" $ do
        out <- translateScript "case $x in foo* ) echo ok ;; esac"
        T.isInfixOf "case 'foo*'" out H.@? "expected quoted glob pattern",
      H.testCase "Case patterns with expansion keep glob meta" $ do
        out <- translateScript "case $x in ${Y}* ) echo ok ;; esac"
        T.isInfixOf "printf '%s%s'" out H.@? "expected printf pattern builder"
        T.isInfixOf "string join ' ' $Y ; or printf ''" out H.@? "expected expansion string join in pattern"
        H.assertBool "expected case pattern to be computed" (T.isInfixOf "case (" out),
      H.testCase "Case pattern expansion hoists side effects" $ do
        out <- translateScript "case $x in ${Y:=1}) echo ok ;; esac"
        T.isInfixOf "set --global Y '1'" out H.@? "expected assignment before switch"
        T.isInfixOf "string join ' ' $Y ; or printf ''" out H.@? "expected pattern to use Y",
      H.testCase "Case switch expansion hoists side effects" $ do
        out <- translateScript "case ${X:=1} in 1) echo ok ;; esac"
        T.isInfixOf "set --global X '1'" out H.@? "expected assignment before switch"
        T.isInfixOf "switch (string join ' ' $X ; or printf '')" out H.@? "expected switch to use X",
      H.testCase "Read flags translate to fish equivalents" $ do
        outN <- translateScript "read -n 3 foo"
        T.isInfixOf "read --nchars 3 foo" outN H.@? "expected nchars flag"
        outT <- translateScript "read -t 5 bar"
        T.isInfixOf "read --timeout 5 bar" outT H.@? "expected timeout flag"
        outU <- translateScript "read -u 9 baz"
        T.isInfixOf "read --fd 9 baz" outU H.@? "expected fd flag"
        outA <- translateScript "read -a arr"
        T.isInfixOf "read --array arr" outA H.@? "expected array flag",
      H.testCase "Source passes args" $ do
        out <- translateScript "source /tmp/script.sh a b"
        T.isInfixOf "source '/tmp/script.sh' 'a' 'b'" out H.@? "expected args passed to source",
      H.testCase "Trap translates to fish trap syntax" $ do
        out <- translateScript "trap 'echo bye' EXIT"
        T.isInfixOf "trap '--on-exit' 'echo bye'" out H.@? "expected fish trap on exit"
    ]
