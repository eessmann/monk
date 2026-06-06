{-# LANGUAGE OverloadedStrings #-}

module Unit.Translation
  ( unitTranslationTests,
  )
where

import Data.Text qualified as T
import Monk.Translation
  ( TranslationResult (..),
    defaultConfig,
    parseBashScript,
    renderTranslation,
    stateWarnings,
    strictConfig,
    translationState,
    translateParseResult,
    warnMessage,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import TestSupport

unitTranslationTests :: TestTree
unitTranslationTests =
  testGroup
    "Translation"
    [ H.testCase "Process substitution output redirect lowers to status-preserving temp-file block" $ do
        out <- translateScript "printf hi > >(wc -c > out)"
        T.isInfixOf "set --local __monk_psub_file $__monk_psub_dir'/stdout'" out H.@? "expected temp file setup for output process substitution"
        T.isInfixOf "printf 'hi' > $__monk_psub_file" out H.@? "expected producer stdout redirected to temp file"
        T.isInfixOf "cat $__monk_psub_file | wc '-c' > 'out'" out H.@? "expected consumer fed from temp file"
        T.isInfixOf "set --local __monk_psub_status" out H.@? "expected producer status capture"
        T.isInfixOf "fish '--no-config' '-c' 'exit $argv[1]' $__monk_psub_status" out H.@? "expected exact status restoration"
        H.assertBool "unexpected plain pipeline for status-sensitive output process substitution" (not (T.isInfixOf "printf 'hi' | wc" out))
        H.assertBool "unexpected helper for exact output process substitution" (not (T.isInfixOf "__monk_procsub_out" out)),
      H.testCase "Multiple process substitution output redirects lower without command-substitution helper" $ do
        out <- translateScript "printf hi > >(cat > one)\nprintf bye > >(cat > two)"
        T.count "__monk_procsub_out" out @?= 0
        T.count "set --local __monk_psub_file" out @?= 2
        T.isInfixOf "printf 'hi' > $__monk_psub_file" out H.@? "expected first temp-file producer"
        T.isInfixOf "printf 'bye' > $__monk_psub_file" out H.@? "expected second temp-file producer",
      H.testCase "Command substitution preserves command redirections" $ do
        out <- translateScript "echo $(printf hi > /tmp/monk-count)"
        T.isInfixOf "printf 'hi' > '/tmp/monk-count'" out H.@? "expected command-substitution redirection",
      H.testCase "Process substitution output preserves consumer redirections" $ do
        out <- translateScript "printf hi > >(wc -c > /tmp/monk-count)"
        T.isInfixOf "wc '-c' > '/tmp/monk-count'" out H.@? "expected process-substitution body redirection",
      H.testCase "Echo -e lowers to printf %b" $ do
        out <- translateScript "echo -e \"hi\\nthere\""
        T.isInfixOf "printf '%b\\n'" out H.@? "expected printf %b with newline",
      H.testCase "Echo -n stays echo with -n" $ do
        out <- translateScript "echo -n hi"
        T.isInfixOf "echo '-n' 'hi'" out H.@? "expected echo -n",
      H.testCase "Echo -E stays echo without escapes" $ do
        out <- translateScript "echo -E \"hi\\nthere\""
        T.isInfixOf "echo" out H.@? "expected echo preserved",
      H.testCase "Echo without options stays echo" $ do
        out <- translateScript "echo hello"
        out @?= "echo 'hello'",
      H.testCase "Echo -- preserves literal arguments" $ do
        out <- translateScript "echo -- -n"
        out @?= "echo '--' '-n'",
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
        T.isInfixOf "> (string join ' ' -- $OUT ; or printf '')" out H.@? "expected redirection to use OUT",
      H.testCase "Heredoc expansion hoists side effects" $ do
        let script = "cat <<EOF\n${VAL:=ok}\nEOF\n"
        out <- translateScript script
        T.isInfixOf "set --global VAL 'ok'" out H.@? "expected assignment before heredoc"
        T.isInfixOf "string join ' ' -- $VAL ; or printf ''" out H.@? "expected heredoc to use VAL",
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
        T.isInfixOf "string join ' ' -- $a ; or printf ''" out H.@? "expected variable join in word"
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
      H.testCase "Command substitution preserves status conjunctions" $ do
        out <- translateScript "echo $(false || true && false)"
        H.assertBool
          ("expected command-substitution conjunctions, got: " <> T.unpack out)
          (T.isInfixOf "or " out && T.isInfixOf "and " out),
      H.testCase "Errexit guard is command-substitution aware" $ do
        let script =
              T.unlines
                [ "set -e",
                  "digitCount() {",
                  "  local num=$1 count=0",
                  "  while ((num != 0)); do",
                  "    ((++count))",
                  "    ((num = num / 10))",
                  "  done",
                  "  echo \"$count\"",
                  "}",
                  "echo $(digitCount 12)"
                ]
        out <- translateScript script
        T.isInfixOf "status 'is-command-substitution'" out H.@? "expected runtime command-substitution guard"
        H.assertBool "unexpected function-wide return workaround" (not (T.isInfixOf "or return $status" out)),
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
      H.testCase "Strict mode fails on status-context subshell" $ do
        result <- parseBashScript "spec.sh" "if (echo hi); then echo ok; fi"
        case translateParseResult strictConfig result of
          Left _ -> pure ()
          Right _ -> H.assertFailure "expected translation failure in strict mode",
      H.testCase "Strict mode fails on command-substitution subshell" $ do
        result <- parseBashScript "spec.sh" "echo $( (echo hi) )"
        case translateParseResult strictConfig result of
          Left _ -> pure ()
          Right _ -> H.assertFailure "expected translation failure in strict mode",
      H.testCase "Command-substitution subshell keeps its body in non-strict mode" $ do
        out <- translateScript "echo $( (echo hi) )"
        T.isInfixOf "echo 'hi'" out H.@? "expected translated subshell body in command substitution"
        H.assertBool
          ("unexpected subshell collapse in command substitution: " <> T.unpack out)
          (not (T.isInfixOf "(true)" out)),
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
      H.testCase "For loop avoids fish readonly underscore variable" $ do
        out <- translateScript "for _ in 1; do true; done"
        H.assertBool "unexpected readonly underscore loop variable" (not (T.isInfixOf "for _ in" out))
        T.isInfixOf "for __monk_underscore in" out H.@? "expected safe underscore loop variable",
      H.testCase "For loop rewrites underscore body references with scoped binding" $ do
        out <- translateScript "for _ in a; do echo \"$_\"; done"
        T.isInfixOf "for __monk_underscore in 'a'" out H.@? "expected safe loop variable"
        T.isInfixOf "$__monk_underscore" out H.@? "expected body reference to renamed loop variable",
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
      H.testCase "Simplifier elides trivial begin wrapper in else branch" $ do
        out <- translateScript "if [[ $x == foo ]]; then echo ok; else true; fi"
        H.assertBool
          ("unexpected trivial begin wrapper in else branch: " <> T.unpack out)
          (not (T.isInfixOf "else\n  begin\n    true\n  end" out)),
      H.testCase "Simplifier keeps multi-statement pipeline stages wrapped" $ do
        out <- translateScript "echo hi | FOO=bar BAR=baz cat"
        T.isInfixOf "| begin" out H.@? "expected wrapped pipeline stage"
        T.isInfixOf "set --local --export FOO 'bar'" out H.@? "expected first prelude assignment inside pipeline stage"
        T.isInfixOf "set --local --export BAR 'baz'" out H.@? "expected second prelude assignment inside pipeline stage",
      H.testCase "Simplifier keeps conjunction stages wrapped when preludes remain" $ do
        out <- translateScript "FOO=bar BAR=baz true && echo ok"
        T.isInfixOf "begin" out H.@? "expected wrapped conjunction stage"
        T.isInfixOf "and echo 'ok'" out H.@? "expected conjunction preserved",
      H.testCase "Simplifier does not elide background wrappers around instrumented jobs" $ do
        out <- translateScript "FOO=bar BAR=baz true &"
        T.isInfixOf "end &" out H.@? "expected background block wrapper to remain"
        T.isInfixOf "set --local --export FOO 'bar'" out H.@? "expected exported prelude inside background job"
        T.isInfixOf "set --local --export BAR 'baz'" out H.@? "expected second exported prelude inside background job",
      H.testCase "Simplifier preserves redirected brace groups" $ do
        out <- translateScript "{ echo hi; } > out"
        T.isInfixOf "begin" out H.@? "expected redirected block wrapper to remain"
        T.isInfixOf "> 'out'" out H.@? "expected redirected block suffix to remain",
      H.testCase "Simplifier flattens nested scope-neutral prelude begins" $ do
        out <- translateScript "{ { X=1; }; echo hi; }"
        T.count "begin" out @?= 1
        T.isInfixOf "set --global X '1'" out H.@? "expected scope-neutral set to flatten"
        T.isInfixOf "echo 'hi'" out H.@? "expected command to remain",
      H.testCase "Simplifier flattens nested trivial begin wrappers" $ do
        out <- translateScript "{ { true; }; }"
        H.assertBool
          ("unexpected begin wrapper after simplification: " <> T.unpack out)
          (not (T.isInfixOf "begin" out))
        T.isInfixOf "true" out H.@? "expected safe command to remain",
      H.testCase "Simplifier elides pipeline-local wrapper only for single safe stage" $ do
        out <- translateScript "echo hi | { { cat; }; }"
        T.isInfixOf "| cat" out H.@? "expected simplified single-command pipeline stage"
        H.assertBool
          ("unexpected begin wrapper in simplified pipeline stage: " <> T.unpack out)
          (not (T.isInfixOf "| begin" out)),
      H.testCase "Simplifier preserves scope-changing prelude begin wrappers" $ do
        out <- translateScript "{ { FOO=bar true; }; echo hi; }"
        T.count "begin" out @?= 2
        T.isInfixOf "set --local --export FOO 'bar'" out H.@? "expected local export to keep inner scope"
        T.isInfixOf "echo 'hi'" out H.@? "expected command to remain",
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
        T.isInfixOf "string join ' ' -- $Y ; or printf ''" out H.@? "expected expansion string join in pattern"
        H.assertBool "expected case pattern to be computed" (T.isInfixOf "case (" out),
      H.testCase "Case pattern expansion hoists side effects" $ do
        out <- translateScript "case $x in ${Y:=1}) echo ok ;; esac"
        T.isInfixOf "set --global Y '1'" out H.@? "expected assignment before switch"
        T.isInfixOf "string join ' ' -- $Y ; or printf ''" out H.@? "expected pattern to use Y",
      H.testCase "Case switch expansion hoists side effects" $ do
        out <- translateScript "case ${X:=1} in 1) echo ok ;; esac"
        T.isInfixOf "set --global X '1'" out H.@? "expected assignment before switch"
        T.isInfixOf "switch (string join ' ' -- $X ; or printf '')" out H.@? "expected switch to use X",
      H.testCase "Read flags translate to fish equivalents" $ do
        outD <- translateScript "read -d : first second"
        T.isInfixOf "__monk_read_capture_delim" outD H.@? "expected delimiter capture helper"
        T.isInfixOf "__monk_read_assign" outD H.@? "expected Bash-style assignment helper"
        H.assertBool
          ("unexpected delimiter warning comment in exact helper path: " <> T.unpack outD)
          (not (T.isInfixOf "read delimiter semantics may differ between bash and fish" outD))
        outS <- translateScript "read -s secret"
        T.isInfixOf "read --silent secret" outS H.@? "expected silent flag"
        outN <- translateScript "read -n 3 foo"
        T.isInfixOf "read --nchars 3 foo" outN H.@? "expected nchars flag"
        outT <- translateScript "read -t 5 bar"
        T.isInfixOf "read --timeout 5 bar" outT H.@? "expected timeout flag"
        outU <- translateScript "read -u 9 baz"
        T.isInfixOf "__monk_read_capture_delim" outU H.@? "expected exact helper for numeric fd read"
        T.isInfixOf "<&9" outU H.@? "expected numeric fd redirection in helper path"
        outA <- translateScript "read -a arr"
        T.isInfixOf "__monk_read_capture_delim" outA H.@? "expected exact helper for array read"
        T.isInfixOf "set --global arr $__monk_read_fields" outA H.@? "expected exact array assignment",
      H.testCase "Read helpers are registered once" $ do
        out <- translateScript "read -d : a b\nread -d : c d"
        T.count "function __monk_read_capture_delim" out @?= 1
        T.count "function __monk_read_assign" out @?= 1,
      H.testCase "Background jobs use Monk tracking runtime" $ do
        out <- translateScript "false &\nbg=$!\nwait \"$bg\""
        T.isInfixOf "__monk_bg_status_path" out H.@? "expected background status helper"
        T.isInfixOf "set --global __monk_last_job $__monk_bg_seq" out H.@? "expected Monk job token"
        T.isInfixOf "__monk_wait" out H.@? "expected translated wait helper"
        T.isInfixOf "printf '%s\\n' $__monk_bg_status > $__monk_bg_status_file" out H.@? "expected status file write"
        H.assertBool "expected $! to lower to Monk job token" (not (T.isInfixOf "$last_pid" out)),
      H.testCase "Background runtime is registered once" $ do
        out <- translateScript "false &\nwait \"$!\"\ntrue &\nwait \"$!\""
        T.count "function __monk_bg_status_path" out @?= 1
        T.count "function __monk_wait" out @?= 1,
      H.testCase "Pipefail helper is registered once" $ do
        out <- translateScript "set -o pipefail\nfalse | true\ntrue | false"
        T.count "function __monk_pipefail" out @?= 1,
      H.testCase "Exact read delimiter array helper emits no semantic warnings" $ do
        result <- parseBashScript "spec.sh" "read -d '' -ra fields"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("translateParseResult failed: " <> show err)
          Right translation -> do
            let out = renderTranslation translation
                warnMessages = map warnMessage (stateWarnings (translationState translation))
            T.isInfixOf "__monk_read_assign" out H.@? "expected exact array assignment helper"
            T.isInfixOf "__monk_read_capture_delim 'null'" out H.@? "expected null-delimited capture helper"
            H.assertBool
              ("unexpected warnings in exact array path: " <> show warnMessages)
              ( "read delimiter semantics may differ between bash and fish" `notElem` warnMessages
                  && "read IFS splitting semantics may differ between bash and fish" `notElem` warnMessages
              ),
      H.testCase "Multi-variable delimiter reads use exact helper without warnings" $ do
        result <- parseBashScript "spec.sh" "read -d : one two three"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("translateParseResult failed: " <> show err)
          Right translation -> do
            let out = renderTranslation translation
                warnMessages = map warnMessage (stateWarnings (translationState translation))
            T.isInfixOf "__monk_read_assign" out H.@? "expected exact variable assignment helper"
            H.assertBool
              ("unexpected warnings in exact multi-var path: " <> show warnMessages)
              ( "read delimiter semantics may differ between bash and fish" `notElem` warnMessages
                  && "read IFS splitting semantics may differ between bash and fish" `notElem` warnMessages
              ),
      H.testCase "Mixed delimiter flag clusters use exact helper" $ do
        result <- parseBashScript "spec.sh" "read -rsd: -n 3 field"
        case translateParseResult defaultConfig result of
          Left err -> H.assertFailure ("translateParseResult failed: " <> show err)
          Right translation -> do
            let out = renderTranslation translation
                warnMessages = map warnMessage (stateWarnings (translationState translation))
            T.isInfixOf "__monk_read_capture_delim" out H.@? "expected exact capture helper"
            H.assertBool
              ("unexpected delimiter warning in mixed exact path: " <> show warnMessages)
              ("read delimiter semantics may differ between bash and fish" `notElem` warnMessages),
      H.testCase "No-var null delimiter assigns REPLY exactly" $ do
        out <- translateScript "read -d ''"
        T.isInfixOf "__monk_read_capture_delim 'null'" out H.@? "expected null-delimited helper path"
        T.isInfixOf "set --global REPLY" out H.@? "expected REPLY assignment"
        H.assertBool
          "unexpected delimiter warning note"
          (not (T.isInfixOf "read delimiter semantics may differ between bash and fish" out)),
      H.testCase "Delimiter values normalize to the first character" $ do
        out <- translateScript "read -d '::' field"
        T.isInfixOf "__monk_read_capture_delim 'char' ':'" out H.@? "expected normalized delimiter helper call"
        H.assertBool "unexpected multi-character delimiter in helper call" (not (T.isInfixOf "'::'" out)),
      H.testCase "Source passes args" $ do
        out <- translateScript "source /tmp/script.sh a b"
        T.isInfixOf "source '/tmp/script.sh' 'a' 'b'" out H.@? "expected args passed to source",
      H.testCase "Trap translates to fish trap syntax" $ do
        out <- translateScript "trap 'echo bye' EXIT"
        T.isInfixOf "set --global __monk_trap_body_exit 'echo bye'" out H.@? "expected trap body capture"
        T.isInfixOf "function __monk_trap_exit --on-process-exit %self" out H.@? "expected on-process-exit helper"
        T.isInfixOf "eval $__monk_trap_body_exit" out H.@? "expected trap body via captured variable"
        H.assertBool "unexpected direct eval of trap body" (not (T.isInfixOf "eval 'echo bye'" out)),
      H.testCase "Trap uses distinct per-signal body variables" $ do
        out <- translateScript "trap 'echo first' EXIT INT\ntrap 'echo second' EXIT"
        T.isInfixOf "set --global __monk_trap_body_exit 'echo first'" out H.@? "expected EXIT body capture"
        T.isInfixOf "set --global __monk_trap_body_int 'echo first'" out H.@? "expected INT body capture"
        T.isInfixOf "set --global __monk_trap_body_exit 'echo second'" out H.@? "expected EXIT body overwrite"
        H.assertBool "unexpected INT body overwrite" (not (T.isInfixOf "set --global __monk_trap_body_int 'echo second'" out)),
      H.testCase "Trap clear removes Monk-generated handlers" $ do
        out <- translateScript "trap - EXIT INT"
        out @?=
          T.intercalate
            "\n"
            [ "functions '-e' '__monk_trap_exit'",
              "set '-e' '__monk_trap_body_exit'",
              "functions '-e' '__monk_trap_sig_INT'",
              "set '-e' '__monk_trap_body_int'"
            ],
      H.testCase "Trap normalizes SIG-prefixed signals" $ do
        out <- translateScript "trap 'echo hi' SIGINT"
        T.isInfixOf "function __monk_trap_sig_INT --on-signal INT" out H.@? "expected SIGINT to normalize to INT"
    ]
