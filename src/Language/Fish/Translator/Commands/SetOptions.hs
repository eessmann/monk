module Language.Fish.Translator.Commands.SetOptions
  ( SetOptionParse (..),
    parseSetOptions,
  )
where

import Data.Text qualified as T
import Language.Fish.Translator.Token (tokenHasExpansion, tokenToLiteralText)
import ShellCheck.AST

-- | Parsed `set` flags that we care about.
data SetOptionParse = SetOptionParse
  { setErrexit :: Maybe Bool,
    setPipefail :: Maybe Bool,
    setIssues :: [Text],
    setSawOptions :: Bool,
    setHasNonOptionArgs :: Bool
  }
  deriving stock (Show, Eq)

defaultSetOptionParse :: SetOptionParse
defaultSetOptionParse =
  SetOptionParse
    { setErrexit = Nothing,
      setPipefail = Nothing,
      setIssues = [],
      setSawOptions = False,
      setHasNonOptionArgs = False
    }

parseSetOptions :: [Token] -> SetOptionParse
parseSetOptions = go defaultSetOptionParse
  where
    go acc [] = acc
    go acc (t : ts)
      | tokenHasExpansion t =
          acc
            { setIssues = setIssues acc <> ["Bash set options with expansions require manual review"],
              setHasNonOptionArgs = True
            }
      | otherwise =
          let txt = tokenToLiteralText t
           in if txt == "--"
                then acc {setSawOptions = True, setHasNonOptionArgs = not (null ts)}
                else
                  if (T.isPrefixOf "-" txt || T.isPrefixOf "+" txt) && T.length txt > 1
                    then parseFlagToken acc txt ts
                    else acc {setHasNonOptionArgs = True}

    parseFlagToken acc txt ts =
      let sign = T.take 1 txt
          flags = T.drop 1 txt
          enabled = sign == "-"
          acc' = acc {setSawOptions = True}
       in parseFlags acc' enabled (T.unpack flags) ts

    parseFlags acc _ [] ts = go acc ts
    parseFlags acc enabled (f : fs) ts =
      case f of
        'e' ->
          parseFlags acc {setErrexit = Just enabled} enabled fs ts
        'u' ->
          parseFlags acc {setIssues = setIssues acc <> ["Bash set -u/nounset has no fish equivalent; manual review required"]} enabled fs ts
        'o' ->
          case ts of
            (optTok : rest)
              | tokenHasExpansion optTok ->
                  acc
                    { setIssues = setIssues acc <> ["Bash set -o with dynamic option requires manual review"],
                      setHasNonOptionArgs = True
                    }
              | otherwise ->
                  let opt = tokenToLiteralText optTok
                      acc' = applyNamedOpt acc enabled opt
                   in parseFlags acc' enabled fs rest
            [] ->
              acc {setIssues = setIssues acc <> ["Bash set -o requires an option; manual review required"]}
        _ ->
          parseFlags acc {setIssues = setIssues acc <> ["Unsupported set flag: -" <> T.singleton f]} enabled fs ts

    applyNamedOpt acc enabled opt =
      case opt of
        "pipefail" -> acc {setPipefail = Just enabled}
        "errexit" -> acc {setErrexit = Just enabled}
        "nounset" ->
          acc
            { setIssues = setIssues acc <> ["Bash set -o nounset has no fish equivalent; manual review required"]
            }
        _ ->
          acc
            { setIssues = setIssues acc <> ["Bash set -o " <> opt <> " has no fish equivalent; manual review required"]
            }
