module ShellToFish
  ( translateRoot,
    translateToken,
  )
where

import Data.Text qualified as T
import FishAST
import ShellCheck.AST
import ShellCheck.ASTLib
--------------------------------------------------------------------------------
---- 1. Main Translation
--------------------------------------------------------------------------------

translateRoot :: Root -> FishStatement
translateRoot (Root topToken) = translateToken topToken

--------------------------------------------------------------------------------
---- 2. High-level dispatch
--------------------------------------------------------------------------------

translateToken :: Token -> FishStatement
translateToken token =
  case token of
    ----------------------------------------------------------------
    -- Script: top-level list
    ----------------------------------------------------------------
    T_Script _ _ stmts ->
      StmtList (map translateToken stmts)

    ----------------------------------------------------------------
    -- Simple Command
    ----------------------------------------------------------------
    T_SimpleCommand _ assignments rest ->
      StmtList (assignmentsToSets assignments ++ [translateSimpleCommand rest])

    ----------------------------------------------------------------
    -- Pipeline, possibly with "!"
    ----------------------------------------------------------------
    T_Pipeline _ bang cmds ->
      translatePipeline bang cmds

    ----------------------------------------------------------------
    -- If
    ----------------------------------------------------------------
    T_IfExpression _ conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch

    ----------------------------------------------------------------
    -- While, Until
    ----------------------------------------------------------------
    T_WhileExpression _ cond body ->
      Stmt (While (ExprBool (translateBoolTokens cond)) (map translateToken body))

    T_UntilExpression _ cond body ->
      Stmt
        ( While
            (negateTBool (ExprBool (translateBoolTokens cond)))
            (map translateToken body)
        )

    ----------------------------------------------------------------
    -- Functions
    ----------------------------------------------------------------
    T_Function _ _ _ funcName body ->
      translateFunction funcName body

    ----------------------------------------------------------------
    -- BraceGroup => just statements
    ----------------------------------------------------------------
    T_BraceGroup _ tokens ->
      StmtList (map translateToken tokens)

    ----------------------------------------------------------------
    -- Subshell => "begin ... end"
    ----------------------------------------------------------------
    T_Subshell _ tokens ->
      StmtList [Stmt (Begin (map translateToken tokens))]

    ----------------------------------------------------------------
    -- Logical AND/OR
    ----------------------------------------------------------------
    T_AndIf _ l r ->
      Stmt (JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))

    T_OrIf _ l r ->
      Stmt (JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))

    ----------------------------------------------------------------
    -- Background
    ----------------------------------------------------------------
    T_Backgrounded _ bgToken ->
      Stmt (Background (translateTokenToStatusCmd bgToken))

    ----------------------------------------------------------------
    -- Annotations => skip
    ----------------------------------------------------------------
    T_Annotation _ _ inner ->
      translateToken inner

    ----------------------------------------------------------------
    -- For var in ...
    ----------------------------------------------------------------
    T_ForIn _ var tokens body ->
      Stmt (For (T.pack var) (ArgList (map argLiteralText tokens)) (map translateToken body))

    ----------------------------------------------------------------
    -- Case
    ----------------------------------------------------------------
    T_CaseExpression _ switchExpr cases ->
      Stmt (Switch (argLiteralText switchExpr) (map translateCase cases))

    ----------------------------------------------------------------
    -- Possibly more patterns...
    ----------------------------------------------------------------
    _ ->
      Comment $ "Unimplemented token: " <> showToken token

--------------------------------------------------------------------------------
---- 3. Debug
--------------------------------------------------------------------------------

showToken :: Token -> Text
showToken = T.pack . show

--------------------------------------------------------------------------------
---- 4. Commands & Pipelines
--------------------------------------------------------------------------------

translateSimpleCommand :: [Token] -> FishStatement
translateSimpleCommand cmdTokens =
  case cmdTokens of
    [] ->
      Stmt (Command "true" [])
    (c : args) ->
      case maybeRewriteSpecialBuiltin c args of
        Just builtin -> Stmt builtin
        Nothing ->
          -- Check if [ expr ] => rewrite to "test"?
          case isSingleBracketTest c args of
            Just testCmd ->
              Stmt testCmd
            Nothing ->
              Stmt (Command (literalText c) (map translateArgOrRedirect args))

--------------------------------------------------------------------------------
---- 5. Special built-ins (shift, unset, etc.)
--------------------------------------------------------------------------------

maybeRewriteSpecialBuiltin :: Token -> [Token] -> Maybe (FishCommand TStatus)
maybeRewriteSpecialBuiltin cmdToken args =
  let cmdName = T.strip (literalText cmdToken)
   in case cmdName of
        "shift" ->
          -- shift => set argv = argv[2..-1]
          Just (Command "set" [Arg $ ArgLiteral "argv[2..-1]"])
        "unset" ->
          Just (Command "" (rewriteUnsetArgs args))
        "hash" ->
          Just (Command "true" [])
        _ ->
          Nothing

rewriteUnsetArgs :: [Token] -> [FishArgOrRedirect]
rewriteUnsetArgs [] = [Arg $ ArgLiteral "true"]
rewriteUnsetArgs tokens =
  let stringed = map (T.strip . literalText) tokens
      (flags, names) = span (\txt -> T.isPrefixOf "-" txt) stringed
      isFunc = "-f" `elem` flags
   in if null names
        then [Arg $ ArgLiteral "true"]
        else
          if isFunc
            then
              [ Arg $ ArgLiteral $
                  T.intercalate
                    "; "
                    (map (\n -> "functions -e " <> n) names)
              ]
            else
              [ Arg $ ArgLiteral $
                  T.intercalate
                    "; "
                    (map (\n -> "set -e " <> n) names)
              ]

--------------------------------------------------------------------------------
---- 6. Single-bracket test rewriting: [ expr ]
--------------------------------------------------------------------------------

-- Minimal example: [ X op Y ] => test X op Y
-- If we see T_Literal "[", T_Literal "something", T_Literal "]" => try rewriting
-- This is just a naive approach.
isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = literalText bracketToken
   in if bracketTxt == "["
        then
          case args of
            [] -> Nothing
            _ ->
              -- check if last token is ']', remove it
              let lastToken = last args
                  lastText = literalText lastToken
                  middle = init args
               in if lastText == "]"
                    then Just (rewriteTest middle)
                    else Nothing
        else Nothing

-- We'll produce "test <...>" for the entire expression.
-- In a more advanced approach, parse each operator, e.g. -f, -n, =, !=, etc.
rewriteTest :: [Token] -> FishCommand TStatus
rewriteTest middle =
  let expression = T.unwords (map literalText middle)
   in Command "test" [Arg $ ArgLiteral expression]

--------------------------------------------------------------------------------
---- 7. Pipeline => fish Pipeline or Not
--------------------------------------------------------------------------------

translatePipeline :: [Token] -> [Token] -> FishStatement
translatePipeline bang cmds =
  case cmds of
    [] -> Stmt (Command "true" [])
    [single] ->
      if null bang
        then Stmt (translateTokenToStatusCmd single)
        else Stmt (Not (translateTokenToStatusCmd single))
    (c1 : c2 : rest) ->
      let firstCmd = translateTokenToStatusCmd c1
          otherCmds = map translateTokenToStatusCmd (c2 : rest)
          pipe = Pipeline (firstCmd :| otherCmds)
       in if null bang
            then Stmt pipe
            else Stmt (Not pipe)

--------------------------------------------------------------------------------
---- 8. If
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> FishStatement
translateIfExpression conditionBranches elseBranch =
  StmtList (go conditionBranches)
  where
    go [] = map translateToken elseBranch
    go ((condTokens, thenTokens) : rest) =
      [ Stmt $
          If
            (ExprBool (translateBoolTokens condTokens))
            (map translateToken thenTokens)
            (go rest)
      ]

--------------------------------------------------------------------------------
---- 9. Function
--------------------------------------------------------------------------------

translateFunction :: String -> Token -> FishStatement
translateFunction funcName bodyToken =
  let bodyStmts =
        case bodyToken of
          T_BraceGroup _ stmts -> map translateToken stmts
          _ -> [translateToken bodyToken]
   in Stmt $
        Function
          FishFunction
            { funcName = T.pack funcName,
              funcFlags = [],
              funcParams = [],
              funcBody = bodyStmts
            }

--------------------------------------------------------------------------------
---- 10. Variable Assignments
--------------------------------------------------------------------------------

assignmentsToSets :: [Token] -> [FishStatement]
assignmentsToSets assignments =
  [ translateAssignment assignment
    | assignment <- assignments,
      isAssignment assignment
  ]

translateAssignment :: Token -> FishStatement
translateAssignment tok =
  case tok of
    T_Assignment _ _ var _ val ->
      Stmt $
        Set
          ScopeLocal
          (T.pack var)
          (argLiteralText val)
    _ ->
      Comment $ "Unrecognized assignment pattern: " <> showToken tok

--------------------------------------------------------------------------------
---- 11. Boolean Expressions (for if/while)
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> BoolExpr
translateBoolTokens = \case
  [] -> BoolLiteral True
  [T_Bang _] -> BoolLiteral False
  [T_Bang _, t] -> BoolNot (translateBoolToken t)
  [t] -> translateBoolToken t
  (cond : T_AND_IF _ : rest) -> BoolAnd (translateBoolToken cond) (translateBoolTokens rest)
  (cond : T_OR_IF _ : rest) -> BoolOr (translateBoolToken cond) (translateBoolTokens rest)
  tokens ->
    BoolCommand (translateTokenToStatusCmd (makeSinglePipeline tokens))
  where
    makeSinglePipeline [] = T_SimpleCommand (Id 0) [] []
    makeSinglePipeline (t : ts) = T_Pipeline (getId t) [] (t : ts)

translateBoolToken :: Token -> BoolExpr
translateBoolToken = \case
  T_Literal _ "true" -> BoolLiteral True
  T_Literal _ "false" -> BoolLiteral False
  -- If this is an arithmetic expression, handle it
  T_Arithmetic _ exprTokens ->
    -- For a boolean check, fish doesn't have direct "math" boolean,
    -- so let's do 'test (math <expr>) -ne 0'
    BoolCommand (Command "test" [(Arg $ ArgLiteral "(math ") <> argLiteralTextList exprTokens <> (Arg $ ArgLiteral ") -ne 0")])
  _ -> BoolCommand (translateTokenToStatusCmd)

--------------------------------------------------------------------------------
---- 12. Negate TBool
--------------------------------------------------------------------------------

negateTBool :: FishExpr TBool -> FishExpr TBool
negateTBool = \case
  ExprBool (BoolNot b) -> ExprBool b
  ExprBool b -> ExprBool (BoolNot b)
  x -> ExprBool (BoolNot (BoolCommand (Command "test" []))) -- fallback

--------------------------------------------------------------------------------
---- 13. TStatus Commands
--------------------------------------------------------------------------------

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = \case
  T_SimpleCommand _ assignments rest ->
    case rest of
      [] ->
        if null assignments
          then Command "true" []
          else Command "true" (assignmentsToArgs assignments)
      (c : args) ->
        case maybeRewriteSpecialBuiltin c args of
          Just b -> b
          Nothing ->
            -- check if single-bracket test
            case isSingleBracketTest c args of
              Just tcmd -> tcmd
              Nothing ->
                Command
                  (literalText c)
                  (assignmentsToArgs assignments ++ map translateArgOrRedirect args)

  T_Pipeline _ bang cmds ->
    translatePipelineToStatus bang cmds

  T_Arithmetic _ exprTokens ->
    -- e.g. `$(( x + 1 ))` => fish 'math x + 1', returning TStatus
    -- We might do "set _tmp (math x+1); test $_tmp -ne 0"
    -- For direct "command" that yields TStatus, let's do a direct "math" and see if 0 or not
    -- This is a naive approach:
    Command "math" [argLiteralTextList exprTokens]

  other ->
    Command (literalText other) []

translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  case cmds of
    [] -> Command "true" []
    [single] ->
      if null bang
        then translateTokenToStatusCmd single
        else Not (translateTokenToStatusCmd single)
    (c1 : c2 : rest) ->
      let firstCmd = translateTokenToStatusCmd c1
          otherCmds = map translateTokenToStatusCmd (c2 : rest)
       in if null bang
            then Pipeline (firstCmd :| otherCmds)
            else Not (Pipeline (firstCmd :| otherCmds))

assignmentsToArgs :: [Token] -> [FishArgOrRedirect]
assignmentsToArgs assignments =
  [ Arg (argLiteralText assignment)
    | assignment <- assignments,
      isAssignment assignment
  ]

--------------------------------------------------------------------------------
---- 14. Argument or Redirect
--------------------------------------------------------------------------------

translateArgOrRedirect :: Token -> FishArgOrRedirect
translateArgOrRedirect = \case
  T_IoFile _ op file ->
    let op' = case op of
          T_Less _ -> RedirectIn
          T_Greater _ -> RedirectOut
          T_DGREAT _ -> RedirectOutAppend
          T_CLOBBER _ -> RedirectOut
          _ -> error $ "Unsupported IOFile: " <> show op
        file' = argLiteralText file
     in Redir op' file'
  T_IoDuplicate _ op num ->
    let op' = case op of
          T_LESSAND _ -> RedirectIn
          T_GREATAND _ -> RedirectOut
          _ -> error $ "Unsupported IoDuplicate: " <> show op
        num' = ArgLiteral (T.pack num)
     in Redir op' num'
  other -> Arg (translateArg other)

--------------------------------------------------------------------------------
---- 15. Arguments (including param expansions + arithmetic expansions)
--------------------------------------------------------------------------------

translateArg :: Token -> ArgStr
translateArg = \case
  T_Literal _ _ ->
    ArgLiteral . literalText
  T_SingleQuoted _ s ->
    \_ -> ArgLiteral (T.pack s)
  T_DoubleQuoted _ parts ->
    \_ -> ArgLiteral (T.concat (map literalText parts))
  T_Arithmetic _ exprTokens ->
    -- `$(( x + 1 ))` => `ArgSubstituted (Command "math" [ArgLiteral "x+1"])`
    ArgSubstituted . Command "math" . pure . argLiteralTextList
  T_DollarBraced _ _ _ ->
    translateParamExp
  T_Backticked _ stmts ->
    \_ -> ArgSubstituted (translateTokenToStrCmd (T_Backticked (getId (head stmts)) stmts))
  T_DollarExpansion _ stmts ->
    \_ -> ArgSubstituted (translateTokenToStrCmd (T_DollarExpansion (getId (head stmts)) stmts))
  T_DollarBraceCommandExpansion _ stmts ->
    \_ -> ArgSubstituted (translateTokenToStrCmd (T_DollarBraceCommandExpansion (getId (head stmts)) stmts))
  other ->
    ArgLiteral . literalText

--------------------------------------------------------------------------------
---- 15a. More thorough param expansions
--------------------------------------------------------------------------------

translateParamExp :: Token -> ArgStr
translateParamExp token@(T_DollarBraced _ _ expansions) =
  case parseParamExpansion expansions of
    Just fishArg -> fishArg
    Nothing ->
      ArgLiteral ("<unsupported param expansion: " <> literalText token <> ">")

translateParamExp other = ArgLiteral (literalText other)

-- A more complex approach might parse expansions in detail. For brevity, we do partial coverage:
-- e.g. we detect forms like ${VAR:-DEFAULT}, ${#VAR}, ${VAR%pattern}, etc.
parseParamExpansion :: [Token] -> Maybe ArgStr
parseParamExpansion expansions =
  -- expansions is typically the content inside ${ ... }. 
  -- We'll do a naive check for:
  --   #VAR 
  --   VAR:-something
  --   VAR%...  or VAR%%...
  --   ...
  case expansions of
    -- e.g. [T_Literal _ "#VAR"]
    [lit@(T_Literal _ text)] ->
      if T.isPrefixOf "#" text
        then Just (ArgSubstituted (Command "string length" [ArgLiteral ("$" <> T.drop 1 text)]))
        else Just (ArgVariable text)
    -- e.g. "VAR:-DEFAULT"
    [lit@(T_Literal _ text)] | Just (var, def) <- detectColonDash text ->
      Just (ArgSubstituted (Command "" [rewriteColonDash var def]))
    -- e.g. "VAR%PATTERN"
    [lit@(T_Literal _ text)] | Just (var, pat) <- detectPercent text ->
      Just (ArgSubstituted (Command "string replace" [ArgLiteral "-r", ArgLiteral (makePercentPattern pat), ArgLiteral "", ArgLiteral ("$" <> var)]))
    _ ->
      Nothing

-- Detect forms like "VAR:-DEFAULT"
detectColonDash :: Text -> Maybe (Text, Text)
detectColonDash txt =
  case T.splitOn ":-" txt of
    [var, def] -> Just (var, def)
    _ -> Nothing

rewriteColonDash :: Text -> Text -> FishArg TStr
rewriteColonDash var defValue =
  -- ${VAR:-def} => if $VAR is non-empty => $VAR else def
  -- fish approach: (test -n "$VAR" && echo "$VAR" || echo "defValue")
  ArgLiteral $
    "(test -n \"$" <> var <> "\"; and echo \"$" <> var <> "\"; or echo \"" <> defValue <> "\")"

-- Detect "VAR%PATTERN"
detectPercent :: Text -> Maybe (Text, Text)
detectPercent txt =
  -- naive approach: find first '%'
  let (var, patWithPercent) = T.breakOn "%" txt
   in if T.null patWithPercent
        then Nothing
        else Just (var, T.drop 1 patWithPercent)

makePercentPattern :: Text -> Text
makePercentPattern pat = "^(" <> pat <> ")"

--------------------------------------------------------------------------------
---- 16. Command Substitutions returning TStr
--------------------------------------------------------------------------------

translateTokenToStrCmd :: Token -> FishCommand TStr
translateTokenToStrCmd = \case
  T_Backticked _ stmts ->
    SubcommandOutput (map translateToken stmts)
  T_DollarExpansion _ stmts ->
    SubcommandOutput (map translateToken stmts)
  T_DollarBraceCommandExpansion _ stmts ->
    SubcommandOutput (map translateToken stmts)
  other ->
    SubcommandOutput [translateToken other]

--------------------------------------------------------------------------------
---- 17. Case items
--------------------------------------------------------------------------------

translateCase :: (CaseType, [Token], [Token]) -> CaseItem
translateCase (_, patterns, body) =
  CaseItem (map argLiteralText patterns) (map translateToken body)

--------------------------------------------------------------------------------
---- 18. Generic Helpers
--------------------------------------------------------------------------------

literalText :: Token -> Text
literalText tok = T.pack (getLiteralStringDef "" tok)

argLiteralText :: Token -> FishArg 'TStr
argLiteralText tok = ArgLiteral (literalText tok)

-- For lists of tokens (e.g. an arithmetic expression), just join them
argLiteralTextList :: [Token] -> FishArg 'TStr
argLiteralTextList ts = ArgLiteral (T.unwords (map literalText ts))