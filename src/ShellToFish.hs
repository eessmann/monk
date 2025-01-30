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
-- Main Translation Function
--------------------------------------------------------------------------------

translateRoot :: Root -> FishStatement
translateRoot (Root topToken) = translateToken topToken

--------------------------------------------------------------------------------
-- Token Translation
--------------------------------------------------------------------------------

translateToken :: Token -> FishStatement
translateToken token =
  case token of
    -- A top-level script is just a list of commands/statements
    T_Script _ _ stmts ->
      StmtList (map translateToken stmts)

    -- A simple command can have variable assignments plus the actual command
    T_SimpleCommand _ assignments rest ->
      StmtList (assignmentsToSets assignments <> [translateSimpleCommand rest])

    -- A pipeline might have an optional 'bang' (!). In shellcheck AST,
    -- the second list is the sequence of commands, the first is the 'bang' tokens.
    T_Pipeline _ bang cmds ->
      translatePipeline bang cmds

    -- If-then-elif-else
    T_IfExpression _ conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch

    -- While loops
    T_WhileExpression _ cond body ->
      -- We translate the condition to a FishExpr TBool by wrapping a BoolExpr
      Stmt (While (ExprBool (translateBoolTokens cond)) (map translateToken body))

    -- Until loops => "until <cond>; do <body>; done" => while !(cond)
    T_UntilExpression _ cond body ->
      Stmt
        ( While
            (negateTBool (ExprBool (translateBoolTokens cond)))
            (map translateToken body)
        )

    -- Functions
    T_Function _ _ _ funcName body ->
      translateFunction funcName body

    -- A `{ ... }` group in bash => just a list of statements in fish
    T_BraceGroup _ tokens ->
      StmtList (map translateToken tokens)

    -- A subshell `( ... )` => fish does not have the same concept,
    -- but we can approximate with `begin ... end`
    T_Subshell _ tokens ->
      StmtList [Stmt (Begin (map translateToken tokens))]

    -- Logical AND / OR operators (&& / ||)
    -- The pattern synonyms are T_AndIf id left right, T_OrIf id left right
    T_AndIf id left right ->
      Stmt (JobControl ConjAnd (translateTokenToStatusCmd left) (translateTokenToStatusCmd right))

    T_OrIf id left right ->
      Stmt (JobControl ConjOr (translateTokenToStatusCmd left) (translateTokenToStatusCmd right))

    -- Backgrounding with &
    T_Backgrounded _ bgToken ->
      Stmt (Background (translateTokenToStatusCmd bgToken))

    -- ShellCheck "annotation" tokens are basically wrappers: skip them
    T_Annotation _ _ inner ->
      translateToken inner

    -- For var in words; do ...
    T_ForIn _ var tokens body ->
      let fishVar = T.pack var
          fishListArg = ArgList (map argLiteralText tokens)
       in Stmt (For fishVar fishListArg (map translateToken body))

    -- "case <switch> in pattern) body ;; esac" => fish "switch <switch> case pattern ... end"
    T_CaseExpression _ switchExpr cases ->
      Stmt (Switch (argLiteralText switchExpr) (map translateCase cases))

    -- Fallback for anything unimplemented
    _ ->
      Comment $ "Unimplemented token: " <> showToken token

--------------------------------------------------------------------------------
-- Token to String Conversion for Debugging
--------------------------------------------------------------------------------

showToken :: Token -> Text
showToken = T.pack . show

--------------------------------------------------------------------------------
-- Command Translation
--------------------------------------------------------------------------------

translateSimpleCommand :: [Token] -> FishStatement
translateSimpleCommand cmdTokens =
  case cmdTokens of
    [] ->
      -- No actual command -> 'true' to avoid empty command in fish
      Stmt (Command "true" [])
    (c : args) ->
      Stmt (Command (literalText c) (map translateArgOrRedirect args))

--------------------------------------------------------------------------------
-- Pipeline Translation
--------------------------------------------------------------------------------

translatePipeline :: [Token] -> [Token] -> FishStatement
translatePipeline bang cmds =
  case cmds of
    [] ->
      -- If there's an empty pipeline, just do 'true'
      Stmt (Command "true" [])
    [single] ->
      if null bang
        then Stmt (translateTokenToStatusCmd single)
        else Stmt (Not (translateTokenToStatusCmd single))
    (c1 : c2 : rest) ->
      -- We have at least two commands in the pipeline
      let firstCmd = translateTokenToStatusCmd c1
          otherCmds = map translateTokenToStatusCmd (c2 : rest)
          pipelineCmds = firstCmd :| otherCmds
          pipe = Pipeline pipelineCmds
       in if null bang
            then Stmt pipe
            else Stmt (Not pipe)

--------------------------------------------------------------------------------
-- If Expression Translation
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> FishStatement
translateIfExpression conditionBranches elseBranch =
  -- Build nested If ... Else from left to right
  StmtList (go conditionBranches)
  where
    go [] =
      -- If no more condition branches, the else body is run
      map translateToken elseBranch

    go ((condTokens, thenTokens) : rest) =
      [ Stmt $
          If
            (ExprBool (translateBoolTokens condTokens))
            (map translateToken thenTokens)
            (go rest) -- the else is either next branch or final
      ]

--------------------------------------------------------------------------------
-- Function Definition Translation
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
-- Variable Assignment Translation
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
      let scope = ScopeLocal
          setCmd =
            Set
              scope
              (T.pack var)
              (argLiteralText val)
       in Stmt setCmd
    _ ->
      Comment $ "Unrecognized assignment pattern: " <> showToken tok

--------------------------------------------------------------------------------
-- Condition Translation
-- We'll return a BoolExpr so we can wrap it in ExprBool
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> BoolExpr
translateBoolTokens = \case
  -- No tokens => 'true'
  [] -> BoolLiteral True

  -- Single '!' => false
  [T_Bang _] -> BoolLiteral False

  -- "! foo" => not(translate foo)
  [T_Bang _, t] -> BoolNot (translateBoolToken t)

  -- Single => interpret that as a command test
  [t] -> translateBoolToken t

  -- "cond && rest"
  (cond : T_AND_IF _ : rest) ->
    BoolAnd (translateBoolToken cond) (translateBoolTokens rest)

  -- "cond || rest"
  (cond : T_OR_IF _ : rest) ->
    BoolOr (translateBoolToken cond) (translateBoolTokens rest)

  -- If we can't parse a simpler pattern, treat it as a single command
  -- pipeline. That yields a status => cast that to a bool.
  tokens ->
    BoolCommand (translateTokenToStatusCmd (makeSinglePipeline tokens))
  where
    -- Wrap a list of tokens in a ShellCheck T_Pipeline so we can reuse
    -- translateTokenToStatusCmd. We'll pass an empty "bang" list.
    makeSinglePipeline :: [Token] -> Token
    makeSinglePipeline [] =
    -- Decide what to do if no tokens are given:
    -- For example, fallback to a simple 'true' command, or throw an error, etc.
    -- This snippet uses a fallback T_SimpleCommand with no arguments:
      T_SimpleCommand (Id 0) [] []
    makeSinglePipeline (t : ts) =
    -- Here 't' is the first token, and 'ts' is the rest.
    -- We create a T_Pipeline node using the first token's 'Id' and
    -- passing the entire (t:ts) list as the pipeline's commands.
      T_Pipeline (getId t) [] (t : ts)

-- Translate a single token into a BoolExpr
translateBoolToken :: Token -> BoolExpr
translateBoolToken = \case
  -- e.g. literal "true" => BoolLiteral True
  T_Literal _ "true" -> BoolLiteral True
  T_Literal _ "false" -> BoolLiteral False
  -- otherwise interpret as a command
  other -> BoolCommand (translateTokenToStatusCmd other)

--------------------------------------------------------------------------------
-- Negate a FishExpr TBool
--------------------------------------------------------------------------------

negateTBool :: FishExpr TBool -> FishExpr TBool
negateTBool = \case
  -- If it's already an ExprBool with BoolNot, we can remove double negation
  ExprBool (BoolNot b) -> ExprBool b
  ExprBool b -> ExprBool (BoolNot b)
  -- Fallback: if we had any other TBool expression
  x -> ExprBool (BoolNot (BoolCommand (Command (T.pack "test") []))) -- or something fallback

--------------------------------------------------------------------------------
-- Command Translation to TStatus
--------------------------------------------------------------------------------

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest ->
      case rest of
        [] ->
          -- Just assignments => fish "set" then "true"
          if null assignments
            then Command "true" []
            else Command "true" (assignmentsToArgs assignments)
        (c : args) ->
          Command
            (literalText c)
            (assignmentsToArgs assignments <> map translateArgOrRedirect args)

    T_Pipeline _ bang commands ->
      translatePipelineToStatus bang commands

    T_AndIf _ l r ->
      JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)

    T_OrIf _ l r ->
      JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)

    _ ->
      -- Fallback: interpret any leftover as a "command <literal of token>"
      Command (literalText token) []

--------------------------------------------------------------------------------

translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  case cmds of
    [] ->
      Command "true" []
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

--------------------------------------------------------------------------------
-- Assignments to Args
--------------------------------------------------------------------------------

assignmentsToArgs :: [Token] -> [FishArgOrRedirect]
assignmentsToArgs assignments =
  [ Arg (argLiteralText assignment)
    | assignment <- assignments,
      isAssignment assignment
  ]

--------------------------------------------------------------------------------
-- Argument or Redirect Translation
--------------------------------------------------------------------------------

translateArgOrRedirect :: Token -> FishArgOrRedirect
translateArgOrRedirect tok =
  case tok of
    -- Redirections like > file, < file
    T_IoFile _ op file ->
      let op' = case op of
            T_Less _ -> RedirectIn
            T_Greater _ -> RedirectOut
            T_DGREAT _ -> RedirectOutAppend
            T_CLOBBER _ -> RedirectOut
            _ ->
              error $
                "Unsupported IOFile operation: " <> show op
          file' = argLiteralText file
       in Redir op' file'

    -- e.g. 2>&1 or 2<&3
    T_IoDuplicate _ op num ->
      let op' = case op of
            T_LESSAND _ -> RedirectIn
            T_GREATAND _ -> RedirectOut
            _ ->
              error $
                "Unsupported IoDuplicate operation: " <> show op
          num' = ArgLiteral (T.pack num)
       in Redir op' num'

    -- Otherwise, it's an argument
    _ -> Arg (translateArg tok)

--------------------------------------------------------------------------------
-- Argument Translation
--------------------------------------------------------------------------------

-- We only return string-typed arguments. If you need TBool, TInt, etc.
-- you can add separate logic or function overloads.
translateArg :: Token -> ArgStr
translateArg tok =
  case tok of
    T_Literal _ _ ->
      ArgLiteral (literalText tok)

    T_SingleQuoted _ s ->
      ArgLiteral (T.pack s)

    T_DoubleQuoted _ strs ->
      ArgLiteral (T.concat (map literalText strs))

    -- A bare parameter expansion, e.g. ${VAR}
    T_DollarBraced _ False var ->
      ArgVariable (literalText var)

    -- Command substitutions: backticks, $( ), ${ } for commands
    T_Backticked _ tokens ->
      ArgSubstituted (translateTokenToStrCmd (T_Backticked (getId tok) tokens))

    T_DollarExpansion _ tokens ->
      ArgSubstituted (translateTokenToStrCmd (T_DollarExpansion (getId tok) tokens))

    T_DollarBraceCommandExpansion _ tokens ->
      ArgSubstituted (translateTokenToStrCmd (T_DollarBraceCommandExpansion (getId tok) tokens))

    -- Fallback
    _ ->
      ArgLiteral (literalText tok)

--------------------------------------------------------------------------------
-- Command-substitution translation to FishCommand TStr
--------------------------------------------------------------------------------

-- In fish, any subcommand that runs is captured as a string if used in $(..).
-- We'll use our custom 'SubcommandOutput [FishStatement]' constructor for that.
translateTokenToStrCmd :: Token -> FishCommand TStr
translateTokenToStrCmd tok =
  case tok of
    -- For example, a backtick might be multiple statements
    T_Backticked _ stmts ->
      SubcommandOutput (map translateToken stmts)

    T_DollarExpansion _ stmts ->
      SubcommandOutput (map translateToken stmts)

    T_DollarBraceCommandExpansion _ stmts ->
      SubcommandOutput (map translateToken stmts)

    -- If we somehow get something else, just wrap it
    _ ->
      SubcommandOutput [translateToken tok]

--------------------------------------------------------------------------------
-- Case Translation
--------------------------------------------------------------------------------

translateCase :: (CaseType, [Token], [Token]) -> CaseItem
translateCase (_, patterns, body) =
  CaseItem
    (map argLiteralText patterns)
    (map translateToken body)

--------------------------------------------------------------------------------
-- Helper: get the literal Text from a Token
--------------------------------------------------------------------------------

literalText :: Token -> Text
literalText tok = T.pack (getLiteralStringDef "" tok)

argLiteralText :: Token -> ArgStr
argLiteralText tok = ArgLiteral (literalText tok)