{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot,
    translateToken,
  )
where

import Prelude hiding (show) -- Use Relude
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralStringDef, isAssignment)
import GHC.Show (show) -- Use GHC.Show

--------------------------------------------------------------------------------
-- 1. Main translation functions
--------------------------------------------------------------------------------

translateRoot :: Root -> FishStatement
translateRoot (Root topToken) = translateToken topToken

-- | Dispatch on a ShellCheck Token to produce a FishStatement.
translateToken :: Token -> FishStatement
translateToken token =
  case token of
    T_Script _ _ stmts ->
      wrapStmtList (map translateToken stmts)

    -- Handle VAR=val command ...
    T_SimpleCommand _ assignments cmdToks ->
      translateSimpleCommand assignments cmdToks

    T_Pipeline _ bang cmds ->
      translatePipeline bang cmds

    -- Handle sequential commands (;)
    T_CmdSeparator _ t1 t2 ->
      StmtList [translateToken t1, translateToken t2] -- Simple sequence
    T_SeqSeparator _ t1 t2 ->
      StmtList [translateToken t1, translateToken t2] -- Simple sequence, often newline

    T_IfExpression _ conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch

    T_WhileExpression _ cond body ->
      case toNonEmptyStmtList (map translateToken body) of
        Just neBody -> Stmt (While (translateBoolTokens cond) neBody)
        Nothing     -> Comment "Skipped empty while loop body"

    T_UntilExpression _ cond body ->
      case toNonEmptyStmtList (map translateToken body) of
         Just neBody -> Stmt (While (negateBoolExpr (translateBoolTokens cond)) neBody)
         Nothing     -> Comment "Skipped empty until loop body"

    T_Function _ _ _ funcName body ->
      translateFunction funcName body

    T_BraceGroup _ tokens ->
      -- Braces often define a scope or just group; Begin/End creates scope in Fish
      case toNonEmptyStmtList (map translateToken tokens) of
         Just neBody -> Stmt (Begin neBody) -- Use Begin/End for grouping/scope
         Nothing     -> Comment "Skipped empty brace group"

    T_Subshell _ tokens ->
      -- Subshell implies execution in a separate environment.
      -- Fish equivalent is tricky. `fish -c '...'` or `begin...end` captures output/status
      -- For now, use Begin/End block, acknowledging it's not a perfect match for isolation.
      case toNonEmptyStmtList (map translateToken tokens) of
         Just neBody -> Stmt (Begin neBody)
         Nothing     -> Comment "Skipped empty subshell"

    T_AndIf _ l r ->
      Stmt (JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_OrIf _ l r ->
      Stmt (JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))

    T_Backgrounded _ bgToken ->
      -- Ensure the command returns TStatus for Background
      Stmt (Background (translateTokenToStatusCmd bgToken))

    T_Annotation _ _ inner ->
      translateToken inner -- Ignore annotations

    T_ForIn _ var tokens body ->
      -- tokens are words to iterate over, body is list of commands
      case toNonEmptyStmtList (map translateToken body) of
        Just neBody ->
          Stmt (For @TStr (T.pack var)
                          (ExprListLit (map translateTokenToExpr tokens)) -- Translate words to expressions
                          neBody)
        Nothing -> Comment "Skipped empty for loop body"

    T_CaseExpression _ switchExpr cases ->
      translateCaseExpression switchExpr cases

    -- Here Documents: Translate to echo | command
    T_HereDoc _ _ _ limitStr contentTokens ->
      translateHereDoc limitStr contentTokens

    -- Explicitly ignore or comment out unsupported constructs
    T_CoProc {} -> Comment "Unsupported: Coprocess (coproc)"
    T_ArithmeticFor {} -> Comment "Unsupported: Arithmetic C-style for loop ((...))"
    T_SelectExpression {} -> Comment "Unsupported: Select expression"
    T_LetExpression {} -> Comment "Unsupported: Let expression"

    -- Default case for tokens that don't form a standalone statement
    -- or are potentially part of other constructs handled elsewhere.
    -- This might include literals, parameters, etc., when not in a command context.
    -- If they appear top-level, they might be no-ops or errors.
    _ ->
      Comment ("Skipped token at statement level: " <> T.pack (show token))


--------------------------------------------------------------------------------
-- 2. Helper functions
--------------------------------------------------------------------------------

-- Safely convert a list of statements to NonEmpty, adding a comment if empty.
-- Useful where Fish syntax requires a non-empty block (for, while, begin, etc.)
toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (/= SemiNl) stmts) -- Filter out empty separators

-- Wrap potentially multiple statements resulting from a translation
wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts

-- Generate comments for unsupported features
unsupported :: Show a => Text -> a -> FishStatement
unsupported constructName x = Comment ("Unsupported " <> constructName <> ": " <> T.pack (show x))

-- Translate a token into an expression (typically string)
translateTokenToExpr :: Token -> FishExpr TStr
translateTokenToExpr = \case
  T_Literal _ s -> ExprLiteral (T.pack s)
  T_SingleQuoted _ s -> ExprLiteral (T.pack s)
  T_DoubleQuoted _ parts -> ExprLiteral (T.concat (map tokenToLiteralText parts))
  T_Arithmetic _ exprToken ->
    -- Use `math` command substitution
    ExprSubstituted (SubcommandOutputStr (translateArithmetic exprToken :| []))
  t@(T_DollarBraced _ _ _) ->
    translateParamExp t
  T_Backticked _ stmts ->
    case toNonEmptyStmtList (map translateToken stmts) of
      Just neBody -> ExprSubstituted (SubcommandOutputStr neBody)
      Nothing     -> ExprLiteral "" -- Empty backticks result in empty string
  -- T_DollarExpansion and T_DollarBraceCommandExpansion are essentially $(...)
  T_DollarExpansion _ stmts ->
    case toNonEmptyStmtList (map translateToken stmts) of
      Just neBody -> ExprSubstituted (SubcommandOutputStr neBody)
      Nothing     -> ExprLiteral ""
  T_DollarBraceCommandExpansion _ stmts ->
    case toNonEmptyStmtList (map translateToken stmts) of
      Just neBody -> ExprSubstituted (SubcommandOutputStr neBody)
      Nothing     -> ExprLiteral ""
  -- Other tokens might represent variables or require context
  T_Parameter _ name -> ExprVariable @TStr (T.pack name) Nothing
  other -> ExprLiteral (tokenToLiteralText other) -- Default to literal text

-- Extract literal text from various token types
tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""


--------------------------------------------------------------------------------
-- 3. Simple command and assignment translation
--------------------------------------------------------------------------------

-- Handles `VAR=val command ...` by translating to `set -lx VAR val; command ...`
-- Handles simple commands, rewriting builtins or translating to `Command`
translateSimpleCommand :: [Token] -> [Token] -> FishStatement
translateSimpleCommand assignments cmdTokens =
  let fishAssignments = mapMaybe translateAssignment assignments
      fishCmd = translateCommandTokens cmdTokens

      -- Combine assignments and command
  in case (NE.nonEmpty fishAssignments, fishCmd) of
        (Just neAssigns, Just fCmd) ->
            -- Need to wrap the command to apply env vars locally.
            -- Use `begin; set -lx ...; cmd; end`
            case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
                Just body -> Stmt $ Begin body
                Nothing -> Comment "Empty command with assignments" -- Should not happen
        (Just neAssigns, Nothing) ->
            -- Only assignments, treat them as local sets
            StmtList (NE.toList neAssigns)
        (Nothing, Just fCmd) ->
            -- Only a command
            Stmt fCmd
        (Nothing, Nothing) ->
            -- Empty simple command (e.g., just ';')
            SemiNl
            -- Stmt (Command "true" []) -- Or represent as 'true'

-- Translate individual assignment tokens `VAR=val` or `VAR+=val` etc.
translateAssignment :: Token -> Maybe FishStatement
translateAssignment tok =
  case tok of
    T_Assignment _ _ var maybeOp val ->
      let fishVar = T.pack var
          -- Fish uses `set -lx` for temporary env vars for a command.
          -- For standalone assignments, `set -l` is usually appropriate.
          fishScope = ScopeLocal
          -- TODO: Handle operators like += (requires list append in Fish)
          fishValExpr = translateTokenToExpr val
      in Just $ Stmt (Set fishScope fishVar fishValExpr)
    _ -> Nothing

-- Translate the command part (name + args) of a simple command
translateCommandTokens :: [Token] -> Maybe (FishCommand TStatus)
translateCommandTokens cmdTokens =
  case cmdTokens of
    [] ->
      Nothing -- No command part
    (c : args) ->
      Just $ case maybeRewriteSpecialBuiltin c args of
        Just builtin -> builtin
        Nothing ->
          case isSingleBracketTest c args of
            Just testCmd -> testCmd
            Nothing ->
              Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)

-- Rewrite special builtins that differ significantly in Fish
maybeRewriteSpecialBuiltin :: Token -> [Token] -> Maybe (FishCommand TStatus)
maybeRewriteSpecialBuiltin cmdToken args =
  let cmdName = T.strip (tokenToLiteralText cmdToken)
      argExprs = map translateTokenToExpr args
   in case cmdName of
        "shift" ->
          -- `shift N` translates to `set argv (string sub --start=(math N+1) $argv)`
          -- `shift` (N=1) translates to `set argv $argv[2..]`
          let nExpr = fromMaybe (ExprNumLiteral 1) (args ^? ix 0 >>= Just . translateTokenToExpr) -- Simplified: assumes arg is number
          in Just (Command "set" [ExprVal (ExprLiteral "argv"), ExprVal (ExprVariable @"TList TStr" "argv" (Just (RangeIndex nExpr (ExprNumLiteral (-1)))))]) -- Simplified: argv[N..-1]
             -- Accurate translation is more complex, handling N dynamically

        "unset" -> Just (rewriteUnset args)

        "hash" -> Just (Command "true" []) -- Fish manages paths automatically

        "export" -> Just (rewriteExport args)

        "eval" -> Just (rewriteEval args)

        -- `.` or `source` - handled by `Source` constructor if possible
        "." | not (null args) -> Just (Source (translateTokenToExpr (head args))) -- Assuming first arg is filename
        "source" | not (null args) -> Just (Source (translateTokenToExpr (head args)))

        -- `trap` is complex and has no direct equivalent for most signals.
        "trap" -> Just (Command "echo" [ExprVal (ExprLiteral "# Warning: 'trap' command ignored")])

        _ -> Nothing

rewriteUnset :: [Token] -> FishCommand TStatus
rewriteUnset tokens =
  let stringedArgs = map (T.strip . tokenToLiteralText) tokens
      (flags, names) = span (\txt -> T.isPrefixOf "-" txt) stringedArgs
      isFunc = "-f" `elem` flags
      isVar = "-v" `elem` flags || not isFunc -- Default to variable
      targetNames = map ExprLiteral names
  in if null names
       then Command "true" [] -- unset with no names is no-op
       else if isFunc
            then Command "functions" (ExprVal (ExprLiteral "--erase") : map ExprVal targetNames)
            else Command "set" (ExprVal (ExprLiteral "--erase") : map ExprVal targetNames)

rewriteExport :: [Token] -> FishCommand TStatus
rewriteExport tokens =
  case mapMaybe parseExportArg tokens of
    [] -> Command "set" [ExprVal (ExprLiteral "-x")] -- `export` with no args lists exported vars
    assigns -> Command "set" (ExprVal (ExprLiteral "-gx") : assigns) -- `set -gx VAR VAL` or `set -gx VAR`

parseExportArg :: Token -> Maybe ExprOrRedirect
parseExportArg tok = case tok of
    T_Assignment _ _ var _ val -> Just (ExprVal (ExprVariable @TStr (T.pack var) Nothing)) -- Export VAR=VAL
    T_Literal _ var -> Just (ExprVal (ExprVariable @TStr (T.pack var) Nothing)) -- Export VAR
    _ -> Nothing


rewriteEval :: [Token] -> FishCommand TStatus
rewriteEval tokens =
  let combinedArgs = T.unwords (map tokenToLiteralText tokens)
  in Command "eval" [ExprVal (ExprLiteral combinedArgs)]


--------------------------------------------------------------------------------
-- 4. Single-bracket test rewriting: [ expr ] -> test expr
--------------------------------------------------------------------------------

isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "["
      then case NE.nonEmpty args of
             Just neArgs ->
               let lastToken = NE.last neArgs
                   lastText = tokenToLiteralText lastToken
                   middle = NE.init neArgs
                in if lastText == "]"
                   then Just (Command "test" (map translateTokenToExprOrRedirect middle))
                   else Nothing -- Missing closing bracket
             Nothing -> Nothing -- Empty `[` command
      else Nothing


--------------------------------------------------------------------------------
-- 5. Pipeline translation
--------------------------------------------------------------------------------

translatePipeline :: [Token] -> [Token] -> FishStatement
translatePipeline bang cmds =
  case mapMaybe translateTokenToMaybeStatusCmd cmds of
    [] -> Stmt (Command "true" []) -- Empty pipeline
    (c:cs) -> let pipe = Pipeline (c :| cs)
              in if null bang -- No '!' negation
                   then Stmt pipe
                   else Stmt (Not pipe)

-- Helper for pipeline, translating tokens that should result in a status command
translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    -- Commands that naturally return status
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Pipeline _ bang cmds -> Just (translatePipelineToStatus bang cmds)
    T_AndIf _ l r -> Just (JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_OrIf _ l r -> Just (JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_IfExpression _ cb eb -> Just (translateIfToStatus cb eb)
    T_WhileExpression _ c b -> Just (translateWhileToStatus c b)
    T_UntilExpression _ c b -> Just (translateUntilToStatus c b)
    T_Subshell _ tokens -> Just (translateSubshellToStatus tokens)
    T_BraceGroup _ tokens -> Just (translateBraceGroupToStatus tokens)

    -- Other constructs might not directly yield status, or are handled elsewhere
    _ -> Nothing


--------------------------------------------------------------------------------
-- 6. if/while/until/subshell/brace that return status
--------------------------------------------------------------------------------

translateIfToStatus :: [([Token], [Token])] -> [Token] -> FishCommand TStatus
translateIfToStatus conditionBranches elseBranch =
  -- Wrap the If statement in `begin ... end` to capture its status
  Begin (translateIfExpression conditionBranches elseBranch :| [])

translateWhileToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateWhileToStatus cond body =
  case toNonEmptyStmtList (map translateToken body) of
    Just neBody -> Begin (Stmt (While (translateBoolTokens cond) neBody) :| [])
    Nothing     -> Command "true" [] -- Empty loop is success

translateUntilToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateUntilToStatus cond body =
  case toNonEmptyStmtList (map translateToken body) of
    Just neBody -> Begin (Stmt (While (negateBoolExpr (translateBoolTokens cond)) neBody) :| [])
    Nothing     -> Command "true" [] -- Empty loop is success

translateSubshellToStatus :: [Token] -> FishCommand TStatus
translateSubshellToStatus tokens =
  case toNonEmptyStmtList (map translateToken tokens) of
    Just neBody -> Begin neBody
    Nothing     -> Command "true" []

translateBraceGroupToStatus :: [Token] -> FishCommand TStatus
translateBraceGroupToStatus tokens =
  case toNonEmptyStmtList (map translateToken tokens) of
    Just neBody -> Begin neBody
    Nothing     -> Command "true" []


--------------------------------------------------------------------------------
-- 7. if-expression translation
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> FishStatement
translateIfExpression conditionBranches elseBranch =
  Stmt (translateIf' conditionBranches (map translateToken elseBranch))
  where
    translateIf' :: [([Token], [Token])] -> [FishStatement] -> FishCommand TUnit
    translateIf' [] elseStmts =
      -- Base case: no more branches, just the final else block (if any)
      case toNonEmptyStmtList elseStmts of
          Just neElse -> Begin neElse -- Wrap else block
          Nothing     -> Command "true" [] -- No else, treat as success (TUnit command)

    translateIf' ((condTokens, thenTokens) : rest) elseStmts =
      let condition = translateBoolTokens condTokens
          thenBlock = map translateToken thenTokens
          elseBlock = [Stmt (translateIf' rest elseStmts)] -- Recursive call for else-if/else
      in case toNonEmptyStmtList thenBlock of
           Just neThen -> If condition neThen elseBlock
           Nothing -> If condition (Comment "Empty 'then' block" :| []) elseBlock -- Handle empty then


--------------------------------------------------------------------------------
-- 8. Function definition translation
--------------------------------------------------------------------------------

translateFunction :: String -> Token -> FishStatement
translateFunction funcName bodyToken =
  let bodyStmts = case bodyToken of
                    T_BraceGroup _ stmts -> map translateToken stmts
                    T_Subshell _ stmts -> map translateToken stmts -- Treat subshell body similarly
                    _ -> [translateToken bodyToken] -- Single command body
      funcNameT = T.pack funcName
      -- Fish functions don't declare positional params; use $argv or argparse
  in case toNonEmptyStmtList bodyStmts of
       Just neBody ->
         Stmt (Function FishFunction
            { funcName = funcNameT,
              funcFlags = [], -- TODO: Translate ShellCheck function annotations?
              funcParams = [], -- Ignore POSIX params
              funcBody = neBody
            })
       Nothing -> Comment ("Skipped function with empty body: " <> funcNameT)


--------------------------------------------------------------------------------
-- 9. Case expression translation
--------------------------------------------------------------------------------

translateCaseExpression :: Token -> [(CaseType, [Token], [Token])] -> FishStatement
translateCaseExpression switchExpr cases =
  let switchArg = translateTokenToExpr switchExpr
      caseItems = mapMaybe translateCaseItem cases
  in case NE.nonEmpty caseItems of
       Just neCases -> Stmt (Switch switchArg neCases)
       Nothing      -> Comment "Skipped case expression with no valid cases"

translateCaseItem :: (CaseType, [Token], [Token]) -> Maybe CaseItem
translateCaseItem (_, patterns, body) =
  let patternExprs = map translateTokenToExpr patterns
      bodyStmts = map translateToken body
  in case (NE.nonEmpty patternExprs, toNonEmptyStmtList bodyStmts) of
       (Just nePatterns, Just neBody) -> Just CaseItem {casePatterns = nePatterns, caseBody = neBody}
       _ -> Nothing -- Skip cases with no patterns or empty body


--------------------------------------------------------------------------------
-- 10. Here Document Translation (Using echo pipe)
--------------------------------------------------------------------------------
translateHereDoc :: String -> [Token] -> FishStatement
translateHereDoc _limitStr contentTokens =
  -- Fish doesn't have here-docs. Emulate with echo -e "..." | command
  -- This assumes the heredoc is used as stdin for the *next* command.
  -- ShellCheck's AST structure for T_HereDoc needs careful handling regarding
  -- how it's attached to the command that reads it.
  -- This translation assumes the command follows immediately or pipeline follows.
  let content = T.intercalate "\n" (map tokenToLiteralText contentTokens)
      -- Create an echo command providing the content
      echoCmd = Command "echo" [ExprVal (ExprLiteral content)] -- Basic echo, consider -e ?
  in Stmt echoCmd -- This echo needs to be piped to the actual command


--------------------------------------------------------------------------------
-- 11. Boolean expression translation
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> FishExpr TBool
translateBoolTokens tokens = ExprBoolExpr (go tokens)
  where
    go :: [Token] -> BoolExpr
    go = \case
      [] -> BoolCommand (Command "true" []) -- Empty condition is true
      [T_Literal _ s] | s == "true" -> BoolCommand (Command "true" [])
      [T_Literal _ s] | s == "false" -> BoolCommand (Command "false" [])
      [T_Bang _ , t] -> BoolNot (translateBoolTokens [t]) -- Handle ! negation
      [t] -> translateBoolToken t -- Single token condition

      -- Handle AND/OR chains (Bash evaluation order)
      (t : T_AND_IF _ : rest) -> BoolAnd (ExprBoolExpr (translateBoolToken t)) (ExprBoolExpr (go rest))
      (t : T_OR_IF _ : rest) -> BoolOr (ExprBoolExpr (translateBoolToken t)) (ExprBoolExpr (go rest))

      -- Default: Assume the tokens form a command whose status is the boolean value
      ts -> BoolCommand (translateTokensToStatusCmd ts)

-- Translate a single token within a boolean expression context
translateBoolToken :: Token -> BoolExpr
translateBoolToken = \case
  T_Literal _ s | s == "true" -> BoolCommand (Command "true" [])
  T_Literal _ s | s == "false" -> BoolCommand (Command "false" [])
  T_Arithmetic _ exprToken ->
    -- Use `test $(math ...) -ne 0`
    let mathCmd = translateArithmetic exprToken
        substitutedExpr = ExprSubstituted mathCmd
    in BoolTestOp Neq substitutedExpr (ExprNumLiteral 0) -- test (math ...) != 0
  T_SimpleCommand _ assigns toks ->
      BoolCommand (translateCommandTokensToStatus assigns toks)
  T_Pipeline _ bang cmds ->
      BoolCommand (translatePipelineToStatus bang cmds)
  -- Test command `[` or `test`
  t | Just testCmd <- isSingleBracketTest t [] -> BoolCommand testCmd -- `[` without args
  t@(T_SimpleCommand _ _ (cmd:_)) | tokenToLiteralText cmd == "test" ->
      BoolCommand (translateCommandTokensToStatus [] [t]) -- Translate `test ...` command
  t | Just testCmd <- isSingleBracketTest cmd args, T_SimpleCommand _ _ (cmd:args) <- t ->
      BoolCommand testCmd -- Translate `[ ... ]` command

  -- Default: treat token as a command to execute for status
  t -> BoolCommand (translateTokenToStatusCmd t)

-- Negate a boolean expression for 'until' loops etc.
negateBoolExpr :: FishExpr TBool -> FishExpr TBool
negateBoolExpr (ExprBoolExpr (BoolNot b)) = b -- double negation
negateBoolExpr other = ExprBoolExpr (BoolNot other)


--------------------------------------------------------------------------------
-- 12. Commands that return a status (TStatus)
--------------------------------------------------------------------------------

-- Translate a list of tokens assumed to form a command, returning status
translateTokensToStatusCmd :: [Token] -> FishCommand TStatus
translateTokensToStatusCmd tokens =
  case tokens of
    [] -> Command "true" []
    -- Try parsing as known command structures first
    [T_SimpleCommand _ a r] -> translateCommandTokensToStatus a r
    [T_Pipeline _ b c] -> translatePipelineToStatus b c
    [T_IfExpression _ cb eb] -> translateIfToStatus cb eb
    [T_WhileExpression _ c b] -> translateWhileToStatus c b
    [T_UntilExpression _ c b] -> translateUntilToStatus c b
    [T_Subshell _ ts] -> translateSubshellToStatus ts
    [T_BraceGroup _ ts] -> translateBraceGroupToStatus ts
    [T_AndIf _ l r] -> JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)
    [T_OrIf _ l r] -> JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)

    -- Fallback: Treat as a simple command if structure isn't recognized above
    (c:args) -> Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)


-- Translate a single token to a command returning status
translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

-- Translate T_SimpleCommand components into a status command
translateCommandTokensToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateCommandTokensToStatus assignments cmdTokens = fromMaybe (Command "true" []) $ do
    fishCmd <- translateCommandTokens cmdTokens
    if null assignments
    then pure fishCmd
    else case mapMaybe translateAssignment assignments of
             [] -> pure fishCmd -- No effective assignments
             assignStmts ->
                 -- Wrap in begin/end block with assignments
                 case toNonEmptyStmtList (assignStmts ++ [Stmt fishCmd]) of
                    Just body -> pure $ Begin body
                    Nothing -> pure $ Command "true" [] -- Should not happen


-- Translate T_Pipeline components into a status command
translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  case mapMaybe translateTokenToMaybeStatusCmd cmds of
    [] -> Command "true" []
    (c:cs) -> let pipe = Pipeline (c :| cs)
              in if null bang then pipe else Not pipe

-- Translate T_Arithmetic into a command (e.g., `math ... > /dev/null`)
-- Arithmetic expression itself doesn't have status; use `math` command.
translateArithmetic :: Token -> FishCommand TStatus
translateArithmetic exprToken =
   let mathArgs = ExprVal (translateTokenToExpr exprToken)
   -- Execute `math` and check its status. Redirect output to avoid printing result.
   in Redirect (Command "math" [mathArgs]) RedirectOut (ExprLiteral "/dev/null")


--------------------------------------------------------------------------------
-- 13. Argument and redirection translation (ExprOrRedirect)
--------------------------------------------------------------------------------

translateTokenToExprOrRedirect :: Token -> ExprOrRedirect
translateTokenToExprOrRedirect = \case
  -- Redirections
  T_IoFile _ op file ->
    let op' = case op of
                T_Less _     -> RedirectIn
                T_Greater _  -> RedirectOut
                T_DGREAT _   -> RedirectOutAppend
                T_CLOBBER _  -> RedirectOut -- Overwrite redirect >|
                -- T_LESSGREAT <> not directly supported easily
                _ -> RedirectOut -- Default or error?
        fileExpr = translateTokenToExpr file
    in RedirectVal op' fileExpr
  T_IoDuplicate _ op numStr ->
    let op' = case op of
                T_LESSAND _  -> RedirectIn -- <& N
                T_GREATAND _ -> RedirectOut -- >& N
                _ -> RedirectOut -- Default or error?
        -- Target can be '-', N, or potentially a filename in Bash ({var}>&) - complex
        targetExpr = ExprLiteral (T.pack numStr) -- Assume number or '-' for now
    in RedirectVal op' targetExpr -- Needs refinement for &>, file descriptors etc.

  -- Here Strings <<<
  T_HereString _ word ->
      -- Emulate with echo | cmd; Fish has no direct <<<
      -- This is tricky as it should provide stdin to the command.
      -- Similar issue to HereDoc - need context of the command.
      -- For now, translate the word as a simple expression.
      ExprVal (translateTokenToExpr word)

  -- Default: Translate token as an expression value
  other -> ExprVal (translateTokenToExpr other)


--------------------------------------------------------------------------------
-- 14. Parameter expansion translation --> FishExpr TStr
--------------------------------------------------------------------------------

-- Translate ${...} forms
translateParamExp :: Token -> FishExpr TStr
translateParamExp token = case token of
  T_DollarBraced _ _ expansion -> parseAndTranslateParamExpansion expansion
  T_Parameter _ name -> ExprVariable @TStr (T.pack name) Nothing -- Simple $VAR
  _ -> ExprLiteral (tokenToLiteralText token) -- Fallback

parseAndTranslateParamExpansion :: Token -> FishExpr TStr
parseAndTranslateParamExpansion expansionToken =
  let expansionText = tokenToLiteralText expansionToken
   in fromMaybe (ExprLiteral ("<Unsupported Expansion: " <> expansionText <> ">")) $
      msum [ tryLength expansionText
           , trySubstring expansionText
           , tryReplace expansionText
           , tryDefault expansionText
           , tryPrefixRemoval expansionText
           , trySuffixRemoval expansionText
           , tryCaseModification expansionText -- Fish >= 3.x with `string lower/upper`
           , Just (ExprVariable @TStr expansionText Nothing) -- Default: treat as variable name
           ]

-- Helper functions for different expansion types:

tryLength :: Text -> Maybe (FishExpr TStr)
tryLength txt | T.isPrefixOf "#" txt =
  let var = T.drop 1 txt
  in Just $ ExprSubstituted $ SubcommandOutputStr $
       (Stmt $ Command "string" [ ExprVal (ExprLiteral "length"),
                                   ExprVal (ExprVariable @TStr var Nothing) ]) :| []
tryLength _ = Nothing

trySubstring :: Text -> Maybe (FishExpr TStr)
trySubstring txt | T.count ":" txt >= 1 && not (":-" `T.isInfixOf` txt || ":+" `T.isInfixOf` txt || ":?" `T.isInfixOf` txt || ":=" `T.isInfixOf` txt)=
  case T.splitOn ":" txt of
    [var, offset] -> Just $ stringSub var offset Nothing
    [var, offset, len] -> Just $ stringSub var offset (Just len)
    _ -> Nothing
trySubstring _ = Nothing

stringSub :: Text -> Text -> Maybe Text -> FishExpr TStr
stringSub var offset maybeLen =
  let baseArgs = [ ExprVal (ExprLiteral "sub"),
                   ExprVal (ExprLiteral "--start") , ExprVal (ExprLiteral offset) ]
      lenArgs = maybe [] (\l -> [ExprVal (ExprLiteral "--length"), ExprVal (ExprLiteral l)]) maybeLen
      varArg = ExprVal (ExprVariable @TStr var Nothing)
  in ExprSubstituted $ SubcommandOutputStr $
       (Stmt $ Command "string" (baseArgs ++ lenArgs ++ [varArg])) :| []

tryReplace :: Text -> Maybe (FishExpr TStr)
tryReplace txt | T.count "/" txt >= 1 =
  let (varPart, patternPart) = T.breakOn "/" txt
      parts = T.splitOn "/" (T.drop 1 patternPart) -- Drop leading /
  in case parts of
       -- Replace first: ${VAR/pattern/replacement}
       [pattern, replacement] -> Just $ stringReplace varPart pattern replacement False
       -- Replace all: ${VAR//pattern/replacement}
       ["", pattern, replacement] -> Just $ stringReplace varPart pattern replacement True
       _ -> Nothing
tryReplace _ = Nothing

stringReplace :: Text -> Text -> Text -> Bool -> FishExpr TStr
stringReplace var pattern replacement replaceAll =
  let flagArgs = if replaceAll then [ExprVal (ExprLiteral "--all")] else []
      mainArgs = [ ExprVal (ExprLiteral "replace") ] ++ flagArgs ++
                 [ ExprVal (ExprLiteral pattern),
                   ExprVal (ExprLiteral replacement),
                   ExprVal (ExprVariable @TStr var Nothing) ]
  in ExprSubstituted $ SubcommandOutputStr $
       (Stmt $ Command "string" mainArgs) :| []

tryDefault :: Text -> Maybe (FishExpr TStr)
tryDefault txt = msum
  [ T.splitOn ":-" txt & \(v, d) -> guard (length d == 1) >> Just (defaultVal v (head d) False) -- ${VAR:-default}
  , T.splitOn "-" txt  & \(v, d) -> guard (length d == 1) >> Just (defaultVal v (head d) False) -- ${VAR-default} (POSIX)
  , T.splitOn ":=" txt & \(v, d) -> guard (length d == 1) >> Just (defaultVal v (head d) True)  -- ${VAR:=default}
  , T.splitOn "=" txt  & \(v, d) -> guard (length d == 1) >> Just (defaultVal v (head d) True)   -- ${VAR=default} (POSIX assign)
  , T_splitOn ":?" txt & \(v, e) -> guard (length e == 1) >> Just (errorIfNull v (head e))      -- ${VAR:?err}
  , T.splitOn "?" txt  & \(v, e) -> guard (length e == 1) >> Just (errorIfNull v (head e))       -- ${VAR?err} (POSIX)
  , T.splitOn ":+" txt & \(v, a) -> guard (length a == 1) >> Just (alternativeVal v (head a))   -- ${VAR:+alt}
  , T.splitOn "+" txt  & \(v, a) -> guard (length a == 1) >> Just (alternativeVal v (head a))    -- ${VAR+alt} (POSIX)
  ]

defaultVal :: Text -> Text -> Bool -> FishExpr TStr
defaultVal var def assign =
  -- Fish: test -n "$VAR"; and echo "$VAR"; or echo "DEFAULT"
  -- If assign=True, need `set VAR DEFAULT` before `or echo`. Hard to do in pure expression.
  -- Translate := and = as just :- and - for now. Assignment requires statement context.
  let checkCmd = Command "test" [ExprVal (ExprLiteral "-n"), ExprVal (ExprVariable @TStr var Nothing)]
      andCmd = Command "echo" [ExprVal (ExprVariable @TStr var Nothing)]
      orCmd = Command "echo" [ExprVal (ExprLiteral def)]
      job = JobControl ConjOr (JobControl ConjAnd checkCmd andCmd) orCmd
  in ExprSubstituted $ SubcommandOutputStr $ (Stmt job :| [])

alternativeVal :: Text -> Text -> FishExpr TStr
alternativeVal var alt =
  -- Fish: test -n "$VAR"; and echo "ALT"; or echo ""
  let checkCmd = Command "test" [ExprVal (ExprLiteral "-n"), ExprVal (ExprVariable @TStr var Nothing)]
      andCmd = Command "echo" [ExprVal (ExprLiteral alt)]
      orCmd = Command "echo" [ExprVal (ExprLiteral "")]
      job = JobControl ConjOr (JobControl ConjAnd checkCmd andCmd) orCmd
  in ExprSubstituted $ SubcommandOutputStr $ (Stmt job :| [])

errorIfNull :: Text -> Text -> FishExpr TStr
errorIfNull var err =
  -- Fish: test -n "$VAR"; or echo "ERR" >&2; and exit 1; and echo "$VAR"
  -- This requires statement sequence, cannot be pure expression easily.
  -- Translate as just the variable for now, ignoring error checking.
   ExprVariable @TStr var Nothing
   -- Or return an error literal: ExprLiteral ("<Cannot translate ${..?..} error check>")

tryPrefixRemoval :: Text -> Maybe (FishExpr TStr)
tryPrefixRemoval txt = msum
  [ T.splitOn "#" txt & \(v,p) -> guard (length p == 1) >> Just (removePrefix v (head p) False) -- ${VAR#pat}
  , T.splitOn "##" txt & \(v,p) -> guard (length p == 1) >> Just (removePrefix v (head p) True) -- ${VAR##pat}
  ]

removePrefix :: Text -> Text -> Bool -> FishExpr TStr
removePrefix var pattern longest =
  let flag = if longest then "--max=1" else "--max=1" -- Fish `string replace` doesn't directly do shortest/longest match in this way
      -- Use regex mode `-r` and `^` anchor. Shortest/Longest needs careful regex pattern.
      -- Simplification: remove first match.
      regexPattern = "^" <> pattern
      args = [ ExprVal (ExprLiteral "replace"), ExprVal (ExprLiteral "-r"),
               ExprVal (ExprLiteral regexPattern), ExprVal (ExprLiteral ""),
               ExprVal (ExprVariable @TStr var Nothing) ]
  in ExprSubstituted $ SubcommandOutputStr $ (Stmt (Command "string" args)) :| []

trySuffixRemoval :: Text -> Maybe (FishExpr TStr)
trySuffixRemoval txt = msum
  [ T.splitOn "%" txt & \(v,p) -> guard (length p == 1) >> Just (removeSuffix v (head p) False) -- ${VAR%pat}
  , T.splitOn "%%" txt & \(v,p) -> guard (length p == 1) >> Just (removeSuffix v (head p) True) -- ${VAR%%pat}
  ]

removeSuffix :: Text -> Text -> Bool -> FishExpr TStr
removeSuffix var pattern longest =
  let flag = if longest then "--max=1" else "--max=1" -- See removePrefix comment
      regexPattern = pattern <> "$"
      args = [ ExprVal (ExprLiteral "replace"), ExprVal (ExprLiteral "-r"),
               ExprVal (ExprLiteral regexPattern), ExprVal (ExprLiteral ""),
               ExprVal (ExprVariable @TStr var Nothing) ]
  in ExprSubstituted $ SubcommandOutputStr $ (Stmt (Command "string" args)) :| []


tryCaseModification :: Text -> Maybe (FishExpr TStr)
tryCaseModification txt = msum
  [ T.splitOn "^" txt & \(v,p) -> guard (T.null p) >> Just (stringCase v True True) -- ${VAR^} - First upper
  , T.splitOn "^^" txt & \(v,p) -> guard (T.null p) >> Just (stringCase v True False) -- ${VAR^^} - All upper
  , T.splitOn "," txt & \(v,p) -> guard (T.null p) >> Just (stringCase v False True) -- ${VAR,} - First lower
  , T.splitOn ",," txt & \(v,p) -> guard (T.null p) >> Just (stringCase v False False) -- ${VAR,,} - All lower
    -- Bash patterns like ${VAR^pattern} are more complex, involving matching.
  ]

stringCase :: Text -> Bool -> Bool -> FishExpr TStr
stringCase var toUpper firstOnly =
  let cmd = if toUpper then "upper" else "lower"
      -- Fish `string upper/lower` affects the whole string. Emulating first char is complex.
      -- Translate only ^^ and ,, for now.
      args = [ ExprVal (ExprLiteral cmd), ExprVal (ExprVariable @TStr var Nothing) ]
  in if firstOnly
     then ExprLiteral ("<Unsupported first char case mod: " <> var <> ">") -- Fallback
     else ExprSubstituted $ SubcommandOutputStr $ (Stmt (Command "string" args)) :| []