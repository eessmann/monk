module Language.Fish.Translator
  ( translateRoot,
    translateToken,
  )
where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.ASTLib

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
      StmtList (map translateToken stmts)

    T_SimpleCommand _ assignments rest ->
      StmtList (assignmentsToSets assignments ++ [translateSimpleCommand rest])

    T_Pipeline _ bang cmds ->
      translatePipeline bang cmds

    T_IfExpression _ conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch

    T_WhileExpression _ cond body ->
      Stmt (While (ExprBool (translateBoolTokens cond)) (map translateToken body))

    T_UntilExpression _ cond body ->
      Stmt (While (negateTBool (ExprBool (translateBoolTokens cond))) (map translateToken body))

    T_Function _ _ _ funcName body ->
      translateFunction funcName body

    T_BraceGroup _ tokens ->
      StmtList (map translateToken tokens)

    T_Subshell _ tokens ->
      StmtList [Stmt (Begin (map translateToken tokens))]

    T_AndIf _ l r ->
      Stmt (JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_OrIf _ l r ->
      Stmt (JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))

    T_Backgrounded _ bgToken ->
      Stmt (Background (translateTokenToStatusCmd bgToken))

    T_Annotation _ _ inner ->
      translateToken inner

    T_ForIn _ var tokens body ->
      Stmt (For (T.pack var) (ArgList (map argLiteralText tokens)) (map translateToken body))

    T_CaseExpression _ switchExpr cases ->
      Stmt (Switch (argLiteralText switchExpr) (map translateCase cases))
      
    -- Extended redirection: here–document.
    T_HereDoc _ dashed quoted str tokens ->
      Stmt (HereDoc (ArgLiteral (T.pack str))
                     (ArgLiteral (T.unwords (map literalText tokens))))
    
    _ ->
      unsupported token

--------------------------------------------------------------------------------
-- 2. Debug and helper functions
--------------------------------------------------------------------------------

showToken :: Token -> T.Text
showToken = T.pack . show

unsupported :: Show a => a -> FishStatement
unsupported x = Comment ("Unsupported construct: " <> T.pack (show x))

--------------------------------------------------------------------------------
-- 3. Simple command and built–in rewriting
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
          case isSingleBracketTest c args of
            Just testCmd -> Stmt testCmd
            Nothing ->
              Stmt (Command (literalText c) (map translateArgOrRedirect args))

maybeRewriteSpecialBuiltin :: Token -> [Token] -> Maybe (FishCommand TStatus)
maybeRewriteSpecialBuiltin cmdToken args =
  let cmdName = T.strip (literalText cmdToken)
   in case cmdName of
        "shift" ->
          Just (Command "set" [Arg (ArgLiteral "argv[2..-1]")])
        "unset" ->
          Just (Command "" (rewriteUnsetArgs args))
        "hash" ->
          Just (Command "true" [])
        _ ->
          Nothing

rewriteUnsetArgs :: [Token] -> [FishArgOrRedirect]
rewriteUnsetArgs [] = [Arg (ArgLiteral "true")]
rewriteUnsetArgs tokens =
  let stringed = map (T.strip . literalText) tokens
      (flags, names) = span (\txt -> T.isPrefixOf "-" txt) stringed
      isFunc = "-f" `elem` flags
   in if null names
        then [Arg (ArgLiteral "true")]
        else
          if isFunc
            then [ Arg (ArgLiteral (T.intercalate "; " (map (\n -> "functions -e " <> n) names))) ]
            else [ Arg (ArgLiteral (T.intercalate "; " (map (\n -> "set -e " <> n) names))) ]

--------------------------------------------------------------------------------
-- 4. Single–bracket test rewriting: [ expr ]
--------------------------------------------------------------------------------

isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = literalText bracketToken
  in if bracketTxt == "["
     then case NE.nonEmpty args of
            Nothing -> Nothing
            Just neArgs ->
              let lastToken = NE.last neArgs
                  lastText  = literalText lastToken
                  middle    = NE.init neArgs
              in if lastText == "]" then Just (rewriteTest middle) else Nothing
     else Nothing

rewriteTest :: [Token] -> FishCommand TStatus
rewriteTest middle =
  let expression = T.unwords (map literalText middle)
  in Command "test" [Arg (ArgLiteral expression)]

--------------------------------------------------------------------------------
-- 5. Pipeline translation
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
      let firstCmd  = translateTokenToStatusCmd c1
          otherCmds = map translateTokenToStatusCmd (c2 : rest)
          pipe      = Pipeline (firstCmd :| otherCmds)
      in if null bang
           then Stmt pipe
           else Stmt (Not pipe)

--------------------------------------------------------------------------------
-- 6. if–expression translation
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> FishStatement
translateIfExpression conditionBranches elseBranch =
  StmtList (go conditionBranches)
  where
    go [] = map translateToken elseBranch
    go ((condTokens, thenTokens) : rest) =
      [ Stmt (If (ExprBool (translateBoolTokens condTokens))
                  (map translateToken thenTokens)
                  (go rest))
      ]

--------------------------------------------------------------------------------
-- 7. Function definition translation
--------------------------------------------------------------------------------

translateFunction :: String -> Token -> FishStatement
translateFunction funcName bodyToken =
  let bodyStmts =
        case bodyToken of
          T_BraceGroup _ stmts -> map translateToken stmts
          _ -> [translateToken bodyToken]
  in Stmt (Function FishFunction
         { funcName = T.pack funcName,
           funcFlags = [],
           funcParams = [],
           funcBody = bodyStmts
         })

--------------------------------------------------------------------------------
-- 8. Assignment translation
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
      Stmt (Set ScopeLocal (T.pack var) (argLiteralText val))
    _ ->
      unsupported tok

assignmentsToArgs :: [Token] -> [FishArgOrRedirect]
assignmentsToArgs assignments =
  [ Arg (argLiteralText assignment)
    | assignment <- assignments,
      isAssignment assignment
  ]

--------------------------------------------------------------------------------
-- 9. Boolean expression translation
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> BoolExpr
translateBoolTokens = \case
  [] -> BoolLiteral True
  [T_Bang _] -> BoolLiteral False
  [T_Bang _ , t] -> BoolNot (translateBoolToken t)
  [t] -> translateBoolToken t
  (cond : T_AND_IF _ : rest) ->
      BoolAnd (translateBoolToken cond) (translateBoolTokens rest)
  (cond : T_OR_IF _ : rest) ->
      BoolOr (translateBoolToken cond) (translateBoolTokens rest)
  tokens ->
      BoolCommand (translateTokenToStatusCmd (makeSinglePipeline tokens))
  where
    makeSinglePipeline [] = T_SimpleCommand (Id 0) [] []
    makeSinglePipeline (t : ts) = T_Pipeline (getId t) [] (t : ts)

translateBoolToken :: Token -> BoolExpr
translateBoolToken = \case
  T_Literal _ s | s == "true" -> BoolLiteral True
  T_Literal _ s | s == "false" -> BoolLiteral False
  T_Arithmetic _ exprToken ->
    -- Wrap the arithmetic expression in a command and use test.
    BoolCommand (Command "test" [Arg (ArgLiteral ("(math " <> T.unwords (map literalText [exprToken]) <> ") -ne 0"))])
  t ->
    BoolCommand (translateTokenToStatusCmd t)

--------------------------------------------------------------------------------
-- 10. Negate boolean expressions
--------------------------------------------------------------------------------

negateTBool :: FishExpr TBool -> FishExpr TBool
negateTBool = \case
  ExprBool (BoolNot b) -> ExprBool b
  ExprBool b         -> ExprBool (BoolNot b)
  x                  -> ExprBool (BoolNot (BoolCommand (Command "test" [])))

--------------------------------------------------------------------------------
-- 11. Commands that return a status (TStatus)
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
            case isSingleBracketTest c args of
              Just tcmd -> tcmd
              Nothing ->
                Command (literalText c)
                        (assignmentsToArgs assignments ++ map translateArgOrRedirect args)
  T_Pipeline _ bang cmds ->
    translatePipelineToStatus bang cmds
  T_Arithmetic _ exprToken ->
    Command "math" [Arg (argLiteralTextList [exprToken])]
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
      let firstCmd  = translateTokenToStatusCmd c1
          otherCmds = map translateTokenToStatusCmd (c2 : rest)
      in if null bang
           then Pipeline (firstCmd :| otherCmds)
           else Not (Pipeline (firstCmd :| otherCmds))

--------------------------------------------------------------------------------
-- 12. Argument and redirection translation
--------------------------------------------------------------------------------

translateArgOrRedirect :: Token -> FishArgOrRedirect
translateArgOrRedirect = \case
  T_IoFile _ op file ->
    let op' = case op of
                T_Less _     -> RedirectIn
                T_Greater _  -> RedirectOut
                T_DGREAT _   -> RedirectOutAppend
                T_CLOBBER _  -> RedirectOut
                _ -> error ("Unsupported IOFile: " <> show op)
    in Redir op' (argLiteralText file)
  T_IoDuplicate _ op num ->
    let op' = case op of
                T_LESSAND _  -> RedirectIn
                T_GREATAND _ -> RedirectOut
                _ -> error ("Unsupported IoDuplicate: " <> show op)
    in Redir op' (ArgLiteral (T.pack num))
  other ->
    Arg (translateArg other)

translateArg :: Token -> FishArg 'TStr
translateArg = \case
  T_Literal _ s ->
    ArgLiteral (T.pack s)
  T_SingleQuoted _ s ->
    ArgLiteral (T.pack s)
  T_DoubleQuoted _ parts ->
    ArgLiteral (T.concat (map literalText parts))
  T_Arithmetic _ exprToken ->
    ArgSubstituted (SubcommandOutput [Stmt (Command "math" [Arg (argLiteralTextList [exprToken])])])
  t@(T_DollarBraced _ _ _) ->
    translateParamExp t
  T_Backticked _ stmts ->
    case stmts of
      [] -> ArgLiteral "<empty command substitution>"
      (s:_) -> ArgSubstituted (translateTokenToStrCmd (T_Backticked (getId s) stmts))
  T_DollarExpansion _ stmts ->
    ArgSubstituted (translateTokenToStrCmd (T_DollarExpansion (getId (headOrError stmts)) stmts))
  T_DollarBraceCommandExpansion _ stmts ->
    ArgSubstituted (translateTokenToStrCmd (T_DollarBraceCommandExpansion (getId (headOrError stmts)) stmts))
  other ->
    ArgLiteral (literalText other)

headOrError :: [a] -> a
headOrError []    = error "Expected non-empty list"
headOrError (x:_) = x

argLiteralTextList :: [Token] -> FishArg 'TStr
argLiteralTextList ts = ArgLiteral (T.unwords (map literalText ts))

argLiteralText :: Token -> FishArg 'TStr
argLiteralText tok = ArgLiteral (literalText tok)

--------------------------------------------------------------------------------
-- 13. Parameter expansion translation
--------------------------------------------------------------------------------

-- Expanded to support substring extraction and replacement
translateParamExp :: Token -> FishArg 'TStr
translateParamExp token@(T_DollarBraced _ _ expansions) =
  case parseParamExpansion [expansions] of
    Just fishArg -> fishArg
    Nothing ->
      ArgLiteral ("<unsupported param expansion: " <> literalText token <> ">")
translateParamExp t = ArgLiteral (literalText t)

parseParamExpansion :: [Token] -> Maybe (FishArg 'TStr)
parseParamExpansion expansions =
  case expansions of
    [T_Literal _ text] ->
      let text' = T.pack text
      in if T.isPrefixOf "#" text' then
           -- Length expansion: ${#VAR}
           let var = T.drop 1 text'
           in Just (ArgSubstituted (SubcommandOutput [Stmt (Command "string length" [Arg (ArgLiteral ("$" <> var))])]))
         else if T.isInfixOf ":" text' && not (T.isInfixOf ":-" text') then
           -- Substring extraction: ${VAR:offset} or ${VAR:offset:length}
           case T.splitOn ":" text' of
             (var:offset:rest) ->
               let lenArg = case rest of
                              [] -> ""
                              (l:_) -> l
                   args = filter (not . T.null) $ ["--start=" <> offset] <> (if lenArg /= "" then ["--length=" <> lenArg] else [])
               in Just (ArgSubstituted (SubcommandOutput [Stmt (Command "string sub" (map (Arg . ArgLiteral) (args <> ["$" <> var])))]))
             _ -> Nothing
         else if T.isInfixOf "/" text' then
           -- Replacement: ${VAR/pattern/replacement} or ${VAR//pattern/replacement}
           let parts = T.splitOn "/" text'
           in case parts of
                (var:"":pattern:replacement:_) ->
                  -- Replace all occurrences (double slash)
                  Just (ArgSubstituted (SubcommandOutput [Stmt (Command "string replace" [Arg (ArgLiteral "--all"), Arg (ArgLiteral pattern), Arg (ArgLiteral replacement), Arg (ArgLiteral ("$" <> var))])]))
                (var:pattern:replacement:_) ->
                  Just (ArgSubstituted (SubcommandOutput [Stmt (Command "string replace" [Arg (ArgLiteral pattern), Arg (ArgLiteral replacement), Arg (ArgLiteral ("$" <> var))])]))
                _ -> Nothing
         else
           case detectColonDash text' of
             Just (var, def) ->
               Just (ArgSubstituted (SubcommandOutput [Stmt (Command "" [Arg (ArgLiteral (rewriteColonDash var def))])]))
             Nothing ->
               case detectPrefixRemoval text' of
                 Just (var, pat, isLongest) ->
                   Just (ArgSubstituted (SubcommandOutput [Stmt (Command "string replace"
                                             [ Arg (ArgLiteral "-r")
                                             , Arg (ArgLiteral (makePrefixPattern pat isLongest))
                                             , Arg (ArgLiteral "")
                                             , Arg (ArgLiteral ("$" <> var))
                                             ])]))
                 Nothing ->
                   case detectSuffixRemoval text' of
                     Just (var, pat, isLongest) ->
                       Just (ArgSubstituted (SubcommandOutput [Stmt (Command "string replace"
                                             [ Arg (ArgLiteral "-r")
                                             , Arg (ArgLiteral (makeSuffixPattern pat isLongest))
                                             , Arg (ArgLiteral "")
                                             , Arg (ArgLiteral ("$" <> var))
                                             ])]))
                     Nothing ->
                       Just (ArgVariable text')
    _ -> Nothing

detectColonDash :: T.Text -> Maybe (T.Text, T.Text)
detectColonDash txt =
  case T.splitOn ":-" txt of
    [var, def] -> Just (var, def)
    _ -> Nothing

detectPrefixRemoval :: T.Text -> Maybe (T.Text, T.Text, Bool)
detectPrefixRemoval txt =
  case T.breakOn "#" txt of
    (var, rest)
      | not (T.null rest) && not (T.isPrefixOf "#" var) ->
          let (hashes, pattern) = T.span (== '#') rest
          in case T.length hashes of
               1 -> Just (var, pattern, False)
               2 -> Just (var, pattern, True)
               _ -> Nothing
    _ -> Nothing

detectSuffixRemoval :: T.Text -> Maybe (T.Text, T.Text, Bool)
detectSuffixRemoval txt =
  case T.breakOn "%" txt of
    (var, rest)
      | not (T.null rest) && not (T.isPrefixOf "%" var) ->
          let (percents, pattern) = T.span (== '%') rest
          in case T.length percents of
               1 -> Just (var, pattern, False)
               2 -> Just (var, pattern, True)
               _ -> Nothing
    _ -> Nothing

makePrefixPattern :: T.Text -> Bool -> T.Text
makePrefixPattern pat isLongest =
  if isLongest
    then T.concat ["^(", pat, ")(.*)"]
    else T.concat ["^(", pat, ")(.*)"]

makeSuffixPattern :: T.Text -> Bool -> T.Text
makeSuffixPattern pat isLongest =
  if isLongest
    then T.concat ["(.*)(", pat, ")$"]
    else T.concat ["(.*)(", pat, ")$"]

rewriteColonDash :: T.Text -> T.Text -> T.Text
rewriteColonDash var defValue =
  T.concat ["(test -n \"$", var, "\"; and echo \"$", var, "\"; or echo \"", defValue, "\")"]

--------------------------------------------------------------------------------
-- 14. Command substitution returning a string (TStr)
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
-- 15. Case clause translation
--------------------------------------------------------------------------------

translateCase :: (CaseType, [Token], [Token]) -> CaseItem
translateCase (_, patterns, body) =
  CaseItem (map argLiteralText patterns) (map translateToken body)

--------------------------------------------------------------------------------
-- 16. Generic helper: extract a literal text from a token.
--------------------------------------------------------------------------------

literalText :: Token -> T.Text
literalText tok = T.pack (getLiteralStringDef "" tok)