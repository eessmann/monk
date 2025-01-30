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
    T_Script _ _ stmts -> StmtList (map translateToken stmts)
    T_SimpleCommand _ assignments rest ->
      StmtList (assignmentsToSets assignments ++ [translateSimpleCommand rest])
    T_Pipeline id bang cmds -> translatePipeline id bang cmds
    T_IfExpression id conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch id
    T_WhileExpression id cond body ->
      Stmt (While (translateBoolTokens cond) (map translateToken body))
    T_UntilExpression id cond body ->
      Stmt
        ( While
            (negateTBool (translateBoolTokens cond))
            (map translateToken body)
        )
    T_Function _ _ _ funcName body ->
      translateFunction funcName body
    T_BraceGroup _ tokens -> StmtList (map translateToken tokens)
    T_Subshell _ tokens -> StmtList [Stmt (Begin (map translateToken tokens))]
    T_AndIf id l r -> Stmt (JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_OrIf _ l r -> Stmt (JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_Backgrounded _ token -> Stmt (Background (translateTokenToStatusCmd token))
    T_Annotation _ _ token -> translateToken token
    T_ForIn _ var tokens body -> Stmt (For (T.pack var) (ArgList (map argLiteralText tokens)) (map translateToken body))
    T_CaseExpression _ switchExpr cases -> Stmt (Switch (argLiteralText switchExpr) (map translateCase cases))
    _ -> Comment $ "Unimplemented token: " <> showToken token
  where
    cmd = translateTokenToStatusCmd

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
    [] -> Stmt (Command "true" [])
    (c : args) -> Stmt (Command (literalText c) (map translateArgOrRedirect args))

translatePipeline :: Id -> [Token] -> [Token] -> FishStatement
translatePipeline id bang cmds =
  case cmds of
    [] -> Stmt (Command "true" []) -- Empty pipeline
    [cmd] ->
      let translatedCmd = translateTokenToStatusCmd cmd
       in if null bang
            then Stmt translatedCmd
            else Stmt (Not translatedCmd)
    _ ->
      Stmt $ Pipeline (translateTokenToStatusCmd (head cmds) :| map translateTokenToStatusCmd (tail cmds))

--------------------------------------------------------------------------------
-- If Expression Translation
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> Id -> FishStatement
translateIfExpression conditionBranches elseBranch id =
  let translateBranches [] = map translateToken elseBranch
      translateBranches ((condTokens, thenTokens) : rest) =
        [ Stmt
            ( If
                (translateBoolTokens condTokens)
                (map translateToken thenTokens)
                (translateBranches rest)
            )
        ]
   in StmtList (translateBranches conditionBranches)

--------------------------------------------------------------------------------
-- Function Definition Translation
--------------------------------------------------------------------------------

translateFunction :: String -> Token -> FishStatement
translateFunction funcName bodyToken =
  let bodyStmts =
        case bodyToken of
          T_BraceGroup _ stmts -> map translateToken stmts
          _ -> [translateToken bodyToken]
   in Stmt
        ( Function
            FishFunction
              { funcName = T.pack funcName,
                funcFlags = [],
                funcParams = [],
                funcBody = bodyStmts
              }
        )

--------------------------------------------------------------------------------
-- Variable Assignment Translation
--------------------------------------------------------------------------------

assignmentsToSets :: [Token] -> [FishStatement]
assignmentsToSets assignments =
  [ translateAssignment assignment | assignment <- assignments, isAssignment assignment
  ]

translateAssignment :: Token -> FishStatement
translateAssignment tok =
  case tok of
    T_Assignment _ mode var _ val ->
      let scope = ScopeLocal -- Could be refined based on context
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
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> FishExpr TBool
translateBoolTokens tokens =
  case tokens of
    [] -> ExprBool (BoolLiteral True)
    [T_Bang id] -> ExprBool (BoolLiteral False)
    [T_Bang id, t] -> negateTBool (translateBoolToken t)
    [t] -> translateBoolToken t
    (t1 : t2 : rest) ->
      case t2 of
        T_AND_IF id ->
          ExprBool
            ( BoolAnd
                (boolExpr (translateBoolToken t1))
                (boolExpr (translateBoolTokens rest))
            )
        T_OR_IF id ->
          ExprBool
            ( BoolOr
                (boolExpr (translateBoolToken t1))
                (boolExpr (translateBoolTokens rest))
            )
        _ ->
          ExprBool (BoolCommand (translateTokenToStatusCmd (T_Pipeline (getId t1) [] tokens)))
  where
    boolExpr x = x

translateBoolToken :: Token -> FishExpr TBool
translateBoolToken token =
  case token of
    T_Literal _ "true" -> ExprBool (BoolLiteral True)
    T_Literal _ "false" -> ExprBool (BoolLiteral False)
    -- Add more cases as needed
    _ ->
      ExprBool (BoolCommand (translateTokenToStatusCmd token))

--------------------------------------------------------------------------------
-- Negation of Boolean Expressions
--------------------------------------------------------------------------------

negateTBool :: FishExpr TBool -> FishExpr TBool
negateTBool (ExprBool (BoolNot b)) = ExprBool b
negateTBool (ExprBool b) = ExprBool (BoolNot b)
negateTBool x = ExprBool (BoolNot (boolExpr x))
  where
    boolExpr :: FishExpr TBool -> BoolExpr
    boolExpr (ExprBool b) = b
    boolExpr y = BoolCommand (translateTokenToStatusCmd (ExprSubstituted y))

--------------------------------------------------------------------------------
-- Command Translation to TStatus
--------------------------------------------------------------------------------

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest ->
      case rest of
        (c : args) ->
          Command
            (literalText c)
            (assignmentsToArgs assignments ++ map translateArgOrRedirect args)
        [] ->
          Command "true" (assignmentsToArgs assignments)
    T_Pipeline _ bang commands ->
      case commands of
        [] -> Command "true" []
        [cmd] -> if null bang then translateTokenToStatusCmd cmd else Not (translateTokenToStatusCmd cmd)
        cmd : cmds -> Pipeline (translateTokenToStatusCmd (cmd) :| map translateTokenToStatusCmd (cmds))
    T_AndIf l r id -> JobControl ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)
    T_OrIf l r id -> JobControl ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)
    -- Add more command types as needed
    _ -> Command (literalText token) []

--------------------------------------------------------------------------------
-- Assignments to Args
--------------------------------------------------------------------------------

assignmentsToArgs :: [Token] -> [FishArgOrRedirect]
assignmentsToArgs assignments =
  [ Arg (argLiteralText assignment) | assignment <- assignments, isAssignment assignment
  ]

--------------------------------------------------------------------------------
-- Argument or Redirect Translation
--------------------------------------------------------------------------------

translateArgOrRedirect :: Token -> FishArgOrRedirect
translateArgOrRedirect tok =
  case tok of
    T_IoFile id op file ->
      let op' = case op of
            T_Less _ -> RedirectIn
            T_Greater _ -> RedirectOut
            T_DGREAT _ -> RedirectOutAppend
            T_CLOBBER _ -> RedirectOut
            _ -> error $ "Unsupported IOFile operation: " <> show op
          file' = argLiteralText file
       in Redir op' file'
    T_IoDuplicate id op num ->
      let op' = case op of
            T_LESSAND _ -> RedirectIn
            T_GREATAND _ -> RedirectOut
            _ -> error $ "Unsupported IoDuplicate operation: " <> show op
          num' = ArgLiteral (literalText num)
       in Redir op' num'
    -- Add more redirection cases as needed
    _ -> Arg (translateArg tok)

--------------------------------------------------------------------------------
-- Argument Translation
--------------------------------------------------------------------------------

translateArg :: Token -> FishArg a
translateArg tok =
  case tok of
    T_Literal _ str -> ArgLiteral (literalText tok)
    T_SingleQuoted _ str -> ArgLiteral (T.pack str)
    T_DoubleQuoted _ strs -> ArgLiteral (T.concat (map literalText strs))
    T_DollarBraced _ False var -> ArgVariable (literalText var)
    T_Backticked _ tokens -> ArgSubstituted (translateTokenToStatusCmd (T_Backticked (getId tok) tokens))
    T_DollarExpansion _ tokens -> ArgSubstituted (translateTokenToStatusCmd (T_DollarExpansion (getId tok) tokens))
    T_DollarBraceCommandExpansion _ tokens -> ArgSubstituted (translateTokenToStatusCmd (T_DollarBraceCommandExpansion (getId tok) tokens))
    -- Add more cases as needed
    _ -> ArgLiteral (literalText tok)

--------------------------------------------------------------------------------
-- Case Translation
--------------------------------------------------------------------------------

translateCase :: (CaseType, [Token], [Token]) -> CaseItem
translateCase (_, patterns, body) =
  CaseItem (map argLiteralText patterns) (map translateToken body)

--------------------------------------------------------------------------------
-- Helper: get the literal Text from a Token
--------------------------------------------------------------------------------

literalText :: Token -> Text
literalText tok = T.pack (getLiteralStringDef "" tok)

argLiteralText :: Token -> FishArg TStr
argLiteralText tok = ArgLiteral (literalText tok)