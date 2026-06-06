{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Fish.Translator.Rename
  ( renameStatementVariable,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.Translator.Syntax

renameStatementVariable :: Text -> Text -> FishStatement -> FishStatement
renameStatementVariable old new = renameStmt
  where
    renameText txt
      | txt == old = new
      | otherwise = txt

    renameStmt = \case
      Stmt cmd -> Stmt (renameCommand cmd)
      StmtList stmts -> StmtList (map renameStmt stmts)
      Comment txt -> Comment txt
      EmptyStmt -> EmptyStmt

    renameCommand :: FishCommand t -> FishCommand t
    renameCommand = \case
      Command name args -> Command name (map renameExprOrRedirect args)
      Set flags var expr -> Set flags (renameText var) (renameExpr expr)
      Function fishFn ->
        Function
          fishFn
            { funcBody = fmap renameStmt (funcBody fishFn)
            }
      For var listExpr body suffix ->
        For (renameText var) (renameExpr listExpr) (fmap renameStmt body) (map renameExprOrRedirect suffix)
      While cond body suffix ->
        While (renameJobList cond) (fmap renameStmt body) (map renameExprOrRedirect suffix)
      Begin body suffix ->
        Begin (fmap renameStmt body) (map renameExprOrRedirect suffix)
      If cond thn els suffix ->
        If (renameJobList cond) (fmap renameStmt thn) (map renameStmt els) (map renameExprOrRedirect suffix)
      Switch expr cases suffix ->
        Switch (renameExpr expr) (fmap renameCaseItem cases) (map renameExprOrRedirect suffix)
      Break -> Break
      Continue -> Continue
      Return mexpr -> Return (fmap renameExpr mexpr)
      Exit mexpr -> Exit (fmap renameExpr mexpr)
      Source expr -> Source (renameExpr expr)
      Eval expr -> Eval (renameExpr expr)
      Read flags vars -> Read flags (map renameText vars)
      Echo args -> Echo (fmap renameExpr args)
      Printf fmt args -> Printf (renameExpr fmt) (map renameExpr args)
      Pipeline pipe -> Pipeline (renamePipeline pipe)
      JobConj conj -> JobConj (renameConjunction conj)
      Semicolon left right -> Semicolon (renameCommand left) (renameCommand right)
      Not cmd -> Not (renameCommand cmd)
      Background cmd -> Background (renameCommand cmd)
      Wait mexpr -> Wait (fmap renameExpr mexpr)
      Exec cmd args -> Exec (renameExpr cmd) (map renameExprOrRedirect args)
      Decorated dec cmd -> Decorated dec (renameCommand cmd)

    renameExpr :: forall t. FishExpr t -> FishExpr t
    renameExpr = \case
      ExprLiteral txt -> ExprLiteral txt
      ExprNumLiteral i -> ExprNumLiteral i
      ExprVariable varRef -> ExprVariable (renameVarRef varRef)
      ExprSpecialVar special -> ExprSpecialVar special
      ExprStringConcat left right -> ExprStringConcat (renameExpr left) (renameExpr right)
      ExprStringOp op expr -> ExprStringOp op (renameExpr expr)
      ExprJoinList expr -> ExprJoinList (renameExpr expr)
      ExprMath args -> ExprMath (fmap renameExpr args)
      ExprCommandSubst stmts -> ExprCommandSubst (fmap renameStmt stmts)
      ExprListLiteral exprs -> ExprListLiteral (map renameExpr exprs)
      ExprListConcat left right -> ExprListConcat (renameExpr left) (renameExpr right)
      ExprGlob glob -> ExprGlob glob
      ExprProcessSubst stmts -> ExprProcessSubst (fmap renameStmt stmts)

    renameVarRef :: FishVarRef t -> FishVarRef t
    renameVarRef = \case
      VarAll name -> VarAll (renameText name)
      VarScalar name -> VarScalar (renameText name)
      VarIndex name idx -> VarIndex (renameText name) (renameIndex idx)

    renameIndex :: FishIndex a b -> FishIndex a b
    renameIndex = \case
      IndexSingle expr -> IndexSingle (renameExpr expr)
      IndexRange start end -> IndexRange (fmap renameExpr start) (fmap renameExpr end)
      IndexList exprs -> IndexList (fmap renameExpr exprs)

    renameExprOrRedirect :: ExprOrRedirect -> ExprOrRedirect
    renameExprOrRedirect = \case
      ExprVal expr -> ExprVal (renameExpr expr)
      RedirectVal redir -> RedirectVal (renameRedirect redir)

    renameRedirect :: Redirect -> Redirect
    renameRedirect redir =
      redir
        { redirTarget = renameRedirectTarget (redirTarget redir)
        }

    renameRedirectTarget = \case
      RedirectFile expr -> RedirectFile (renameExpr expr)
      RedirectTargetFD fd -> RedirectTargetFD fd
      RedirectClose -> RedirectClose

    renamePipeline MkFishJobPipeline {jpTime, jpVariables, jpStatement, jpCont, jpBackgrounded} =
      MkFishJobPipeline
        { jpTime,
          jpVariables = map renameVariableAssignment jpVariables,
          jpStatement = renameStmt jpStatement,
          jpCont = map renamePipeCont jpCont,
          jpBackgrounded
        }

    renamePipeCont PipeTo {jpcVariables, jpcStatement} =
      PipeTo
        { jpcVariables = map renameVariableAssignment jpcVariables,
          jpcStatement = renameStmt jpcStatement
        }

    renameVariableAssignment MkVariableAssignment {vaName, vaValue} =
      MkVariableAssignment
        { vaName = renameText vaName,
          vaValue = fmap renameExpr vaValue
        }

    renameConjunction MkFishJobConjunction {jcDecorator, jcJob, jcContinuations} =
      MkFishJobConjunction
        { jcDecorator,
          jcJob = renamePipeline jcJob,
          jcContinuations = map renameConjunctionCont jcContinuations
        }

    renameConjunctionCont = \case
      JCAnd pipe -> JCAnd (renamePipeline pipe)
      JCOr pipe -> JCOr (renamePipeline pipe)

    renameJobList (MkFishJobList jobs) =
      MkFishJobList (fmap renameConjunction jobs)

    renameCaseItem MkCaseItem {casePatterns, caseBody} =
      MkCaseItem
        { casePatterns = NE.map renameExpr casePatterns,
          caseBody = fmap renameStmt caseBody
        }
