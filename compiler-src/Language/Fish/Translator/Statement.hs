{-# LANGUAGE DataKinds #-}

-- | Shared structural statement construction. These helpers never render or
-- interpret syntax and do not own semantic state.
module Language.Fish.Translator.Statement
  ( arg,
    scalarVar,
    builtin,
    external,
    assign,
    assignList,
    bodyNE,
    jobOf,
    condition,
    asCommand,
    ifStatements,
    testEquals,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Identifier (compilerCommandName)

arg :: (Typeable t) => FishExpr t -> ExprOrRedirect
arg = ExprVal

scalarVar :: Identifier -> FishExpr TStr
scalarVar = ExprQuotedVariable . VarScalar

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name arguments = Stmt (Decorated DecBuiltin (Command (compilerCommandName name) arguments))

external :: Text -> [ExprOrRedirect] -> FishStatement
external name arguments = Stmt (Decorated DecCommand (Command (compilerCommandName name) arguments))

assign :: [SetFlag] -> Identifier -> FishExpr TStr -> FishStatement
assign flags name value = Stmt (Decorated DecBuiltin (Set flags name (ExprListLiteral [value])))

assignList :: [SetFlag] -> Identifier -> FishExpr (TList TStr) -> FishStatement
assignList flags name value = Stmt (Decorated DecBuiltin (Set flags name value))

bodyNE :: [FishStatement] -> NonEmpty FishStatement
bodyNE = fromMaybe (builtin "true" [] :| []) . NE.nonEmpty

jobOf :: FishStatement -> FishJobPipeline
jobOf statement = MkFishJobPipeline False [] statement []

condition :: [FishStatement] -> FishJobList
condition statements = MkFishJobList (MkFishJobConjunction Nothing (jobOf (asCommand statements)) [] :| [])

asCommand :: [FishStatement] -> FishStatement
asCommand [statement] = statement
asCommand statements = Stmt (Begin (bodyNE statements) [])

ifStatements :: [FishStatement] -> [FishStatement] -> [FishStatement] -> FishStatement
ifStatements predicate yes no = Stmt (If (condition predicate) (bodyNE yes) no [])

testEquals :: FishExpr TStr -> Text -> FishStatement
testEquals value expected = builtin "test" [arg value, arg (ExprLiteral "="), arg (ExprLiteral expected)]
