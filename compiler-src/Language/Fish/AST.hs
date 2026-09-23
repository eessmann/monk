{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 914
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}
#endif
{-# LANGUAGE PatternSynonyms #-}

module Language.Fish.AST
  ( -- * Core types
    FishType (..),
    FishStatement (..),
    FishCommand (..),
    CommandGrammar (..),
    CommandName,
    commandNameText,
    Executable,
    literalExecutable,
    variableExecutable,
    foldExecutable,
    stageToStatement,
    ExprOrRedirect (..),
    FishExpr (..),
    FishVarRef (..),
    FishIndex (..),

    -- * Special variables and helpers
    SpecialVarRef (..),
    GlobPattern (..),
    GlobPart (..),
    StringOp (..),
    ReadFlag (..),
    SetFlag (..),

    -- * Job model
    VariableAssignment (..),
    FishJobPipeline (..),
    pattern MkFishJobPipeline,
    JobPipeCont (..),
    pattern PipeTo,
    pattern PipeErrorTo,
    Conjunction (..),
    FishJobConjCont (..),
    FishJobConjunction (..),
    FishJobList (..),

    -- * Type synonyms
    CmdStr,
    CmdInt,
    CmdBool,
    CmdList,
    CmdStatus,
    CmdUnit,
    ExprStr,
    ExprInt,
    ExprBool,
    ExprList,
    ExprStatus,
    ExprUnit,

    -- * Operators and flags
    Redirect (..),
    RedirectDirection (..),
    FileRedirectMode (..),
    OutputRedirectMode (..),
    RedirectTarget (..),
    Decoration (..),
    CaseItem (..),

    -- * Functions
    FishFunction (..),
    FunctionFlag (..),

    -- * Source tracking
    SourcePos (..),
    SourceRange (..),

    -- * Equality helpers
    eqGADT,
    eqFishExprSameType,

    -- * Example
    exampleAST,
  )
where

import Language.Fish.AST.Common
import Language.Fish.AST.Example (exampleAST)
import Language.Fish.AST.Types
