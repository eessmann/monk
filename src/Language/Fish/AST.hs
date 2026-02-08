module Language.Fish.AST
  ( -- * Core types
    FishType (..),
    FishStatement (..),
    FishCommand (..),
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
    JobPipeCont (..),
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
    RedirectOp (..),
    RedirectSource (..),
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

import Language.Fish.AST.Example (exampleAST)
import Language.Fish.AST.Types
