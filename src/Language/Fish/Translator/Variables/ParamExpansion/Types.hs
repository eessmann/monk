{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Fish.Translator.Variables.ParamExpansion.Types
  ( ParamExpansion (..),
    ParamCore (..),
    ParamSimple (..),
    ParamOpKind (..),
    ParamOpCond (..),
    ParamOperator (..),
    ParamModifier (..),
    CaseMod (..),
    PatternAnchor (..),
  )
where

import Language.Fish.AST
import ShellCheck.AST (Token)

data ParamExpansion (t :: FishType) where
  ParamExpansionList :: ParamCore -> ParamExpansion (TList TStr)
  ParamExpansionStr :: ParamCore -> ParamExpansion TStr

data ParamCore
  = ParamCoreSimple ParamSimple
  | ParamCoreOperator Text ParamOperator
  | ParamCoreModifier Text ParamModifier

data ParamSimple = MkParamSimple
  { simpleName :: Maybe Text,
    simpleIndex :: Maybe (FishIndex TStr (TList TStr))
  }

data ParamOpKind = OpDefault | OpAssign | OpError | OpAlt

data ParamOpCond = CondSet | CondNonEmpty

data ParamOperator = MkParamOperator ParamOpKind ParamOpCond [Token]

data ParamModifier
  = ModAltSelf
  | ModLength Text
  | ModSubstring Text (Maybe Text)
  | ModPatternRemoval Bool Bool Text
  | ModPatternReplacement Bool Text Text PatternAnchor
  | ModCase CaseMod

data CaseMod = CaseUpper | CaseLower

data PatternAnchor = AnchorStart | AnchorEnd | AnchorNone
