{-# LANGUAGE DataKinds #-}

-- | Quote-aware byte patterns. The normalized envelope permits active @*@ and
-- @?@ only; literal fragments retain all bytes. Input travels through a framed
-- pipe rather than process argv, so large scalar values do not hit ARG_MAX.
module Language.Fish.Translator.Pattern
  ( matchPattern,
    expandPathname,
    requiresRuntime,
    sourceRequiresRuntime,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Primitive qualified as Primitive

matchPattern :: Text -> FishExpr TStr -> [(Bool, FishExpr TStr)] -> FishStatement
matchPattern helper subject parts
  | not (requiresRuntime parts) = Stmt (Decorated DecBuiltin (Command "test" [ExprVal subject, ExprVal (ExprLiteral "="), ExprVal (foldl' ExprStringConcat (ExprLiteral "") (map snd parts))]))
  | otherwise = Primitive.primitiveStatement helper (Primitive.MatchPattern subject parts)

requiresRuntime :: [(Bool, FishExpr TStr)] -> Bool
requiresRuntime = any (\case (False, _) -> False; (True, ExprLiteral value) -> T.any (`elem` ['*', '?']) value; _ -> True)

expandPathname :: Text -> [(Bool, FishExpr TStr)] -> FishExpr (TList TStr)
expandPathname helper parts =
  ExprCommandSubst
    ( Stmt
        (Primitive.primitiveCommand helper (Primitive.ExpandPathname parts) [PipeTo [] (Stmt (Decorated DecBuiltin (Command "string" [ExprVal (ExprLiteral "split0")])))])
        NE.:| []
    )

-- The normalized pattern already records which fragments are active; choosing
-- its capability before emission does not inspect or detach generated values.
sourceRequiresRuntime :: P.Pattern -> Bool
sourceRequiresRuntime (P.MkPattern parts) = any active parts
  where
    active (P.LiteralPattern _) = False
    active (P.ActivePattern (P.Literal value)) = T.any (`elem` ['*', '?']) value
    active (P.ActivePattern _) = True
