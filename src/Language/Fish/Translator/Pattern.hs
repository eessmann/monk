{-# LANGUAGE DataKinds #-}

-- | Quote-aware byte patterns. The normalized envelope permits active @*@ and
-- @?@ only; literal fragments retain all bytes. Input travels through a framed
-- pipe rather than process argv, so large scalar values do not hit ARG_MAX.
module Language.Fish.Translator.Pattern
  ( matchPattern,
    expandPathname,
    requiresRuntime,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.DSL.Internal

matchPattern :: Text -> FishExpr TStr -> [(Bool, FishExpr TStr)] -> FishCommand TStatus
matchPattern helper subject parts
  | not (requiresRuntime parts) = Decorated DecBuiltin (Command "test" [ExprVal subject, ExprVal (ExprLiteral "="), ExprVal (foldl' ExprStringConcat (ExprLiteral "") (map snd parts))])
  | otherwise = framedPrimitive helper "pattern" (ExprLiteral "match" : subject : frames parts) []

requiresRuntime :: [(Bool, FishExpr TStr)] -> Bool
requiresRuntime = any (\case (False, _) -> False; (True, ExprLiteral value) -> T.any (`elem` ['*', '?']) value; _ -> True)

expandPathname :: Text -> [(Bool, FishExpr TStr)] -> FishExpr (TList TStr)
expandPathname helper parts =
  ExprCommandSubst
    ( Stmt
        (framedPrimitive helper "glob" (frames parts) [PipeTo [] (Stmt (Decorated DecBuiltin (Command "string" [ExprVal (ExprLiteral "split0")])))])
        NE.:| []
    )

frames :: [(Bool, FishExpr TStr)] -> [FishExpr TStr]
frames = concatMap (\(active, value) -> [ExprLiteral (if active then "1" else "0"), value])

framedPrimitive :: Text -> Text -> [FishExpr TStr] -> [JobPipeCont] -> FishCommand TStatus
framedPrimitive helper program inputs continuations =
  Pipeline
    ( MkFishJobPipeline
        False
        []
        (Stmt (Decorated DecBuiltin (Command "printf" (map ExprVal (ExprLiteral "%s\\0" : inputs)))))
        (PipeTo [] (Stmt (Command helper (map (ExprVal . ExprLiteral) ["--abi", "1", program]))) : continuations)
        False
    )
