{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Complete framed requests for bounded byte and field primitives. Every
-- required operand is scalar; only the argv tail may contribute many fields.
module Language.Fish.Translator.Primitive
  ( Primitive (..),
    ExpansionMode (..),
    IntegerProgram (..),
    primitiveFields,
    primitiveOpcode,
    primitiveCommand,
    primitiveStatement,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan (BinaryOperator, UnaryOperator, binaryOpcode, unaryOpcode)
import Language.Bash.Plan.Operator (PatternTrim (..), Replacement (..))
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Identifier (compilerCommandName)
import Monk.Runtime.Abi2
import Numeric (showHex)

data ExpansionMode = QuotedExpansion | LiteralExpansion | SplitExpansion

-- A postfix program is constructed as an expression tree, so the encoder
-- cannot emit a stack-underflowing sequence or a mismatched operation arity.
data IntegerProgram
  = PushInteger (FishExpr TStr)
  | ApplyUnary UnaryOperator IntegerProgram
  | ApplyBinary BinaryOperator IntegerProgram IntegerProgram

type role Primitive nominal

data Primitive (operation :: CliOpcode) where
  SplitFields :: FishExpr TStr -> FishExpr TStr -> Primitive CliSplit
  QuotedArguments :: FishExpr TStr -> FishExpr TStr -> Bool -> FishExpr (TList TStr) -> Primitive CliArgv
  PlatformBytes :: ByteString -> ByteString -> Primitive CliBytesPlatform
  ExpandFields :: FishExpr TStr -> [(ExpansionMode, FishExpr TStr)] -> Primitive CliExpansion
  MatchPattern :: FishExpr TStr -> [(Bool, FishExpr TStr)] -> Primitive CliPattern
  ReplacePattern :: Replacement -> FishExpr TStr -> Text -> Text -> Primitive CliPattern
  TrimPattern :: PatternTrim -> FishExpr TStr -> [(Bool, FishExpr TStr)] -> Primitive CliPatternParts
  ExpandPathname :: [(Bool, FishExpr TStr)] -> Primitive CliGlob
  ReadInteger :: FishExpr TStr -> Primitive CliInteger
  UnaryInteger :: UnaryOperator -> FishExpr TStr -> Primitive CliInteger
  BinaryInteger :: BinaryOperator -> FishExpr TStr -> FishExpr TStr -> Primitive CliInteger
  BatchInteger :: IntegerProgram -> Primitive CliInteger

primitiveOpcode :: Primitive operation -> CliOpcode
primitiveOpcode = \case
  SplitFields {} -> CliSplit
  QuotedArguments {} -> CliArgv
  PlatformBytes {} -> CliBytesPlatform
  ExpandFields {} -> CliExpansion
  MatchPattern {} -> CliPattern
  ReplacePattern {} -> CliPattern
  TrimPattern {} -> CliPatternParts
  ExpandPathname {} -> CliGlob
  ReadInteger {} -> CliInteger
  UnaryInteger {} -> CliInteger
  BinaryInteger {} -> CliInteger
  BatchInteger {} -> CliInteger

primitiveFields :: Primitive operation -> [SomeArgument]
primitiveFields = \case
  SplitFields ifs value -> map scalar [ifs, value]
  QuotedArguments prefix suffix forceField values -> [scalar prefix, scalar suffix, literal (if forceField then "1" else "0"), SomeArgument (ListArgument values)]
  PlatformBytes darwin linux -> map (literal . hexBytes) [darwin, linux]
  ExpandFields ifs parts -> scalar ifs : concatMap expansionFields parts
  MatchPattern subject parts -> literal (toText (patternOpcodeName PatternMatch)) : scalar subject : patternFields parts
  ReplacePattern operation subject patternValue replacement ->
    [literal (toText (patternOpcodeName (case operation of ReplaceFirst -> PatternReplaceFirst; ReplaceAll -> PatternReplaceAll))), scalar subject, literal patternValue, literal replacement]
  TrimPattern operation subject parts ->
    literal (toText (patternOpcodeName (case operation of PrefixShort -> PatternTrimPrefixShort; PrefixLong -> PatternTrimPrefixLong; SuffixShort -> PatternTrimSuffixShort; SuffixLong -> PatternTrimSuffixLong))) : scalar subject : patternFields parts
  ExpandPathname parts -> patternFields parts
  ReadInteger value -> [integerOperation IntegerRead, scalar value]
  UnaryInteger operation value -> [integerOperation (unaryOpcode operation), scalar value]
  BinaryInteger operation left right -> [integerOperation (binaryOpcode operation), scalar left, scalar right]
  BatchInteger program -> integerOperation IntegerBatch : programFields program
  where
    integerOperation = literal . toText . integerOpcodeName
    programFields = \case
      PushInteger value -> [integerOperation IntegerPush, scalar value]
      ApplyUnary operation inner -> programFields inner <> [integerOperation (unaryOpcode operation)]
      ApplyBinary operation left right -> programFields left <> programFields right <> [integerOperation (binaryOpcode operation)]
    expansionFields (mode, value) = [literal (case mode of QuotedExpansion -> "q"; LiteralExpansion -> "l"; SplitExpansion -> "e"), scalar value]
    patternFields = concatMap (\(active, value) -> [literal (if active then "1" else "0"), scalar value])
    hexBytes = T.concat . map (\byte -> let digits = toText (showHex byte "") in T.justifyRight 2 '0' digits) . BS.unpack
    scalar = SomeArgument . ScalarArgument
    literal = scalar . ExprLiteral

primitiveCommand :: Text -> Primitive operation -> [JobPipeCont] -> FishCommand Compound TStatus
primitiveCommand helper request continuations =
  Pipeline
    ( MkFishJobPipeline
        False
        []
        (Stmt (Decorated DecBuiltin (Command "printf" (ExprVal (ExprLiteral "%s\\0") : map argumentExpression (primitiveFields request)))))
        (PipeTo [] (Stmt (Command (compilerCommandName helper) (map (ExprVal . ExprLiteral) ["--abi", "2", toText (cliOpcodeName (primitiveOpcode request))]))) : continuations)
    )

primitiveStatement :: Text -> Primitive operation -> FishStatement
primitiveStatement helper request = Stmt (primitiveCommand helper request [])
