-- | A closed arithmetic operation tree. No expression text reaches the runtime.
module Language.Bash.Arithmetic.Plan
  ( ArithmeticExpr (..),
    ArithmeticOrigin (..),
    UnaryOperator (..),
    BinaryOperator (..),
    UpdatePosition (..),
    UpdateDirection (..),
    normalizeArithmetic,
    constantValue,
    unaryName,
    binaryName,
  )
where

import Control.Monad (foldM)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Monk.Runtime.Integer qualified as Integer
import ShellCheck.AST

data ArithmeticExpr
  = ArithmeticLocated ArithmeticOrigin ArithmeticExpr
  | ArithmeticLiteral Integer
  | ArithmeticVariable Text
  | ArithmeticUnary UnaryOperator ArithmeticExpr
  | ArithmeticBinary BinaryOperator ArithmeticExpr ArithmeticExpr
  | ArithmeticAssign Text (Maybe BinaryOperator) ArithmeticExpr
  | ArithmeticUpdate UpdatePosition UpdateDirection Text
  | ArithmeticConditional ArithmeticExpr ArithmeticExpr ArithmeticExpr
  | ArithmeticSequence (NonEmpty ArithmeticExpr)
  deriving stock (Eq, Show)

-- | Original parser identity, retained without carrying executable syntax.
newtype ArithmeticOrigin = MkArithmeticOrigin Int
  deriving stock (Eq, Ord, Show)

data UnaryOperator = UnaryPlus | UnaryMinus | LogicalNot | BitwiseNot
  deriving stock (Eq, Show)

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | Power
  | ShiftLeft
  | ShiftRight
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  | BitAnd
  | BitXor
  | BitOr
  | LogicalAnd
  | LogicalOr
  deriving stock (Eq, Show)

data UpdatePosition = PrefixUpdate | PostfixUpdate
  deriving stock (Eq, Show)

data UpdateDirection = Increment | Decrement
  deriving stock (Eq, Show)

normalizeArithmetic :: Token -> Either Text ArithmeticExpr
normalizeArithmetic token =
  let Id tokenIdentity = getId token
   in ArithmeticLocated (MkArithmeticOrigin tokenIdentity) <$> normalizeNode token

normalizeNode :: Token -> Either Text ArithmeticExpr
normalizeNode = \case
  T_Arithmetic _ inner -> normalizeArithmetic inner
  T_DollarArithmetic _ inner -> normalizeArithmetic inner
  T_DollarBracket _ inner -> normalizeArithmetic inner
  TA_Parenthesis _ inner -> normalizeArithmetic inner
  TA_Sequence _ values -> case NE.nonEmpty values of
    Nothing -> pure (ArithmeticLiteral 0)
    Just (single NE.:| []) -> normalizeArithmetic single
    Just values' -> ArithmeticSequence <$> traverse normalizeArithmetic values'
  TA_Expansion _ [single] -> normalizeArithmetic single
  TA_Variable _ name [] -> ArithmeticVariable <$> variableName (toText name)
  T_DollarBraced _ _ (T_NormalWord _ [T_Literal _ name]) -> ArithmeticVariable <$> variableName (toText name)
  T_DollarBraced _ _ (T_Literal _ name) -> ArithmeticVariable <$> variableName (toText name)
  T_DollarBraced _ _ (T_NormalWord _ [T_ParamSubSpecialChar _ "#"]) -> pure (ArithmeticVariable "#")
  T_ParamSubSpecialChar _ "#" -> pure (ArithmeticVariable "#")
  T_Literal _ spelling -> ArithmeticLiteral <$> integerLiteral (toText spelling)
  TA_Unary _ spelling inner ->
    let operation = T.filter (/= '|') (toText spelling)
        position = if T.isPrefixOf "|" (toText spelling) then PostfixUpdate else PrefixUpdate
     in case operation of
          "++" -> ArithmeticUpdate position Increment <$> lvalue inner
          "--" -> ArithmeticUpdate position Decrement <$> lvalue inner
          _ -> ArithmeticUnary <$> unaryOperator operation <*> normalizeArithmetic inner
  TA_Binary _ "," left right -> do
    leftValue <- normalizeArithmetic left
    rightValue <- normalizeArithmetic right
    pure (ArithmeticSequence (leftValue NE.:| [rightValue]))
  TA_Binary _ spelling left right -> ArithmeticBinary <$> binaryOperator (toText spelling) <*> normalizeArithmetic left <*> normalizeArithmetic right
  TA_Trinary _ condition yes no -> ArithmeticConditional <$> normalizeArithmetic condition <*> normalizeArithmetic yes <*> normalizeArithmetic no
  TA_Assignment _ spelling target right -> do
    name <- lvalue target
    operation <- if spelling == "=" then pure Nothing else Just <$> maybe (Left "Unsupported arithmetic assignment operator") binaryOperator (T.stripSuffix "=" (toText spelling))
    ArithmeticAssign name operation <$> normalizeArithmetic right
  _ -> Left "Arithmetic requires a parsed integer operation and scalar operands; array and runtime-expression forms are unsupported"

lvalue :: Token -> Either Text Text
lvalue = \case
  TA_Variable _ name [] -> variableName (toText name)
  TA_Parenthesis _ inner -> lvalue inner
  _ -> Left "Arithmetic assignment requires a scalar variable"

variableName :: Text -> Either Text Text
variableName value =
  case T.uncons value of
    Just (firstCharacter, rest) | initial firstCharacter && T.all subsequent rest -> pure value
    _ -> Left "Arithmetic variable is not an ordinary scalar name"
  where
    initial character = character == '_' || isAsciiLower character || isAsciiUpper character
    subsequent character = initial character || isDigit character

unaryOperator :: Text -> Either Text UnaryOperator
unaryOperator = \case
  "+" -> pure UnaryPlus
  "-" -> pure UnaryMinus
  "!" -> pure LogicalNot
  "~" -> pure BitwiseNot
  _ -> Left "Unsupported arithmetic unary operator"

binaryOperator :: Text -> Either Text BinaryOperator
binaryOperator operation =
  maybe (Left "Unsupported arithmetic binary operator") Right $
    List.lookup
      operation
      [ ("+", Add),
        ("-", Subtract),
        ("*", Multiply),
        ("/", Divide),
        ("%", Remainder),
        ("**", Power),
        ("<<", ShiftLeft),
        (">>", ShiftRight),
        ("<", LessThan),
        ("<=", LessEqual),
        (">", GreaterThan),
        (">=", GreaterEqual),
        ("==", Equal),
        ("!=", NotEqual),
        ("&", BitAnd),
        ("^", BitXor),
        ("|", BitOr),
        ("&&", LogicalAnd),
        ("||", LogicalOr)
      ]

integerLiteral :: Text -> Either Text Integer
integerLiteral spelling = do
  (base, digits) <- case T.breakOn "#" spelling of
    (baseText, suffix) | not (T.null suffix) -> do
      when (T.null baseText || T.length baseText > 2 || T.isPrefixOf "0" baseText || not (T.all isDigit baseText)) (Left invalid)
      base <- maybe (Left invalid) Right (readMaybe (toString baseText) :: Maybe Integer)
      unless (base >= 2 && base <= 64) (Left invalid)
      pure (base, T.drop 1 suffix)
    _ | Just digits <- T.stripPrefix "0x" spelling <|> T.stripPrefix "0X" spelling -> pure (16, digits)
    _ | T.length spelling > 1 && T.isPrefixOf "0" spelling -> pure (8, T.drop 1 spelling)
    _ -> pure (10, spelling)
  when (T.null digits) (Left invalid)
  unsigned <- foldM (digit base) 0 (T.unpack digits)
  pure (if unsigned >= 2 ^ (63 :: Int) then unsigned - modulus else unsigned)
  where
    modulus = 2 ^ (64 :: Int)
    invalid = "Invalid Bash integer literal: " <> spelling
    digit base value character = do
      let ordinal
            | isDigit character = ord character - ord '0'
            | isAsciiLower character = ord character - ord 'a' + 10
            | isAsciiUpper character = ord character - ord 'A' + if base <= 36 then 10 else 36
            | character == '@' = 62
            | character == '_' = 63
            | otherwise = 64
      unless (toInteger ordinal < base) (Left invalid)
      pure ((value * base + toInteger ordinal) `mod` modulus)

unaryName :: UnaryOperator -> Text
unaryName = \case
  UnaryPlus -> "pos"
  UnaryMinus -> "neg"
  LogicalNot -> "not"
  BitwiseNot -> "invert"

binaryName :: BinaryOperator -> Text
binaryName = \case
  Add -> "add"
  Subtract -> "sub"
  Multiply -> "mul"
  Divide -> "div"
  Remainder -> "rem"
  Power -> "pow"
  ShiftLeft -> "shl"
  ShiftRight -> "shr"
  LessThan -> "lt"
  LessEqual -> "le"
  GreaterThan -> "gt"
  GreaterEqual -> "ge"
  Equal -> "eq"
  NotEqual -> "ne"
  BitAnd -> "and"
  BitXor -> "xor"
  BitOr -> "or"
  LogicalAnd -> "logical-and"
  LogicalOr -> "logical-or"

constantValue :: ArithmeticExpr -> Maybe Integer
constantValue = \case
  ArithmeticLocated _ inner -> constantValue inner
  ArithmeticLiteral value -> Just (Integer.wrap value)
  ArithmeticUnary operator inner -> constantValue inner >>= \value -> primitive (unaryName operator) [value]
  ArithmeticBinary LogicalAnd left right -> do
    value <- constantValue left
    if value == 0 then Just 0 else boolInteger . (/= 0) <$> constantValue right
  ArithmeticBinary LogicalOr left right -> do
    value <- constantValue left
    if value /= 0 then Just 1 else boolInteger . (/= 0) <$> constantValue right
  ArithmeticBinary operator left right -> do
    a <- constantValue left
    b <- constantValue right
    primitive (binaryName operator) [a, b]
  ArithmeticConditional predicate yes no -> do
    value <- constantValue predicate
    constantValue (if value /= 0 then yes else no)
  ArithmeticSequence values -> NE.last <$> traverse constantValue values
  _ -> Nothing
  where
    primitive :: Text -> [Integer] -> Maybe Integer
    primitive operation values = rightToMaybe (Integer.integerValue (encodeUtf8 operation) (map (encodeUtf8 . (show :: Integer -> Text)) values))
    boolInteger value = if value then 1 else 0
