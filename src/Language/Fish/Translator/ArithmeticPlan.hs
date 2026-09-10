{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Materialize an integer operation tree through bounded native primitives.
-- Evaluation order and lazy branches remain structural Fish statements.
module Language.Fish.Translator.ArithmeticPlan
  ( ArithmeticMaterialization (..),
    materializeArithmetic,
  )
where

import Control.Monad.State.Strict qualified as State
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan
import Language.Fish.DSL.Internal
import Monk.Runtime.Integer qualified as Integer
import Monk.Translation.Types (NativeOperation (NativeInteger), RuntimeRequirement, nativeRuntimeRequirement)

data ArithmeticMaterialization = MkArithmeticMaterialization
  { arithmeticStatements :: [FishStatement],
    arithmeticValue :: FishExpr TStr,
    arithmeticStatus :: FishExpr TStr,
    arithmeticError :: FishExpr TStr,
    arithmeticErrorOrigin :: FishExpr TStr,
    arithmeticHelpers :: [FishStatement],
    arithmeticRequirements :: [RuntimeRequirement]
  }
  deriving stock (Eq, Show)

materializeArithmetic ::
  Text ->
  Text ->
  (Text -> FishExpr TStr) ->
  (Text -> FishExpr TStr -> [FishStatement]) ->
  ArithmeticExpr ->
  Either Text ArithmeticMaterialization
materializeArithmetic nativeHelper prefix getter setter expression
  | validPrefix prefix,
    Just value <- constantValue expression =
      pure (MkArithmeticMaterialization [] (ExprLiteral (show value)) (ExprLiteral (if value == 0 then "1" else "0")) (ExprLiteral "") (ExprLiteral "") [] [])
  | otherwise = materialize
  where
    materialize = do
      unless (validPrefix prefix) (Left "Arithmetic temporary prefix must be a reserved Fish identifier")
      let ((statements, value), temporaryCount) = State.runState (lower Nothing (foldConstants expression)) (0 :: Int)
          initialization = [declare (temporary index) (ExprLiteral "") | index <- [0 .. temporaryCount - 1]]
          status = prefix <> "_status"
          resultName = prefix <> "_result"
          finish = [guardSuccess [assign resultName value, assign status (ExprLiteral "0"), conditional (equal value (ExprLiteral "0")) [assign status (ExprLiteral "1")] []]]
      pure
        MkArithmeticMaterialization
          { arithmeticStatements = declare errorName (ExprLiteral "") : declare originName (ExprLiteral "") : declare status (ExprLiteral "2") : declare resultName (ExprLiteral "") : initialization <> statements <> finish,
            arithmeticValue = variable resultName,
            arithmeticStatus = variable status,
            arithmeticError = variable errorName,
            arithmeticErrorOrigin = variable originName,
            arithmeticHelpers = [helperDefinition | temporaryCount > 0],
            arithmeticRequirements = [nativeRuntimeRequirement NativeInteger "Bash signed-64-bit integer primitives" | temporaryCount > 0]
          }
    errorName = prefix <> "_error"
    originName = prefix <> "_error_origin"
    helperName = nativeHelper <> "_integer"
    temporary index = prefix <> "_v" <> show index
    fresh = do
      index <- State.get
      State.put (index + 1)
      pure (temporary index)
    guardSuccess = conditional (equal (variable errorName) (ExprLiteral "")) `flip` []
    retainOrigin origin = assign originName (ExprLiteral (maybe "" (\(MkArithmeticOrigin tokenIdentity) -> show tokenIdentity) origin))
    failure origin code = [assign errorName (ExprLiteral code), retainOrigin origin]
    operation origin name arguments = do
      valueName <- fresh
      packetName <- fresh
      let packet = ExprVariable (VarAll packetName)
          member index = ExprQuotedVariable (VarIndex packetName (IndexSingle (ExprNumLiteral index)))
          capture = Stmt (builtin "set" [ExprVal (ExprLiteral packetName), ExprVal (ExprCommandSubst (Stmt (Command helperName (map ExprVal (ExprLiteral name : arguments))) NE.:| []))])
          validPacket = builtin "test" [ExprVal (ExprCommandSubst (Stmt (builtin "count" [ExprVal packet]) NE.:| [])), ExprVal (ExprLiteral "="), ExprVal (ExprLiteral "3")]
          accepted = conditional (equal (member 1) (ExprLiteral "ok")) [assign valueName (member 2)] [assign errorName (member 3), retainOrigin origin]
      pure ([guardSuccess [capture, conditional validPacket [accepted] (failure origin "runtime-helper-failure")]], variable valueName)
    lower :: Maybe ArithmeticOrigin -> ArithmeticExpr -> State.State Int ([FishStatement], FishExpr TStr)
    lower origin candidate
      | Just count <- pureCost candidate,
        count >= 2 = do
          (before, frames) <- pureFrames origin candidate
          (after, result) <- operation origin "batch" frames
          pure (before <> after, result)
      | otherwise = lowerNode origin candidate
    -- Admission proves numeric scalar reads (including unset-as-zero). The
    -- batch contains only total operations and no lazy or mutation boundary.
    pureFrames origin = \case
      ArithmeticLocated location inner -> pureFrames (Just location) inner
      ArithmeticLiteral value -> pure ([], [ExprLiteral "push", ExprLiteral (show (wrapInteger value))])
      ArithmeticVariable name -> pure ([], [ExprLiteral "push", getter name])
      ArithmeticUnary operator inner -> do
        (before, frames) <- pureFrames origin inner
        pure (before, frames <> [ExprLiteral (unaryName operator)])
      ArithmeticBinary operator left right -> do
        (beforeLeft, leftFrames) <- pureFrames origin left
        (beforeRight, rightFrames) <- pureFrames origin right
        pure (beforeLeft <> beforeRight, leftFrames <> rightFrames <> [ExprLiteral (binaryName operator)])
      value -> do
        (before, result) <- lower origin value
        pure (before, [ExprLiteral "push", result])
    lowerNode origin = \case
      ArithmeticLocated location inner -> lower (Just location) inner
      ArithmeticLiteral value -> pure ([], ExprLiteral (show (wrapInteger value)))
      ArithmeticVariable name -> operation origin "read" [getter name]
      ArithmeticUnary operator inner -> do
        (before, value) <- lower origin inner
        (after, result) <- operation origin (unaryName operator) [value]
        pure (before <> after, result)
      ArithmeticBinary LogicalAnd left right -> lazyBinary origin False left right
      ArithmeticBinary LogicalOr left right -> lazyBinary origin True left right
      ArithmeticBinary operator left right -> do
        (beforeLeft, leftValue) <- lower origin left
        (beforeRight, rightValue) <- lower origin right
        (after, result) <- operation origin (binaryName operator) [leftValue, rightValue]
        pure (beforeLeft <> beforeRight <> after, result)
      ArithmeticAssign name Nothing right -> do
        (before, result) <- lower origin right
        pure (before <> [guardSuccess (setter name result)], result)
      ArithmeticAssign name (Just operator) right -> do
        (beforeLeft, leftValue) <- lower origin (ArithmeticVariable name)
        (beforeRight, rightValue) <- lower origin right
        (after, result) <- operation origin (binaryName operator) [leftValue, rightValue]
        pure (beforeLeft <> beforeRight <> after <> [guardSuccess (setter name result)], result)
      ArithmeticUpdate position direction name -> do
        (before, oldValue) <- lower origin (ArithmeticVariable name)
        (after, newValue) <- operation origin (if direction == Increment then "add" else "sub") [oldValue, ExprLiteral "1"]
        pure (before <> after <> [guardSuccess (setter name newValue)], if position == PrefixUpdate then newValue else oldValue)
      ArithmeticConditional condition yes no -> do
        (before, conditionValue) <- lower origin condition
        (yesStatements, yesValue) <- lower origin yes
        (noStatements, noValue) <- lower origin no
        result <- fresh
        let choose = conditional (notEqual conditionValue (ExprLiteral "0")) (yesStatements <> [guardSuccess [assign result yesValue]]) (noStatements <> [guardSuccess [assign result noValue]])
        pure (before <> [guardSuccess [choose]], variable result)
      ArithmeticSequence values -> do
        evaluated <- traverse (lower origin) values
        pure (concatMap fst (toList evaluated), snd (NE.last evaluated))
    lazyBinary origin truthy left right = do
      (beforeLeft, leftValue) <- lower origin left
      (beforeRight, rightValue) <- lower origin right
      result <- fresh
      let shortValue = if truthy then "1" else "0"
          continue = beforeRight <> [guardSuccess [assign result (ExprLiteral "0"), conditional (notEqual rightValue (ExprLiteral "0")) [assign result (ExprLiteral "1")] []]]
          shortCondition = (if truthy then notEqual else equal) leftValue (ExprLiteral "0")
      pure (beforeLeft <> [guardSuccess [conditional shortCondition [assign result (ExprLiteral shortValue)] continue]], variable result)
    helperDefinition =
      Stmt
        ( Function
            MkFishFunction
              { funcName = helperName,
                funcFlags = [FuncUnknownFlag "--no-scope-shadowing"],
                funcParams = [],
                funcBody =
                  Stmt
                    ( Pipeline
                        ( MkFishJobPipeline
                            False
                            []
                            (Stmt (Decorated DecBuiltin (Command "printf" [ExprVal (ExprLiteral "%s\\0"), ExprVal (ExprVariable (VarAll "argv"))])))
                            [PipeTo [] (Stmt (Command nativeHelper [ExprVal (ExprLiteral "--abi"), ExprVal (ExprLiteral "1"), ExprVal (ExprLiteral "integer")]))]
                            False
                        )
                    )
                    NE.:| []
              }
        )

validPrefix :: Text -> Bool
validPrefix text =
  case T.uncons text of
    Just (firstCharacter, rest) -> initial firstCharacter && T.all (\character -> initial character || isDigit character) rest
    Nothing -> False
  where
    initial character = character == '_' || isAsciiLower character || isAsciiUpper character

variable :: Text -> FishExpr TStr
variable = ExprQuotedVariable . VarScalar

builtin :: Text -> [ExprOrRedirect] -> FishCommand TStatus
builtin name = Decorated DecBuiltin . Command name

declare :: Text -> FishExpr TStr -> FishStatement
declare name value = Stmt (builtin "set" [ExprVal (ExprLiteral "--local"), ExprVal (ExprLiteral name), ExprVal value])

assign :: Text -> FishExpr TStr -> FishStatement
assign name value = Stmt (builtin "set" [ExprVal (ExprLiteral name), ExprVal value])

equal :: FishExpr TStr -> FishExpr TStr -> FishCommand TStatus
equal left right = builtin "test" [ExprVal left, ExprVal (ExprLiteral "="), ExprVal right]

notEqual :: FishExpr TStr -> FishExpr TStr -> FishCommand TStatus
notEqual left right = builtin "test" [ExprVal left, ExprVal (ExprLiteral "!="), ExprVal right]

conditional :: FishCommand TStatus -> [FishStatement] -> [FishStatement] -> FishStatement
conditional command yes no =
  let pipeline = MkFishJobPipeline False [] (Stmt command) [] False
      jobs = MkFishJobList (MkFishJobConjunction Nothing pipeline [] NE.:| [])
      body = fromMaybe (Stmt (builtin "true" []) NE.:| []) (NE.nonEmpty yes)
   in Stmt (If jobs body no [])

wrapInteger :: Integer -> Integer
wrapInteger = Integer.wrap

-- Only successful closed operations are folded. Failed operations keep their
-- original origin and remain in the runtime error/control region.
foldConstants :: ArithmeticExpr -> ArithmeticExpr
foldConstants expression =
  let simplified = case expression of
        ArithmeticLocated origin inner -> ArithmeticLocated origin (foldConstants inner)
        ArithmeticUnary operator inner -> ArithmeticUnary operator (foldConstants inner)
        ArithmeticBinary operator left right -> ArithmeticBinary operator (foldConstants left) (foldConstants right)
        ArithmeticAssign name operator right -> ArithmeticAssign name operator (foldConstants right)
        ArithmeticConditional predicate yes no -> ArithmeticConditional (foldConstants predicate) (foldConstants yes) (foldConstants no)
        ArithmeticSequence values -> ArithmeticSequence (fmap foldConstants values)
        other -> other
   in maybe simplified ArithmeticLiteral (constantValue simplified)

pureCost :: ArithmeticExpr -> Maybe Int
pureCost = \case
  ArithmeticLocated _ inner -> pureCost inner
  ArithmeticLiteral _ -> Just 0
  ArithmeticVariable _ -> Just 0
  ArithmeticUnary _ inner -> (1 +) <$> pureCost inner
  ArithmeticBinary operator left right
    | operator `notElem` [Divide, Remainder, Power, LogicalAnd, LogicalOr] -> (\a b -> 1 + a + b) <$> pureCost left <*> pureCost right
  _ -> Nothing
