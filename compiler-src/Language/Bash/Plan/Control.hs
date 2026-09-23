{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Generative lexical scopes. Every executable boundary, including a loop,
-- owns fresh witnesses; only a matching scope can consume its control exits.
module Language.Bash.Plan.Control
  ( Control,
    Root,
    RootKind (..),
    BodyRoot,
    bodyRootWitness,
    EntryRoot,
    entryRootBody,
    withContextControl,
    LoopTarget,
    LoopKey,
    ReturnTarget,
    ShiftTarget,
    SetArgumentsTarget,
    SourceArguments (..),
    ReturnRole (..),
    ArgvRole (..),
    withEntryControl,
    withLoopControl,
    withFunctionControl,
    withSourceControl,
    withChildControl,
    withHandlerControl,
    rootWitness,
    activeFunction,
    inSource,
    loopTarget,
    loopKey,
    returnTarget,
    shiftTarget,
    setArgumentsTarget,
    returnsFromSource,
    sameReturnTarget,
    sameShiftTarget,
    consumeLoop,
    consumeReturn,
    consumeShift,
    consumeSetArguments,
  )
where

import Monk.Compiler.Context (Context, Entry, contextConfig)
import Monk.Translation.Types (EntryMode (..), TranslateConfig (entryMode))

data SourceArguments = BorrowCallerArguments | OwnSourceArguments deriving stock (Show, Eq)

data SourceScope = OutsideSource | InsideSource SourceArguments

data FunctionScope = OutsideFunction | InsideFunction Text

-- A lexical scope says which exits agree; its finite role says where a
-- complete body may be packed. Constructors remain private.
data RootKind = EntryRootKind | FunctionRootKind | SourceRootKind | ChildRootKind | HandlerRootKind

type role BodyRoot nominal nominal

newtype BodyRoot (kind :: RootKind) scope = BodyRoot (Root scope)

bodyRootWitness :: BodyRoot kind scope -> Root scope
bodyRootWitness (BodyRoot root) = root

type role EntryRoot nominal nominal nominal

newtype EntryRoot (owner :: Type) (entry :: Entry) scope = EntryRoot (BodyRoot EntryRootKind scope)

entryRootBody :: EntryRoot owner entry scope -> BodyRoot EntryRootKind scope
entryRootBody (EntryRoot root) = root

withContextControl :: Context owner target entry provider -> (forall scope. EntryRoot owner entry scope -> Control scope -> result) -> result
withContextControl context consume = withEntryControl (entryMode (contextConfig context)) $ \control -> consume (EntryRoot (BodyRoot (rootWitness control))) control

type role Root nominal

data Root (scope :: Type) = Root

type role LoopTarget nominal

newtype LoopTarget (scope :: Type) = LoopTarget LoopKey
  deriving stock (Show, Eq, Ord)

newtype LoopKey = LoopKey Natural deriving stock (Show, Eq, Ord)

data ReturnRole = FunctionReturn | SourceReturn | EntryReturn deriving stock (Show, Eq)

data ArgvRole = FunctionArguments | SourceArguments | EntryArguments deriving stock (Show, Eq)

type role ReturnTarget nominal

newtype ReturnTarget (scope :: Type) = ReturnTarget ReturnRole deriving stock (Show, Eq)

type role ShiftTarget nominal

newtype ShiftTarget (scope :: Type) = ShiftTarget ArgvRole deriving stock (Show, Eq)

type role SetArgumentsTarget nominal

data SetArgumentsTarget (scope :: Type) = StandaloneArguments deriving stock (Show, Eq)

type role Control nominal

data Control scope = Control (Root scope) EntryMode FunctionScope SourceScope (Maybe (LoopTarget scope))

withEntryControl :: EntryMode -> (forall scope. Control scope -> result) -> result
withEntryControl mode consume = consume (Control Root mode OutsideFunction OutsideSource Nothing)

withLoopControl :: Control outer -> (forall scope. LoopTarget scope -> Control scope -> result) -> result
withLoopControl (Control _ mode function source previous) consume =
  let target = LoopTarget (LoopKey (maybe 1 (\(LoopTarget (LoopKey depth)) -> depth + 1) previous))
   in consume target (Control Root mode function source (Just target))

withFunctionControl :: Text -> Control outer -> (forall scope. BodyRoot FunctionRootKind scope -> Control scope -> result) -> result
withFunctionControl name (Control _ mode _ source _) consume = consume (BodyRoot Root) (Control Root mode (InsideFunction name) source Nothing)

withSourceControl :: SourceArguments -> Control outer -> (forall scope. BodyRoot SourceRootKind scope -> Control scope -> result) -> result
withSourceControl arguments (Control _ mode function _ _) consume = consume (BodyRoot Root) (Control Root mode function (InsideSource arguments) Nothing)

withChildControl :: Control outer -> (forall scope. BodyRoot ChildRootKind scope -> Control scope -> result) -> result
withChildControl (Control _ mode function source _) consume = consume (BodyRoot Root) (Control Root mode function source Nothing)

withHandlerControl :: Control outer -> (forall scope. BodyRoot HandlerRootKind scope -> Control scope -> result) -> result
withHandlerControl (Control _ mode function source _) consume = consume (BodyRoot Root) (Control Root mode function source Nothing)

rootWitness :: Control scope -> Root scope
rootWitness (Control root _ _ _ _) = root

activeFunction :: Control scope -> Maybe Text
activeFunction (Control _ _ function _ _) = case function of
  OutsideFunction -> Nothing
  InsideFunction name -> Just name

inSource :: Control scope -> Bool
inSource (Control _ _ _ source _) = case source of
  OutsideSource -> False
  InsideSource _ -> True

loopTarget :: Control scope -> Maybe (LoopTarget scope)
loopTarget (Control _ _ _ _ target) = target

loopKey :: LoopTarget scope -> LoopKey
loopKey (LoopTarget key) = key

returnTarget :: Control scope -> Maybe (ReturnTarget scope)
returnTarget (Control _ mode function source _) = fmap ReturnTarget $ case source of
  InsideSource _ -> Just SourceReturn
  OutsideSource -> case function of
    InsideFunction _ -> Just FunctionReturn
    OutsideFunction -> case mode of
      Sourceable -> Just EntryReturn
      Standalone -> Nothing

shiftTarget :: Control scope -> Maybe (ShiftTarget scope)
shiftTarget (Control _ mode function source _) = fmap ShiftTarget $ case source of
  InsideSource OwnSourceArguments -> Just SourceArguments
  InsideSource BorrowCallerArguments -> Nothing
  OutsideSource -> case function of
    InsideFunction _ -> Just FunctionArguments
    OutsideFunction -> case mode of
      Standalone -> Just EntryArguments
      Sourceable -> Nothing

setArgumentsTarget :: Control scope -> Maybe (SetArgumentsTarget scope)
setArgumentsTarget (Control _ mode _ source _) = case (mode, source) of
  (Standalone, OutsideSource) -> Just StandaloneArguments
  _ -> Nothing

returnsFromSource :: ReturnTarget scope -> Bool
returnsFromSource (ReturnTarget SourceReturn) = True
returnsFromSource _ = False

consumeLoop :: Root scope -> LoopTarget scope -> LoopKey
consumeLoop Root = loopKey

consumeReturn :: Root scope -> ReturnTarget scope -> ReturnRole
consumeReturn Root (ReturnTarget role) = role

consumeShift :: Root scope -> ShiftTarget scope -> ArgvRole
consumeShift Root (ShiftTarget role) = role

consumeSetArguments :: Root scope -> SetArgumentsTarget scope -> ()
consumeSetArguments Root StandaloneArguments = ()

sameReturnTarget :: ReturnTarget left -> ReturnTarget right -> Bool
sameReturnTarget (ReturnTarget left) (ReturnTarget right) = left == right

sameShiftTarget :: ShiftTarget left -> ShiftTarget right -> Bool
sameShiftTarget (ShiftTarget left) (ShiftTarget right) = left == right
