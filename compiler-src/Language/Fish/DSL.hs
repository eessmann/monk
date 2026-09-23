{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 914
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}
#endif

module Language.Fish.DSL
  ( FishType (..),
    SetFlag (..),
    ReadFlag (..),
    FunctionFlag (..),
    Decoration (..),
    Conjunction (..),
    SourcePos (..),
    SourceRange (..),
    CommandName,
    commandName,
    commandNameText,
    Executable,
    literalExecutable,
    variableExecutable,
    execute,
    Identifier,
    identifier,
    identifierText,
    Expr,
    pattern Str,
    pattern IntLit,
    str,
    int,
    var,
    vars,
    varIndex,
    specialStatus,
    specialPipeStatuses,
    list,
    concatStr,
    joinList,
    math,
    commandSubst,
    processSubst,
    GlobPattern (..),
    GlobPart (..),
    glob,
    Index,
    IndexShape (..),
    IndexResult,
    singleIndex,
    rangeIndex,
    manyIndexes,
    Arg,
    ArgumentType,
    arg,
    redirect,
    redirectArg,
    Redirect,
    RedirectStream,
    StreamKind,
    ModeKind,
    TargetKind,
    RedirectForm,
    stdout,
    stderr,
    stdin,
    both,
    fd,
    RedirectMode,
    overwrite,
    append,
    input,
    clobber,
    RedirectTarget,
    fileTarget,
    fdTarget,
    closeTarget,
    Command,
    CommandGrammar (..),
    StageGrammar,
    BackgroundGrammar,
    CommandRole (..),
    CommandResult,
    command,
    set,
    echo,
    printf,
    source,
    eval,
    exit,
    return_,
    break_,
    continue_,
    not_,
    background,
    wait,
    exec,
    decorate,
    semicolon,
    begin,
    beginWithRedirects,
    beginBlock,
    beginBlockWithRedirects,
    pipeline,
    pipelineWithTime,
    Stage,
    stage,
    Pipeline,
    pipelineValue,
    pipelineValueWithTime,
    JobContinuation,
    andThen,
    orElse,
    JobConjunction,
    jobConjunction,
    job,
    JobList,
    jobList,
    condition,
    if_,
    while,
    for,
    CaseItem,
    caseItem,
    switch,
    function,
    read_,
    Stmt,
    stmt,
    comment,
    empty,
    Block,
    block,
    Script,
    script,
    renderScript,
  )
where

import Language.Fish.DSL.Executable (commandName)
import Language.Fish.DSL.Internal
import Language.Fish.DSL.Lower qualified as Lower
import Language.Fish.DSL.Name (identifier)
import Language.Fish.Pretty qualified as Pretty
import Relude hiding (empty, stderr, stdin, stdout)

pattern Str :: Text -> Expr TStr
pattern Str txt = ExprLiteral txt

pattern IntLit :: Int -> Expr TInt
pattern IntLit value = ExprNumLiteral value

str :: Text -> Expr TStr
str = ExprLiteral

int :: Int -> Expr TInt
int = ExprNumLiteral

var :: Identifier -> Expr TStr
var = ExprQuotedVariable . VarScalar

vars :: Identifier -> Expr (TList TStr)
vars = ExprVariable . VarAll

varIndex :: Identifier -> Index shape -> Expr (IndexResult shape TStr)
varIndex name = \case
  MkIndexSingle idx -> ExprQuotedVariable (VarIndex name (IndexSingle idx))
  MkIndexRange start end -> ExprVariable (VarIndex name (IndexRange start end))
  MkIndexList indexes -> ExprVariable (VarIndex name (IndexList indexes))

specialStatus :: Expr TInt
specialStatus = ExprSpecialVar SVStatus

specialPipeStatuses :: Expr (TList TInt)
specialPipeStatuses = ExprSpecialVar SVPipestatus

list :: [Expr TStr] -> Expr (TList TStr)
list = ExprListLiteral

concatStr :: Expr TStr -> Expr TStr -> Expr TStr
concatStr = ExprStringConcat

joinList :: Expr (TList TStr) -> Expr TStr
joinList = ExprJoinList

math :: NonEmpty (Expr TStr) -> Expr TInt
math = ExprMath

commandSubst :: NonEmpty Stmt -> Expr (TList TStr)
commandSubst = ExprCommandSubst

processSubst :: NonEmpty Stmt -> Expr TStr
processSubst = ExprProcessSubst

glob :: GlobPattern -> Expr (TList TStr)
glob = ExprGlob

singleIndex :: Expr TInt -> Index IndexOneShape
singleIndex = MkIndexSingle

rangeIndex :: Maybe (Expr TInt) -> Maybe (Expr TInt) -> Index IndexRangeShape
rangeIndex = MkIndexRange

manyIndexes :: NonEmpty (Expr TInt) -> Index IndexManyShape
manyIndexes = MkIndexList

arg :: forall t. (ArgumentType t, Typeable t) => Expr t -> Arg
arg expr = argumentTypeWitness (Proxy @t) `seq` ExprVal expr

redirect :: (RedirectForm stream mode target) => RedirectStream stream -> RedirectMode mode -> RedirectTarget target -> Redirect
redirect = buildRedirect

redirectArg :: Redirect -> Arg
redirectArg = RedirectVal

stdout :: RedirectStream SingleStream
stdout = DescriptorStream 1

stderr :: RedirectStream SingleStream
stderr = DescriptorStream 2

stdin :: RedirectStream SingleStream
stdin = DescriptorStream 0

both :: RedirectStream CombinedStreams
both = BothStreams

fd :: Natural -> RedirectStream SingleStream
fd = DescriptorStream

overwrite :: RedirectMode OverwriteMode
overwrite = Overwrite

append :: RedirectMode AppendRedirectMode
append = Append

input :: RedirectMode InputMode
input = Input

clobber :: RedirectMode ClobberMode
clobber = Clobber

fileTarget :: Expr TStr -> RedirectTarget FileTarget
fileTarget = RedirectFile

fdTarget :: Natural -> RedirectTarget DescriptorTarget
fdTarget = RedirectTargetFD

closeTarget :: RedirectTarget DescriptorTarget
closeTarget = RedirectClose

command :: CommandName -> [Arg] -> Command Atomic ReturnsStatus
command = Command

execute :: Executable -> [Arg] -> Command Atomic ReturnsStatus
execute = CommandExpr

set :: [SetFlag] -> Identifier -> Expr (TList TStr) -> Command Atomic ReturnsUnit
set = Set

echo :: NonEmpty (Expr TStr) -> Command Atomic ReturnsUnit
echo = Echo

printf :: Expr TStr -> [Expr TStr] -> Command Atomic ReturnsUnit
printf = Printf

source :: Expr TStr -> Command Atomic ReturnsStatus
source = Source

eval :: Expr TStr -> Command Atomic ReturnsStatus
eval = Eval

exit :: Maybe (Expr TInt) -> Command Atomic ReturnsStatus
exit = Exit

return_ :: Maybe (Expr TInt) -> Command ControlGrammar ReturnsStatus
return_ = Return

break_ :: Command ControlGrammar ReturnsUnit
break_ = Break

continue_ :: Command ControlGrammar ReturnsUnit
continue_ = Continue

not_ :: (StageGrammar grammar) => Command grammar ReturnsStatus -> Command BlockGrammar ReturnsStatus
not_ = Not

background :: (Typeable (CommandResult r), BackgroundGrammar grammar) => Command grammar r -> Command Asynchronous ReturnsStatus
background = Background

wait :: Maybe (Expr TInt) -> Command Atomic ReturnsStatus
wait = Wait

exec :: Executable -> [Arg] -> Command ControlGrammar ReturnsStatus
exec = Exec

decorate :: (Typeable (CommandResult r)) => Decoration -> Command Atomic r -> Command Atomic r
decorate = Decorated

semicolon ::
  (Typeable (CommandResult left), Typeable (CommandResult right)) =>
  Command leftGrammar left ->
  Command rightGrammar right ->
  Command Compound right
semicolon = Semicolon

begin :: NonEmpty Stmt -> Command BlockGrammar ReturnsStatus
begin body = Begin body []

beginWithRedirects :: NonEmpty Stmt -> [Redirect] -> Command BlockGrammar ReturnsStatus
beginWithRedirects = Begin

beginBlock :: Block -> Command BlockGrammar ReturnsStatus
beginBlock (MkBlock body) = Begin body []

beginBlockWithRedirects :: Block -> [Redirect] -> Command BlockGrammar ReturnsStatus
beginBlockWithRedirects (MkBlock body) = Begin body

pipeline :: NonEmpty Stage -> Command Compound ReturnsStatus
pipeline = pipelineWithTime False

pipelineWithTime :: Bool -> NonEmpty Stage -> Command Compound ReturnsStatus
pipelineWithTime timed = Pipeline . pipelineValueWithTime timed

stage :: (StageGrammar grammar) => Command grammar ReturnsStatus -> Stage
stage = MkStage

pipelineValue :: NonEmpty Stage -> Pipeline
pipelineValue = pipelineValueWithTime False

pipelineValueWithTime :: Bool -> NonEmpty Stage -> Pipeline
pipelineValueWithTime timed (headStage :| rest) =
  JobPipeline
    { jpTime = timed,
      jpVariables = [],
      jpStatement = headStage,
      jpCont = pipeContinuation <$> rest
    }
  where
    pipeContinuation next =
      PipeToStage
        { jpcVariables = [],
          jpcStatement = next
        }

andThen :: Pipeline -> JobContinuation
andThen = JCAnd

orElse :: Pipeline -> JobContinuation
orElse = JCOr

jobConjunction :: Maybe Conjunction -> Pipeline -> [JobContinuation] -> JobConjunction
jobConjunction = MkFishJobConjunction

job :: JobConjunction -> Command Compound ReturnsStatus
job = JobConj

jobList :: NonEmpty JobConjunction -> JobList
jobList = MkFishJobList

condition :: (StageGrammar grammar) => Command grammar ReturnsStatus -> JobList
condition cmd =
  jobList
    (jobConjunction Nothing (pipelineValue (stage cmd :| [])) [] :| [])

if_ :: JobList -> Block -> [Stmt] -> [Redirect] -> Command BlockGrammar ReturnsStatus
if_ cond (MkBlock thn) = If cond thn

while :: JobList -> Block -> [Redirect] -> Command BlockGrammar ReturnsStatus
while cond (MkBlock body) = While cond body

for :: Identifier -> Expr (TList TStr) -> Block -> [Redirect] -> Command BlockGrammar ReturnsStatus
for name values (MkBlock body) = For name values body

caseItem :: NonEmpty (Expr TStr) -> Block -> CaseItem
caseItem patterns (MkBlock body) = MkCaseItem patterns body

switch :: Expr TStr -> NonEmpty CaseItem -> [Redirect] -> Command BlockGrammar ReturnsStatus
switch = Switch

function :: Text -> [FunctionFlag] -> [Identifier] -> Block -> Command Definition ReturnsUnit
function name flags params (MkBlock body) =
  Function
    MkFishFunction
      { funcName = name,
        funcFlags = flags,
        funcParams = params,
        funcBody = body
      }

read_ :: [ReadFlag] -> [Identifier] -> Command Atomic ReturnsStatus
read_ = Read

stmt :: (Typeable (CommandResult r)) => Command grammar r -> Stmt
stmt = Stmt

comment :: Text -> Stmt
comment = Comment

empty :: Stmt
empty = EmptyStmt

block :: NonEmpty Stmt -> Block
block = MkBlock

script :: [Stmt] -> Script
script = MkScript

renderScript :: Script -> Text
renderScript = Pretty.renderFish . Lower.lowerScript
