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
    RedirectStream,
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
    readWrite,
    RedirectTarget,
    fileTarget,
    fdTarget,
    closeTarget,
    Command,
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

import Language.Fish.DSL.Internal
import Language.Fish.DSL.Lower qualified as Lower
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

var :: Text -> Expr TStr
var = ExprVariable . VarScalar

vars :: Text -> Expr (TList TStr)
vars = ExprVariable . VarAll

varIndex :: Text -> Index shape -> Expr (IndexResult shape TStr)
varIndex name = \case
  MkIndexSingle idx -> ExprVariable (VarIndex name (IndexSingle idx))
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

redirect :: RedirectStream -> RedirectMode -> RedirectTarget -> Arg
redirect stream mode target = RedirectVal (MkRedirect (toRedirectSource stream) (toRedirectOp mode) target)

stdout :: RedirectStream
stdout = Stdout

stderr :: RedirectStream
stderr = Stderr

stdin :: RedirectStream
stdin = Stdin

both :: RedirectStream
both = Both

fd :: Int -> RedirectStream
fd = Fd

overwrite :: RedirectMode
overwrite = Overwrite

append :: RedirectMode
append = Append

input :: RedirectMode
input = Input

clobber :: RedirectMode
clobber = Clobber

readWrite :: RedirectMode
readWrite = ReadWrite

fileTarget :: Expr TStr -> RedirectTarget
fileTarget = RedirectFile

fdTarget :: Int -> RedirectTarget
fdTarget = RedirectTargetFD

closeTarget :: RedirectTarget
closeTarget = RedirectClose

command :: Text -> [Arg] -> Command ReturnsStatus
command = Command

set :: [SetFlag] -> Text -> Expr (TList TStr) -> Command ReturnsUnit
set = Set

echo :: NonEmpty (Expr TStr) -> Command ReturnsUnit
echo = Echo

printf :: Expr TStr -> [Expr TStr] -> Command ReturnsUnit
printf = Printf

source :: Expr TStr -> Command ReturnsStatus
source = Source

eval :: Expr TStr -> Command ReturnsStatus
eval = Eval

exit :: Maybe (Expr TInt) -> Command ReturnsStatus
exit = Exit

return_ :: Maybe (Expr TInt) -> Command ReturnsStatus
return_ = Return

break_ :: Command ReturnsUnit
break_ = Break

continue_ :: Command ReturnsUnit
continue_ = Continue

not_ :: Command ReturnsStatus -> Command ReturnsStatus
not_ = Not

background :: (Typeable (CommandResult r)) => Command r -> Command ReturnsStatus
background = Background

wait :: Maybe (Expr TInt) -> Command ReturnsStatus
wait = Wait

exec :: Expr TStr -> [Arg] -> Command ReturnsStatus
exec = Exec

decorate :: (Typeable (CommandResult r)) => Decoration -> Command r -> Command r
decorate = Decorated

semicolon ::
  (Typeable (CommandResult left), Typeable (CommandResult right)) =>
  Command left ->
  Command right ->
  Command right
semicolon = Semicolon

begin :: NonEmpty Stmt -> Command ReturnsStatus
begin body = Begin body []

beginWithRedirects :: NonEmpty Stmt -> [Arg] -> Command ReturnsStatus
beginWithRedirects = Begin

beginBlock :: Block -> Command ReturnsStatus
beginBlock (MkBlock body) = Begin body []

beginBlockWithRedirects :: Block -> [Arg] -> Command ReturnsStatus
beginBlockWithRedirects (MkBlock body) = Begin body

pipeline :: NonEmpty Stage -> Command ReturnsStatus
pipeline = pipelineWithTime False

pipelineWithTime :: Bool -> NonEmpty Stage -> Command ReturnsStatus
pipelineWithTime timed = Pipeline . pipelineValueWithTime timed

stage :: Command ReturnsStatus -> Stage
stage = MkStage

pipelineValue :: NonEmpty Stage -> Pipeline
pipelineValue = pipelineValueWithTime False

pipelineValueWithTime :: Bool -> NonEmpty Stage -> Pipeline
pipelineValueWithTime timed (MkStage headStage :| rest) =
  MkFishJobPipeline
    { jpTime = timed,
      jpVariables = [],
      jpStatement = Stmt headStage,
      jpCont = pipeContinuation <$> rest,
      jpBackgrounded = False
    }
  where
    pipeContinuation (MkStage next) =
      PipeTo
        { jpcVariables = [],
          jpcStatement = Stmt next
        }

andThen :: Pipeline -> JobContinuation
andThen = JCAnd

orElse :: Pipeline -> JobContinuation
orElse = JCOr

jobConjunction :: Maybe Conjunction -> Pipeline -> [JobContinuation] -> JobConjunction
jobConjunction = MkFishJobConjunction

job :: JobConjunction -> Command ReturnsStatus
job = JobConj

jobList :: NonEmpty JobConjunction -> JobList
jobList = MkFishJobList

condition :: Command ReturnsStatus -> JobList
condition cmd =
  jobList
    (jobConjunction Nothing (pipelineValue (stage cmd :| [])) [] :| [])

if_ :: JobList -> Block -> [Stmt] -> [Arg] -> Command ReturnsStatus
if_ cond (MkBlock thn) = If cond thn

while :: JobList -> Block -> [Arg] -> Command ReturnsStatus
while cond (MkBlock body) = While cond body

for :: Text -> Expr (TList TStr) -> Block -> [Arg] -> Command ReturnsStatus
for name values (MkBlock body) = For name values body

caseItem :: NonEmpty (Expr TStr) -> Block -> CaseItem
caseItem patterns (MkBlock body) = MkCaseItem patterns body

switch :: Expr TStr -> NonEmpty CaseItem -> [Arg] -> Command ReturnsStatus
switch = Switch

function :: Text -> [FunctionFlag] -> [Text] -> Block -> Command ReturnsUnit
function name flags params (MkBlock body) =
  Function
    MkFishFunction
      { funcName = name,
        funcFlags = flags,
        funcParams = params,
        funcBody = body
      }

read_ :: [ReadFlag] -> [Text] -> Command ReturnsStatus
read_ = Read

stmt :: (Typeable (CommandResult r)) => Command r -> Stmt
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

toRedirectSource :: RedirectStream -> RedirectSource
toRedirectSource = \case
  Stdout -> RedirectStdout
  Stderr -> RedirectStderr
  Stdin -> RedirectStdin
  Both -> RedirectBoth
  Fd handle -> RedirectFD handle

toRedirectOp :: RedirectMode -> RedirectOp
toRedirectOp = \case
  Overwrite -> RedirectOut
  Append -> RedirectOutAppend
  Input -> RedirectIn
  Clobber -> RedirectClobber
  ReadWrite -> RedirectReadWrite
