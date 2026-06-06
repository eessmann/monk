{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

module Language.Fish.DSL
  ( Raw.FishType (..),
    Raw.SetFlag (..),
    Raw.ReadFlag (..),
    Raw.FunctionFlag (..),
    Raw.Decoration (..),
    Raw.Conjunction (..),
    Raw.SourcePos (..),
    Raw.SourceRange (..),
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
    Raw.GlobPattern (..),
    Raw.GlobPart (..),
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
  )
where

import Language.Fish.AST qualified as Raw
import Language.Fish.DSL.Internal
import Relude hiding (empty, stderr, stdin, stdout)

pattern Str :: Text -> Expr 'Raw.TStr
pattern Str txt <- UnsafeExpr (Raw.ExprLiteral txt)
  where
    Str txt = str txt

pattern IntLit :: Int -> Expr 'Raw.TInt
pattern IntLit value <- UnsafeExpr (Raw.ExprNumLiteral value)
  where
    IntLit value = int value

str :: Text -> Expr 'Raw.TStr
str = UnsafeExpr . Raw.ExprLiteral

int :: Int -> Expr 'Raw.TInt
int = UnsafeExpr . Raw.ExprNumLiteral

var :: Text -> Expr 'Raw.TStr
var = UnsafeExpr . Raw.ExprVariable . Raw.VarScalar

vars :: Text -> Expr ('Raw.TList 'Raw.TStr)
vars = UnsafeExpr . Raw.ExprVariable . Raw.VarAll

varIndex :: Text -> Index shape -> Expr (IndexResult shape 'Raw.TStr)
varIndex name = \case
  UnsafeIndexSingle idx ->
    UnsafeExpr (Raw.ExprVariable (Raw.VarIndex name (Raw.IndexSingle (lowerExpr idx))))
  UnsafeIndexRange start end ->
    UnsafeExpr (Raw.ExprVariable (Raw.VarIndex name (Raw.IndexRange (lowerExpr <$> start) (lowerExpr <$> end))))
  UnsafeIndexList indexes ->
    UnsafeExpr (Raw.ExprVariable (Raw.VarIndex name (Raw.IndexList (lowerExpr <$> indexes))))

specialStatus :: Expr 'Raw.TInt
specialStatus = UnsafeExpr (Raw.ExprSpecialVar Raw.SVStatus)

specialPipeStatuses :: Expr ('Raw.TList 'Raw.TInt)
specialPipeStatuses = UnsafeExpr (Raw.ExprSpecialVar Raw.SVPipestatus)

list :: [Expr 'Raw.TStr] -> Expr ('Raw.TList 'Raw.TStr)
list = UnsafeExpr . Raw.ExprListLiteral . fmap lowerExpr

concatStr :: Expr 'Raw.TStr -> Expr 'Raw.TStr -> Expr 'Raw.TStr
concatStr left right = UnsafeExpr (Raw.ExprStringConcat (lowerExpr left) (lowerExpr right))

joinList :: Expr ('Raw.TList 'Raw.TStr) -> Expr 'Raw.TStr
joinList expr = UnsafeExpr (Raw.ExprJoinList (lowerExpr expr))

math :: NonEmpty (Expr 'Raw.TStr) -> Expr 'Raw.TInt
math exprs = UnsafeExpr (Raw.ExprMath (lowerExpr <$> exprs))

commandSubst :: NonEmpty Stmt -> Expr ('Raw.TList 'Raw.TStr)
commandSubst stmts = UnsafeExpr (Raw.ExprCommandSubst (lowerStmt <$> stmts))

processSubst :: NonEmpty Stmt -> Expr 'Raw.TStr
processSubst stmts = UnsafeExpr (Raw.ExprProcessSubst (lowerStmt <$> stmts))

glob :: Raw.GlobPattern -> Expr ('Raw.TList 'Raw.TStr)
glob = UnsafeExpr . Raw.ExprGlob

singleIndex :: Expr 'Raw.TInt -> Index 'IndexOne
singleIndex = UnsafeIndexSingle

rangeIndex :: Maybe (Expr 'Raw.TInt) -> Maybe (Expr 'Raw.TInt) -> Index 'IndexRange
rangeIndex = UnsafeIndexRange

manyIndexes :: NonEmpty (Expr 'Raw.TInt) -> Index 'IndexMany
manyIndexes = UnsafeIndexList

arg :: forall t. (ArgumentType t, Typeable t) => Expr t -> Arg
arg = UnsafeArgExpr

redirect :: RedirectStream -> RedirectMode -> RedirectTarget -> Arg
redirect stream mode target =
  UnsafeArgRedirect
    ( Raw.MkRedirect
        (lowerRedirectStream stream)
        (lowerRedirectMode mode)
        (lowerRedirectTarget target)
    )

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

fileTarget :: Expr 'Raw.TStr -> RedirectTarget
fileTarget = UnsafeRedirectTarget . Raw.RedirectFile . lowerExpr

fdTarget :: Int -> RedirectTarget
fdTarget = UnsafeRedirectTarget . Raw.RedirectTargetFD

closeTarget :: RedirectTarget
closeTarget = UnsafeRedirectTarget Raw.RedirectClose

command :: Text -> [Arg] -> Command 'ReturnsStatus
command name args = UnsafeCommand (Raw.Command name (lowerArg <$> args))

set :: [Raw.SetFlag] -> Text -> Expr ('Raw.TList 'Raw.TStr) -> Command 'ReturnsUnit
set flags name values = UnsafeCommand (Raw.Set flags name (lowerExpr values))

echo :: NonEmpty (Expr 'Raw.TStr) -> Command 'ReturnsUnit
echo exprs = UnsafeCommand (Raw.Echo (lowerExpr <$> exprs))

printf :: Expr 'Raw.TStr -> [Expr 'Raw.TStr] -> Command 'ReturnsUnit
printf fmt args = UnsafeCommand (Raw.Printf (lowerExpr fmt) (lowerExpr <$> args))

source :: Expr 'Raw.TStr -> Command 'ReturnsStatus
source expr = UnsafeCommand (Raw.Source (lowerExpr expr))

eval :: Expr 'Raw.TStr -> Command 'ReturnsStatus
eval expr = UnsafeCommand (Raw.Eval (lowerExpr expr))

exit :: Maybe (Expr 'Raw.TInt) -> Command 'ReturnsStatus
exit expr = UnsafeCommand (Raw.Exit (lowerExpr <$> expr))

return_ :: Maybe (Expr 'Raw.TInt) -> Command 'ReturnsStatus
return_ expr = UnsafeCommand (Raw.Return (lowerExpr <$> expr))

break_ :: Command 'ReturnsUnit
break_ = UnsafeCommand Raw.Break

continue_ :: Command 'ReturnsUnit
continue_ = UnsafeCommand Raw.Continue

not_ :: Command 'ReturnsStatus -> Command 'ReturnsStatus
not_ cmd = UnsafeCommand (Raw.Not (lowerCommand cmd))

background :: Command r -> Command 'ReturnsStatus
background (UnsafeCommand cmd) = UnsafeCommand (Raw.Background cmd)

wait :: Maybe (Expr 'Raw.TInt) -> Command 'ReturnsStatus
wait expr = UnsafeCommand (Raw.Wait (lowerExpr <$> expr))

exec :: Expr 'Raw.TStr -> [Arg] -> Command 'ReturnsStatus
exec cmd args = UnsafeCommand (Raw.Exec (lowerExpr cmd) (lowerArg <$> args))

decorate :: Raw.Decoration -> Command r -> Command r
decorate dec (UnsafeCommand cmd) = UnsafeCommand (Raw.Decorated dec cmd)

semicolon :: Command left -> Command right -> Command right
semicolon (UnsafeCommand left) (UnsafeCommand right) = UnsafeCommand (Raw.Semicolon left right)

begin :: NonEmpty Stmt -> Command 'ReturnsStatus
begin = beginBlock . block

beginWithRedirects :: NonEmpty Stmt -> [Arg] -> Command 'ReturnsStatus
beginWithRedirects body = beginBlockWithRedirects (block body)

beginBlock :: Block -> Command 'ReturnsStatus
beginBlock body = UnsafeCommand (Raw.Begin (lowerBlock body) [])

beginBlockWithRedirects :: Block -> [Arg] -> Command 'ReturnsStatus
beginBlockWithRedirects body redirs = UnsafeCommand (Raw.Begin (lowerBlock body) (lowerArg <$> redirs))

pipeline :: NonEmpty Stage -> Command 'ReturnsStatus
pipeline = pipelineWithTime False

pipelineWithTime :: Bool -> NonEmpty Stage -> Command 'ReturnsStatus
pipelineWithTime timed stages = UnsafeCommand (Raw.Pipeline (lowerPipelineWithTime timed stages))

stage :: Command 'ReturnsStatus -> Stage
stage = UnsafeStage

pipelineValue :: NonEmpty Stage -> Pipeline
pipelineValue = pipelineValueWithTime False

pipelineValueWithTime :: Bool -> NonEmpty Stage -> Pipeline
pipelineValueWithTime timed stages = UnsafePipeline (lowerPipelineWithTime timed stages)

andThen :: Pipeline -> JobContinuation
andThen = UnsafeAndThen

orElse :: Pipeline -> JobContinuation
orElse = UnsafeOrElse

jobConjunction :: Maybe Raw.Conjunction -> Pipeline -> [JobContinuation] -> JobConjunction
jobConjunction decorator headJob continuations =
  UnsafeJobConjunction
    ( Raw.MkFishJobConjunction
        decorator
        (lowerPipelineValue headJob)
        (lowerJobContinuation <$> continuations)
    )

job :: JobConjunction -> Command 'ReturnsStatus
job conj = UnsafeCommand (Raw.JobConj (lowerJobConjunction conj))

jobList :: NonEmpty JobConjunction -> JobList
jobList conjs = UnsafeJobList (Raw.MkFishJobList (lowerJobConjunction <$> conjs))

condition :: Command 'ReturnsStatus -> JobList
condition cmd =
  jobList
    ( jobConjunction Nothing (pipelineValue (stage cmd :| [])) []
        :| []
    )

if_ :: JobList -> Block -> [Stmt] -> [Arg] -> Command 'ReturnsStatus
if_ cond thn els redirs =
  UnsafeCommand (Raw.If (lowerJobList cond) (lowerBlock thn) (lowerStmt <$> els) (lowerArg <$> redirs))

while :: JobList -> Block -> [Arg] -> Command 'ReturnsStatus
while cond body redirs =
  UnsafeCommand (Raw.While (lowerJobList cond) (lowerBlock body) (lowerArg <$> redirs))

for :: Text -> Expr ('Raw.TList 'Raw.TStr) -> Block -> [Arg] -> Command 'ReturnsStatus
for name values body redirs =
  UnsafeCommand (Raw.For name (lowerExpr values) (lowerBlock body) (lowerArg <$> redirs))

caseItem :: NonEmpty (Expr 'Raw.TStr) -> Block -> CaseItem
caseItem patterns body =
  UnsafeCaseItem (Raw.MkCaseItem (lowerExpr <$> patterns) (lowerBlock body))

switch :: Expr 'Raw.TStr -> NonEmpty CaseItem -> [Arg] -> Command 'ReturnsStatus
switch scrutinee cases redirs =
  UnsafeCommand (Raw.Switch (lowerExpr scrutinee) (lowerCaseItem <$> cases) (lowerArg <$> redirs))

function :: Text -> [Raw.FunctionFlag] -> [Text] -> Block -> Command 'ReturnsUnit
function name flags params body =
  UnsafeCommand
    ( Raw.Function
        Raw.MkFishFunction
          { Raw.funcName = name,
            Raw.funcFlags = flags,
            Raw.funcParams = params,
            Raw.funcBody = lowerBlock body
          }
    )

read_ :: [Raw.ReadFlag] -> [Text] -> Command 'ReturnsStatus
read_ flags names = UnsafeCommand (Raw.Read flags names)

stmt :: Command r -> Stmt
stmt (UnsafeCommand cmd) = UnsafeStmt (Raw.Stmt cmd)

comment :: Text -> Stmt
comment = UnsafeStmt . Raw.Comment

empty :: Stmt
empty = UnsafeStmt Raw.EmptyStmt

block :: NonEmpty Stmt -> Block
block = UnsafeBlock

script :: [Stmt] -> Script
script = UnsafeScript
