{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Fish.AST.Common
  ( FishType (..),
    FunctionFlag (..),
    SetFlag (..),
    Conjunction (..),
    RedirectSource (..),
    RedirectOp (..),
    Decoration (..),
    SourcePos (..),
    SourceRange (..),
    SpecialVarRef (..),
    GlobPattern (..),
    GlobPart (..),
    StringOp (..),
    ReadFlag (..),
  )
where

data FishType
  = TStr
  | TInt
  | TBool
  | TList FishType
  | TStatus
  | TUnit
  deriving stock (Show, Eq)

-- | Function flags accepted by @function@.
data FunctionFlag
  = FuncDescription Text
  | FuncOnEvent Text
  | FuncOnVariable Text
  | FuncOnJobExit Text
  | FuncOnProcessExit Text
  | FuncWraps Text
  | FuncHelp
  | FuncInheritVariable
  | FuncUnknownFlag Text
  deriving stock (Show, Eq)

data SetFlag
  = SetLocal
  | SetFunction
  | SetGlobal
  | SetUniversal
  | SetExport
  | SetUnexport
  | SetAppend
  | SetPrepend
  | SetErase
  | SetPath
  | SetQuery
  deriving stock (Show, Eq)

-- | Conjunction keywords used by fish: @and@ / @or@.
data Conjunction
  = ConjAnd
  | ConjOr
  deriving stock (Show, Eq)

-- | Redirection source.
data RedirectSource
  = RedirectStdout
  | RedirectStderr
  | RedirectStdin
  | RedirectBoth
  | RedirectFD Int
  deriving stock (Show, Eq)

-- | Redirection operator.
data RedirectOp
  = RedirectOut
  | RedirectOutAppend
  | RedirectIn
  | RedirectClobber
  | RedirectReadWrite
  deriving stock (Show, Eq)

-- | Command decoration (@builtin@, @command@, @exec@).
data Decoration
  = DecBuiltin
  | DecCommand
  | DecExec
  deriving stock (Show, Eq)

-- | A source position in an input file (1-based line/column).
data SourcePos = SourcePos
  { srcFile :: Text,
    srcLine :: Int,
    srcColumn :: Int
  }
  deriving stock (Show, Eq, Ord)

-- | A source range with start and end positions.
data SourceRange = SourceRange
  { rangeStart :: SourcePos,
    rangeEnd :: SourcePos
  }
  deriving stock (Show, Eq, Ord)

-- | Typed special variables available in fish.
data SpecialVarRef (t :: FishType) where
  SVStatus :: SpecialVarRef TInt
  SVPipestatus :: SpecialVarRef (TList TInt)
  SVArgv :: SpecialVarRef (TList TStr)
  SVPID :: SpecialVarRef TInt
  SVLastPID :: SpecialVarRef TInt
  SVHostname :: SpecialVarRef TStr
  SVUser :: SpecialVarRef TStr
  SVHome :: SpecialVarRef TStr
  SVPWD :: SpecialVarRef TStr

deriving stock instance Show (SpecialVarRef t)

instance Eq (SpecialVarRef t) where
  SVStatus == SVStatus = True
  SVPipestatus == SVPipestatus = True
  SVArgv == SVArgv = True
  SVPID == SVPID = True
  SVLastPID == SVLastPID = True
  SVHostname == SVHostname = True
  SVUser == SVUser = True
  SVHome == SVHome = True
  SVPWD == SVPWD = True
  _ == _ = False

-- | A glob pattern composed of parts.
newtype GlobPattern = GlobPattern [GlobPart]
  deriving stock (Show, Eq)

-- | A component of a glob pattern.
data GlobPart
  = GlobLiteral Text
  | GlobStar
  | GlobStarStar
  | GlobQuestion
  | GlobCharClass Text
  | GlobBraces (NonEmpty Text)
  deriving stock (Show, Eq)

-- | String operations used by @string@.
data StringOp
  = StrLength
  | StrLower
  | StrUpper
  | StrEscape
  | StrUnescape
  | StrSplit Text
  | StrJoin Text
  | StrReplace Text Text
  | StrMatch Text
  deriving stock (Show, Eq)

-- | Flags for the @read@ builtin.
data ReadFlag
  = ReadPrompt Text
  | ReadLocal
  | ReadGlobal
  | ReadUniversal
  | ReadExport
  | ReadSilent
  | ReadArray
  | ReadNull
  | ReadDelimiter Text
  | ReadNChars Text
  | ReadTimeout Text
  | ReadFD Text
  deriving stock (Show, Eq)
