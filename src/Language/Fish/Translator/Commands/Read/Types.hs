module Language.Fish.Translator.Commands.Read.Types
  ( ReadParseResult (..),
    ExactReadTarget (..),
    ExactReadDelimiter (..),
    ExactReadDelim (..),
    ScopedReadTarget,
    ExactReadCaptureStrategy (..),
  )
where

import Language.Fish.Translator.Syntax

data ReadParseResult = MkReadParseResult
  { readFlags :: [ReadFlag],
    readVars :: [Text],
    readIssues :: [Text],
    readUnsupported :: Bool,
    readRaw :: Bool
  }
  deriving stock (Show, Eq)

data ExactReadTarget
  = ExactReadArray Text
  | ExactReadVars [Text]
  deriving stock (Show, Eq)

data ExactReadDelimiter
  = ExactReadDelimited Text
  | ExactReadNull
  deriving stock (Show, Eq)

data ExactReadDelim = MkExactReadDelim
  { erdDelimiter :: ExactReadDelimiter,
    erdPrompt :: Maybe Text,
    erdSilent :: Bool,
    erdTimeout :: Maybe Text,
    erdNChars :: Maybe Text,
    erdFD :: Maybe Int,
    erdRaw :: Bool,
    erdTarget :: ExactReadTarget
  }
  deriving stock (Show, Eq)

type ScopedReadTarget = (Text, [SetFlag])

data ExactReadCaptureStrategy
  = CaptureViaPipeline
  | CaptureViaFile
  deriving stock (Show, Eq)
