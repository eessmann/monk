{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module InternalPositive (inspectDense, inspectArtifact, inspectView, quotedExecutable, ownedLoopExit, inspectNative, materializeNormalized, ownedRegion, boundedPrimitive, ownedEntry, integerPrimitive, ownedFunction, standaloneLoop) where

import qualified Data.Functor.Identity as Identity
import qualified Data.Text as T
import Language.Bash.Arithmetic.Plan (BinaryOperator (Add))
import Language.Bash.Plan (ForBody, OwnedPlan, Statement (..), forBody)
import qualified Language.Bash.Plan as P
import qualified Language.Bash.Plan.Control as Control
import Language.Bash.Plan.Effects (nativeRegionStatement, proveNativeRegion)
import Language.Bash.Plan.Normalize.Control (Control, breakStatement, rootWitness, withLoopControl)
import Language.Bash.Plan.Normalize.State (DenseUpdate, FlowFacts, lookupDense, writeDenseAt)
import Language.Fish.DSL.Internal (CommandGrammar (..), ExprOrRedirect (..), FishCommand (..), FishExpr (..), FishStatement (..), FishType (..), Script, variableExecutable)
import Language.Fish.Translator.Emission (renderEmission)
import qualified Language.Fish.Translator.Primitive as Primitive
import qualified Language.Fish.Translator.Region as Region
import Monk.Compiler.Artifact (Artifact, ArtifactView, Phase (Admitted, Draft, Normalized), artifactEntry, materializeArtifact, viewScript)
import Monk.Compiler.Context (Context)
import Monk.Runtime.Abi2 (CliOpcode (CliSplit))
import Monk.Translation.Types (EntryMode (Standalone))

inspectDense :: FlowFacts world -> T.Text -> Int -> Maybe (DenseUpdate world)
inspectDense facts name index = do
  proof <- lookupDense facts name
  writeDenseAt facts proof index

inspectArtifact :: Artifact owner target entry provider Admitted -> Script
inspectArtifact = artifactEntry

inspectView :: ArtifactView owner target entry provider -> Script
inspectView = viewScript

quotedExecutable :: FishCommand Atomic TStatus
quotedExecutable = CommandExpr (variableExecutable "command") []

ownedLoopExit :: Control outer -> ForBody
ownedLoopExit control = withLoopControl control $ \target loop -> forBody (rootWitness loop) target [Statement Nothing (breakStatement target)]

-- A normalized owner can transition into a draft; it cannot bypass admission.
materializeNormalized :: OwnedPlan owner target entry provider Normalized -> Maybe (Artifact owner target entry provider Draft)
materializeNormalized = either (const Nothing) Just . materializeArtifact False

inspectNative :: Statement scope -> Maybe (Statement scope)
inspectNative statement = nativeRegionStatement <$> proveNativeRegion statement

-- Closing a region with its complete consumer retains its defining emission.
ownedRegion :: [FishStatement]
ownedRegion = Identity.runIdentity $ do
  value <- Region.runScalar (pure (Region.literalScalar "true"))
  pure (renderEmission (fmap (\valueExpression -> [Stmt (Command "printf" [ExprVal valueExpression])]) value))

boundedPrimitive :: FishStatement
boundedPrimitive = Primitive.primitiveStatement "runtime" request
  where
    request :: Primitive.Primitive CliSplit
    request = Primitive.SplitFields (ExprLiteral " ") (ExprLiteral "value")

standaloneLoop :: ForBody
standaloneLoop = Control.withEntryControl Standalone ownedLoopExit

-- Only the entry root minted from this context can seal its normalized body.
ownedEntry :: Context owner target entry provider -> OwnedPlan owner target entry provider Normalized
ownedEntry context = Control.withContextControl context $ \root _ -> P.sealSourcePlan context (P.entryBody root []) mempty

ownedFunction :: Control outer -> Statement outer
ownedFunction control = Control.withFunctionControl "f" control $ \root _ -> Statement Nothing (P.DefineFunction "f" (P.scopedBody root []))

integerPrimitive :: FishStatement
integerPrimitive = Primitive.primitiveStatement "runtime" (Primitive.BinaryInteger Add (ExprLiteral "1") (ExprLiteral "2"))
