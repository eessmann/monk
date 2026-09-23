module Unit.PlanEffects (unitPlanEffectTests) where

import Data.Set qualified as S
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Effects qualified as Effects
import Monk.Translation.Types (EntryMode (Standalone))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlanEffectTests :: TestTree
unitPlanEffectTests =
  testGroup
    "Finite plan effects"
    [ H.testCase "effect set preserves conservative option transitions" $ do
        let effects = Effects.statementEffects (statement (P.SetOption P.Errexit False)) <> Effects.statementEffects (statement (P.SetOption P.Pipefail False))
        Effects.effectKinds effects H.@?= S.fromList [Effects.MayEnableErrexit, Effects.Pipefail]
        H.assertBool "disabling errexit still requests its modeled state" (Effects.effectMayEnableErrexit effects)
        H.assertBool "pipefail transition remains conservative" (Effects.effectPipefail effects),
      H.testCase "output region proof is bound to the original statement" $ do
        let original = statement (P.Invoke (P.Builtin "printf") [P.OneField (P.Literal "%s"), P.OneField (P.Variable "value")])
        Effects.effectKinds (Effects.statementEffects original) H.@?= S.singleton Effects.Output
        Effects.effectReads (Effects.statementEffects original) H.@?= S.singleton "value"
        case Effects.proveNativeRegion original of
          Nothing -> H.assertFailure "scalar reads and byte output are native candidates"
          Just proof -> Effects.nativeRegionStatement proof H.@?= original,
      H.testCase "native capability refuses writes, array reads, functions and environment" $ do
        let nodes = [P.Assign P.Global "x" (P.Literal "1"), P.Invoke (P.Function "f") [], P.Invoke (P.External "true") [], P.Invoke (P.Builtin "printf") [P.OneField (P.ArrayElement "values" 0)], P.SetOption P.Errexit False]
        forM_ nodes $ \node -> H.assertBool "effectful node has no native region proof" (isNothing (Effects.proveNativeRegion (statement node))),
      H.testCase "child closure retains flags while owning its variable snapshot" $ do
        let child = P.MkChildRegion Nothing (Control.withEntryControl Standalone $ \entry -> Control.withChildControl entry $ \root _ -> P.scopedBody root [statement (P.Assign P.Global "local_write" (P.Literal "1")), statement (P.SetOption P.Errexit False), statement (P.Invoke (P.External "cat") [])]) mempty (S.singleton "captured") (S.singleton "values") False
            effects = Effects.statementEffects (statement (P.Subshell child))
        Effects.effectReads effects H.@?= S.singleton "captured"
        Effects.effectWrites effects H.@?= mempty
        Effects.effectArrays effects H.@?= S.singleton "values"
        H.assertBool "closed child environment certificate remains authoritative" (not (Effects.effectExternalEnvironment effects))
        Effects.effectKinds effects H.@?= S.fromList [Effects.MayEnableErrexit, Effects.Output],
      H.testCase "nested effect union retains read and write identities" $ do
        let effects = Effects.statementEffects (statement (P.Sequence [statement (P.Assign P.Global "x" (P.Variable "y")), statement (P.Invoke (P.External "cat") [])]))
        Effects.effectWrites effects H.@?= S.singleton "x"
        Effects.effectReads effects H.@?= S.singleton "y"
        Effects.effectKinds effects H.@?= S.fromList [Effects.ExternalEnvironment, Effects.Output]
    ]

statement :: P.StatementNode scope -> P.Statement scope
statement = P.Statement Nothing
