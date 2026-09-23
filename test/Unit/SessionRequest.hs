{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Unit.SessionRequest (unitSessionRequestTests) where

import Data.List.NonEmpty qualified as NE
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Emission (emit, renderEmission)
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Session.Request qualified as Request
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitSessionRequestTests :: TestTree
unitSessionRequestTests =
  testGroup
    "Complete session request templates"
    [ H.testCase "open preserves site, descriptor, mode and empty path frames" $
        expressions (Request.FdOpen site 9 Request.AppendFile (ExprLiteral "")) H.@?= literals ["fd-open", "source name.sh", "12", "9", "append", ""],
      H.testCase "read preserves delimiters, IFS and exact destination cardinality" $
        expressions (Request.Read site 0 True "" Nothing (ExprLiteral " \t\n") (Request.ScalarVariables ("first" :| ["second"]))) H.@?= literals ["read", "source name.sh", "12", "0", "1", "", "-1", " \t\n", "scalar", "2"],
      H.testCase "external argument fields retain empty scalar and list expansion" $ do
        let arguments = [SomeArgument (ScalarArgument (ExprLiteral "")), SomeArgument (ListArgument (ExprVariable (VarAll "argv")))]
            request = Request.Run (Request.singleBody (Request.ExternalSiteStage site (ExprLiteral "program") arguments))
        expressions request H.@?= literals ["run", "external-site", "source name.sh", "12", "program", ""] <> [ExprVal (ExprVariable (VarAll "argv"))],
      H.testCase "snapshot keeps the child script structural and ordered" $ do
        let child = MkScript [Stmt (Command "printf" [ExprVal (ExprLiteral "child")])]
            request = Request.Spawn (Request.singleBody (Request.SnapshotStage child (ExprLiteral "2") [SomeArgument (ScalarArgument (ExprLiteral ""))]))
        expressions request H.@?= literals ["spawn", "snapshot"] <> [ExprVal (ExprEmbeddedScript child)] <> literals ["2", ""],
      H.testCase "pipeline captures each complete stage after its own operand effects" $ do
        let marker text = Stmt (Command "marker" [ExprVal (ExprLiteral text)])
            firstStage = emit [marker "first"] >> pure (Request.SomeStage (Request.ExternalStage (ExprLiteral "one") []))
            secondStage = emit [marker "second"] >> pure (Request.SomeStage (Request.ExternalStage (ExprLiteral "two") []))
            output = renderEmission $ do
              a <- Request.prepareStage "first_stage" firstStage
              b <- Request.prepareStage "second_stage" secondStage
              pure [Session.request "test_" (Request.Run (Request.pipelineBody (ExprLiteral "0") (a :| [b])))]
        case output of
          [beforeFirst, Stmt (Decorated DecBuiltin (Command "set" _)), beforeSecond, Stmt (Decorated DecBuiltin (Command "set" _)), Stmt (Command "test_session_request" fields)] -> do
            beforeFirst H.@?= marker "first"
            beforeSecond H.@?= marker "second"
            take 4 fields H.@?= literals ["run", "pipeline", "0", "2"]
          _ -> H.assertFailure "stage capture escaped or changed operand evaluation order"
    ]
  where
    site = Request.Site (ExprLiteral "source name.sh") (ExprLiteral "12")

expressions :: Request.Request operation -> [ExprOrRedirect]
expressions = map argumentExpression . NE.toList . Request.requestArguments

literals :: [Text] -> [ExprOrRedirect]
literals = map (ExprVal . ExprLiteral)
