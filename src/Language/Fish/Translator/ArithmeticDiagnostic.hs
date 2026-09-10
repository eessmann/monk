{-# LANGUAGE DataKinds #-}

-- | Emit the exact source diagnostic selected by the first failing operation.
-- The caller owns expansion-fatal versus arithmetic-command control routing.
module Language.Fish.Translator.ArithmeticDiagnostic
  ( arithmeticDiagnostic,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Bash.Arithmetic.Plan (ArithmeticOrigin (..))
import Language.Bash.Arithmetic.Source (ArithmeticSite, arithmeticSiteMessages)
import Language.Fish.DSL.Internal
import Language.Fish.Translator.ArithmeticPlan

arithmeticDiagnostic :: Bool -> ArithmeticSite -> ArithmeticMaterialization -> [FishStatement]
arithmeticDiagnostic command site materialized =
  [ conditional (equal (arithmeticErrorOrigin materialized) (ExprLiteral (show tokenIdentity)))
      $ conditional (equal (arithmeticError materialized) (ExprLiteral code))
      $ Stmt
        ( Decorated DecBuiltin $
            Command
              "printf"
              [ ExprVal (ExprLiteral "%s"),
                ExprVal (ExprLiteral message),
                RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))
              ]
        )
  | (MkArithmeticOrigin tokenIdentity, code, message) <- arithmeticSiteMessages command site
  ]
  where
    equal left right = Decorated DecBuiltin (Command "test" [ExprVal left, ExprVal (ExprLiteral "="), ExprVal right])
    conditional condition body =
      Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] (Stmt condition) [] False) [] NE.:| [])) (body NE.:| []) [] [])
