{-# LANGUAGE OverloadedStrings #-}

module Unit.Refactor
  ( unitRefactorTests,
  )
where

import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import TestSupport (translateScript)

unitRefactorTests :: TestTree
unitRefactorTests =
  testGroup
    "Refactor Seams"
    [ H.testCase "Null-delimited array reads still lower through the exact helper path" $ do
        out <- translateScript "read -d '' -a items"
        T.isInfixOf "__monk_read_capture_delim" out H.@? "expected capture helper"
        T.isInfixOf "__monk_read_assign_array" out H.@? "expected array assignment helper",
      H.testCase "Delimited multi-var reads keep the generalized assignment helper path" $ do
        out <- translateScript "IFS=: read -u 3 -d : left right"
        T.isInfixOf "__monk_read_capture_delim" out H.@? "expected capture helper"
        T.isInfixOf "__monk_read_assign_vars" out H.@? "expected multi-var assignment helper",
      H.testCase "Default assignment expansions still lower through conditional set logic" $ do
        out <- translateScript "echo ${name=value}"
        T.isInfixOf "set '-q' 'name'" out H.@? "expected set-check guard"
        T.isInfixOf "set --global name" out H.@? "expected assignment branch",
      H.testCase "Case-modification expansions still lower through string helpers" $ do
        out <- translateScript "echo ${name^^}"
        T.isInfixOf "string upper" out H.@? "expected case-modifier lowering"
    ]
