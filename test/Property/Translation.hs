{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Property.Translation
  ( propertyTranslationTests,
  )
where

import Data.Text qualified as T
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
import TestSupport

propertyTranslationTests :: TestTree
propertyTranslationTests =
  testGroup
    "Translation properties"
    [ QC.testProperty "Translate array index uses 1-based indexing" $
        QC.forAll (QC.chooseInt (0, 5)) $ \n ->
          QCM.monadicIO $ do
            let script = "echo ${arr[" <> T.pack (show n) <> "]}"
                expected = "$arr[" <> T.pack (show (n + 1)) <> "]"
            out <- QCM.run (translateScript script)
            QCM.assert (T.isInfixOf expected out),
      QC.testProperty "Translate array assignment uses 1-based indexing" $
        QC.forAll (QC.chooseInt (0, 5)) $ \n ->
          QCM.monadicIO $ do
            let script = "arr[" <> T.pack (show n) <> "]=foo"
                expected = "arr[" <> T.pack (show (n + 1)) <> "]"
            out <- QCM.run (translateScript script)
            QCM.assert (T.isInfixOf expected out),
      QC.testProperty "Substring expansion uses 1-based start" $
        QC.forAll (QC.chooseInt (0, 5)) $ \n ->
          QCM.monadicIO $ do
            let script = "echo ${var:" <> T.pack (show n) <> ":2}"
                expected = "'--start' " <> T.pack (show (n + 1))
            out <- QCM.run (translateScript script)
            QCM.assert (T.isInfixOf expected out),
      QC.testProperty "Case modification uses string upper/lower" $
        QC.forAll QC.arbitrary $ \upper ->
          QCM.monadicIO $ do
            let script = if upper then "echo ${var^^}" else "echo ${var,,}"
                expected = if upper then "string upper" else "string lower"
            out <- QCM.run (translateScript script)
            QCM.assert (T.isInfixOf expected out),
      QC.testProperty "Double bracket equality uses string match -q" $
        QC.forAll genSimpleWord $ \pat ->
          QCM.monadicIO $ do
            let script = "if [[ $x == " <> pat <> " ]]; then echo ok; fi"
            out <- QCM.run (translateScript script)
            QCM.assert (T.isInfixOf "string 'match' '-q' '--'" out),
      QC.testProperty "Arithmetic command emits math with redirect" $
        QC.forAll (QC.chooseInt (0, 5)) $ \n ->
          QCM.monadicIO $ do
            let script = "((" <> T.pack (show n) <> "))"
            out <- QCM.run (translateScript script)
            QCM.assert (T.isInfixOf "math" out && T.isInfixOf "/dev/null" out),
      QC.testProperty "Double bracket boolean trees preserve match/not counts" $
        QC.forAll genCond $ \cond ->
          QCM.monadicIO $ do
            let script = "if [[ " <> renderCond cond <> " ]]; then echo ok; fi"
                (eqCount, regexCount, notCount) = condCounts cond
            outMaybe <- QCM.run (translateScriptMaybe script)
            case outMaybe of
              Nothing -> QCM.pre False
              Just out -> do
                let matchQ = T.count "string 'match' '-q' '--'" out
                    matchQR = T.count "string 'match' '-qr' '--'" out
                    nots = T.count "not " out
                QCM.assert (matchQ == eqCount && matchQR == regexCount && nots == notCount)
    ]

-- Generators for boolean [[ ... ]] shapes ------------------------------------

data Cond
  = CAtom Atom
  | CNot Cond
  | CAnd Cond Cond
  | COr Cond Cond
  | CParens Cond
  deriving stock (Eq, Show)

data Atom
  = AtomEq Text Text
  | AtomRegex Text Text
  deriving stock (Eq, Show)

genVarName :: QC.Gen Text
genVarName = QC.elements ["x", "y", "z", "foo"]

genSimpleWord :: QC.Gen Text
genSimpleWord =
  let chars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['*', '_']
   in T.pack <$> QC.listOf1 (QC.elements chars)

genGlobPattern :: QC.Gen Text
genGlobPattern = do
  base <- genSimpleWord
  QC.frequency
    [ (3, pure base),
      (1, pure (base <> "*")),
      (1, pure ("*" <> base))
    ]

genRegexPattern :: QC.Gen Text
genRegexPattern =
  QC.elements
    [ "^foo",
      "bar[0-9]+",
      "^baz$",
      "qux.*"
    ]

genAtom :: QC.Gen Atom
genAtom =
  QC.frequency
    [ (3, AtomEq <$> genVarName <*> genGlobPattern),
      (2, AtomRegex <$> genVarName <*> genRegexPattern)
    ]

genCond :: QC.Gen Cond
genCond = QC.sized go
  where
    go 0 = CAtom <$> genAtom
    go n =
      QC.frequency
        [ (4, CAtom <$> genAtom),
          (2, CNot <$> go (n - 1)),
          (2, CParens <$> go (n - 1)),
          (2, CAnd <$> go (n `div` 2) <*> go (n `div` 2)),
          (2, COr <$> go (n `div` 2) <*> go (n `div` 2))
        ]

renderCond :: Cond -> Text
renderCond = \case
  CAtom atom -> renderAtom atom
  CNot c -> "! " <> wrap c
  CAnd a b -> wrap a <> " && " <> wrap b
  COr a b -> wrap a <> " || " <> wrap b
  CParens c -> "( " <> renderCond c <> " )"
  where
    renderAtom = \case
      AtomEq var pat -> "$" <> var <> " == " <> pat
      AtomRegex var pat -> "$" <> var <> " =~ " <> pat

    wrap c =
      case c of
        CAtom {} -> renderCond c
        CNot {} -> renderCond c
        CParens {} -> renderCond c
        _ -> "( " <> renderCond c <> " )"

condCounts :: Cond -> (Int, Int, Int)
condCounts = \case
  CAtom (AtomEq _ _) -> (1, 0, 0)
  CAtom (AtomRegex _ _) -> (0, 1, 0)
  CNot c ->
    let (eqC, reC, notC) = condCounts c
     in (eqC, reC, notC + 1)
  CAnd a b -> sumCounts (condCounts a) (condCounts b)
  COr a b -> sumCounts (condCounts a) (condCounts b)
  CParens c -> condCounts c
  where
    sumCounts (a1, b1, c1) (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)
