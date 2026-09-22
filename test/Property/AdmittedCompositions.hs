module Property.AdmittedCompositions (admittedCompositionTests) where

import Data.Text qualified as T
import Monk.Translation
import ShellSupport
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

-- Every generated program is in the mandatory useful core. Admission failures
-- are failures, never discarded samples; shrinking preserves that distinction.
newtype Program = MkProgram [Step]
  deriving stock (Show)

data Step
  = Print Text
  | Split Text
  | Arithmetic Int Int
  | Conditional Bool
  | Fallthrough
  | FunctionCall Text
  | ArrayPrint [Text]
  | ReadInput Text
  | BackgroundExit Int
  | Group [Step]
  | Isolated [Step]
  deriving stock (Show)

admittedCompositionTests :: TestTree
admittedCompositionTests =
  testGroup
    "Admitted generated compositions"
    [ QC.testProperty "bounded compositions preserve output/status with shrinking" $
        QC.withMaxSuccess 100 $
          QC.forAllShrink genProgram shrinkProgram $ \program -> QCM.monadicIO $ do
            enabled <- QCM.run shouldRunIntegration
            case enabled of
              Left reason -> do
                QCM.monitor (QC.label ("SKIPPED: " <> reason))
                QCM.assert True
              Right () -> do
                let source = renderProgram program
                QCM.monitor (QC.counterexample ("Bash program:\n" <> toString source))
                translated <- QCM.run (translateBashScript strictConfig "generated-composition.bash" source)
                case translated of
                  Left failure -> do
                    QCM.monitor (QC.counterexample ("ADMISSION_REGRESSION: " <> show failure))
                    QCM.assert False
                  Right result -> do
                    environment <- QCM.run prepareEnv
                    bash <- QCM.run (runShellWithMode ShellRunExec ShellBash environment source [] "")
                    fish <- QCM.run (runShellWithMode ShellRunExec ShellFish environment (renderTranslation result) [] "")
                    let equal = observations bash == observations fish
                        clean = null (translationDiagnostics result)
                    QCM.monitor (QC.tabulate "admission" [if clean then "zero diagnostics" else "diagnosed"])
                    QCM.monitor
                      ( QC.counterexample
                          ( (if clean then "ZERO_DIAGNOSTIC_MISMATCH" else "DIAGNOSED_MISMATCH")
                              <> "\nBash: "
                              <> show (observations bash)
                              <> "\nFish: "
                              <> show (observations fish)
                              <> "\nGenerated Fish:\n"
                              <> toString (T.take 16000 (renderTranslation result))
                              <> "\n[Fish preview bounded to 16000 characters]"
                          )
                      )
                    QCM.assert equal
    ]
  where
    observations result = (rrExit result, rrStdout result, rrStderr result)

genProgram :: QC.Gen Program
genProgram = do
  count <- QC.chooseInt (1, 5)
  MkProgram <$> QC.vectorOf count (genStep 2)

genStep :: Int -> QC.Gen Step
genStep depth =
  QC.frequency
    ( [ (4, Print <$> word),
        (2, Split <$> QC.elements ["", "a:b", "a::b:", ":a", "a b"]),
        (2, Arithmetic <$> QC.chooseInt (-20, 20) <*> QC.chooseInt (1, 9)),
        (2, Conditional <$> QC.arbitrary),
        (1, pure Fallthrough),
        (1, FunctionCall <$> word),
        (2, ArrayPrint <$> (QC.chooseInt (0, 3) >>= \count -> QC.vectorOf count word)),
        (2, ReadInput <$> word),
        (1, BackgroundExit <$> QC.chooseInt (0, 3))
      ]
        <> [(2, constructor <$> nested) | depth > 0, constructor <- [Group, Isolated]]
    )
  where
    nested = QC.chooseInt (1, 3) >>= \count -> QC.vectorOf count (genStep (depth - 1))
    word = QC.elements ["", "one", "two words", "a:b", "line\nbreak", "literal *"]

shrinkProgram :: Program -> [Program]
shrinkProgram (MkProgram steps) = [MkProgram values | values <- QC.shrinkList shrinkStep steps, not (null values)]
  where
    shrinkStep (Print value) = [Print "" | not (T.null value)]
    shrinkStep (Split value) = [Split "" | not (T.null value)]
    shrinkStep (Arithmetic a b) = [Arithmetic x y | (x, y) <- QC.shrink (a, b), y > 0, abs x <= 20, y <= 9]
    shrinkStep (FunctionCall value) = [FunctionCall "" | not (T.null value)]
    shrinkStep (ArrayPrint values) = [ArrayPrint smaller | smaller <- QC.shrinkList (const [""]) values, smaller /= values]
    shrinkStep (ReadInput value) = [ReadInput "" | not (T.null value)]
    shrinkStep (BackgroundExit code) = [BackgroundExit 0 | code /= 0]
    shrinkStep (Group values) = values <> [Group smaller | smaller <- QC.shrinkList shrinkStep values, not (null smaller)]
    shrinkStep (Isolated values) = [Isolated smaller | smaller <- QC.shrinkList shrinkStep values, not (null smaller)]
    shrinkStep _ = []

renderProgram :: Program -> Text
renderProgram (MkProgram steps) = T.unlines (map renderStep steps)

renderStep :: Step -> Text
renderStep = \case
  Print value -> "x=" <> quote value <> "; printf '<%s>\\n' \"$x\""
  Split value -> "IFS=:; x=" <> quote value <> "; printf '<%s>\\n' $x; IFS=' \t\n'"
  Arithmetic a b -> "printf '%s\\n' \"$(( (" <> show a <> ") / " <> show b <> " * " <> show b <> "))\""
  Conditional flag -> "if " <> (if flag then "true" else "false") <> "; then printf 'yes\\n'; else printf 'no\\n'; fi"
  Fallthrough -> "case x in x) printf 'a\\n' ;& y) printf 'b\\n' ;;& x) printf 'c\\n' ;; esac"
  FunctionCall value -> "visit() { local x=" <> quote value <> "; printf '<%s>\\n' \"$x\"; }; visit"
  ArrayPrint values -> "items=(" <> T.unwords (map quote values) <> "); printf '<%s>\\n' \"${items[@]}\""
  ReadInput value -> "read -r item <<< " <> quote value <> "; printf '<%s>\\n' \"$item\""
  BackgroundExit code -> "(exit " <> show code <> ") & task=$!; wait \"$task\"; printf 'job:%s\\n' \"$?\""
  Group body -> "{ " <> T.intercalate "; " (map renderStep body) <> "; }"
  Isolated body -> "( " <> T.intercalate "; " (map renderStep body) <> " )"
  where
    quote value = "'" <> T.replace "'" "'\\''" value <> "'"
