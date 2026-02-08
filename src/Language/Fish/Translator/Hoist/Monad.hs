module Language.Fish.Translator.Hoist.Monad
  ( HoistedM,
    hoistM,
    fromPairM,
    toPairM,
  )
where

import Language.Fish.AST (FishStatement)
import Language.Fish.Translator.Hoist (Hoisted, fromPair, hoist, toPair)
import Language.Fish.Translator.Monad (TranslateM)

type HoistedM a = TranslateM (Hoisted a)

hoistM :: [FishStatement] -> a -> HoistedM a
hoistM pre val = pure (hoist pre val)

fromPairM :: TranslateM ([FishStatement], a) -> HoistedM a
fromPairM = fmap fromPair

toPairM :: HoistedM a -> TranslateM ([FishStatement], a)
toPairM = fmap toPair
