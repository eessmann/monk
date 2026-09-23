{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module ForgeBackgroundGrammar where

import Language.Fish.DSL

instance BackgroundGrammar Asynchronous
