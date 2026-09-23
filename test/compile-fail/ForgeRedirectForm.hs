{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ForgeRedirectForm where

import Language.Fish.DSL

instance {-# OVERLAPPABLE #-} RedirectForm stream mode target
