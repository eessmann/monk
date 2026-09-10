module ForgeResult where

import Monk.AST (script)
import Monk.Translation

forged :: TranslationResult
forged = MkTranslationResult (script []) [] []
