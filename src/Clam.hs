-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- See README for more info
module Clam
  ( projectName,
    module ClamPrettyPrinter,
    module BashParser,
    module ShellToFish
  )
where
import BashParser
import ClamPrettyPrinter
import ShellToFish


projectName :: Text
projectName = "clam"
