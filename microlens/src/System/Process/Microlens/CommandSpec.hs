{-# LANGUAGE TypeFamilies #-}
-- |
-- Module       : System.Process.Lens.CommandSpec
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
-- This module provides the associated optics and combinators
-- for working with 'CommandSpec' objects. 'CommandSpec' consists of two
-- cases: a Shell command, which is a command to execute naively in the shell,
-- and a Raw command which is a command path together with its arguments.
--
-- 'CommandSpec' has two cases, and therefore a prism into those two cases.
-- There is also a convenient 'Traversal' available for working with the arglist
-- of a Raw command, as well as associated 'Review's for each prism, and combinators
-- for working with arguments monoidally.
--
-- We provide classy variants for all useful prisms
--
module System.Process.Microlens.CommandSpec
( -- * Traversals
  arguments
  -- * Combinators
, arguing
) where

import Lens.Micro
import System.Process


-- $setup
-- >>> import Lens.Micro
-- >>> import System.Process
-- >>> :set -XTypeApplications
-- >>> :set -XRank2Types

-- | 'Traversal'' into the arguments of a command
--
-- Examples:
--
-- >>> RawCommand "/bin/ls" ["-l"] ^. arguments
-- ["-l"]
--
arguments :: Traversal' CmdSpec [String]
arguments f c = case c of
  RawCommand fp args -> RawCommand fp <$> f args
  t -> pure t
-- | Append an argument to the argument list of a 'RawCommand'
--
-- Examples:
--
-- >>> arguing "-h" $ RawCommand "/bin/ls" ["-l"]
-- RawCommand "/bin/ls" ["-l","-h"]
--
-- >>> arguing "-h" (RawCommand "/bin/ls" ["-l"]) ^. arguments
-- ["-l","-h"]
--
arguing :: String -> CmdSpec -> CmdSpec
arguing s = arguments <>~ [s]
