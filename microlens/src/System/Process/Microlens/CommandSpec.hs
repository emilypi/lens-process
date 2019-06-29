{-# LANGUAGE TypeFamilies #-}
-- |
-- Module       : System.Process.Microlens.CommandSpec
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
-- 'CommandSpec' has two cases, and therefore a 'Traversal' into those two cases.
-- There is also a convenient 'Traversal' available for working with the arglist
-- of a Raw command and combinators for working with arguments monoidally.
--
-- We provide classy variants for all useful traversals
--
module System.Process.Microlens.CommandSpec
( -- * Traversals
  _RawCommand
, _ShellCommand
, arguments
  -- * Classy Traversals
, IsShell(..)
, IsRaw(..)
  -- * Combinators
, arguing
) where


import Control.Applicative

import Lens.Micro
import System.Process

-- $setup
-- >>> import Lens.Micro
-- >>> import System.Process
-- >>> :set -XTypeApplications
-- >>> :set -XRank2Types

-- | A 'Traversal'' into the 'ShellCommand' case of a 'CmdSpec'
--
-- Examples:
--
--
-- >>> ShellCommand "ls -l" ^? _ShellCommand
-- Just "ls -l"
--
-- >>> RawCommand "/bin/ls" ["-l"] ^? _ShellCommand
-- Nothing
--
_ShellCommand :: Traversal' CmdSpec String
_ShellCommand f c = case c of
  ShellCommand s -> fmap ShellCommand (f s)
  _ -> pure c

-- | A 'Traversal'' into the 'RawCommand' case of a 'CmdSpec'
--
-- Examples:
--
-- >>> RawCommand "/bin/ls" ["-l"] ^? _RawCommand
-- Just ("/bin/ls",["-l"])
--
-- >>> RawCommand "/bin/ls" ["-l"] ^? _ShellCommand
-- Nothing
--
-- >>> RawCommand "/bin/ls" ["-l"] ^. _RawCommand . _1
-- "/bin/ls"
--
-- >>> RawCommand "/bin/ls" ["-l"] ^. _RawCommand . _2
-- ["-l"]
--
_RawCommand :: Traversal' CmdSpec (FilePath, [String])
_RawCommand f c = case c of
  RawCommand fp s -> fmap (uncurry RawCommand) $ f (fp, s)
  _ -> pure c

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
arguments = _RawCommand . traverse

-- | Classy 'Traversal'' into the shell command of a 'CmdSpec'
--
class IsShell a where
  _Shell :: Traversal' a String
  {-# MINIMAL _Shell #-}

instance IsShell CmdSpec where
  _Shell = _ShellCommand

-- | Classy 'Traversal'' into the raw command of a 'CmdSpec'
--
class IsRaw a where
  _Raw :: Traversal' a (FilePath, [String])
  {-# MINIMAL _Raw #-}

instance IsRaw CmdSpec where
  _Raw = _RawCommand


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
