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
module System.Process.Lens.CommandSpec
( -- * Traversals
  arguments
  -- * Prisms
, _ShellCommand
, _RawCommand
  -- * Classy Prisms
, IsShell(..)
, IsRaw(..)
  -- * Combinators
, arguing
, shellOf
, rawOf
) where

import Control.Lens
import System.Process


-- $setup
-- >>> import Control.Lens
-- >>> import System.Process
-- >>> :set -XTypeApplications
-- >>> :set -XRank2Types

-- ---------------------------------------------------------- --
-- Optics

-- | A prism into the 'ShellCommand' case of a 'CmdSpec'
--
-- Examples:
--
-- >>> _ShellCommand # "ls -l"
-- ShellCommand "ls -l"
--
-- >>> ShellCommand "ls -l" ^? _ShellCommand
-- Just "ls -l"
--
-- >>> RawCommand "/bin/ls" ["-l"] ^? _ShellCommand
-- Nothing
--
_ShellCommand :: Prism' CmdSpec String
_ShellCommand = prism' ShellCommand $ \c -> case c of
  ShellCommand s -> Just s
  _ -> Nothing

-- | A prism into the 'RawCommand' case of a 'CmdSpec'
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
_RawCommand :: Prism' CmdSpec (FilePath, [String])
_RawCommand = prism' (uncurry RawCommand) $ \c -> case c of
  RawCommand fp s -> Just (fp, s)
  _ -> Nothing

-- | Classy prism into the shell command of a 'CmdSpec'
--
-- Examples:
--
-- >>> f :: IsShell a => a -> Maybe String; f = preview _Shell
-- >>> f $ _ShellCommand # "ls -l"
-- Just "ls -l"
--
class IsShell a where
  _Shell :: Prism' a String
  {-# MINIMAL _Shell #-}

instance IsShell CmdSpec where
  _Shell = _ShellCommand

-- | Classy prism into the raw command of a 'CmdSpec'
--
-- Examples:
--
-- >>> f :: IsRaw a => a -> Maybe FilePath; f = preview (_Raw . _1)
-- >>> f $ _RawCommand # ("/bin/ls", ["ls -l"])
-- Just "/bin/ls"
--
class IsRaw a where
  _Raw :: Prism' a (FilePath, [String])
  {-# MINIMAL _Raw #-}

instance IsRaw CmdSpec where
  _Raw = _RawCommand

-- | 'Traversal'' into the arguments of a command
--
-- Examples:
--
-- >>> RawCommand "/bin/ls" ["-l"] ^. arguments
-- ["-l"]
--
arguments :: IsRaw a => Traversal' a [String]
arguments = _Raw . traverse

-- ---------------------------------------------------------- --
-- Combinators

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
arguing :: IsRaw a => String -> a -> a
arguing s = arguments <>~ [s]

-- | Lift a 'String' into a type via 'ShellCommand' with a prism into the
--
-- Examples:
--
-- >>> shellOf @CmdSpec "ls"
-- ShellCommand "ls"
--
shellOf :: IsShell a => String -> a
shellOf s = _Shell # s

-- | Lift a 'FilePath' and list of arguments into a type via 'RawCommand'
-- with a prism into the command
--
-- Examples:
--
-- >>> rawOf @CmdSpec "/bin/ls" ["-l"]
-- RawCommand "/bin/ls" ["-l"]
--
rawOf :: IsRaw a => FilePath -> [String] -> a
rawOf fp ss = _Raw # (fp,ss)
