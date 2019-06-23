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


-- ---------------------------------------------------------- --
-- Optics

-- | A prism into the 'ShellCommand' case of a 'CmdSpec'
--
_ShellCommand :: Prism' CmdSpec String
_ShellCommand = prism' ShellCommand $ \c -> case c of
  ShellCommand s -> Just s
  _ -> Nothing

-- | A prism into the 'RawCommand' case of a 'CmdSpec'
--
_RawCommand :: Prism' CmdSpec (FilePath, [String])
_RawCommand = prism' (uncurry RawCommand) $ \c -> case c of
  RawCommand fp s -> Just (fp, s)
  _ -> Nothing

-- | Classy prism into the shell command of a 'CmdSpec'
--
class IsShell a where
  _Shell :: Prism' a String
  {-# MINIMAL _Shell #-}

instance IsShell CmdSpec where
  _Shell = _ShellCommand

-- | Classy prism into the shell command of a 'CmdSpec'
--
class IsRaw a where
  _Raw :: Prism' a (FilePath, [String])
  {-# MINIMAL _Raw #-}

instance IsRaw CmdSpec where
  _Raw = _RawCommand

-- | 'Traversal'' into the arguments of a command
--
arguments :: IsRaw a => Traversal' a [String]
arguments = _Raw . traverse

-- ---------------------------------------------------------- --
-- Combinators

-- | Append an argument to the argument list of a 'RawCommand'
--
arguing :: IsRaw a => String -> a -> a
arguing s = arguments <>~ [s]

-- | Lift a 'String' into a type via 'ShellCommand' with a prism into the
-- command
--
shellOf :: IsShell a => String -> a
shellOf s = _Shell # s

-- | Lift a 'FilePath' and list of arguments into a type via 'RawCommand'
-- with a prism into the command
--
rawOf :: IsRaw a => FilePath -> [String] -> a
rawOf fp ss = _Raw # (fp,ss)
