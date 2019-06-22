{-# LANGUAGE LambdaCase #-}
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
( -- * Optics
  _ShellCommand
, _RawCommand
, arguments
  -- * Classes
, IsShell(..)
, IsRaw(..)
  -- * Combinators
, arguing
) where

import Control.Lens
import System.Process


-- ---------------------------------------------------------- --
-- Optics

-- | A prism into the 'ShellCommand' case of a 'CmdSpec'
--
_ShellCommand :: Prism' CmdSpec String
_ShellCommand = prism' ShellCommand $ \case
  ShellCommand s -> Just s
  _ -> Nothing

-- | A prism into the 'RawCommand' case of a 'CmdSpec'
--
_RawCommand :: Prism' CmdSpec (FilePath, [String])
_RawCommand = prism' (uncurry RawCommand) $ \case
  RawCommand fp s -> Just (fp, s)
  _ -> Nothing

-- | 'Traversal'' into the arguments of a command
--
arguments :: Traversal' CmdSpec [String]
arguments = _RawCommand . traverse

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

-- ---------------------------------------------------------- --
-- Combinators

-- | Append an argument to the argument list of a 'RawCommand'
--
arguing :: String -> CmdSpec -> CmdSpec
arguing s = arguments <>~ [s]
