{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- |
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
  -- * Classes
, HasShell(..)
, HasRaw(..)
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

-- | Classy prism into the shell command of a 'CmdSpec'
--
class (a ~ CmdSpec) => HasShell a where
  _Shell :: Prism' a String
  _Shell = _ShellCommand

-- | Classy prism into the shell command of a 'CmdSpec'
--
class (a ~ CmdSpec) => HasRaw a where
  _Raw :: Prism' a (FilePath, [String])
  _Raw = _RawCommand

-- ---------------------------------------------------------- --
-- Combinators

-- | Append arguments to the argument list of a 'RawCommand'
--
arguing :: String -> CmdSpec -> CmdSpec
arguing s = over (_RawCommand . traverse) (<> [s])
