{-# LANGUAGE Rank2Types #-}
-- |
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, RankNTypes
--
-- This module provides all of the optical exports, as well ask any associated
-- combinators. For just the optics, see 'System.Process.Lens.Optics', or
-- if you are in need of something lighter weight, just for working with a
-- 'CreateProcess' in terms of getters and setters, see <https://hackage.haskell.org/package/microlens>
--
-- For more information on usage and how to work with lenses, see <http://github.com/ekmett/lens/wiki> and the tests for example uses. You can also ask questions on Freenode
-- in the #haskell-lens channel.
--
module System.Process.Lens
( -- * Optics
  _ShellCommand
, _RawCommand
, arguments
, _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
, cmdspec_
, cwd_
, env_
, stdin
, stdout
, stderr
, closefds
, creategroup
, delegatectlc
, detachconsole
, newsession
, childgroup
, childuser
, useprocessjobs
  -- * Classes
, IsRaw(..)
, IsShell(..)
, IsInherit(..)
, IsUseHandle(..)
, IsCreatePipe(..)
, IsNoStream(..)
, HasStdin(..)
, HasStdout(..)
, HasStderr(..)
  -- * Combinators
, inheriting
, piping
, handling
, nostreaming
, arguing
, rawOf
, shellOf
, usehandleOf
) where


import System.Process.Lens.CommandSpec
import System.Process.Lens.CreateProcess
import System.Process.Lens.StdStream
