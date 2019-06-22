-- |
-- Module       : Sysetem.Process.Lens.Optics
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- Just the (classy) optics
--
module System.Process.Lens.Optics
( -- * Prisms
  _ShellCommand
, _RawCommand
, _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
  -- * Isos
, _Handler
  -- * Lenses
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
, hstdin
, hstdout
, hstderr
, hhandle
  -- * Classy Prisms
, IsRaw(..)
, IsShell(..)
, IsInherit(..)
, IsUseHandle(..)
, IsCreatePipe(..)
, IsNoStream(..)
  -- * Classy Lenses
, HasStdin(..)
, HasStdout(..)
, HasStderr(..)
) where

import System.Process.Lens.CommandSpec
import System.Process.Lens.CreateProcess
import System.Process.Lens.Internal
import System.Process.Lens.StdStream
