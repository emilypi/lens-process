{-# LANGUAGE CPP #-}
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
( -- * Data
  ProcessHandler(..)
, -- * Prisms
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
, stdin_
, stdout_
, stderr_
, closefds
, creategroup
, delegatectlc
, newsession
#if MIN_VERSION_process(1, 3, 0)
, detachconsole
, createnewconsole
#endif
#if MIN_VERSION_process(1, 4, 0)
, childgroup
, childuser
#endif
#if MIN_VERSION_process(1, 5, 0)
, useprocessjobs
#endif
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
import System.Process.Lens.ProcessHandler
import System.Process.Lens.StdStream
