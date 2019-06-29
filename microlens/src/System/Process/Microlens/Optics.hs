{-# LANGUAGE CPP #-}
-- |
-- Module       : System.Process.Microlens.Optics
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- Just the (classy) optics
--
module System.Process.Microlens.Optics
(  -- * Traversals
  _ShellCommand
, _RawCommand
, _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
, arguments
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

import System.Process.Microlens.CommandSpec
import System.Process.Microlens.CreateProcess
import System.Process.Microlens.ProcessHandler
import System.Process.Microlens.StdStream
