{-# LANGUAGE Rank2Types #-}
-- |
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, RankNTypes
--
-- Everything all together
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
, closing
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
