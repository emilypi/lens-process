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
, HasRaw(..)
, HasShell(..)
, HasInherit(..)
, HasUseHandle(..)
, HasCreatePipe(..)
, HasNoStream(..)
  -- * Combinators
, stdoutOf
, stdinOf
, stderrOf
, closing
, inheriting
, piping
, handling
, nostreaming
) where

import System.Process.Lens.CommandSpec
import System.Process.Lens.CreateProcess
import System.Process.Lens.StdStream
