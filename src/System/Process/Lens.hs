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
, stdoutOf
, stdinOf
, stderrOf
, closing
, inheriting
, piping
, handling
, nostreaming
, arguing
, rawOf
, shellOf
, usehandleOf
  -- * System Process
, proc
, shell
) where

import Control.Lens

import System.Process (CreateProcess(..), CmdSpec(..))
import qualified System.Process as System
import System.Process.Lens.CommandSpec
import System.Process.Lens.CreateProcess
import System.Process.Lens.Internal
import System.Process.Lens.StdStream


proc :: FilePath -> [String] -> CreateProcess
proc cmd args = defaultCreateProcess & cmdspec_ .~ RawCommand cmd args

shell :: String -> CreateProcess
shell cmd = defaultCreateProcess & cmdspec_ .~ ShellCommand cmd
