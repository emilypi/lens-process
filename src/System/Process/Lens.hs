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
, inheriting
, piping
, handling
, nostreaming
, arguing
, rawOf
, shellOf
, usehandleOf
) where


import Control.Lens

import Data.Functor.Contravariant

import System.Process (CreateProcess(..))
import qualified System.Process as System
import System.Process.Lens.CommandSpec
import System.Process.Lens.CreateProcess
import System.Process.Lens.Internal
import System.Process.Lens.StdStream


createProcess :: CreateProcess -> IO ProcessHandler
createProcess = createProcess_ "createProcess"

createProcess_ :: String -> CreateProcess -> IO ProcessHandler
createProcess_ s cp = view (from _Handler) <$> System.createProcess_ s cp

shell :: String -> CreateProcess
shell s = defaultCreateProcess & cmdspec_ .~ shellOf s

proc :: FilePath -> [String] -> CreateProcess
proc fp s = defaultCreateProcess & cmdspec_ .~ rawOf fp s
