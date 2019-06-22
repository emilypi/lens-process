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
module System.Process.Microlens
( -- * Lenses
  cmdspec_
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
  -- * Classy Lenses
, HasStdin(..)
, HasStdout(..)
, HasStderr(..)
) where

import System.Process.Microlens.CreateProcess
