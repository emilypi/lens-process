{-# LANGUAGE Rank2Types #-}
-- |
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
{-# LANGUAGE CPP #-}
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
module System.Process.Microlens
( -- * Lenses
  cmdspec_
, cwd_
, env_
, stdin_
, stdout_
, stderr_
, closefds
, creategroup
, delegatectlc
#if MIN_VERSION_process(1, 3, 0)
, detachconsole
, createnewconsole
, newsession
#endif
#if MIN_VERSION_process(1, 4, 0)
, childgroup
, childuser
#endif
#if MIN_VERSION_process(1, 5, 0)
, useprocessjobs
#endif
  -- * Traversals
, arguments
  -- * Classy Lenses
, HasStdin(..)
, HasStdout(..)
, HasStderr(..)
  -- * Combinators
, arguing
) where

import System.Process.Microlens.CommandSpec
import System.Process.Microlens.CreateProcess
