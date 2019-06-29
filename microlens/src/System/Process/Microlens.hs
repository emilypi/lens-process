{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : System.Process.Microlens
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, RankNTypes
--
-- This module provides all of the optical exports, as well ask any associated
-- combinators. For just the optics, see 'System.Process.Microlens.Optics', or
-- if you are in need of something lighter weight, just for working with a
-- 'CreateProcess' in terms of getters and setters, see <https://hackage.haskell.org/package/microlens>
--
-- For more information on usage and how to work with lenses, see <http://github.com/ekmett/lens/wiki> and the tests for example uses. You can also ask questions on Freenode
-- in the #haskell-lens channel.
--
module System.Process.Microlens
( module System.Process.Microlens.CmdSpec
, module System.Process.Microlens.CreateProcess
, module System.Process.Microlens.ProcessHandler
, module System.Process.Microlens.StdStream
) where

import System.Process.Microlens.CmdSpec
import System.Process.Microlens.CreateProcess
import System.Process.Microlens.ProcessHandler
import System.Process.Microlens.StdStream
