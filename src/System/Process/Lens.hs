{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : System.Process.Lens
-- Copyright 	: (c) 2019-2021 Emily Pillmore
-- License	: BSD
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
module System.Process.Lens
( module System.Process.Lens.CmdSpec
, module System.Process.Lens.CreateProcess
, module System.Process.Lens.ProcessHandler
, module System.Process.Lens.StdStream
) where


import System.Process.Lens.CmdSpec
import System.Process.Lens.CreateProcess
import System.Process.Lens.ProcessHandler
import System.Process.Lens.StdStream
