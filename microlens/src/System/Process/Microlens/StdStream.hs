{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : System.Process.Lens.StdStream
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- This module provides the associated optics and combinators
-- for working with 'StdStream' objects. 'StdStream' consists of four
-- cases, for which we provide prisms and classy variants, as well as
-- a single 'Review' for the only non-trivial 'Review' - 'UseHandle'.
--
module System.Process.Lens.StdStream
( -- * Prisms
  _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
  -- * Classy Prisms
, IsInherit(..)
, IsUseHandle(..)
, IsCreatePipe(..)
, IsNoStream(..)
  -- * Combinators
, usehandleOf
, inheriting
, piping
, handling
, nostreaming
) where

import Control.Lens

import System.IO (Handle)
import System.Process


-- $setup
-- >>> import Lens.Micro
-- >>> import qualified System.IO as System (stdin, stdout)
-- >>> import System.Process
-- >>> :set -XTypeApplications
-- >>> :set -XRank2Types

-- ---------------------------------------------------------- --
-- Combinators

-- | Given a lens into a 'StdStream', overwrite to 'Inherit' so that
-- the stream inherits from its parent process
--
-- Examples:
--
-- >>> inheriting ($) CreatePipe
-- Inherit
--
inheriting :: Lens' a StdStream -> a -> a
inheriting l = set l Inherit

-- | Given a lens into a 'StdStream', overwrite to 'CreatePipe'.
--
-- Examples:
--
-- >>> piping ($) NoStream
-- CreatePipe
--
piping :: Lens' a StdStream -> a -> a
piping l = set l CreatePipe

-- | Given a lens into a 'StdStream' and a handle, set the handle using
-- 'UseHandle'. Note that this is the only really interesting case for anything
-- with a lens into a handle inculding 'StdStream'.
--
-- Examples:
--
--
-- >>> handling ($) System.stdin $ UseHandle System.stdout
-- UseHandle {handle: <stdin>}
--
-- >>> handling ($) System.stdin NoStream
-- NoStream
--
-- >>> handling ($) System.stdin Inherit
-- Inherit
--
handling :: IsUseHandle b => Lens' a b -> Handle -> a -> a
handling l = set $ l . _UsesHandle

-- | Given a lens into a 'StdStream', set to 'NoStream'
--
-- Examples:
--
-- >>> nostreaming ($) Inherit
-- NoStream
--
nostreaming :: Lens' a StdStream -> a -> a
nostreaming l = set l NoStream
