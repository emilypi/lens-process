{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : System.Process.Microlens.StdStream
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- This module provides the associated optics and combinators
-- for working with 'StdStream' objects. 'StdStream' consists of four
-- cases, for which we provide traversals for each case
--
module System.Process.Microlens.StdStream
( -- * Traversals
  _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
  -- * Classy Traversals
, IsCreatePipe(..)
, IsInherit(..)
, IsUseHandle(..)
, IsNoStream(..)
  -- * Combinators
, inheriting
, piping
, handling
, nostreaming
) where


import Control.Applicative

import Lens.Micro

import System.IO (Handle)
import System.Process


-- $setup
-- >>> import Lens.Micro
-- >>> import qualified System.IO as System (stdin, stdout)
-- >>> import System.Process
-- >>> :set -XTypeApplications
-- >>> :set -XRank2Types

-- ---------------------------------------------------------- --
-- Traversals

-- | A 'Traversal'' into the 'Inherit' structure of a 'StdStream'
--
_Inherit :: Traversal' StdStream StdStream
_Inherit f s = case s of
  Inherit -> f s
  _ -> pure s

-- | A 'Traversal'' into the 'UseHandle' structure's Handle for a 'StdStream'
--
_UseHandle :: Traversal' StdStream Handle
_UseHandle f s = case s of
  UseHandle h -> fmap UseHandle (f h)
  _ -> pure s

-- | A 'Traversal'' into the 'CreatePipe' structure of a 'StdStream'
--
_CreatePipe :: Traversal' StdStream StdStream
_CreatePipe f s = case s of
  CreatePipe -> f s
  _ -> pure s

-- | A 'Traversal'' into the 'NoStream' structure of a 'StdStream'
--
_NoStream :: Traversal' StdStream StdStream
_NoStream f s = case s of
  NoStream -> f s
  _ -> pure s

-- ---------------------------------------------------------- --
-- Classes

-- | Class constraint proving a type has a prism into an 'Inherit'
-- structure. Any 'StdStream' will have a prism into `Inherit' -
-- it is just an overwrite to 'Inherit'
--
class IsInherit a where
  _Inherits :: Traversal' a StdStream
  {-# MINIMAL _Inherits #-}

instance IsInherit StdStream where
  _Inherits = _Inherit

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure.
--
class IsUseHandle a where
  _UsesHandle :: Traversal' a Handle
  {-# MINIMAL _UsesHandle #-}

instance IsUseHandle StdStream where
  _UsesHandle = _UseHandle

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure. Any 'StdStream' will have a prism into
-- 'CreatePipe' - it is just an overwrite to 'CreatePipe'
--
class IsCreatePipe a where
  _CreatesPipe :: Traversal' a StdStream
  {-# MINIMAL _CreatesPipe #-}

instance IsCreatePipe StdStream where
  _CreatesPipe = _CreatePipe

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure. Any 'StdStream' will have a prism into
-- 'NoStream' - it is just an overwrite to 'NoStream'.
--
class IsNoStream a where
  _NoStreams :: Traversal' a StdStream
  {-# MINIMAL _NoStreams #-}

instance IsNoStream StdStream where
  _NoStreams = _NoStream

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
-- >>> handling ($) System.stdout Inherit
-- UseHandle {handle: <stdout>}
--
handling :: Lens' a StdStream -> Handle -> a -> a
handling l h = set l (UseHandle h)

-- | Given a lens into a 'StdStream', set to 'NoStream'
--
-- Examples:
--
-- >>> nostreaming ($) Inherit
-- NoStream
--
nostreaming :: Lens' a StdStream -> a -> a
nostreaming l = set l NoStream
