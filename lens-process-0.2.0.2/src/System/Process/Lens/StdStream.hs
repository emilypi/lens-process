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
-- >>> import Control.Lens
-- >>> import qualified System.IO as System (stdin, stdout)
-- >>> import System.Process
-- >>> :set -XTypeApplications
-- >>> :set -XRank2Types

-- ---------------------------------------------------------- --
-- Optics

-- | A prism into the 'Inherit' structure of a 'StdStream'
--
-- Examples:
--
-- >>> _Inherit # CreatePipe
-- Inherit
--
_Inherit :: Prism' StdStream StdStream
_Inherit = prism' (const Inherit) $ \s -> case s of
  Inherit -> Just Inherit
  _ -> Nothing

-- | A prism into the 'UseHandle' structure's Handle for a 'StdStream'
--
-- Examples:
--
--
-- >>> _UseHandle # System.stdin
-- UseHandle {handle: <stdin>}
--
_UseHandle :: Prism' StdStream Handle
_UseHandle = prism' UseHandle $ \s -> case s of
  UseHandle t -> Just t
  _ -> Nothing

-- | A prism into the 'CreatePipe' structure of a 'StdStream'
--
-- Examples:
--
-- >>> _CreatePipe # Inherit
-- CreatePipe
--
_CreatePipe :: Prism' StdStream StdStream
_CreatePipe = prism' (const CreatePipe) $ \s -> case s of
  CreatePipe -> Just CreatePipe
  _ -> Nothing

-- | A prism into the 'NoStream' structure of a 'StdStream'
--
-- Examples:
--
-- >>> _NoStream # CreatePipe
-- NoStream
--
_NoStream :: Prism' StdStream StdStream
_NoStream = prism' (const NoStream) $ \s -> case s of
  NoStream -> Just NoStream
  _ -> Nothing

-- ---------------------------------------------------------- --
-- Classes

-- | Class constraint proving a type has a prism into an 'Inherit'
-- structure. Any 'StdStream' will have a prism into `Inherit' -
-- it is just an overwrite to 'Inherit'
--
class IsInherit a where
  _Inherits :: Prism' a StdStream
  {-# MINIMAL _Inherits #-}

instance IsInherit StdStream where
  _Inherits = _Inherit

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure.
--
class IsUseHandle a where
  _UsesHandle :: Prism' a Handle
  {-# MINIMAL _UsesHandle #-}

instance IsUseHandle StdStream where
  _UsesHandle = _UseHandle

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure. Any 'StdStream' will have a prism into
-- 'CreatePipe' - it is just an overwrite to 'CreatePipe'
--
class IsCreatePipe a where
  _CreatesPipe :: Prism' a StdStream
  {-# MINIMAL _CreatesPipe #-}

instance IsCreatePipe StdStream where
  _CreatesPipe = _CreatePipe

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure. Any 'StdStream' will have a prism into
-- 'NoStream' - it is just an overwrite to 'NoStream'.
--
class IsNoStream a where
  _NoStreams :: Prism' a StdStream
  {-# MINIMAL _NoStreams #-}

instance IsNoStream StdStream where
  _NoStreams = _NoStream

-- ---------------------------------------------------------- --
-- Combinators

-- | Inject a handle into something with a prism into the handle
--
-- Examples:
--
-- >>> usehandleOf @StdStream System.stdin
-- UseHandle {handle: <stdin>}
--
usehandleOf :: IsUseHandle a => Handle -> a
usehandleOf h = _UsesHandle # h

-- | Given a lens into a 'StdStream', overwrite to 'Inherit' so that
-- the stream inherits from its parent process
--
-- Examples:
--
-- >>> inheriting ($) CreatePipe
-- Inherit
--
inheriting :: IsInherit a => Lens' a StdStream -> a -> a
inheriting l = set l Inherit

-- | Given a lens into a 'StdStream', overwrite to 'CreatePipe'.
--
-- Examples:
--
-- >>> piping ($) NoStream
-- CreatePipe
--
piping :: IsCreatePipe a => Lens' a StdStream -> a -> a
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
handling :: IsUseHandle a => Lens' a StdStream -> Handle -> a -> a
handling l = set $ l . _UseHandle

-- | Given a lens into a 'StdStream', set to 'NoStream'
--
-- Examples:
--
-- >>> nostreaming ($) Inherit
-- NoStream
--
nostreaming :: IsNoStream a => Lens' a StdStream -> a -> a
nostreaming l = set l NoStream
