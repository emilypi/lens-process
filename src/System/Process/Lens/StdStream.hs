{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : System.Process.Lens.StdStream
-- Copyright 	: (c) 2019-2021 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- This module provides the associated optics and combinators
-- for working with 'StdStream' objects. 'StdStream' consists of four
-- cases, for which we provide prisms and classy variants.
--
module System.Process.Lens.StdStream
( -- * Prisms
  _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
  -- * Classy Prisms
, AsInherit(..)
, AsUseHandle(..)
, AsCreatePipe(..)
, AsNoStream(..)
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

-- | A 'Prism'' into the 'Inherit' structure of a 'StdStream'
--
-- Examples:
--
-- >>> _Inherit # ()
-- Inherit
--
_Inherit :: Prism' StdStream ()
_Inherit = prism' (const Inherit) $ \case
  Inherit -> Just ()
  _ -> Nothing

-- | A 'Prism'' into the 'UseHandle' structure's Handle for a 'StdStream'
--
-- Examples:
--
--
-- >>> _UseHandle # System.stdin
-- UseHandle {handle: <stdin>}
--
_UseHandle :: Prism' StdStream Handle
_UseHandle = prism' UseHandle $ \case
  UseHandle t -> Just t
  _ -> Nothing

-- | A 'Prism'' into the 'CreatePipe' structure of a 'StdStream'
--
-- Examples:
--
-- >>> _CreatePipe # ()
-- CreatePipe
--
_CreatePipe :: Prism' StdStream ()
_CreatePipe = prism' (const CreatePipe) $ \case
  CreatePipe -> Just ()
  _ -> Nothing

-- | A prism into the 'NoStream' structure of a 'StdStream'
--
-- Examples:
--
-- >>> _NoStream # ()
-- NoStream
--
_NoStream :: Prism' StdStream ()
_NoStream = prism' (const NoStream) $ \case
  NoStream -> Just ()
  _ -> Nothing

-- ---------------------------------------------------------- --
-- Classes

-- | Class constraint proving a type has a prism into an 'Inherit'
-- structure. Any 'StdStream' will have a prism into `Inherit' -
-- it is just an overwrite to 'Inherit'
--
class AsInherit a where
  _Inherits :: Prism' a ()
  {-# MINIMAL _Inherits #-}

instance AsInherit StdStream where
  _Inherits = _Inherit
  {-# inline _Inherits #-}

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure.
--
class AsUseHandle a where
  _UsesHandle :: Prism' a Handle
  {-# MINIMAL _UsesHandle #-}

instance AsUseHandle StdStream where
  _UsesHandle = _UseHandle
  {-# inline _UsesHandle #-}

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure. Any 'StdStream' will have a prism into
-- 'CreatePipe' - it is just an overwrite to 'CreatePipe'
--
class AsCreatePipe a where
  _CreatesPipe :: Prism' a ()
  {-# MINIMAL _CreatesPipe #-}

instance AsCreatePipe StdStream where
  _CreatesPipe = _CreatePipe
  {-# inline _CreatesPipe #-}

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure. Any 'StdStream' will have a prism into
-- 'NoStream' - it is just an overwrite to 'NoStream'.
--
class AsNoStream a where
  _NoStreams :: Prism' a ()
  {-# MINIMAL _NoStreams #-}

instance AsNoStream StdStream where
  _NoStreams = _NoStream
  {-# inline _NoStreams #-}

-- ---------------------------------------------------------- --
-- Combinators

-- | Inject a handle into something with a prism into the handle
--
-- Examples:
--
-- >>> usehandleOf @StdStream System.stdin
-- UseHandle {handle: <stdin>}
--
usehandleOf :: AsUseHandle a => Handle -> a
usehandleOf = review _UsesHandle
{-# inline usehandleOf #-}

-- | Given a lens into a 'StdStream', overwrite to 'Inherit' so that
-- the stream inherits from its parent process
--
-- Examples:
--
-- >>> inheriting id CreatePipe
-- Inherit
--
inheriting :: Lens' a StdStream -> a -> a
inheriting l = set l Inherit
{-# inline inheriting #-}

-- | Given a lens into a 'StdStream', overwrite to 'CreatePipe'.
--
-- Examples:
--
-- >>> piping id NoStream
-- CreatePipe
--
piping :: Lens' a StdStream -> a -> a
piping l = set l CreatePipe
{-# inline piping #-}

-- | Given a lens into a 'StdStream' and a handle, set the handle using
-- 'UseHandle'. Note that this is the only really interesting case for anything
-- with a lens into a handle inculding 'StdStream'.
--
-- Examples:
--
--
-- >>> handling id System.stdin $ UseHandle System.stdout
-- UseHandle {handle: <stdin>}
--
-- >>> handling id System.stdin NoStream
-- NoStream
--
-- >>> handling id System.stdin Inherit
-- Inherit
--
handling :: Lens' a StdStream -> Handle -> a -> a
handling l = set (l . _UseHandle)
{-# inline handling #-}

-- | Given a lens into a 'StdStream', set to 'NoStream'
--
-- Examples:
--
-- >>> nostreaming id Inherit
-- NoStream
--
nostreaming :: Lens' a StdStream -> a -> a
nostreaming l = set l NoStream
{-# inline nostreaming #-}
