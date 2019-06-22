{-# LANGUAGE LambdaCase #-}
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
-- 'StdStream' prisms and classy prisms.
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
) where

import Control.Lens

import System.IO (Handle)
import System.Process


-- ---------------------------------------------------------- --
-- Optics

-- | A prism into the 'Inherit' structure of a 'StdStream'
--
_Inherit :: Prism' StdStream StdStream
_Inherit = prism' (const Inherit) $ \case
  Inherit -> Just Inherit
  _ -> Nothing

-- | A prism into the 'UseHandle' structure's Handle for a 'StdStream'
--
_UseHandle :: Prism' StdStream Handle
_UseHandle = prism' UseHandle $ \case
  UseHandle t -> Just t
  _ -> Nothing

-- | A prism into the 'CreatePipe' structure of a 'StdStream'
--
_CreatePipe :: Prism' StdStream StdStream
_CreatePipe = prism' (const CreatePipe) $ \case
  CreatePipe -> Just CreatePipe
  _ -> Nothing

-- | A prism into the 'NoStream' structure of a 'StdStream'
--
_NoStream :: Prism' StdStream StdStream
_NoStream = prism' (const NoStream) $ \case
  NoStream -> Just NoStream
  _ -> Nothing

-- ---------------------------------------------------------- --
-- Classes

-- | Class constraint proving a type has a prism into an 'Inherit'
-- structure. In general, this seems redundant, however, this class
-- is a proof on `a` that `a` is an 'Inherit', which is a wonderful
-- thing to prove.
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
-- a 'UseHandle' structure.
--
class IsCreatePipe a where
  _CreatesPipe :: Prism' a StdStream
  {-# MINIMAL _CreatesPipe #-}

instance IsCreatePipe StdStream where
  _CreatesPipe = _CreatePipe

-- | Class constraint proving a type has a prism into a 'Handle' via
-- a 'UseHandle' structure.
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
usehandleOf :: IsUseHandle a => Handle -> a
usehandleOf h = _UsesHandle # h

