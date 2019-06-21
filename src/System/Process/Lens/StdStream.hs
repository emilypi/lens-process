{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
-- 'StdStream' prisms and classy prisms.
--
module System.Process.Lens.StdStream
( -- * Optics
  _Inherit
, _UseHandle
, _CreatePipe
, _NoStream
  -- * Classes
, HasInherit(..)
, HasUseHandle(..)
, HasCreatePipe(..)
, HasNoStream(..)
) where

import Control.Lens

import System.IO (Handle)
import System.Process


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

-- | Class constraint proving an 'a' has a prism into an 'Inherit'
-- structure. In general, this seems redundant, however, this class
-- is a proof on 'a' that 'a' is an 'Inherit', which is a wonderful
-- thing to prove.
class (a ~ StdStream) => HasInherit a where
  _Inherits :: Prism' a StdStream
  _Inherits = _Inherit

-- | Class constraint proving an 'a' has a prism into a 'Handle' via
-- a 'UseHandle' structure.
--
class (a ~ StdStream) => HasUseHandle a where
  _UsesHandle :: Prism' a Handle
  _UsesHandle = _UseHandle

-- | Class constraint proving an 'a' has a prism into a 'Handle' via
-- a 'UseHandle' structure.
--
class (a ~ StdStream) => HasCreatePipe a where
  _CreatesPipe :: Prism' a StdStream
  _CreatesPipe = _CreatePipe

-- | Class constraint proving an 'a' has a prism into a 'Handle' via
-- a 'UseHandle' structure.
--
class (a ~ StdStream) => HasNoStream a where
  _NoStreams :: Prism' a StdStream
  _NoStreams = _NoStream
