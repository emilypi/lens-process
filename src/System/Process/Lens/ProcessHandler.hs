{-# LANGUAGE CPP #-}
-- |
-- Module       : Sysetem.Process.Lens.ProcessHandler
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- Convenient data type with associated optics + isos for working
-- with the output of a 'createProcess' call.
--
module System.Process.Lens.ProcessHandler
( -- * Types
  ProcessHandler(..)
  -- * Isos
, _Handler
  -- * Lenses
, hstdin
, hstdout
, hstderr
, hhandle
) where


import Control.Lens

import System.IO
import System.Process


-- | A convenient handler for the output of a 'createProcess' call.
-- This data containes 4 components:
--
data ProcessHandler =
  ProcessHandler
    { _hstdin :: Maybe Handle
      -- ^ a handle to stdin if it was requested
    , _hstdout :: Maybe Handle
      -- ^ a handle to stdout if it was requested
    , _hstderr :: Maybe Handle
      -- ^ a handle to stderr if it was requested
    , _hhandle :: ProcessHandle
      -- ^ a process handle, containing a pid lock, information regarding
      -- ctcl-c delegation, and closed/open handle status info.
    }

-- | A lens into the stdin handle if requested
--
hstdin :: Lens' ProcessHandler (Maybe Handle)
hstdin = lens _hstdin (\t b -> t { _hstdin = b })

-- | A lens into the stdout handle if requested
--
hstdout :: Lens' ProcessHandler (Maybe Handle)
hstdout = lens _hstdout (\t b -> t { _hstdout = b })

-- | A lens into the stderr handle if requested
--
hstderr :: Lens' ProcessHandler (Maybe Handle)
hstderr = lens _hstderr (\t b -> t { _hstderr = b })

-- | A lens into the process handle
--
hhandle :: Lens' ProcessHandler ProcessHandle
hhandle = lens _hhandle (\t b -> t { _hhandle = b })

-- | An iso between the ProcessHandler data and its product
-- representation
--
_Handler :: Iso' ProcessHandler (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
_Handler = iso
  (\(ProcessHandler a b c p) -> (a,b,c,p))
  (\(a,b,c,p) -> ProcessHandler a b c p)
