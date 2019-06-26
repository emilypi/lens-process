{-# LANGUAGE CPP #-}
-- |
-- Module       : Sysetem.Process.Lens.Optics
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- Just the (classy) optics
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
  -- * Defaults
, defaultCreateProcess
) where


import Control.Lens

import System.IO
import System.Process


-- | A convenient handler for the output of a 'createProcess' call.
-- This data containes 4 components:
--
-- 1. a handle to stdin if it was requested
-- 2. a handle to stdout if it was requested
-- 3. a handle to stderr if it was requested
-- 4. a process handle, containing a pid lock, information regarding
--    ctcl-c delegation, and closed/open handle status info.
--
data ProcessHandler =
  ProcessHandler
    { _hstdin :: Maybe Handle
    , _hstdout :: Maybe Handle
    , _hstderr :: Maybe Handle
    , _hhandle :: ProcessHandle
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

-- | A default for a 'CreateProcess'
--
defaultCreateProcess :: CreateProcess
defaultCreateProcess =
  CreateProcess
    { cmdspec = ShellCommand ""
    , cwd = Nothing
    , env = Nothing
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = False
    , create_group = False
    , delegate_ctlc = False
    , new_session = False
#if MIN_VERSION_process(1, 3, 0)
    , detach_console = False
    , create_new_console = False
#endif
#if MIN_VERSION_process(1, 4, 0)
    , child_group = Nothing
    , child_user = Nothing
#endif
#if MIN_VERSION_process(1, 5, 0)
    , use_process_jobs = False
#endif
    }
