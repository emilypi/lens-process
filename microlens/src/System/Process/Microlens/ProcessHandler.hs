{-# LANGUAGE CPP #-}
-- |
-- Module       : System.Process.Microlens.ProcessHandler
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
module System.Process.Microlens.ProcessHandler
( -- * Types
  ProcessHandler(..)
  -- * Lenses
, hstdin
, hstdout
, hstderr
, hhandle
, _Handler
  -- * Defaults
, defaultCreateProcess
) where


import Lens.Micro

import System.IO
import System.Process


-- | A convenient handler for the output of a 'createProcess' call.
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

-- | Because 'microlens' doesn't export Iso types, we must drop down to lenses
-- and provide the isomorphism this way.
--
_Handler :: Lens' ProcessHandler (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
_Handler = lens
  (\(ProcessHandler a b c d) -> (a,b,c,d))
  (\_ (a,b,c,d) -> ProcessHandler a b c d)

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
