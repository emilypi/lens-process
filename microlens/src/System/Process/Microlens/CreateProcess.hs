{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : Sysetem.Process.Lens.CreateProcess
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies, Rank2Types
--
-- This module provides the associated optics and combinators
-- for working with 'CreateProcess' objects.
--
-- Because 'CreateProcess' was created before the `_` prefix record
-- name convention, some record accessors don't have an apparently
-- "good" name for their corresponding lens. Those that do not are
-- post-fixed with `_` i order to denote their lens status. Thankfully,
-- there are 6 that meet the criteria: 'cmdspec_', 'env_', 'cwd_', 'stdin_',
-- 'stdout_', and 'stderr_'.
--
-- We provide classy variants of what we consider the significant portions
-- of 'CreateProcess' - namely, the `std_in`, `std_out`, and `std_err` entries.
--
--
module System.Process.Microlens.CreateProcess
( -- * Lenses
  cmdspec_
, cwd_
, env_
, stdin_
, stdout_
, stderr_
, closefds
, creategroup
, createnewconsole
, delegatectlc
, detachconsole
, newsession
, childgroup
, childuser
, useprocessjobs
  -- * Classy Lenses
, HasStdin(..)
, HasStdout(..)
, HasStderr(..)
) where


import Lens.Micro

import qualified System.IO as H
import System.Posix.Types
import System.Process


-- ---------------------------------------------------------- --
-- Optics

-- | Lens into the 'cmdspec' entry of the 'CreateProcess' record
--
cmdspec_ :: Lens' CreateProcess CmdSpec
cmdspec_ = lens cmdspec (\t b -> t { cmdspec = b })

-- | Lens into the 'cwd' entry of the 'CreateProcess' record
--
cwd_ :: Lens' CreateProcess (Maybe FilePath)
cwd_ = lens cwd (\t b -> t { cwd = b })

-- | Lens into the 'env' entry of the 'CreateProcess' record
--
env_ :: Lens' CreateProcess (Maybe [(String, String)])
env_ = lens env (\t b -> t { env = b })

-- | Lens into the 'std_in' entry of the 'CreateProcess' record
--
stdin_ :: Lens' CreateProcess StdStream
stdin_ = lens std_in (\t b -> t { std_in = b })

-- | Lens into the 'std_out' entry of the 'CreateProcess' record
--
stdout_ :: Lens' CreateProcess StdStream
stdout_ = lens std_out (\t b -> t { std_out = b })

-- | Lens into the 'std_err' entry of the 'CreateProcess' record
--
stderr_ :: Lens' CreateProcess StdStream
stderr_ = lens std_err (\t b -> t { std_err = b })

-- | Lens into the 'close_fds' entry of the 'CreateProcess' record
--
closefds :: Lens' CreateProcess Bool
closefds = lens close_fds (\t b -> t { close_fds = b })

-- | Lens into the 'create_group' entry of the 'CreateProcess' record
--
creategroup :: Lens' CreateProcess Bool
creategroup = lens create_group (\t b -> t { create_group = b })

-- | Lens into the 'delegate_ctlc' entry of the 'CreateProcess' record
--
delegatectlc :: Lens' CreateProcess Bool
delegatectlc = lens delegate_ctlc (\t b -> t { delegate_ctlc = b })

-- | Lens into the 'detach_console' entry of the 'CreateProcess' record
--
detachconsole :: Lens' CreateProcess Bool
detachconsole = lens detach_console (\t b -> t { detach_console = b })

-- | Lens into the 'create_new_console' entry of the 'CreateProcess' record
--
createnewconsole :: Lens' CreateProcess Bool
createnewconsole = lens create_new_console (\t b -> t { create_new_console = b })

-- | Lens into the 'new_session' entry of the 'CreateProcess' record
--
newsession :: Lens' CreateProcess Bool
newsession = lens new_session (\t b -> t { new_session = b })

-- | Lens into the 'child_group' entry of the 'CreateProcess' record
--
childgroup :: Lens' CreateProcess (Maybe CGid)
childgroup = lens child_group (\t b -> t { child_group = b })

-- | Lens into the 'child_user' entry of the 'CreateProcess' record
--
childuser :: Lens' CreateProcess (Maybe CUid)
childuser = lens child_user (\t b -> t { child_user = b })

-- | Lens into the 'use_process_jobs' entry of the 'CreateProcess' record
--
useprocessjobs :: Lens' CreateProcess Bool
useprocessjobs = lens use_process_jobs (\t b -> t { use_process_jobs = b })

-- ---------------------------------------------------------- --
-- Classes

-- | Classy lens for types with a stdin
--
class HasStdin a where
  _Stdin :: Lens' a StdStream

instance HasStdin StdStream where
  _Stdin = id

instance HasStdin CreateProcess where
  _Stdin = stdin_

-- | Classy lens for types with a stdout
--
class HasStdout a where
  _Stdout :: Lens' a StdStream

instance HasStdout StdStream where
  _Stdout = id

instance HasStdout CreateProcess where
  _Stdout = stdout_

-- | Classy lens for types with a stderr
--
class HasStderr a where
  _Stderr :: Lens' a StdStream

instance HasStderr StdStream where
  _Stderr = id

instance HasStderr CreateProcess where
  _Stderr = stderr_
