{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: Portable
--
-- 'CreateProcess' lenses and combinators
--
module System.Process.Lens.CreateProcess
( -- * Optics
  cmdspec_
, cwd_
, env_
, stdin
, stdout
, stderr
, closefds
, creategroup
, delegatectlc
, detachconsole
, newsession
, childgroup
, childuser
, useprocessjobs
  -- * Combinators
, stdoutOf
, stdinOf
, stderrOf
, clearing
, closing
) where


import Control.Lens

import Data.Functor (void)

import qualified System.IO as H
import System.Posix.Types
import System.Process
import System.Process.Lens.StdStream
import System.Process.Lens.CommandSpec


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
stdin :: Lens' CreateProcess StdStream
stdin = lens std_in (\t b -> t { std_in = b })

-- | Lens into the 'std_out' entry of the 'CreateProcess' record
--
stdout :: Lens' CreateProcess StdStream
stdout = lens std_out (\t b -> t { std_out = b })

-- | Lens into the 'std_err' entry of the 'CreateProcess' record
--
stderr :: Lens' CreateProcess StdStream
stderr = lens std_err (\t b -> t { std_err = b })

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
-- Combinators

-- | Retrieve the stdout handle associated with a 'CreateProcess'
--
stdinOf :: HasUseHandle a => CreateProcess -> Maybe H.Handle
stdinOf = preview $ stdin . _UsesHandle

-- | Retrieve the stdin handle associated with a 'CreateProcess'
--
stdoutOf :: HasUseHandle a => CreateProcess -> Maybe H.Handle
stdoutOf = preview $ stdout . _UsesHandle

-- | Retrieve the stderr handle associated with a 'CreateProcess'
--
stderrOf :: HasUseHandle a => CreateProcess -> Maybe H.Handle
stderrOf = preview $ stderr . _UsesHandle

-- | clear the stdin, stderr, and stdout streams of a 'CreateProcess', deferring to
-- 'Inherit'
--
clearing :: CreateProcess -> CreateProcess
clearing c = c
  & set stderr Inherit
  & set stdin Inherit
  & set stdout Inherit

-- | Close something with a prism into a non-standard 'Handle' in a 'CreateProcess'
--
closing :: HasUseHandle a => Getter CreateProcess a -> CreateProcess -> IO ()
closing l c = case c ^? l . _UsesHandle of
    Nothing -> return ()
    Just h -> go h
  where
    go h
      | h /= H.stdin
      , h /= H.stdout
      , h /= H.stderr = H.hClose h
      | otherwise = return ()