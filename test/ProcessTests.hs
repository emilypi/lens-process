-- |
-- Module       : Main (tests)
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
module Main ( main ) where


import Control.Lens

import System.Process
import System.Process.Lens

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "Lens Process Tests"
  [ cmdSpecTests
  , createProcessTests
  , stdStreamTests
  ]

cmdSpecTests :: TestTree
cmdSpecTests = testGroup "CmdSpec tests"
  [ testGroup "optics are sound" csoptics
  , testGroup "instances are sound" csinstances
  , testGroup "combinators are sound" cscombinators
  ]
createProcessTests :: TestTree
createProcessTests = testGroup "CreateProcess tests"
  [ testCase "optics are sound" mempty
  , testCase "combinators are sound" mempty
  ]

stdStreamTests :: TestTree
stdStreamTests = testGroup "CmdSpec tests"
  [ testCase "optics are sound" mempty
  , testCase "instances are sound" mempty
  , testCase "combinators are sound" mempty
  ]

csoptics :: [TestTree]
csoptics =
  [ testCase "cmdspec optic" $ def ^. cmdspec_ .  _ShellCommand @?= ""
  , testCase "cwd optic" $ def ^. cwd_ @?= Nothing
  , testCase "std_in optic" $ def ^. stdin @?= Inherit
  , testCase "amend args" $ go ^. cmdspec_ . arguments @?= ["-l", "-a"]
  ]
  where
    go = def
      & cmdspec_ .~ RawCommand "ls" ["-l"]
      & cmdspec_ . arguments <>~ ["-a"]

cscombinators :: [TestTree]
cscombinators =
  [ testCase "arguing amends" $
    arguing "-a" (go ^. cmdspec_) @?= RawCommand "ls" ["-l", "-a"]
  ]
  where
    go = def & cmdspec_ .~ RawCommand "ls" ["-l"]

csinstances :: [TestTree]
csinstances =
  [ testCase "_Shell shells out" $
    def ^. cmdspec_ . _Shell @?= ""

  , testCase "_Raw is war_" $
    go ^. cmdspec_ . _Raw . traverse @?= ["-l"]
  ]
  where
    go = def & cmdspec_ .~ RawCommand "ls" ["-l"]

def :: CreateProcess
def = CreateProcess
  { cmdspec = ShellCommand ""
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , detach_console = False
  , create_new_console = False
  , new_session = False
  , child_group = Nothing
  , child_user = Nothing
  , use_process_jobs = False
  }
