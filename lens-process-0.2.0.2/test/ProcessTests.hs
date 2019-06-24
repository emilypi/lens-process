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
import System.Process.Lens.ProcessHandler

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "Lens Process Tests"
  [ cmdSpecTests
  ]

cmdSpecTests :: TestTree
cmdSpecTests = testGroup "CmdSpec tests"
  [ testGroup "optics are sound" csoptics
  , testGroup "instances are sound" csinstances
  , testGroup "combinators are sound" cscombinators
  ]

csoptics :: [TestTree]
csoptics =
  [ testCase "cmdspec optic" $ defaultCreateProcess ^. cmdspec_ .  _ShellCommand @?= ""
  , testCase "cwd optic" $ defaultCreateProcess ^. cwd_ @?= Nothing
  , testCase "std_in optic" $ defaultCreateProcess ^. stdin_ @?= Inherit
  , testCase "amend args" $ go ^. cmdspec_ . arguments @?= ["-l", "-a"]
  ]
  where
    go = defaultCreateProcess
      & cmdspec_ .~ RawCommand "ls" ["-l"]
      & cmdspec_ . arguments <>~ ["-a"]

cscombinators :: [TestTree]
cscombinators =
  [ testCase "arguing amends" $
    arguing "-a" (go ^. cmdspec_) @?= RawCommand "ls" ["-l", "-a"]
  ]
  where
    go = defaultCreateProcess & cmdspec_ .~ RawCommand "ls" ["-l"]

csinstances :: [TestTree]
csinstances =
  [ testCase "_Shell shells out" $
    defaultCreateProcess ^. cmdspec_ . _Shell @?= ""

  , testCase "_Raw is war_" $
    go ^. cmdspec_ . _Raw . traverse @?= ["-l"]
  ]
  where
    go = defaultCreateProcess & cmdspec_ .~ RawCommand "ls" ["-l"]
