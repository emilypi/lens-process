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
  [ testCase "optics are sound" mempty
  , testCase "instances are sound" mempty
  , testCase "combinators are sound" mempty
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
