module Build_doctests where

import Prelude

data Name = NameLib (Maybe String) | NameExe String deriving (Eq, Show)
data Component = Component Name [String] [String] [String] deriving (Eq, Show)

pkgs :: [String]
pkgs = ["-package-id=base-4.11.1.0","-package-id=filepath-1.4.2","-package-id=mcrlns-0.4.11.1-26526de2","-package-id=process-1.6.3.0","-package-id=dctst-0.16.1-b75060c6","-package=microlens-process-0.2.0.0"]

flags :: [String]
flags = ["-i","-i/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/build/autogen","-i/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/build","-i/Users/emilypi/haskell/lens-process/microlens/src","-hide-all-packages","-no-user-package-db","-package-db=/Users/emilypi/.cabal/store/ghc-8.4.3/package.db","-package-db=/Users/emilypi/haskell/lens-process/packagedb/ghc-8.4.3","-package-db=/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/package.conf.inplace","-optP-include","-optP/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/build/autogen/cabal_macros.h","--fast"]

module_sources :: [String]
module_sources = ["System.Process.Microlens","System.Process.Microlens.CreateProcess","System.Process.Microlens.CommandSpec","System.Process.Microlens.Optics","System.Process.Microlens.ProcessHandler","System.Process.Microlens.StdStream"]

-- [NameLib Nothing]
components :: [Component]
components = [Component (NameLib Nothing) ["-package-id=base-4.11.1.0","-package-id=filepath-1.4.2","-package-id=mcrlns-0.4.11.1-26526de2","-package-id=process-1.6.3.0","-package-id=dctst-0.16.1-b75060c6","-package=microlens-process-0.2.0.0"] ["-i","-i/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/build/autogen","-i/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/build","-i/Users/emilypi/haskell/lens-process/microlens/src","-hide-all-packages","-no-user-package-db","-package-db=/Users/emilypi/.cabal/store/ghc-8.4.3/package.db","-package-db=/Users/emilypi/haskell/lens-process/packagedb/ghc-8.4.3","-package-db=/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/package.conf.inplace","-optP-include","-optP/Users/emilypi/haskell/lens-process/build/x86_64-osx/ghc-8.4.3/microlens-process-0.2.0.0/build/autogen/cabal_macros.h","--fast"] ["System.Process.Microlens","System.Process.Microlens.CreateProcess","System.Process.Microlens.CommandSpec","System.Process.Microlens.Optics","System.Process.Microlens.ProcessHandler","System.Process.Microlens.StdStream"]]
