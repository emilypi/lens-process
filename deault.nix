{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8630" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./lens-process.nix { }
