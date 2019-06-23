{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Cabal, cabal-doctest, doctest, filepath
      , lens, process, stdenv, tasty, tasty-hunit
      }:
      mkDerivation {
        pname = "lens-process";
        version = "0.2.0.0";
        src = ./.;
        setupHaskellDepends = [ base Cabal cabal-doctest ];
        libraryHaskellDepends = [ base filepath lens process ];
        testHaskellDepends = [
          base doctest filepath lens process tasty tasty-hunit
        ];
        homepage = "https://github.com/emilypi/lens-process";
        description = "Optics for system processes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
