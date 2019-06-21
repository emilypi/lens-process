{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, filepath, lens, mtl, process
      , stdenv, stm, tasty
      }:
      mkDerivation {
        pname = "lens-process";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base directory filepath lens mtl process stm
        ];
        testHaskellDepends = [
          base directory filepath lens mtl process stm tasty
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
