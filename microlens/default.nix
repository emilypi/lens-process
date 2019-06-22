{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, filepath, microlens, process, stdenv }:
      mkDerivation {
        pname = "microlens-process";
        version = "0.0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base filepath microlens process ];
        doHaddock = false;
        homepage = "https://github.com/emilypi/lens-process";
        description = "Micro-optics for the process library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
