{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc90", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, base, blaze-html, gtk, lib, process
      , temporary, zlib, aeson, servant-server, wai, warp, haskell-language-server
      , hspec, hspec-wai, servant-blaze, http-client, wl-pprint
      , http-conduit, http-types, cmdargs
      }:
      mkDerivation {
        pname = "marlowe-explorer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base blaze-html gtk process temporary zlib
          aeson servant-server wai warp
          haskell-language-server cmdargs
          hspec hspec-wai servant-blaze
          http-client wl-pprint http-conduit http-types
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
