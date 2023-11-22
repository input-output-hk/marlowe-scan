{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc928", doBenchmark ? false }:

let
  pkgs = nixpkgs;
  f = { mkDerivation, base, blaze-html, gtk, lib, process
      , temporary, zlib, aeson, servant-server, wai, warp, haskell-language-server
      , hspec, hspec-wai, servant-blaze, http-client, wl-pprint
      , http-conduit, http-types, errors, zip-archive
      }:
      mkDerivation {
        pname = "marlowe-scan";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base blaze-html gtk process temporary zlib
          aeson servant-server wai warp
          haskell-language-server errors
          hspec hspec-wai servant-blaze
          http-client wl-pprint http-conduit http-types
          zip-archive
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
        shellHook = ''
          fix-hie () {
            gen-hie --stack > hie.yaml
          }
        '';
      };
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
