{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

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
  d = haskellPackages.callPackage f {};
in d // { meta = d.meta // { mainProgram= "marlowe-scan-exe"; }; }
