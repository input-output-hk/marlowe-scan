{ nixpkgs ? import <nixpkgs> {}, tools ? [] }:

let
  pkgs = nixpkgs;
  f = { mkDerivation, base, aeson, aeson-pretty, base16-bytestring, blaze-html,
          blaze-markup, bytestring, containers, errors, extra,
          file-embed, ghc, http-client, http-conduit, http-media,
          http-types, scientific, servant-blaze, servant-server,
          template-haskell, text, utf8-string, wai, warp, wl-pprint,
          zip-archive, zlib, haskell-language-server, lib, hspec, hspec-wai
      }:
      mkDerivation {
        pname = "marlowe-scan";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base aeson aeson-pretty base16-bytestring blaze-html
          blaze-markup bytestring containers errors extra
          file-embed ghc http-client http-conduit http-media
          http-types scientific servant-blaze servant-server
          template-haskell text utf8-string wai warp wl-pprint
          zip-archive zlib haskell-language-server hspec hspec-wai
        ] ++ tools;
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
        shellHook = ''
          fix-hie () {
            gen-hie --stack > hie.yaml
          }
        '';
      };
  d = pkgs.haskellPackages.callPackage f {};
in d // { meta = d.meta // { mainProgram= "marlowe-scan-exe"; }; }
