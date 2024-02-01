{
  description = "MarloweScan - An explorer for Marlowe contracts";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/23.05;

  outputs = { self, nixpkgs }:
    let pkgs = import nixpkgs { system = "x86_64-linux"; }; in
    with pkgs.haskellPackages;
 {
    defaultPackage.x86_64-linux =
      import ./shell.nix { nixpkgs = pkgs; tools = [ cabal-install stack ]; };
    devShells.x86_64-linux.default =
      (import ./shell.nix { nixpkgs = pkgs; tools = [ cabal-install stack ]; }).env;
 };

}
