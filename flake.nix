{
  description = "MarloweScan - An explorer for Marlowe contracts";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/23.05;

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      with haskellPackages;
      import ./shell.nix { nixpkgs = nixpkgs; haskellPackages = haskellPackages; tools = [ cabal-install stack ];};
    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      with haskellPackages;
      (import ./shell.nix { nixpkgs = nixpkgs; haskellPackages = haskellPackages; tools = [ cabal-install stack ];}).env;
 };

}
