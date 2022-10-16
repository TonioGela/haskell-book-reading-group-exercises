let
  pkgs = import <nixpkgs> { };
 in {
  project = pkgs.haskellPackages.callPackage ./pkg.nix { };
  hpc-coveralls = pkgs.haskellPackages.callPackage ./hpc-coveralls.nix { };
}
