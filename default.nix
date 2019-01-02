{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    swinec = nixpkgs.haskellPackages.callPackage ./swinec {};
}
