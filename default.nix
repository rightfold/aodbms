{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    aodbms = nixpkgs.callPackage ./aodbms {};
}
