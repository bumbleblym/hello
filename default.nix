{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc802" }:

nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hello.nix {}
