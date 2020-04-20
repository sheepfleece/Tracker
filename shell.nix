{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  tools   = with haskellPackages; [
    cabal-install
    ghcid
    hasktags
    ghc-core
    alex happy
    hoogle haddock

    pkgs.fish
  ];
  
  project = import ./release.nix;
in
  pkgs.mkShell {
    buildInputs = project.env.nativeBuildInputs ++ tools;
  }
