{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  ghc = haskellPackages.ghcWithPackages(hsPkgs: with hsPkgs; [ aeson hakyll ]);
in
mkShell {
  buildInputs = [
    curl
    ghc
    jq
    python3
    sassc
  ];
}
