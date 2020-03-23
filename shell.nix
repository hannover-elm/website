{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  ghc = haskellPackages.ghcWithPackages(hsPkgs: with hsPkgs; [ aeson hakyll ]);
in
mkShell {
  buildInputs = [
    curl
    elmPackages.elm
    ghc
    jq
    python3
    sassc
  ];
}
