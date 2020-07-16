{
  nixpkgsRev ? "cfe68f2b68b7",
  ribRev ? "42bc7c3",
  pkgs ? import
    (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz")
    {}
}:
let
  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib) addBuildTools;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      rib = self.callCabal2nix "rib"
        (builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRev}.tar.gz")
        {};
    };
  };
in
haskellPackages.developPackage {
  name = "website";
  root = lib.cleanSource ./.;
  modifier = drv:
    addBuildTools drv [
      haskellPackages.cabal-install
      haskellPackages.ghcid
      haskellPackages.ormolu
      pkgs.curl
      pkgs.elmPackages.elm
      pkgs.fsatrace
      pkgs.jq
    ];
}
