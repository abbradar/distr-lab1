{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages
}:

pkgs.lib.overrideDerivation (pkgs.lib.callPackageWith (pkgs // haskellPackages) ./default.nix { }) (x : {
  buildInputs = x.buildInputs ++ [ haskellPackages.cabalInstall haskellPackages.yesodBin ];
})
