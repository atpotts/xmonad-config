{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskell.packages.ghc822 }:
with pkgs;
stdenv.mkDerivation {
  name = "xmonad-config";
  buildInputs = [
   nix
   (haskellPackages.ghcWithPackages
        (haskellPackages: with haskellPackages;
        [
          cabal-install
          hindent
          xmonad
          xmonad-contrib
         ]))
    ];

}
