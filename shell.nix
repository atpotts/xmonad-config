{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskell.packages.ghc822 }:
with pkgs;
stdenv.mkDerivation {
  name = "xmonad-config";
  buildInputs = [
   nix
   hie.hie82
   (haskellPackages.ghcWithHoogle
        (haskellPackages: with haskellPackages;
        [
          cabal-install
          hindent
          xmonad
          xmonad-contrib
         ]))
    ];

}
