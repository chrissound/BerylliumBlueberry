{
  nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
, compiler ? "ghc864" } :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      niobiumcoconut  = self.callCabal2nix "niobiumcoconut"
        (builtins.fetchGit {
          url = "https://github.com/chrissound/NiobiumCoconut.git";
          rev = "7a9c50a3de74568ab5dd7d15d1f90da754a7e11a";
        }) {};
    };
  };
in
(myHaskellPackages.callCabal2nix "blog3000" (./.) {}).overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs or [] ++ [ pkgs.imagemagick ];
})
