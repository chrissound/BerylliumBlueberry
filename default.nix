{
  nixpkgs ? import <nixpkgs> {}
, nixpkgs-unstable ? import <unstable> {}
, sources ? import ./nix/sources.nix
, compiler ? "ghc865" } :
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
          rev = "76f8ad16abc7d3e5474df9ffe9672bdf8a6593f5";
        }) {};
    };
  };
in
(myHaskellPackages.callCabal2nix "blog3000" (./.) {}).overrideAttrs (oldAttrs: {
  buildInputs = (oldAttrs.buildInputs or []) ++ [ pkgs.imagemagick ];
})
