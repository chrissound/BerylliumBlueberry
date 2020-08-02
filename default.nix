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
          rev = "18886b6276e61d62b9bfb5cfb71b8892c9fd2d30";
        }) {};
    };
  };
in
(myHaskellPackages.callCabal2nix "blog3000" (./.) {}).overrideAttrs (oldAttrs: {
  buildInputs = (oldAttrs.buildInputs or []) ++ [ pkgs.imagemagick ];
})
