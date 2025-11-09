{ sources ? import ./nix/sources.nix
, compiler ? "ghc902"
} :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  src = pkgs.lib.cleanSourceWith {
    filter = name: type: !(pkgs.lib.hasSuffix ".cabal" name);
    src = ./.;
  };
  myHaskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: rec {
        niobiumcoconut-lucid  = import (builtins.fetchGit {
          url = "ssh://root@trycatchchris.co.uk:/root/gitrepo/NiobiumCoconut-Lucid";
          rev = "9aa6aa32d9b8559e15fd4a4600f811eff6d5ce29";
        }) {
          sources = sources;
          compiler = compiler;
          niobiumcoconut' = import (builtins.fetchGit {
            url = "ssh://root@trycatchchris.co.uk:/root/gitrepo/NiobiumCoconut";
            rev = "dd523ce64eaf14eefb1ae6298656124aeece475b";
          });
        };
        niobiumcoconut = import (builtins.fetchGit {
            url = "ssh://root@trycatchchris.co.uk:/root/gitrepo/NiobiumCoconut";
            rev = "dd523ce64eaf14eefb1ae6298656124aeece475b";
          }) { sources = sources; compiler = compiler; };
        };
    };
in
myHaskellPackages.callCabal2nix "HaskellNixCabalStarter" (src) {}
