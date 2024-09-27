{
  description = "A simple Flake for Haskell with Cabal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem 
      (system:
        let
          config = {};

          overlays = [
            (final: prev: {
              myHaskellPackages = final.haskellPackages.override {
                overrides = hfinal: hprev: {
                  haskell-playground-nix-flake = 
                    hfinal.callCabal2nix "haskell-playground" ./. {};
                };
              };

              haskell-playground-nix-flake = 
                final.haskell.lib.compose.justStaticExecutables
                  final.myHaskellPackages.haskell-playground-nix-flake;
              
              myDevShell = final.myHaskellPackages.shellFor {
                packages = p: [p.haskell-playground-nix-flake];

                nativeBuildInputs = [
                  final.cabal-install
                  final.haskellPackages.hlint
                  final.haskellPackages.ghc
                  final.haskellPackages.haskell-language-server
                  final.haskellPackages.ormolu
                ];
              };
            })

          ];

          pkgs = import nixpkgs { inherit config overlays system; };

        in
        {
          packages.default = pkgs.haskell-playground-nix-flake;

          devShells.default = pkgs.myDevShell;
        }
      );
}
 