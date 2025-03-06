{
  description = "A toolbox for language construction";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskell-toolchain = with pkgs; [
          ghc
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ];
      in {
        packages.default =
          pkgs.haskellPackages.callCabal2nix "overseerr-auto-requester" ./. { };

        devShells.default = pkgs.mkShell {
          name = "overseerr-auto-requester";

          buildInputs = haskell-toolchain;

          shellHook = ''
            export CABAL_VERSION=$(cabal --version | head -n 1 | cut -d ' ' -f 3)
            export GHC_VERSION=$(ghc --version | head -n 1 | cut -d ' ' -f 8)

            echo
            echo "> Cabal version:  $CABAL_VERSION"
            echo "> GHC version:    $GHC_VERSION"
            echo
          '';
        };
      });
}
