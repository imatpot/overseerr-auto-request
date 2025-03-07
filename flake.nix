{
  description = "Automatically request TV shows or movies on Overseerr.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        name = "overseerr-auto-requester";
        pkgs = nixpkgs.legacyPackages.${system};
        haskell-toolchain = with pkgs; [
          ghc
          cabal-install

          haskell-language-server
          hlint
          ormolu

          zlib
        ];
      in {
        packages.default = pkgs.haskellPackages.callCabal2nix name ./. { };

        devShells.default = pkgs.mkShell {
          inherit name;

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
