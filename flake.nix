{
  description = "Automatically request TV shows or movies on Overseerr.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        name = "overseerr-auto-request";
        pkgs = nixpkgs.legacyPackages.${system};

        toolchain = with pkgs; [
          ghc
          cabal-install

          haskell-language-server
          hlint
          ormolu

          zlib
          just
        ];

        drv = pkgs.haskellPackages.callCabal2nix name ./. { };

        runBin = bin:
          pkgs.runCommand bin { } ''
            mkdir -p $out/bin
            ln -s ${drv}/bin/${bin} $out/bin/
          '';
      in {
        packages = rec {
          default = overseerr-auto-request;
          overseerr-auto-request = runBin name;
          http-sink = runBin "http-sink";
        };

        devShells.default = pkgs.mkShell {
          inherit name;

          buildInputs = toolchain;

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
