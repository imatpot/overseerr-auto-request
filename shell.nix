let
  root = builtins.toString ./.;
  flake = builtins.getFlake ("git+file://" + root);
  system = builtins.currentSystem;
in flake.devShells.${system}.default
