{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        rec {
          formatter = pkgs.nixpkgs-fmt;
          packages.default = pkgs.callPackage ./. { };
          devShells.default = with pkgs; mkShell {
            packages = [
              haskell-language-server
              hlint
              nodePackages.wrangler
              packages.default.buildInputs
              sqlite
            ];
          };
        }
      );
}
