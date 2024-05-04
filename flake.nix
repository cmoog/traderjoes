{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          formatter = with pkgs; writeShellScriptBin "fmt.sh" ''
            export PATH=$PATH:${lib.strings.makeBinPath [
              nixpkgs-fmt ormolu nodePackages.sql-formatter
            ]}
            nixpkgs-fmt .
            ormolu --mode inplace $(git ls-files "*.hs")
            for file in $(git ls-files "*.sql"); do
              sql-formatter --fix $file
            done
          '';
        in
        rec {
          inherit formatter;
          packages.default = pkgs.callPackage ./. { };
          devShells.default = with pkgs; mkShell {
            inputsFrom = [ packages.default ];
            packages = [
              haskell-language-server
              hlint
              nodePackages.wrangler
              sqlite
            ];
          };
        }
      ) // {
      nixosModules.default = import ./module.nix;
      overlays.default = final: prev: {
        traderjoes = prev.callPackage ./. { };
      };
    };
}
