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
              nixpkgs-fmt haskellPackages.fourmolu nodePackages.sql-formatter
            ]}
            nixpkgs-fmt .
            fourmolu --mode inplace $(git ls-files '*.hs')
            for file in $(git ls-files "*.sql"); do
              sql-formatter --fix $file
            done
          '';
        in
        {
          inherit formatter;
          packages.default = pkgs.haskellPackages.callCabal2nix "traderjoes" ./. { };
          devShells.default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.default.env ];
            packages = with pkgs; [
              cabal-install
              haskell-language-server
              haskellPackages.fourmolu
              hlint
              nodePackages.wrangler
              sqlite
            ];
          };
        }
      ) // {
      nixosModules.default = import ./module.nix;
      overlays.default = final: prev: {
        traderjoes = final.haskellPackages.callPackage ./. { };
      };
    };
}
