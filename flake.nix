{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.permittedInsecurePackages = [
            "curl-impersonate-0.5.4"
            "curl-impersonate-ff-0.5.4"
            "curl-impersonate-chrome-0.5.4"
          ];
        };
      in
      {
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
            haskellPackages.cabal-fmt
            curl-impersonate
          ];
        };
      }
    )
    // {
      nixosModules.default = import ./module.nix;
      overlays.default = final: prev: { traderjoes = final.haskellPackages.callPackage ./. { }; };
    };
}
