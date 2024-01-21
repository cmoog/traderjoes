{ stdenv, haskellPackages }:
let
  ghc = haskellPackages.ghcWithPackages (p: with p; [
    sqlite-simple
    http-conduit
    aeson
  ]);
in
stdenv.mkDerivation {
  name = "tjoes-prices";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    ghc -threaded ./Main.hs -o ./Main -Wall -Werror
  '';
  installPhase = ''
    install -Dm555 ./Main $out/bin/tjoes-prices
  '';
}
