{ stdenv, haskellPackages }:
let
  ghc = haskellPackages.ghcWithPackages (p: with p; [
    sqlite-simple
    http-conduit
    aeson
  ]);
in
stdenv.mkDerivation {
  name = "traderjoes";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    ghc -threaded ./Main.hs -o ./Main -Wall -Werror
  '';
  installPhase = ''
    install -Dm555 ./Main $out/bin/traderjoes
  '';
}
