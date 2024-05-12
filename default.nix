{ mkDerivation
, aeson
, base
, blaze-html
, blaze-markup
, bytestring
, directory
, file-embed
, http-conduit
, lib
, sqlite-simple
, time
}:
mkDerivation {
  pname = "traderjoes";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    blaze-html
    blaze-markup
    bytestring
    directory
    file-embed
    http-conduit
    sqlite-simple
    time
  ];
  license = "unknown";
  mainProgram = "traderjoes";
}
