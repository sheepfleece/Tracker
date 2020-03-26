{ mkDerivation, base, persistent-sqlite, stdenv, yesod, yesod-bin
, yesod-static
}:
mkDerivation {
  pname = "imageViewer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base persistent-sqlite yesod yesod-bin yesod-static
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
