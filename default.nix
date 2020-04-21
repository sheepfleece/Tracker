{ mkDerivation, aeson, attoparsec, base, blaze-builder, bytestring
, bytestring-strict-builder, classy-prelude-yesod, containers
, cryptohash-sha1, heredoc, http-types, io-streams, mysql-haskell
, network, persistent-sqlite, polysemy, shakespeare, stdenv, text
, time, timers, wai, yesod, yesod-auth, yesod-auth-oauth
}:
mkDerivation {
  pname = "tracker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base blaze-builder bytestring
    bytestring-strict-builder classy-prelude-yesod containers
    cryptohash-sha1 heredoc http-types io-streams mysql-haskell network
    persistent-sqlite polysemy shakespeare text time timers wai yesod
    yesod-auth yesod-auth-oauth
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
