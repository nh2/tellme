{ mkDerivation, base, bytestring, ConfigFile, containers, directory
, fastcgi, filepath, HTTP, mime-mail, mtl, network, network-uri
, snap-core, stdenv, text
}:
mkDerivation {
  pname = "tellme";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring ConfigFile containers directory fastcgi filepath
    HTTP mime-mail mtl network network-uri snap-core text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
