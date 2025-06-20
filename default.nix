{
  mkDerivation,
  base,
  bytestring,
  ConfigFile,
  containers ,
  data-default,
  directory,
  filepath,
  HTTP,
  http-types,
  mime-mail ,
  mtl,
  network,
  network-uri,
  scotty,
  snap-core,
  stdenv,
  text,
  wai,
}:
mkDerivation {
  pname = "tellme";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    bytestring
    ConfigFile
    containers
    data-default
    directory
    filepath
    HTTP
    http-types
    mime-mail
    mtl
    network
    network-uri
    scotty
    snap-core
    text
    wai
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
