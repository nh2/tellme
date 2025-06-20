{
  mkDerivation,
  haskell,
  fetchFromGitHub,

  base,
  bytestring,
  ConfigFile,
  containers ,
  data-default,
  directory,
  filepath,
  HTTP,
  http-types,
  HUnit,
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
    # See https://github.com/jgoerzen/configfile/issues/13
    (haskell.lib.overrideCabal
      (haskell.lib.markUnbroken (haskell.lib.overrideSrc ConfigFile {
        src = fetchFromGitHub {
          owner = "jgoerzen";
          repo = "configfile";
          rev = "38ae3579dc1f2de70f9967247befe0e94deba269";
          hash = "sha256-N8O/DooHqL66t8gqwX3IT7gFw+loq+c0IB9DWkBNJWg=";
        };
      }))
      (old: {
        libraryHaskellDepends = (old.libraryHaskellDepends or []) ++ [
          HUnit
        ];
      })
    )
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
