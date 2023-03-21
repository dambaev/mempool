{ mkDerivation, lib, base
, hspec, text
, servant, servant-server, servant-client, servant-jsonrpc, servant-jsonrpc-client, servant-swagger, swagger2
, aeson, aeson-pretty
, bytestring
, lens
, warp
, scientific
, persistent, persistent-template, persistent-postgresql, monad-logger
, resource-pool
, cryptohash-sha256, base16-bytestring
, random
, async
, exceptions
, op-energy-api
, ...
}:
mkDerivation {
  pname = "op-energy-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    op-energy-api
    servant servant-server servant-client servant-jsonrpc servant-jsonrpc-client servant-swagger swagger2
    aeson aeson-pretty
    text bytestring
    lens
    scientific
    persistent persistent-template persistent-postgresql monad-logger
    resource-pool
    cryptohash-sha256 base16-bytestring
    random
    exceptions
  ];
  executableHaskellDepends = [ base warp async ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  enableLibraryForGhci = false;
  enableSeparateBinOutput = true;
  testHaskellDepends = [ base hspec text ];
  doBenchmark = false;
  doCheck = false;
  license = lib.licenses.bsd3;
}
