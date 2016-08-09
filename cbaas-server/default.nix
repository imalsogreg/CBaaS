{ mkDerivation, aeson, async, base, base64-bytestring, basic-sop
, bytestring, cbaas-lib, clientsession, configurator, containers
, deepseq, directory, filepath, free, generics-sop, groundhog
, groundhog-postgresql, groundhog-th, heist, hspec, hspec-snap
, http-api-data, JuicyPixels, lens, map-syntax, monad-control
, monad-logger, mtl, optparse-applicative, process, QuickCheck
, readable, resource-pool, servant, servant-docs, servant-js
, servant-snap, snap, snap-core
, snap-loader-dynamic, snap-loader-static, snap-server
, snaplet-postgresql-simple, stdenv, stm, text, time, transformers
, transformers-base, unagi-chan, unix, unordered-containers
, uri-bytestring, uuid, vector, websockets, websockets-snap, wreq
}:
mkDerivation {
  pname = "cbaas-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base64-bytestring basic-sop bytestring cbaas-lib
    configurator containers deepseq directory filepath free
    generics-sop groundhog groundhog-th heist http-api-data JuicyPixels
    lens map-syntax monad-control mtl optparse-applicative process
    resource-pool servant servant-docs servant-js snap
    snap-core stm text time transformers transformers-base unagi-chan
    unix uri-bytestring uuid vector websockets websockets-snap
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring basic-sop bytestring cbaas-lib
    clientsession configurator containers deepseq directory filepath
    generics-sop groundhog groundhog-postgresql groundhog-th heist
    http-api-data JuicyPixels lens map-syntax monad-control
    monad-logger mtl optparse-applicative process readable
    resource-pool servant servant-snap snap snap-core
    snap-loader-dynamic snap-loader-static snap-server
    snaplet-postgresql-simple stm text time transformers
    transformers-base unix uri-bytestring uuid vector websockets
    websockets-snap wreq
  ];
  testHaskellDepends = [
    aeson base basic-sop cbaas-lib generics-sop hspec hspec-snap
    JuicyPixels QuickCheck text unordered-containers vector
  ];
  homepage = "http://github.com/CBMM/CBaaS";
  description = "Center for Brains Minds and Machines as a Service";
  license = stdenv.lib.licenses.bsd3;
}
