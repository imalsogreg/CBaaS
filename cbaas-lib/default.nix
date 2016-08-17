{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, containers, deepseq, free, generics-sop, groundhog, groundhog-th, groundhog-ghcjs
, hspec, http-api-data, JuicyPixels, lens, mtl, QuickCheck, servant
, servant-docs, servant-js, stdenv, stm, text, time, transformers
, transformers-base, unagi-chan, unix, unordered-containers
, uri-bytestring, uuid-types, vector
}:
mkDerivation {
  pname = "cbaas-lib";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring containers deepseq
    free generics-sop groundhog groundhog-ghcjs groundhog-th http-api-data JuicyPixels
    lens mtl servant servant-docs servant-js stm text time transformers
    transformers-base unagi-chan unix unordered-containers
    uri-bytestring uuid-types vector
  ];
  testHaskellDepends = [
    aeson base generics-sop hspec JuicyPixels QuickCheck text groundhog-ghcjs
    unordered-containers vector
  ];
  homepage = "http://github.com/CBMM/CBaaS";
  description = "Center for Brains Minds and Machines as a Service";
  license = stdenv.lib.licenses.bsd3;
}
