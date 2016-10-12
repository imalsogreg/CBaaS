{ mkDerivation, aeson, base, bytestring, cbaas-lib, containers
, groundhog-th, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "cbaas-frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cbaas-lib containers groundhog-th reflex
    reflex-dom text
  ];
  executableHaskellDepends = [ base cbaas-lib reflex reflex-dom ];
  license = stdenv.lib.licenses.bsd3;
}
