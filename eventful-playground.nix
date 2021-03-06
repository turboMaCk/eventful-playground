{ mkDerivation, aeson, base, eventful-core, eventful-memory
, servant, servant-server, servant-websockets, stdenv, stm, wai
, wai-cors, warp, websockets
}:
mkDerivation {
  pname = "eventful-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base eventful-core eventful-memory servant servant-server
    servant-websockets stm wai wai-cors warp websockets
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/turboMaCk/eventful-playground#readme";
  license = stdenv.lib.licenses.bsd3;
}
