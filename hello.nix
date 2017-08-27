{ mkDerivation, base, free, lens, megaparsec, optparse-applicative
, stdenv, text
}:
mkDerivation {
  pname = "hello";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base free lens megaparsec optparse-applicative text
  ];
  homepage = "https://github.com/bumbleblym/hello";
  description = "hello - not a simple \"print Hello World\" problem";
  license = stdenv.lib.licenses.bsd3;
}
