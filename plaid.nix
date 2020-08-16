{ mkDerivation, aeson, base, bytestring, casing, conduit
, conduit-extra, containers, either, errors, hspec, hspec-wai
, http-client, http-client-tls, http-conduit, http-types, microlens
, microlens-th, mtl, network, pretty-simple, QuickCheck
, raw-strings-qq, safe-exceptions, stdenv, text, time, transformers
, wai
}:
mkDerivation {
  pname = "plaid";
  version = "0.1.0.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring casing containers http-client-tls
    http-conduit microlens microlens-th mtl network pretty-simple
    raw-strings-qq safe-exceptions text time
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra either http-client
    http-client-tls microlens microlens-th mtl network pretty-simple
    safe-exceptions text time transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers errors hspec hspec-wai http-types
    microlens microlens-th pretty-simple QuickCheck text time wai
  ];
  homepage = "https://github.com/v0d1ch/plaid#readme";
  description = "Plaid.com api integration library";
  license = stdenv.lib.licenses.bsd3;
}
