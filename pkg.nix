{ mkDerivation, base, checkers, cond, containers, criterion
, generic-lens, hspec, hspec-discover, parsers, QuickCheck, random
, split, stdenv, time, trifecta, vector
}:
mkDerivation {
  pname = "haskell-book-reading-group-exercises";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cond containers criterion generic-lens parsers random split
    time trifecta vector
  ];
  testHaskellDepends = [
    base checkers hspec hspec-discover QuickCheck
  ];
  testToolDepends = [ hspec-discover ];
  license = stdenv.lib.licenses.mit;
}
