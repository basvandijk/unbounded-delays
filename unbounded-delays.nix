{ cabal }:

cabal.mkDerivation (self: {
  pname = "unbounded-delays";
  version = "0.1.0.8";
  src = ./.;
  meta = {
    homepage = "https://github.com/basvandijk/unbounded-delays";
    description = "Unbounded thread delays and timeouts";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
