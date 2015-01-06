{ cabal }:

cabal.mkDerivation (self: {
  pname = "unbounded-delays";
  version = "HEAD";
  src = ./.;
  meta = {
    homepage = "https://github.com/basvandijk/unbounded-delays";
    description = "Unbounded thread delays and timeouts";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
