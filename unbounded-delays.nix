{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "unbounded-delays";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/basvandijk/unbounded-delays";
  description = "Unbounded thread delays and timeouts";
  license = stdenv.lib.licenses.bsd3;
}
