let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      unboundedDelays = self.callPackage ./unbounded-delays.nix {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.unboundedDelays.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
       ] ++ hs.unboundedDelays.propagatedNativeBuildInputs
         ++ hs.unboundedDelays.extraBuildInputs)))
     ];
   }
