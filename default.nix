let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./unbounded-delays.nix {}
