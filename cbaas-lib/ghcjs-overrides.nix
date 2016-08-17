{ reflex-platform, ... }:
let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;
  nixpkgs = (import <nixpkgs> {});
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     groundhog = dontCheck (self.callPackage (cabal2nixResult ../../throwaway/groundhog/groundhog) {});
     groundhog-th = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog-ghcjs) {});
     groundhog-ghcjs = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog-ghcjs) {});
     groundhog-postgresql = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog-ghcjs) {});
  };
}
