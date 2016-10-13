{ reflex-platform, ... }:
let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;
  nixpkgs = (import <nixpkgs> {});
in
reflex-platform.ghc.override {
  overrides = self: super: { 
     # inherit (nixpkgs.haskellPackages) lens clock lens-aeson wreq HUnit;
     servant-snap        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap) {});
     #groundhog-th        = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog/groundhog-th) {}); 
     #roundhog-postgresql = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog/groundhog-postgresql) {});
     groundhog = dontCheck (self.callPackage (cabal2nixResult ../../throwaway/groundhog/groundhog) {});
     groundhog-th = dontCheck (self.callPackage (cabal2nixResult ../../throwaway/groundhog/groundhog-th) {});
     groundhog-postgresql = dontCheck (self.callPackage (cabal2nixResult ../../throwaway/groundhog/groundhog-postgresql) {});
     groundhog-ghcjs = null;
     http-api-data = dontCheck (self.callPackage (cabal2nixResult ../deps/http-api-data) {});
  };
}
