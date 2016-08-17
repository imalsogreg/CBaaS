{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib   = dontCheck (self.callPackage (cabal2nixResult ../deps/reflex-dom-contrib) {});
     groundhog            = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog/groundhog) {});
     groundhog-th         = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog/groundhog-th) {});
     groundhog-ghcjs      = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog-ghcjs) {});
     groundhog-postgresql = null;
     cbaas-lib            = dontCheck (self.callPackage ../cbaas-lib {  });
  };
}
