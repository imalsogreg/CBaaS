{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib   = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     servant              = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
     servant-docs         = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs           = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/yaml-ghcjs) {});
     groundhog-th         = dontCheck (self.callPackage ../deps/groundhog/groundhog-th { compilername = "ghcjs"; });
     groundhog-postgresql = null;
     servant-js           = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-js) {});
     servant-foreign      = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-foreign) {});
     servant-server       = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-server) {});
     servant-matlab       = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-matlab) {});
     cbaas-lib            = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../cbaas-lib) {});
  };
}
