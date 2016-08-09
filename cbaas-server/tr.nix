{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});

in
reflex-platform.ghc.override {
  overrides = self: super: { 
     # reflex-dom-contrib  = (self.callPackage ../deps/reflex-dom-contrib { compilername = "ghc"; });
     # reflex-dom-contrib  = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     servant-snap        = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap) {});
     servant             = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant/servant) {});
     servant-docs        = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant/servant-docs) {});
     groundhog-th        = (self.callPackage ../deps/groundhog/groundhog-th { compilername = "ghc"; });
     servant-js = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant/servant-js) {});
     servant-foreign = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant/servant-foreign) {});
     servant-server = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant/servant-server) {});
     servant-blaze = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant/servant-blaze) {});

     websockets-snap = (self.callPackage (reflex-platform.cabal2nixResult ../deps/websockets-snap) {});

     snaplet-postgresql-simple = (self.callPackage (reflex-platform.cabal2nixResult ../deps/snaplet-postgresql-simple) {});
     hspec-snap  = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/hspec-snap) {});

     snap = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/snap) { async = self.async_2_0_2; });
     heist = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/heist) {});
     xmlhtml = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/xmlhtml) {});
     io-streams = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/io-streams) {});
     io-streams-haproxy = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
     snap-core = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/snap-core) {});
     snap-server  =reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/snap-server) { clock = self.clock_0_5_1; });
     snap-loader-static = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap-loader-static) {});
     snap-loader-dynamic = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap-loader-dynamic) {});
     cbaas-lib            = reflex-platform.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../cbaas-lib) {});
     # zlib-system = nixpkgs.zlib;
  };
}
